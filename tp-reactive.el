;;; tp-reactive.el --- Reactive state storage and registration for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Reactive core of tp: storage for variable dependencies, watchers,
;; computed properties and data variables; registration/unregistration;
;; the variable-watcher shell and the batching queue.  The actual
;; re-rendering of buffers lives in tp-render.el, which installs
;; itself via `tp--reactive-update-function' / `tp--reactive-flush-function'.

;;; Code:

(require 'cl-lib)
(require 'tp-core)

(defvar tp-reactive-deps nil
  "Alist mapping reactive variables to dependent layers.
Each element: (VAR-SYMBOL . ((LAYER-NAME . REACTIVE-PROPS) ...)).")

(defvar tp-layer-watchers nil
  "Alist of layer watchers: (LAYER-NAME . ((VAR-SYMBOL . CALLBACK) ...)).")

(defvar tp-layer-computed nil
  "Alist of computed properties: (LAYER-NAME . ((VAR-SYMBOL . COMPUTE-FN) ...)).")

(defvar tp-layer-data nil
  "Alist of data variables: (LAYER-NAME . (VAR-SYMBOL ...)).")

(defvar tp--batch-update-pending nil
  "Queue of deferred reactive buffer re-renders.
Each entry is a list (LAYER-NAME CHANGED-SYMBOLS WHERE TP-TEXT-AFFECTED).
Entries are created and widened by `tp--queue-batch-update'.")

(defvar tp--batch-update-active nil
  "When non-nil, we are inside a `tp-with-batch-updates' form.")

(defvar tp--reactive-updating nil
  "Non-nil while a reactive update is being applied.
Used as a reentrancy guard: when a variable is set from within an
update (a computed variable being written, or the tp-text two-way
sync), the nested change still updates the variable, but its
re-render is queued in `tp--batch-update-pending' and flushed after
the outermost update completes instead of recursing.")

(defconst tp--compute-error (make-symbol "tp--compute-error")
  "Sentinel distinguishing a failed compute from a legitimate nil result.
Compute functions may legitimately return nil (e.g. a boolean feeding
`invisible'), so error paths return this uninterned sentinel instead
of nil.")

(defun tp--queue-batch-update (layer-name symbol where tp-text-affected)
  "Queue a deferred re-render of LAYER-NAME in `tp--batch-update-pending'.
SYMBOL is the changed variable, WHERE the buffer for buffer-local
changes (nil for global ones), TP-TEXT-AFFECTED non-nil when the
change touches the layer's `tp-text'.  When the layer already has a
pending entry, the entry is widened to the union of both changes:
SYMBOL is added, TP-TEXT-AFFECTED is sticky (once set it stays set)
and WHERE widens to nil (all buffers) as soon as two changes disagree
on it."
  (let ((existing (assoc layer-name tp--batch-update-pending)))
    (if existing
        (progn
          (unless (memq symbol (nth 1 existing))
            (setf (nth 1 existing) (cons symbol (nth 1 existing))))
          (unless (eq (nth 2 existing) where)
            (setf (nth 2 existing) nil))
          (when tp-text-affected
            (setf (nth 3 existing) t)))
      (push (list layer-name (list symbol) where (and tp-text-affected t))
            tp--batch-update-pending))))

(defun tp--register-reactive-deps (layer-name reactive-symbols props)
  "Register REACTIVE-SYMBOLS as dependencies for LAYER-NAME.
PROPS is the original property specification with reactive symbols.
Only the reactive portions of the properties are stored for each variable."
  ;; Register each reactive symbol's dependency with only its relevant properties
  (dolist (rsym reactive-symbols)
    (let* ((var-sym (tp--reactive-var-symbol rsym))
           ;; Extract only the properties that use this specific reactive variable
           (reactive-props (tp--extract-reactive-props props rsym))
           (existing (assoc var-sym tp-reactive-deps)))
      (if existing
          ;; Update or add this layer to existing dependencies
          (let ((layer-entry (assoc layer-name (cdr existing))))
            (if layer-entry
                ;; Update existing entry with new reactive-props
                (setf (cdr layer-entry) reactive-props)
              ;; Add new layer entry
              (push (cons layer-name reactive-props) (cdr existing))))
        ;; Create new dependency entry and add watcher
        (push (cons var-sym (list (cons layer-name reactive-props))) tp-reactive-deps)
        ;; Add variable watcher for this variable
        (unless (boundp var-sym) (set var-sym nil))
        (add-variable-watcher var-sym #'tp--reactive-variable-watcher)))))

(defun tp--unregister-reactive-deps (layer-name)
  "Unregister all reactive dependencies for LAYER-NAME."
  ;; Collect variables that need watcher removal
  (let ((vars-to-clean nil))
    ;; First pass: remove layer from dependencies and collect empty vars
    (dolist (dep tp-reactive-deps)
      (let ((var-sym (car dep)))
        (setf (cdr dep) (assq-delete-all layer-name (cdr dep)))
        ;; If no more dependencies, mark for watcher removal
        (when (null (cdr dep))
          (push var-sym vars-to-clean))))
    ;; Remove watchers for variables with no dependencies
    (dolist (var-sym vars-to-clean)
      (remove-variable-watcher var-sym #'tp--reactive-variable-watcher)))
  ;; Clean up empty dependency entries
  (setq tp-reactive-deps
        (cl-remove-if (lambda (dep) (null (cdr dep))) tp-reactive-deps))
  ;; Also clean up layer watchers, computed properties, and data
  (tp--unregister-layer-watchers layer-name)
  (tp--unregister-layer-computed layer-name)
  (tp--unregister-layer-data layer-name))

(defun tp--layer-has-reactive-deps-p (layer-name)
  "Return non-nil if LAYER-NAME has reactive dependencies registered.
Layers with reactive deps need tp-name for reactive tracking."
  (cl-some (lambda (dep)
             (assoc layer-name (cdr dep)))
           tp-reactive-deps))

(defvar tp--reactive-update-function nil
  "Function applying a reactive update to layer definitions and buffers.
Installed by tp-render.el.  Called with (LAYER-NAME REACTIVE-PROPS
SYMBOL NEWVAL WHERE OVERRIDE-ALIST) after the user watch callbacks
have run.  When nil, variable changes only invoke watch callbacks and
no re-rendering happens.")

(defvar tp--reactive-flush-function nil
  "Function flushing one pending batched update entry.
Installed by tp-render.el.  Called with (LAYER-NAME WHERE
TP-TEXT-AFFECTED).")

(defun tp--reactive-variable-watcher (symbol newval operation where)
  "Watcher function called when a reactive variable changes.
SYMBOL is the variable that changed.
NEWVAL is the new value being set.
OPERATION is the type of operation (set, let, unlet, makunbound, defvaralias).
WHERE indicates where the variable was set:
  - nil for global `setq' or `set'
  - a buffer for `setq-local'
Updates all layers that depend on this variable.

Only `set' operations trigger updates because:
- `let'/`unlet': Temporary bindings that will be restored, no need to update UI
- `makunbound': Variable is being undefined, not a value change
- `defvaralias': Aliasing, the actual value change will trigger a separate `set'

When `tp--batch-update-active' is non-nil, buffer updates are deferred until
the batch completes. Layer definitions are still updated immediately.

Uses `tp--equal-including-string-properties' for comparison to properly detect
changes in text properties when the text content is the same.

The actual recomputation and buffer re-rendering is delegated to
`tp--reactive-update-function', installed by tp-render.el."
  (when (and (not (tp--equal-including-string-properties
                   (when (boundp symbol)
                     (symbol-value symbol))
                   newval))
             (eq operation 'set))
    (tp-debug-log "Variable %s changed: %S -> %S (where: %s)"
                  symbol (when (boundp symbol) (symbol-value symbol)) newval
                  (if where (buffer-name where) "global"))
    (let ((deps (cdr (assoc symbol tp-reactive-deps)))
          (oldval (when (boundp symbol) (symbol-value symbol)))
          ;; Create override alist with the new value
          ;; (watcher is called before the variable is actually updated)
          (override-alist (list (cons symbol newval))))
      (dolist (dep deps)
        (let ((layer-name (car dep))
              ;; Get the reactive props stored directly in the dependency
              (reactive-props (cdr dep)))
          ;; Call user-defined watch callbacks for this layer
          (tp--invoke-layer-watchers layer-name symbol newval oldval)
          ;; Delegate recomputation and re-rendering to the update engine
          (when tp--reactive-update-function
            (funcall tp--reactive-update-function
                     layer-name reactive-props symbol newval
                     where override-alist)))))))

(defun tp--invoke-layer-watchers (layer-name symbol newval oldval)
  "Invoke all registered watcher callbacks for LAYER-NAME watching SYMBOL.
NEWVAL is the new value, OLDVAL is the old value."
  (when-let ((watchers (cdr (assoc layer-name tp-layer-watchers))))
    (dolist (watcher watchers)
      (let ((watch-sym (car watcher))
            (callback (cdr watcher)))
        (when (eq watch-sym symbol)
          (tp-debug-log "  Invoking watcher for %s on %s" watch-sym layer-name)
          (condition-case err
              (funcall callback newval oldval layer-name)
            (error (message "tp: watcher error for %s watching %s: %s"
                            layer-name watch-sym err))))))))

(defun tp--flush-batch-updates ()
  "Flush all pending batch updates.
This processes all updates collected during a `tp-with-batch-updates' form."
  (tp-debug-log "Flushing %d pending batch updates" (length tp--batch-update-pending))
  (let ((processed-layers nil))
    ;; Process each pending update, avoiding duplicate layer updates
    (dolist (pending (nreverse tp--batch-update-pending))
      (let ((layer-name (car pending))
            (where (caddr pending))
            (tp-text-affected (cadddr pending)))
        (unless (memq layer-name processed-layers)
          (push layer-name processed-layers)
          (tp-debug-log "  Batch updating layer %s (tp-text: %s)"
                        layer-name (if tp-text-affected "yes" "no"))
          (when tp--reactive-flush-function
            (funcall tp--reactive-flush-function
                     layer-name where tp-text-affected))))))
  (setq tp--batch-update-pending nil))

(defmacro tp-with-batch-updates (&rest body)
  "Execute BODY with reactive updates batched.
Multiple variable changes within BODY are collected and applied
together at the end, avoiding redundant buffer modifications.

This is useful when changing multiple reactive variables simultaneously:

  (tp-with-batch-updates
    (setq my-color \"red\")
    (setq my-size 14)
    (setq my-text \"Hello\"))

Without batching, each `setq' would trigger a separate buffer update.
With batching, all updates are consolidated and applied once at the end."
  (declare (indent 0) (debug t))
  `(let ((tp--batch-update-active t)
         (tp--batch-update-pending nil))
     (tp-debug-log "Starting batch updates")
     (unwind-protect
         (progn ,@body)
       (tp-debug-log "Ending batch updates")
       (tp--flush-batch-updates))))

(defun tp--register-layer-watchers (layer-name watchers)
  "Register WATCHERS for LAYER-NAME.
WATCHERS is a list of (VAR-SYMBOL CALLBACK) pairs."
  (when watchers
    (let ((watcher-pairs
           (mapcar (lambda (watcher)
                     (cons (car watcher) (cadr watcher)))
                   watchers)))
      (if (assoc layer-name tp-layer-watchers)
          (setf (cdr (assoc layer-name tp-layer-watchers)) watcher-pairs)
        (push (cons layer-name watcher-pairs) tp-layer-watchers)))))

(defun tp--register-layer-computed (layer-name computed)
  "Register COMPUTED variable definitions for LAYER-NAME.
COMPUTED is a list of (VAR-SYMBOL COMPUTE-FN) pairs."
  (when computed
    (let ((computed-pairs
           (mapcar (lambda (comp)
                     (cons (car comp) (cadr comp)))
                   computed)))
      (if (assoc layer-name tp-layer-computed)
          (setf (cdr (assoc layer-name tp-layer-computed)) computed-pairs)
        (push (cons layer-name computed-pairs) tp-layer-computed)))))

(defun tp--unregister-layer-watchers (layer-name)
  "Unregister all watchers for LAYER-NAME."
  (setq tp-layer-watchers (assq-delete-all layer-name tp-layer-watchers)))

(defun tp--unregister-layer-computed (layer-name)
  "Unregister all computed properties for LAYER-NAME."
  (setq tp-layer-computed (assq-delete-all layer-name tp-layer-computed)))

(defun tp--apply-initial-computed (compute)
  "Apply initial computed values using COMPUTE definitions.
COMPUTE is a list of (VAR-SYMBOL COMPUTE-FN) pairs.
Sets the global variables to their computed values.
A compute function returning nil is a legitimate result and is
applied; only computes that signal an error are skipped (see
`tp--compute-error')."
  (dolist (comp compute)
    (let* ((var-sym (car comp))
           (compute-fn (cadr comp))
           (val (condition-case err
                    (funcall compute-fn)
                  (error
                   (message "tp: initial compute error for %s: %s" var-sym err)
                   tp--compute-error))))
      (unless (eq val tp--compute-error)
        (set var-sym val)))))

(defun tp--data-var-symbol (data-entry)
  "Extract the variable symbol from DATA-ENTRY.
DATA-ENTRY can be a symbol or a cons cell (SYMBOL . INITIAL-VALUE)."
  (if (consp data-entry)
      (car data-entry)
    data-entry))

(defun tp--register-layer-data (layer-name data-vars)
  "Register DATA-VARS for LAYER-NAME.
DATA-VARS is a list of variable symbols or cons cells (SYMBOL . INITIAL-VALUE).
Also adds variable watchers so changes to data vars trigger computed updates."
  (when data-vars
    ;; Extract just the symbols for storage
    (let ((var-symbols (mapcar #'tp--data-var-symbol data-vars)))
      (if (assoc layer-name tp-layer-data)
          (setf (cdr (assoc layer-name tp-layer-data)) var-symbols)
        (push (cons layer-name var-symbols) tp-layer-data))
      ;; Add watchers for data variables
      (dolist (var-sym var-symbols)
        (let ((existing (assoc var-sym tp-reactive-deps)))
          (if existing
              ;; Add this layer to existing dependencies
              ;; (with nil props since data vars don't have direct props)
              (let ((layer-entry (assoc layer-name (cdr existing))))
                (unless layer-entry
                  (push (cons layer-name nil) (cdr existing))))
            ;; Create new dependency entry and add watcher
            (push (cons var-sym (list (cons layer-name nil))) tp-reactive-deps)
            (unless (boundp var-sym) (set var-sym nil))
            (add-variable-watcher var-sym #'tp--reactive-variable-watcher)))))))

(defun tp--unregister-layer-data (layer-name)
  "Unregister data variables for LAYER-NAME."
  (setq tp-layer-data (assq-delete-all layer-name tp-layer-data)))

(defun tp--ensure-reactive-variables (var-symbols)
  "Ensure all VAR-SYMBOLS are defined as global variables.
VAR-SYMBOLS can be a list of symbols or cons cells (SYMBOL . INITIAL-VALUE).
If a variable is not bound, define it with the initial value (nil if
not specified).
If a variable has an explicit initial value (cons cell), always update
it to allow re-definition to change initial values."
  (dolist (sym var-symbols)
    (let* ((is-cons (and (consp sym) (not (tp--reactive-symbol-p sym))))
           (var-sym (cond
                     (is-cons (car sym))
                     ((tp--reactive-symbol-p sym)
                      (tp--reactive-var-symbol sym))
                     (t sym)))
           (initial-val (if is-cons (cdr sym) nil)))
      (if is-cons
          ;; For explicit initial values, always update (allows re-definition)
          (set var-sym initial-val)
        ;; For implicit initial values, only set if not already bound
        (unless (boundp var-sym)
          (set var-sym initial-val))))))

;;;###autoload
(defun tp-reactive-reset ()
  "Reset all reactive text property watchers and dependencies."
  (interactive)
  ;; Remove all variable watchers
  (dolist (dep tp-reactive-deps)
    (let ((var-sym (car dep)))
      (remove-variable-watcher var-sym #'tp--reactive-variable-watcher)))
  ;; Clear all registries
  (setq tp-reactive-deps nil)
  (setq tp-layer-watchers nil)
  (setq tp-layer-computed nil)
  (setq tp-layer-data nil))

(provide 'tp-reactive)
;;; tp-reactive.el ends here
