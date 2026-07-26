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
;; the variable-watcher shell and the batching queue state.  The
;; actual re-rendering of buffers - including the queue flush and the
;; public `tp-with-batch-updates' macro - lives in tp-render.el, which
;; installs itself via `tp--reactive-update-function'.

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

(defvar tp--layer-buffers (make-hash-table :test 'equal)
  "Hash table mapping layer names to buffers showing their regions.
Keys are layer names; values are lists of buffers registered via
`tp-reactive--register-layer-buffer'.  Reactive updates walk only
these buffers instead of scanning `buffer-list' (see
`tp-reactive-layer-buffers').  A key holding an empty list means
\"known: no buffer shows this layer\", which is distinct from an
absent key (`unknown').")

(defvar tp--layer-buffers-hook-installed nil
  "Non-nil once the registry's `kill-buffer-hook' pruner is installed.")

(defun tp-reactive--install-kill-buffer-hook ()
  "Install the global `kill-buffer-hook' pruning the buffer registry.
Idempotent; guarded by `tp--layer-buffers-hook-installed'."
  (unless tp--layer-buffers-hook-installed
    (add-hook 'kill-buffer-hook #'tp-reactive--prune-killed-buffer)
    (setq tp--layer-buffers-hook-installed t)))

(defun tp-reactive--prune-killed-buffer ()
  "Drop the buffer being killed from `tp--layer-buffers'.
Runs on `kill-buffer-hook' with the dying buffer current.  The layer
entries themselves are kept: an entry left with an empty list means
\"known: no buffer shows this layer\", not `unknown'."
  (let ((buf (current-buffer)))
    (maphash (lambda (layer bufs)
               (when (memq buf bufs)
                 (puthash layer (delq buf bufs) tp--layer-buffers)))
             tp--layer-buffers)))

(defun tp-reactive--register-layer-buffer (layer-name buffer)
  "Register BUFFER as showing regions of layer LAYER-NAME.
Idempotent: registering the same live BUFFER again keeps a single
entry.  Dead buffers and a nil LAYER-NAME are ignored.  Installs the
`kill-buffer-hook' pruner on first use.  See
`tp-reactive-layer-buffers' for the consumer side of the registry."
  (when (and layer-name (buffer-live-p buffer))
    (tp-reactive--install-kill-buffer-hook)
    (let ((bufs (gethash layer-name tp--layer-buffers)))
      (unless (memq buffer bufs)
        (puthash layer-name (cons buffer bufs) tp--layer-buffers)))))

(defun tp-reactive-layer-buffers (layer-name)
  "Return the live buffers registered as showing layer LAYER-NAME.
Return a list of live buffers - possibly empty, meaning \"known: no
buffer shows this layer\" - or the symbol `unknown' when LAYER-NAME
has no registry entry at all.  Killed buffers still recorded in the
registry are dropped lazily by this accessor.

KNOWN GAP: inserting an already-propertized STRING into a buffer
bypasses the buffer operations that register buffers, so such a
buffer is missing here until a reactive update's full-scan fallback
finds it or `tp-reactive-track-buffer' is called on it."
  (let ((bufs (gethash layer-name tp--layer-buffers 'unknown)))
    (if (eq bufs 'unknown)
        'unknown
      (let ((live (cl-remove-if-not #'buffer-live-p bufs)))
        (unless (= (length live) (length bufs))
          (puthash layer-name live tp--layer-buffers))
        live))))

(defun tp-reactive--buffer-layer-names (&optional buffer)
  "Return the layer names present in BUFFER, in buffer order.
BUFFER defaults to the current buffer; a dead BUFFER yields nil.
Stack-aware: a layer counts as present when its name is the direct
`tp-name' text property of a run (the rendered top layer) or the
`tp-name' of any layer plist inside the run's `tp-layers'
stack-storage property (layers buried below the top, or hidden - see
tp-stack.el).  The `tp-layers' value is read as a plain list of
plists, so this helper stays below the stack module.  Names are
deduplicated with `equal'.  This is the shared scan behind
`tp-reactive-track-buffer' and the anonymous-layer GC's liveness
test `tp--buffer-has-layer-region-p'."
  (let ((buf (or buffer (current-buffer)))
        (found nil))
    (when (buffer-live-p buf)
      (tp--map-intervals
       buf nil nil
       (lambda (_start _end props)
         (let ((direct (plist-get props 'tp-name)))
           (when (and direct (not (member direct found)))
             (push direct found)))
         (dolist (layer (plist-get props 'tp-layers))
           (let ((name (plist-get layer 'tp-name)))
             (when (and name (not (member name found)))
               (push name found)))))))
    (nreverse found)))

;;;###autoload
(defun tp-reactive-track-buffer (&optional buffer)
  "Scan BUFFER for layer regions and register it in the buffer registry.
BUFFER defaults to the current buffer.  Walk BUFFER's text-property
runs and register BUFFER for every layer name found - rendered top
layers (direct `tp-name') as well as layers inside `tp-layers' stack
storage (buried below another layer, or hidden) - so reactive updates
visit it without a full `buffer-list' scan.

Call this after inserting an already-propertized string into a
buffer: string application bypasses the buffer operations that
register buffers (see `tp-reactive-layer-buffers'), and this command
closes that gap.  Return the list of layer names registered, in
buffer order."
  (interactive)
  (let* ((buf (or buffer (current-buffer)))
         (found (tp-reactive--buffer-layer-names buf)))
    (dolist (name found)
      (tp-reactive--register-layer-buffer name buf))
    (when (called-interactively-p 'interactive)
      (message "tp: tracking %d layer(s) in %s"
               (length found) (buffer-name buf)))
    found))

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
  (tp--unregister-layer-data layer-name)
  ;; Drop the layer's buffer-registry entry: an undefined (or about to
  ;; be redefined) layer must not linger as stale "known" state; the
  ;; next update or refresh falls back to a learning full scan.
  (remhash layer-name tp--layer-buffers))

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
  (setq tp-layer-data nil)
  ;; Drop queued re-renders too: entries stranded by an error escaping
  ;; an update would otherwise survive the reset and replay against
  ;; freshly (re)defined layers on the next flush (ARCH-4).
  (setq tp--batch-update-pending nil)
  (clrhash tp--layer-buffers))

(provide 'tp-reactive)
;;; tp-reactive.el ends here
