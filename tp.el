;;; tp.el --- Text Properties manipulation library for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Version: 0.1.0
;; Keywords: convenience text-properties
;; Author: Geekinney (kinneyzhang666@gmail.com)
;; Package-Requires: ((emacs "28.1") (dash "2.19.1"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; tp.el provides a comprehensive text property manipulation library with:
;;
;; Architecture (5 layers, bottom to top):
;;   1. Basic utilities: argument parsing, plist operations, interval handling
;;   2. Core property operations: tp-set, tp-get, tp-at, tp-remove, tp-clear
;;   3. Layer system: multi-layer property stacks with tp-push-layer, tp-pop-layer
;;   4. Reactive system: automatic updates when variables change
;;   5. High-level API: pattern matching, search and navigation
;;
;; See ARCHITECTURE.md for detailed function call hierarchy.
;;
;; Inspired by https://github.com/emacsorphanage/ov
;; Requires Emacs 28.1+ for `object-intervals' function.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)

;;;============================================================================
;;; Layer 1: Global Variables and Configuration
;;;============================================================================

;;; --- Layer Definition Storage ---

(defgroup tp nil
  "Group for tp.el text property manipulation."
  :prefix "tp-"
  :group 'development)

(defvar tp-layer-alist nil
  "Alist of layer definitions: (LAYER-NAME . PROPERTIES).")

(defvar tp-layer-groups nil
  "Alist of layer groups: (GROUP-NAME . (LAYER-NAME1 LAYER-NAME2 ...)).")

;;; --- Reactive System Storage ---

(defvar tp-reactive-deps nil
  "Alist mapping reactive variables to dependent layers.
Each element: (VAR-SYMBOL . ((LAYER-NAME . REACTIVE-PROPS) ...)).")

(defvar tp-layer-watchers nil
  "Alist of layer watchers: (LAYER-NAME . ((VAR-SYMBOL . CALLBACK) ...)).")

(defvar tp-layer-computed nil
  "Alist of computed properties: (LAYER-NAME . ((VAR-SYMBOL . COMPUTE-FN) ...)).")

(defvar tp-layer-data nil
  "Alist of data variables: (LAYER-NAME . (VAR-SYMBOL ...)).")

(defvar tp--anonymous-layer-counter 0
  "Counter for generating unique anonymous layer names.")

;;; --- Debug Mode ---

(defcustom tp-debug-mode nil
  "When non-nil, enable debug logging for reactive updates.
Debug messages are logged to the *tp-debug* buffer and optionally
displayed in the minibuffer based on `tp-debug-echo' setting."
  :type 'boolean
  :group 'tp)

(defcustom tp-debug-echo nil
  "When non-nil and `tp-debug-mode' is enabled, also echo debug messages.
If nil, debug messages are only logged to the *tp-debug* buffer."
  :type 'boolean
  :group 'tp)

(defvar tp-layer-transforms nil
  "Alist of layer transforms: (LAYER-NAME . TRANSFORM-FN).
TRANSFORM-FN receives the value and returns the transformed value.
Used for tp-text transformations like formatting numbers or dates.")

;;; --- Batched Updates ---

(defvar tp--batch-update-pending nil
  "When non-nil, reactive updates are being batched.
This is a list of (LAYER-NAME . CHANGED-VARS) pairs pending update.")

(defvar tp--batch-update-active nil
  "When non-nil, we are inside a `tp-with-batch-updates' form.")

;;; --- Built-in Text Property Names ---

(defconst tp--builtin-text-properties
  '(;; Display and appearance
    face font-lock-face mouse-face display invisible intangible
    ;; Interaction and help
    help-echo cursor keymap local-map pointer
    ;; Stickiness
    front-sticky rear-nonsticky
    ;; Text modification
    read-only insert-in-front-hooks insert-behind-hooks
    modification-hooks point-entered point-left
    ;; Font and composition
    fontified composition hard cursor-intangible
    ;; Line properties
    line-height line-spacing wrap-prefix line-prefix
    ;; Field and input
    field inhibit-line-move-field-capture
    ;; Button and widget
    button category follow-link action
    ;; Syntax and parsing
    syntax-table
    ;; Misc
    yank-handler auto-composed evaporate face-alias)
  "List of built-in Emacs text property names.
These property names are reserved and cannot be used as layer names in `define-tp'.
An error is signaled at macro expansion time (when the `define-tp' form is
evaluated) if a reserved name is used, preventing the layer definition from
being created.")

(defun tp--builtin-text-property-p (name)
  "Return non-nil if NAME is a built-in text property name.
NAME should be a symbol."
  (memq name tp--builtin-text-properties))

;;;============================================================================
;;; Layer 1: Basic Utility Functions
;;;============================================================================

;;; --- Debug Logging ---

(defun tp-debug-log (format-string &rest args)
  "Log a debug message if `tp-debug-mode' is enabled.
FORMAT-STRING and ARGS are passed to `format'."
  (when tp-debug-mode
    (let ((msg (apply #'format format-string args))
          (timestamp (format-time-string "%H:%M:%S.%3N")))
      (with-current-buffer (get-buffer-create "*tp-debug*")
        (goto-char (point-max))
        (insert (format "[%s] %s\n" timestamp msg)))
      (when tp-debug-echo
        (message "[tp] %s" msg)))))

(defun tp-debug-clear ()
  "Clear the *tp-debug* buffer."
  (interactive)
  (when-let ((buf (get-buffer "*tp-debug*")))
    (with-current-buffer buf
      (erase-buffer))))

(defun tp-debug-show ()
  "Show the *tp-debug* buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*tp-debug*")))

;;; --- Anonymous Layer Generation ---

(defun tp--generate-anonymous-layer-name ()
  "Generate a unique symbol for anonymous reactive layers."
  (setq tp--anonymous-layer-counter (1+ tp--anonymous-layer-counter))
  (intern (format "tp-anon-%d" tp--anonymous-layer-counter)))

;;; --- Buffer Utility ---

(defmacro tp-with-current-buffer (buffer-or-name &rest body)
  "Execute BODY in BUFFER-OR-NAME with `inhibit-read-only' bound to t."
  (declare (indent defun))
  `(with-current-buffer ,buffer-or-name
     (let ((inhibit-read-only t))
       ,@body)))

;;;============================================================================
;;; Layer 1: Interval and Property Inspection
;;;============================================================================

(defun tp-intervals (start end &optional object)
  "Return list of property intervals from START to END in OBJECT.
Each element is (START END PROPERTIES). OBJECT defaults to current buffer."
  (let ((intervals (object-intervals (or object (current-buffer)))))
    (mapcar (lambda (tp)
              (let* ((tp-start (- (nth 0 tp) (if (stringp object) 0 start)))
                     (tp-end (- (nth 1 tp) (if (stringp object) 0 start)))
                     (tp-props (nth 2 tp)))
                (list tp-start tp-end tp-props)))
            (seq-filter (lambda (tp)
                          (and (< (nth 0 tp) (if (stringp object) end (+ start end)))
                               (> (nth 1 tp) (if (stringp object) start 0))))
                        intervals))))

(defun tp-empty-p (&optional object)
  "Return t if OBJECT has no text properties.
OBJECT can be string or buffer; nil means current buffer."
  (null (object-intervals (or object (current-buffer)))))

(defun tp-plist (start-or-string &optional end object)
  "Return merged plist of all properties from START to END in OBJECT.
With single STRING argument, return properties of entire string."
  (let (start-pos end-pos obj)
    (if (stringp start-or-string)
        (setq start-pos 0
              end-pos (length start-or-string)
              obj start-or-string)
      (setq start-pos start-or-string
            end-pos end
            obj object))
    (let ((result nil))
      (dolist (interval (tp-intervals start-pos end-pos obj))
        (let ((props (nth 2 interval)))
          (cl-loop for (key val) on props by #'cddr
                   do (setq result (plist-put result key val)))))
      result)))

;;;============================================================================
;;; Layer 1: Plist Deep Merge and Nested Access
;;;============================================================================

(defun tp--deep-merge-plist (base new)
  "Deep merge NEW plist into BASE plist.
For nested plists (starting with keyword), recursively merge.
NEW values override BASE values."
  (let ((result (copy-sequence base)))
    (cl-loop
     for (key val) on new by #'cddr
     do (let ((base-val (plist-get result key)))
          (setq result
                (plist-put
                 result key
                 (cond
                  ;; Both are plists - recursively merge
                  ((and (listp val) (keywordp (car-safe val))
                        (listp base-val) (keywordp (car-safe base-val)))
                   (tp--deep-merge-plist base-val val))
                  ;; Otherwise use new value
                  (t val))))))
    result))

(defun tp--merge-face-values (face1 face2)
  "Merge two face values into one.
FACE1 is the earlier value, FACE2 is the later value.
For face plists (like (:foreground \"red\")), merge with later overriding.
For symbol faces, create a list with FACE2 taking precedence.
Returns the merged face value."
  (cond
   ;; No earlier face - just use later face
   ((null face1) face2)
   ;; No later face - just use earlier face
   ((null face2) face1)
   ;; Both are plists - merge with later overriding earlier
   ((and (listp face1) (keywordp (car-safe face1))
         (listp face2) (keywordp (car-safe face2)))
    (tp--deep-merge-plist face1 face2))
   ;; Later is a plist, earlier is a symbol or list of faces
   ((and (listp face2) (keywordp (car-safe face2)))
    (cond
     ((symbolp face1)
      (list face2 face1))
     ((listp face1)
      (cons face2 face1))
     (t face2)))
   ;; Earlier is a plist, later is a symbol
   ((and (listp face1) (keywordp (car-safe face1))
         (symbolp face2))
    (list face2 face1))
   ;; Later is a symbol - prepend to earlier
   ((symbolp face2)
    (cond
     ((symbolp face1)
      (if (eq face1 face2)
          face2
        (list face2 face1)))
     ((listp face1)
      (if (member face2 face1)
          (cons face2 (remove face2 face1))  ; Move to front
        (cons face2 face1)))
     (t face2)))
   ;; Later is a list of faces - prepend to earlier
   ((listp face2)
    (cond
     ((symbolp face1)
      (if (member face1 face2)
          face2
        (append face2 (list face1))))
     ((listp face1)
      (append face2
              (cl-remove-if (lambda (f) (member f face2)) face1)))
     (t face2)))
   (t face2)))

(defun tp--merge-duplicate-keys (plist)
  "Merge duplicate keys in PLIST into a single key-value pair.
For `face' and `font-lock-face' properties, values are merged so that
later values take precedence over earlier ones for the same sub-properties.
For other properties, later values override earlier ones.

This function is designed for single-call property setting where multiple
properties of the same type can be specified and should be merged.

Example:
  (tp--merge-duplicate-keys \\='(face bold face (:foreground \"red\")))
  => (face ((:foreground \"red\") bold))

  (tp--merge-duplicate-keys \\='(face (:background \"blue\") face (:foreground \"red\")))
  => (face (:background \"blue\" :foreground \"red\"))

  (tp--merge-duplicate-keys \\='(prop1 a prop2 b prop1 c))
  => (prop1 c prop2 b)"
  (let ((key-values (make-hash-table :test 'eq))
        (key-order nil))
    ;; Collect all values for each key in order
    (cl-loop for (key val) on plist by #'cddr
             do (progn
                  (unless (gethash key key-values)
                    (push key key-order))
                  (puthash key
                           (cons val (gethash key key-values))
                           key-values)))
    ;; Reverse key-order to get original order
    (setq key-order (nreverse key-order))
    ;; Build result plist by merging values for each key
    (let ((result nil))
      (dolist (key key-order)
        (let ((values (nreverse (gethash key key-values))))  ; Reverse to get original order
          (if (= (length values) 1)
              ;; Single value - use as-is
              (setq result (append result (list key (car values))))
            ;; Multiple values - merge them
            (let ((merged-val
                   (cond
                    ;; Face properties - use special face merging
                    ((memq key '(face font-lock-face mouse-face))
                     (cl-reduce #'tp--merge-face-values values))
                    ;; Other properties - later overrides earlier
                    (t (car (last values))))))
              (setq result (append result (list key merged-val)))))))
      result)))

(defun tp--get-nested (value path)
  "Get nested value from VALUE following PATH (list of keys).
Supports plists, alists, and list-of-keys extraction."
  (if (null path)
      value
    (let* ((key (car path))
           (rest (cdr path))
           (is-plist-like (and (listp value)
                               (or (keywordp (car value))
                                   (and (symbolp (car value))
                                        (cdr value)
                                        (keywordp (cadr value))))))
           (next-value
            (cond
             ;; Key is a list - extract multiple keys
             ((and (listp key) (not (null key)))
              (when is-plist-like
                (let ((result nil)
                      (plist-part (if (keywordp (car value)) value (cdr value))))
                  (dolist (k key)
                    (let ((v (plist-get plist-part k)))
                      (when v (setq result (plist-put result k v)))))
                  result)))
             ;; Value is plist-like
             (is-plist-like
              (plist-get (if (keywordp (car value)) value (cdr value)) key))
             ;; Value is alist
             ((and (listp value) (consp (car value)))
              (cdr (assoc key value)))
             ;; Other list types
             ((listp value)
              (or (plist-get value key)
                  (cdr (assoc key value))
                  (cl-loop for spec in value
                           when (and (listp spec) (eq (car spec) key))
                           return (if (= (length (cdr spec)) 1) (cadr spec) (cdr spec))
                           when (and (listp spec) (keywordp (car spec)))
                           thereis (plist-get spec key))))
             (t nil))))
      (tp--get-nested next-value rest))))

;;;============================================================================
;;; Layer 4: Reactive System - Symbol Detection and Resolution
;;;============================================================================

(defun tp--reactive-symbol-p (sym)
  "Return non-nil if SYM is a reactive variable symbol (starts with $)."
  (and (symbolp sym)
       (string-prefix-p "$" (symbol-name sym))))

(defun tp--reactive-var-symbol (sym)
  "Convert a reactive symbol SYM (e.g., $foo) to its variable symbol (e.g., foo).
Returns nil if SYM is not a reactive symbol."
  (when (tp--reactive-symbol-p sym)
    (intern (substring (symbol-name sym) 1))))

(defun tp--collect-reactive-symbols (form)
  "Recursively collect all reactive symbols ($-prefixed) from FORM.
Returns a list of reactive symbols found."
  (cond
   ((tp--reactive-symbol-p form)
    (list form))
   ((consp form)
    (append (tp--collect-reactive-symbols (car form))
            (tp--collect-reactive-symbols (cdr form))))
   (t nil)))

(defun tp--extract-reactive-value (val reactive-var)
  "Extract only the parts of VAL that use REACTIVE-VAR.
If VAL is a plist, recursively extract only the key-value pairs containing REACTIVE-VAR.
If VAL directly contains REACTIVE-VAR, return VAL as-is.
REACTIVE-VAR should be the $-prefixed symbol (e.g., $my-color)."
  (cond
   ;; If val is the reactive var itself, return it
   ((eq val reactive-var) val)
   ;; If val is a plist (starts with a keyword), extract reactive parts recursively
   ((and (listp val) (keywordp (car val)))
    (let ((result nil))
      (cl-loop for (key subval) on val by #'cddr
               when (member reactive-var (tp--collect-reactive-symbols subval))
               do (setq result
                        (plist-put result key
                                   (tp--extract-reactive-value subval reactive-var))))
      result))
   ;; Otherwise return val as-is if it contains the reactive var
   (t val)))

(defun tp--extract-reactive-props (plist reactive-var)
  "Extract only the properties from PLIST that use REACTIVE-VAR.
Returns a plist containing only the key-value pairs that reference REACTIVE-VAR.
For nested plists, only the sub-properties containing REACTIVE-VAR are included.
REACTIVE-VAR should be the $-prefixed symbol (e.g., $my-color)."
  (let ((result nil))
    (cl-loop for (key val) on plist by #'cddr
             when (member reactive-var (tp--collect-reactive-symbols val))
             do (setq result
                      (plist-put result key
                                 (tp--extract-reactive-value val reactive-var))))
    result))

(defun tp--resolve-reactive-symbols (form &optional override-alist)
  "Recursively resolve all reactive symbols in FORM to their values.
Reactive symbols ($foo) are replaced with the value of the variable foo.
OVERRIDE-ALIST is an optional alist of (SYMBOL . VALUE) pairs that
override the current variable values (used during watcher callbacks)."
  (cond
   ((tp--reactive-symbol-p form)
    (let* ((var-sym (tp--reactive-var-symbol form))
           (override (assoc var-sym override-alist)))
      (if override
          (cdr override)
        (if (boundp var-sym)
            (symbol-value var-sym)
          nil))))
   ((consp form)
    (cons (tp--resolve-reactive-symbols (car form) override-alist)
          (tp--resolve-reactive-symbols (cdr form) override-alist)))
   (t form)))

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

(defun tp--reactive-variable-watcher (symbol newval operation where)
  "Watcher function called when a reactive variable changes.
SYMBOL is the variable that changed.
NEWVAL is the new value being set.
OPERATION is the type of operation (set, let, unlet, makunbound, defvaralias).
WHERE indicates where the variable was set:
  - nil for global `setq' or `set'
  - a buffer for `setq-local'
Updates all layers that depend on this variable.

Only 'set' operations trigger updates because:
- 'let'/'unlet': Temporary bindings that will be restored, no need to update UI
- 'makunbound': Variable is being undefined, not a value change
- 'defvaralias': Aliasing, the actual value change will trigger a separate 'set'

When `tp--batch-update-active' is non-nil, buffer updates are deferred until
the batch completes. Layer definitions are still updated immediately."
  (when (and (not (equal (symbol-value symbol) newval))
             (eq operation 'set))
    (tp-debug-log "Variable %s changed: %S -> %S (where: %s)"
                  symbol (symbol-value symbol) newval
                  (if where (buffer-name where) "global"))
    (let ((deps (cdr (assoc symbol tp-reactive-deps)))
          (oldval (symbol-value symbol))
          ;; Create override alist with the new value
          ;; (watcher is called before the variable is actually updated)
          (override-alist (list (cons symbol newval))))
      (dolist (dep deps)
        (let* ((layer-name (car dep))
               ;; Get the reactive props stored directly in the dependency
               (reactive-props (cdr dep))
               ;; Check if tp-text is affected by this variable
               (tp-text-affected (plist-member reactive-props 'tp-text)))
          ;; Call user-defined watch callbacks for this layer
          (tp--invoke-layer-watchers layer-name symbol newval oldval)
          ;; Update computed properties for this layer
          (let ((updated-override
                 (tp--update-layer-computed layer-name override-alist)))
            (when reactive-props
              ;; Resolve the reactive props with the new value override
              (let ((resolved-props (tp--resolve-reactive-symbols
                                     reactive-props updated-override)))
                ;; Update only the reactive properties in the layer definition
                (let ((current-props (cdr (assoc layer-name tp-layer-alist))))
                  (when current-props
                    ;; Deep merge the resolved reactive props into the current layer props
                    ;; This preserves nested plist values (like face properties)
                    (setq current-props (tp--deep-merge-plist current-props resolved-props))
                    (tp--set-layer-props layer-name current-props))))))
          ;; Update text regions with this layer (or defer if batching)
          (if tp--batch-update-active
              ;; Batching: defer the buffer update
              ;; Pending format: (layer-name symbols-list where tp-text-affected)
              (let ((existing (assoc layer-name tp--batch-update-pending)))
                (tp-debug-log "  Deferring buffer update for %s (batch mode)" layer-name)
                (if existing
                    ;; Update existing entry: add symbol if not present
                    (let ((symbols (nth 1 existing)))
                      (unless (memq symbol symbols)
                        (setf (nth 1 existing) (cons symbol symbols))))
                  ;; Create new entry
                  (push (list layer-name (list symbol) where tp-text-affected)
                        tp--batch-update-pending)))
            ;; Normal: update immediately
            (tp-debug-log "  Updating layer %s (tp-text affected: %s)"
                          layer-name (if tp-text-affected "yes" "no"))
            (if tp-text-affected
                (tp--update-reactive-text layer-name where)
              (tp--update-layer-regions layer-name where))))))))

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

;;; --- Batched Updates ---

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
          (if tp-text-affected
              (tp--update-reactive-text layer-name where)
            (tp--update-layer-regions layer-name where))))))
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

(defun tp--update-layer-computed (layer-name override-alist)
  "Update computed reactive variables for LAYER-NAME with OVERRIDE-ALIST.
Evaluates compute functions and updates the reactive variable values.
Returns an updated override-alist with the new computed values."
  (when-let ((computed (cdr (assoc layer-name tp-layer-computed))))
    (dolist (comp computed)
      (let* ((var-sym (car comp))
             (compute-fn (cdr comp))
             ;; Temporarily bind variables to their new values from override-alist
             ;; before calling the compute function
             (computed-val
              (condition-case err
                  (cl-progv
                      (mapcar #'car override-alist)
                      (mapcar #'cdr override-alist)
                    (funcall compute-fn))
                (error
                 (message "tp: compute error for %s.%s: %s"
                          layer-name var-sym err)
                 nil))))
        (when computed-val
          ;; Update the global variable
          (set var-sym computed-val)
          ;; Add to override-alist for property resolution
          (push (cons var-sym computed-val) override-alist)
          ;; Also update the layer properties if the computed var is used in props
          (let ((current-props (cdr (assoc layer-name tp-layer-alist))))
            (when current-props
              ;; Collect all reactive props for this layer from tp-reactive-deps
              (let ((all-reactive-props nil))
                (dolist (dep tp-reactive-deps)
                  (let ((layer-entry (assoc layer-name (cdr dep))))
                    (when (and layer-entry (cdr layer-entry))
                      ;; Merge the reactive props
                      (cl-loop for (key val) on (cdr layer-entry) by #'cddr
                               do (setq all-reactive-props
                                        (plist-put all-reactive-props key val))))))
                (when all-reactive-props
                  (let ((resolved-props (tp--resolve-reactive-symbols
                                         all-reactive-props override-alist)))
                    (when resolved-props
                      (cl-loop for (key val) on resolved-props by #'cddr
                               do (setq current-props (plist-put current-props key val)))
                      (tp--set-layer-props layer-name current-props)))))))))))
  override-alist)

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
Sets the global variables to their computed values."
  (dolist (comp compute)
    (let* ((var-sym (car comp))
           (compute-fn (cadr comp))
           (val (condition-case err
                    (funcall compute-fn)
                  (error
                   (message "tp: initial compute error for %s: %s" var-sym err)
                   nil))))
      (when val
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
If a variable is not bound, define it with the initial value (nil if not specified).
If a variable has an explicit initial value (cons cell), always update it to allow
re-definition to change initial values."
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

(defun tp--update-layer-regions (layer-name &optional where)
  "Update text regions that have LAYER-NAME applied.
Re-applies the layer properties using tp-search-map and tp-add.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update all buffers that have the text property (setq case)."
  (let ((props (tp-layer-props layer-name t)))  ; include tp-name for reactive tracking
    (when props
      ;; Callback for tp-search-map: applies props to matched region.
      ;; _TEXT is unused (the matched text), START and END are buffer positions.
      ;; Returns nil to prevent tp-search-map from replacing the text.
      (let ((apply-props-fn (lambda (_text start end)
                              (tp-add start end props)
                              nil)))
        (if (and where (bufferp where) (buffer-live-p where))
            ;; setq-local case: only update the specific buffer
            (tp-with-current-buffer where
              (save-excursion
                (tp-search-map apply-props-fn 'tp-name layer-name)))
          ;; setq case: update all buffers that have the text property
          (dolist (buf (buffer-list))
            (when (buffer-live-p buf)
              (tp-with-current-buffer buf
                (save-excursion
                  (tp-search-map apply-props-fn 'tp-name layer-name))))))))))

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

;;;============================================================================
;;; Layer 4: Reactive Text (tp-text property)
;;;============================================================================

(defun tp--find-tp-text-reactive-var (layer-name)
  "Find the reactive variable symbol used for tp-text in LAYER-NAME.
Returns the variable symbol (e.g., tp-test-counter) if tp-text uses a
reactive variable (e.g., $tp-test-counter), or nil if not found.
Searches through `tp-reactive-deps' to find the original reactive props."
  (catch 'found
    (dolist (dep tp-reactive-deps)
      (let* ((var-sym (car dep))
             (layer-entry (assoc layer-name (cdr dep))))
        (when layer-entry
          (let ((reactive-props (cdr layer-entry)))
            ;; Check if tp-text in reactive-props uses this variable
            (when (plist-member reactive-props 'tp-text)
              (let ((tp-text-val (plist-get reactive-props 'tp-text)))
                ;; Check if tp-text-val is a reactive symbol for this variable
                (when (and (tp--reactive-symbol-p tp-text-val)
                           (eq (tp--reactive-var-symbol tp-text-val) var-sym))
                  (throw 'found var-sym))))))))
    nil))

(defun tp--update-reactive-text (layer-name &optional where)
  "Update text regions that have tp-text property with LAYER-NAME applied.
This is called when a reactive variable bound to tp-text changes.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update all buffers that have the text property (setq case).

If a transform function is registered for LAYER-NAME via `:transform',
it will be applied to the text before updating."
  (let ((props (tp-layer-props layer-name t)))  ; include tp-name for reactive tracking
    (when props
      (let* ((raw-text (plist-get props 'tp-text))
             ;; Apply transformation if registered
             (transform-fn (cdr (assoc layer-name tp-layer-transforms)))
             (new-text (if (and transform-fn raw-text (stringp raw-text))
                           (condition-case err
                               (let ((result (funcall transform-fn raw-text)))
                                 (tp-debug-log "  Transform %s: %S -> %S"
                                               layer-name raw-text result)
                                 result)
                             (error
                              (message "tp: transform error for %s: %s"
                                       layer-name err)
                              raw-text))
                         raw-text)))
        (when (and new-text (stringp new-text))
          (if (and where (bufferp where) (buffer-live-p where))
              ;; setq-local case: only update the specific buffer
              (tp-with-current-buffer where
                (save-excursion
                  (tp--replace-reactive-text-in-buffer layer-name new-text props)))
            ;; setq case: update all buffers that have the text property
            (dolist (buf (buffer-list))
              (when (buffer-live-p buf)
                (tp-with-current-buffer buf
                  (save-excursion
                    (tp--replace-reactive-text-in-buffer layer-name new-text props)))))))))))

(defun tp--replace-reactive-text-in-buffer (layer-name new-text props)
  "Replace text in current buffer for reactive text with LAYER-NAME.
NEW-TEXT is the new text to replace with.
PROPS are the properties to apply to the new text."
  (goto-char (point-min))
  (let ((match (text-property-search-forward 'tp-name layer-name t)))
    (while match
      (let* ((m-start (prop-match-beginning match))
             (m-end (prop-match-end match))
             (old-text (buffer-substring-no-properties m-start m-end)))
        ;; Only replace if text is different
        (unless (equal old-text new-text)
          ;; Delete old text and insert new
          (delete-region m-start m-end)
          (goto-char m-start)
          (insert new-text)
          ;; Apply the layer properties (including tp-text and tp-name) to new text
          (let ((new-end (+ m-start (length new-text))))
            (set-text-properties m-start new-end props))))
      ;; Search for next match
      (setq match (text-property-search-forward 'tp-name layer-name t)))))

(defun tp--handle-tp-text-property (start end props object &optional preserve-props)
  "Handle tp-text property in PROPS for region from START to END in OBJECT.
If tp-text is nil, initialize it to the current text in the region.
If tp-text is a string different from current text, replace the text.
When PRESERVE-PROPS is non-nil, existing text properties are preserved
on the replaced text (used by tp-set and tp-add).
Returns (PROPS NEW-END NEW-OBJECT) where PROPS is the updated props,
NEW-END is the new end position after any text replacement, and
NEW-OBJECT is the new string object (only different for strings with tp-text)."
  (if (not (plist-member props 'tp-text))
      ;; tp-text not in props - return unchanged
      (list props end object)
    (let ((tp-text-val (plist-get props 'tp-text)))
      (cond
       ;; tp-text is nil - initialize it to the current text
       ((null tp-text-val)
        (let ((current-text
               (if (stringp object)
                   (substring object start end)
                 (if object
                     (with-current-buffer object
                       (buffer-substring-no-properties start end))
                   (buffer-substring-no-properties start end)))))
          ;; If tp-text uses a reactive variable, update that variable to match
          ;; This ensures the reactive variable and buffer text stay in sync
          (when-let ((layer-name (plist-get props 'tp-name)))
            (when-let ((reactive-var (tp--find-tp-text-reactive-var layer-name)))
              ;; Update the reactive variable with the current text
              ;; Note: Using global `set` here because the layer definition is global.
              ;; When the variable is changed, the reactive watcher will update all
              ;; buffers that have this layer applied.
              (set reactive-var current-text)
              ;; Also update the layer definition so future accesses see the new value
              (let ((layer-props (cdr (assoc layer-name tp-layer-alist))))
                (when layer-props
                  (tp--set-layer-props layer-name
                                       (plist-put layer-props 'tp-text current-text))))))
          (list (plist-put props 'tp-text current-text) end object)))
       ;; tp-text has a string value - replace the text in the region
       ((stringp tp-text-val)
        ;; Apply transform if layer has one registered
        (let* ((layer-name (plist-get props 'tp-name))
               (transform-fn (when layer-name (cdr (assoc layer-name tp-layer-transforms))))
               (final-text (if transform-fn
                               (condition-case err
                                   (funcall transform-fn tp-text-val)
                                 (error
                                  (message "tp: transform error for %s: %s" layer-name err)
                                  tp-text-val))
                             tp-text-val)))
          (if (stringp object)
              ;; For strings: create a new string with tp-text content
              ;; The new string replaces the original, with props applied
              (let ((new-string (copy-sequence final-text)))
                (list props (length new-string) new-string))
            ;; For buffers: replace text and adjust end position
            (let ((old-text (if object
                                (with-current-buffer object
                                  (buffer-substring-no-properties start end))
                              (buffer-substring-no-properties start end))))
              (if (equal old-text final-text)
                  ;; Same text, no replacement needed
                  (list props end object)
                ;; Need to replace text
                (let ((existing-props (when preserve-props
                                        (if object
                                            (with-current-buffer object
                                              (text-properties-at start))
                                          (text-properties-at start)))))
                  (save-excursion
                    (if object
                        (with-current-buffer object
                          (let ((inhibit-read-only t))
                            (delete-region start end)
                            (goto-char start)
                            (insert final-text)))
                      (let ((inhibit-read-only t))
                        (delete-region start end)
                        (goto-char start)
                        (insert final-text))))
                  (let ((new-end (+ start (length final-text))))
                    ;; Re-apply existing properties to new text region if preserving
                    (when existing-props
                      (cl-loop for (key val) on existing-props by #'cddr
                               do (put-text-property
                                   start new-end key val object)))
                    (list props new-end object))))))))
       ;; Other types - return unchanged
       (t (list props end object))))))

;;;============================================================================
;;; Layer 2: Core Property Functions - Argument Parsing
;;;============================================================================

(defun tp--parse-args (start-or-string end-or-prop props-or-val rest)
  "Parse flexible function arguments and return (OBJECT START END PROPS).
Supports multiple calling conventions:
1. Buffer region: (START END PROPS)
2. Buffer region with object: (START END PROPS OBJECT)
3. String region: (START END PROPS STRING)
4. Entire string with plist: (STRING PROP VAL ...)
5. Entire string with layer: (STRING LAYER-NAME ARG)
6. Entire string with layer and extra props: (STRING LAYER-NAME ARG PROP VAL ...)"
  (let (object start finish props)
    (cond
     ;; First arg is a string - apply to entire string
     ((stringp start-or-string)
      (setq object start-or-string
            start 0
            finish (length start-or-string))
      ;; Check if second arg is a layer/group name or parameterized layer
      (cond
       ;; (tp-set "str" 'layer-name arg ...) - layer with argument and optional extra props
       ((and (symbolp end-or-prop)
             (or (assoc end-or-prop tp-layer-alist)
                 (assoc end-or-prop tp-layer-groups))
             props-or-val)
        ;; Build props: (layer-name arg extra-prop1 val1 ...)
        (setq props (cons end-or-prop (cons props-or-val rest))))
       ;; (tp-set "str" 'layer-name) - layer without argument (legacy)
       ((and (symbolp end-or-prop)
             (or (assoc end-or-prop tp-layer-alist)
                 (assoc end-or-prop tp-layer-groups))
             (null props-or-val)
             (null rest))
        (setq props (list end-or-prop)))
       ;; Standard flat plist: (tp-set "str" 'prop1 val1 'prop2 val2 ...)
       (t
        (setq props (if end-or-prop
                        (if props-or-val
                            (cons end-or-prop (cons props-or-val rest))
                          (list end-or-prop))
                      nil)))))
     ;; First arg is a number - region convention
     ((numberp start-or-string)
      (setq start start-or-string
            finish end-or-prop
            props props-or-val)
      ;; Check if 4th arg (first of rest) is a buffer or string
      (when (and rest (or (bufferp (car rest))
                          (stringp (car rest))))
        (setq object (car rest))))
     (t (error "Invalid first argument: %S" start-or-string)))
    ;; Unwrap double-wrapped properties
    (when (and (listp props) (listp (car-safe props)))
      (setq props (car props)))
    ;; Merge duplicate keys in the plist (for single-call property setting)
    ;; This must happen before tp--resolve-props to properly handle face merging
    ;; Use (cdddr props) for O(1) check - need at least 4 elements (2 key-value pairs) for possible duplicates
    (when (and (listp props) (cdddr props))
      (setq props (tp--merge-duplicate-keys props)))
    ;; Resolve props: handles layer/group names and anonymous reactive plists
    (when props
      (setq props (or (tp--resolve-props props) props)))
    (list object start finish props)))

;;;============================================================================
;;; Layer 2: Core Property Functions - Set/Reset/Add
;;;============================================================================

(defun tp-set (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Set text properties on string or buffer region.

Supports four calling conventions:
1. (tp-set START END PROPS) - current buffer
2. (tp-set START END PROPS BUFFER/STRING) - specific object
3. (tp-set STRING PROP VAL ...) - entire string

PROPS can be a plist or a layer/group name symbol.
Preserves existing properties not specified in PROPS.
Returns modified string or (START . END) cons for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Handle tp-text property specially
    (pcase-let ((`(,new-props ,new-finish ,new-object)
                 (tp--handle-tp-text-property start finish props object t)))
      (setq props new-props finish new-finish object new-object)
      (when (and (stringp object) (plist-member props 'tp-text))
        (setq start 0)))
    ;; Check if we have any existing properties in the range
    (let ((has-existing-props (text-properties-at start object)))
      (if (and (not has-existing-props)
               ;; Also check if this is a uniform range (no intervals)
               (or (stringp object)
                   (= start (or (next-single-property-change start nil object finish) finish))))
          ;; No existing properties - can use set-text-properties to preserve duplicate keys
          (set-text-properties start finish props object)
        ;; Has existing properties - use put-text-property for proper interval handling
        ;; This may lose duplicate keys but correctly handles overlapping regions
        (cl-loop for (key val) on props by #'cddr
                 do (put-text-property start finish key val object))))
    (if (stringp object) object (cons start finish))))

(defun tp-reset (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Completely replace all text properties with PROPS.
Like `tp-set' but replaces ALL existing properties.
Returns modified string or (START . END) cons for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Handle tp-text property
    (pcase-let ((`(,new-props ,new-finish ,new-object)
                 (tp--handle-tp-text-property start finish props object nil)))
      (setq props new-props finish new-finish object new-object)
      (when (and (stringp object) (plist-member props 'tp-text))
        (setq start 0)))
    ;; Completely replace all properties
    (set-text-properties start finish props object)
    (if (stringp object) object (cons start finish))))

(defun tp--prepend-face (new-face existing-face)
  "Prepend NEW-FACE to EXISTING-FACE for the face property.
Returns a face value where NEW-FACE takes precedence.

Examples:
  (tp--prepend-face \\='shadow \\='bold)  => (shadow bold)
  (tp--prepend-face \\='shadow \\='(bold italic))  => (shadow bold italic)
  (tp--prepend-face \\='(:foreground \"red\") \\='(:background \"blue\"))
    => (:background \"blue\" :foreground \"red\")  ; merged plist

If NEW-FACE is a plist (like (:foreground \"red\")), deeply merge it.
If NEW-FACE is a symbol or list of faces, prepend it to create a face list.
Duplicate faces are not added."
  (cond
   ;; No existing face - just use new face
   ((null existing-face) new-face)
   ;; New face is a plist - deep merge with existing
   ((and (listp new-face) (keywordp (car-safe new-face)))
    (cond
     ((and (listp existing-face) (keywordp (car-safe existing-face)))
      (tp--deep-merge-plist existing-face new-face))
     ;; Existing is a symbol or list of faces - wrap new plist and prepend
     ((symbolp existing-face)
      (list new-face existing-face))
     ((listp existing-face)
      (cons new-face existing-face))
     (t new-face)))
   ;; New face is a symbol - prepend to existing
   ((symbolp new-face)
    (cond
     ((symbolp existing-face)
      (if (eq new-face existing-face)
          new-face
        (list new-face existing-face)))
     ((listp existing-face)
      (if (member new-face existing-face)
          existing-face
        (cons new-face existing-face)))
     (t new-face)))
   ;; New face is a list of faces - prepend to existing
   ((listp new-face)
    (cond
     ((symbolp existing-face)
      (if (member existing-face new-face)
          new-face
        (append new-face (list existing-face))))
     ((listp existing-face)
      (append new-face
              (cl-remove-if (lambda (f) (member f new-face)) existing-face)))
     (t new-face)))
   (t new-face)))

(defun tp-add (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Add or update text properties with deep merging.
Unlike `tp-set', deeply merges nested properties.
For `face' property, symbol faces are prepended to existing face list.
Returns modified string or (START . END) cons for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Handle tp-text property
    (pcase-let ((`(,new-props ,new-finish ,new-object)
                 (tp--handle-tp-text-property start finish props object t)))
      (setq props new-props finish new-finish object new-object)
      (when (and (stringp object) (plist-member props 'tp-text))
        (setq start 0)))
    ;; Process each property with deep merging
    (let ((pos start))
      (while (< pos finish)
        (let* ((current-props (text-properties-at pos object))
               (next-pos (or (next-property-change pos object finish) finish)))
          (cl-loop
           for (key val) on props by #'cddr
           do (let* ((current-val (plist-get current-props key))
                     (new-val (cond
                               ((eq key 'face) (tp--prepend-face val current-val))
                               ((and (listp val) (keywordp (car-safe val))
                                     (listp current-val) (keywordp (car-safe current-val)))
                                (tp--deep-merge-plist current-val val))
                               (t val))))
                (put-text-property pos next-pos key new-val object)))
          (setq pos next-pos))))
    (if (stringp object) object (cons start finish))))

;;;============================================================================
;;; Layer 2: Core Property Functions - Get/At
;;;============================================================================

(defun tp-get (start-or-string &optional end-or-property &rest args)
  "Get text property value(s) with support for nested sub-properties.
Returns list of (START END VALUE) intervals.
Use `tp-at' for single position queries.
OBJECT defaults to current buffer."
  (cond
   ;; (tp-get STRING ...) - entire string
   ;; Returns list of (START END VALUE) intervals for all property values
   ((stringp start-or-string)
    (let* ((str start-or-string)
           (len (length str))
           (property nil)
           (sub-path nil))
      (cond
       ;; (tp-get str) - return all property intervals
       ((null end-or-property)
        (let ((intervals nil)
              (pos 0))
          (while (< pos len)
            (let* ((current-props (text-properties-at pos str))
                   (next-pos (or (next-property-change pos str len) len)))
              (when current-props
                (push (list pos next-pos current-props) intervals))
              (setq pos next-pos)))
          (nreverse intervals)))
       ;; (tp-get str '(face :foreground)) - property path as list
       ((listp end-or-property)
        (setq property (car end-or-property))
        (setq sub-path (cdr end-or-property))
        (let ((intervals nil)
              (pos 0))
          (while (< pos len)
            (let* ((prop-value (get-text-property pos property str))
                   (next-pos (or (next-single-property-change
                                  pos property str len)
                                 len))
                   (value (if sub-path
                              (tp--get-nested prop-value sub-path)
                            prop-value)))
              (when value
                (push (list pos next-pos value) intervals))
              (setq pos next-pos)))
          (nreverse intervals)))
       ;; (tp-get str 'face ...) - property as symbol with optional sub-path
       ((symbolp end-or-property)
        (setq property end-or-property)
        (setq sub-path args)
        (let ((intervals nil)
              (pos 0))
          (while (< pos len)
            (let* ((prop-value (get-text-property pos property str))
                   (next-pos (or (next-single-property-change
                                  pos property str len)
                                 len))
                   (value (if sub-path
                              (tp--get-nested prop-value sub-path)
                            prop-value)))
              (when value
                (push (list pos next-pos value) intervals))
              (setq pos next-pos)))
          (nreverse intervals))))))
   ;; (tp-get START END ...) - range form
   ((and (numberp start-or-string)
         (numberp end-or-property))
    (let* ((start start-or-string)
           (end end-or-property)
           (rest-args args)
           (property nil)
           (sub-path nil)
           (object nil))
      ;; Parse remaining args
      (when rest-args
        (cond
         ;; Property path as list: (tp-get 5 20 '(face :underline) obj)
         ((listp (car rest-args))
          (let ((prop-path (car rest-args)))
            (setq property (car prop-path))
            (setq sub-path (cdr prop-path))
            (setq object (cadr rest-args))))
         ;; Property as symbol
         ((symbolp (car rest-args))
          (setq property (car rest-args))
          (setq rest-args (cdr rest-args))
          ;; Remaining args could be sub-path and/or object
          (when rest-args
            (if (or (bufferp (car (last rest-args)))
                    (stringp (car (last rest-args))))
                (progn
                  (setq object (car (last rest-args)))
                  (setq sub-path (butlast rest-args)))
              (setq sub-path rest-args))))
         ;; First arg is object (buffer/string)
         ((or (bufferp (car rest-args)) (stringp (car rest-args)))
          (setq object (car rest-args)))))
      (if property
          ;; Get specific property from range - return list of (START END VALUE) for all intervals
          (let ((pos start)
                (intervals nil))
            (while (< pos end)
              (let* ((prop-value (get-text-property pos property object))
                     (next-pos (or (next-single-property-change
                                    pos property object end)
                                   end))
                     (value (if sub-path
                                (tp--get-nested prop-value sub-path)
                              prop-value)))
                (when value
                  (push (list pos next-pos value) intervals))
                (setq pos next-pos)))
            (nreverse intervals))
        ;; Get all properties from range - return list of (START END PLIST) intervals
        (let ((intervals nil)
              (pos start)
              (obj (or object (current-buffer))))
          (while (< pos end)
            (let* ((current-props (text-properties-at pos obj))
                   (next-pos (or (next-property-change pos obj end) end)))
              (when current-props
                (push (list pos next-pos current-props) intervals))
              (setq pos next-pos)))
          (nreverse intervals)))))
   (t (error "Invalid arguments to tp-get"))))

(defun tp-at (pos &optional property-or-object object)
  "Get text properties at POS in OBJECT, optionally filtered by PROPERTY.

This function supports multiple calling conventions:

1. Get all properties at position:
   (tp-at POS)
   (tp-at POS OBJECT)

2. Get specific property at position:
   (tp-at POS PROPERTY)
   (tp-at POS PROPERTY OBJECT)

3. Get nested sub-property at position:
   (tp-at POS \\='(PROPERTY SUB-KEY ...))
   (tp-at POS \\='(PROPERTY SUB-KEY ...) OBJECT)

POS is the position to query.
PROPERTY-OR-OBJECT can be a property symbol/list, or an object (buffer/string).
OBJECT is the buffer or string to query; nil defaults to current buffer.

For strings, positions are 0-indexed.
For buffers, positions are 1-indexed.

Examples:
  ;; Get all properties at position 5 in current buffer
  (tp-at 5)
  ;; Get all properties at position 0 in string
  (tp-at 0 my-string)
  ;; Get face property at position 5
  (tp-at 5 \\='face)
  ;; Get face property at position 0 in string
  (tp-at 0 \\='face my-string)
  ;; Get nested sub-property
  (tp-at 5 \\='(face :foreground))
  (tp-at 5 \\='(face :box :color))
  (tp-at 5 \\='(display :width))"
  (let ((property nil)
        (sub-path nil)
        (obj nil))
    ;; Parse arguments
    (cond
     ;; property-or-object is nil - just get all props
     ((null property-or-object)
      (setq obj nil))
     ;; property-or-object is a buffer/string - it's the object
     ((or (bufferp property-or-object) (stringp property-or-object))
      (setq obj property-or-object))
     ;; property-or-object is a symbol - it's a property
     ((symbolp property-or-object)
      (setq property property-or-object
            obj object))
     ;; property-or-object is a list - it's a property path
     ((listp property-or-object)
      (setq property (car property-or-object)
            sub-path (cdr property-or-object)
            obj object))
     (t (error "Invalid PROPERTY-OR-OBJECT argument: %S" property-or-object)))
    ;; Get the value
    (if property
        (let ((prop-value (get-text-property pos property obj)))
          (if sub-path
              (tp--get-nested prop-value sub-path)
            prop-value))
      (text-properties-at pos obj))))

;;;============================================================================
;;; Layer 2: Core Property Functions - Remove/Clear
;;;============================================================================

(defun tp--remove-sub (start end property sub-property &optional object)
  "Remove SUB-PROPERTY from PROPERTY between START and END in OBJECT."
  (let* ((pos start))
    (while (< pos end)
      (let* ((current-value (get-text-property pos property object))
             (next-pos (or (next-single-property-change pos property object end) end))
             (new-value
              (cond
               ;; Plist - remove the sub-property
               ((and (listp current-value) (keywordp (car current-value)))
                (let ((result (copy-sequence current-value)))
                  (cl-remf result sub-property)
                  (if result result nil)))
               ;; Other types - leave unchanged
               (t current-value))))
        (if new-value
            (put-text-property pos next-pos property new-value object)
          (remove-text-properties pos next-pos (list property nil) object))
        (setq pos next-pos))))
  nil)

(defun tp--remove-nested-keys (plist keys-to-remove)
  "Remove KEYS-TO-REMOVE from PLIST.
Returns the modified plist, or nil if empty after removal."
  (let ((result (copy-sequence plist)))
    (dolist (key keys-to-remove)
      (cl-remf result key))
    (if (null result) nil result)))

(defun tp--remove-property (start end property object)
  "Internal function to remove PROPERTY from START to END in OBJECT.
PROPERTY can be a symbol or a list for nested removal."
  (cond
   ;; Simple property removal
   ((symbolp property)
    (remove-text-properties start end (list property nil) object))
   ;; Nested property removal
   ((listp property)
    (let* ((prop-name (car property))
           (sub-key (cadr property))
           (nested-keys (caddr property)))
      (if (null nested-keys)
          ;; Remove sub-key from property
          (tp--remove-sub start end prop-name sub-key object)
        ;; Remove nested keys from sub-key
        (let ((pos start))
          (while (< pos end)
            (let* ((current-value (get-text-property pos prop-name object))
                   (next-pos (or (next-single-property-change
                                  pos prop-name object end)
                                 end)))
              (when current-value
                (let* ((sub-value
                        (if (and (listp current-value) (keywordp (car current-value)))
                            (plist-get current-value sub-key)
                          nil))
                       (new-sub-value
                        (when (and (listp sub-value) (keywordp (car sub-value)))
                          (tp--remove-nested-keys sub-value nested-keys)))
                       (new-value
                        (cond
                         ((and (listp current-value) (keywordp (car current-value)))
                          (let ((result (copy-sequence current-value)))
                            (if new-sub-value
                                (plist-put result sub-key new-sub-value)
                              ;; Remove sub-key entirely if no keys remain
                              (cl-remf result sub-key))
                            (if (null result) nil result)))
                         (t current-value))))
                  (if new-value
                      (put-text-property pos next-pos prop-name new-value object)
                    (remove-text-properties pos next-pos (list prop-name nil) object))))
              (setq pos next-pos)))))))))

(defun tp-remove (start-or-string end-or-prop &optional prop-or-sub &rest rest)
  "Remove properties from text.

This function supports multiple calling conventions:

1. Buffer region with property:
   (tp-remove START END PROPERTY)
   (tp-remove START END PROPERTY OBJECT)

2. Buffer region with nested property:
   (tp-remove START END \\='(PROPERTY SUB-KEY))
   (tp-remove START END \\='(PROPERTY SUB-KEY (NESTED-KEYS...)))

3. Entire string with properties to remove:
   (tp-remove STRING PROP1 PROP2 ...)
   (tp-remove \"Hello\" \\='face \\='help-echo)

4. Entire string with sub-property removal:
   (tp-remove STRING PROPERTY SUB-KEY)
   (tp-remove \"Hello\" \\='face :underline)

5. Entire string with nested sub-property removal:
   (tp-remove STRING PROPERTY SUB-KEY \\='(NESTED-KEYS...))
   (tp-remove \"Hello\" \\='face :underline \\='(:style :position))

Returns the modified string for string input, or nil for buffer operations."
  (cond
   ;; First arg is a string - apply to entire string
   ((stringp start-or-string)
    (let ((str start-or-string)
          (start 0)
          (end (length start-or-string)))
      (cond
       ;; (tp-remove str 'face :underline '(:style :position)) - nested sub-property removal with list
       ((and (symbolp end-or-prop)
             (keywordp prop-or-sub)
             rest
             (listp (car rest)))
        (tp--remove-property start end (list end-or-prop prop-or-sub (car rest)) str))
       ;; (tp-remove str 'face :underline :position :style ...) - nested sub-property removal with keywords
       ((and (symbolp end-or-prop)
             (keywordp prop-or-sub)
             rest
             (keywordp (car rest)))
        (tp--remove-property start end (list end-or-prop prop-or-sub rest) str))
       ;; (tp-remove str 'face :underline) - sub-property removal
       ((and (symbolp end-or-prop) (keywordp prop-or-sub))
        (tp--remove-sub start end end-or-prop prop-or-sub str))
       ;; (tp-remove str 'face 'help-echo ...) - multiple properties
       ((symbolp end-or-prop)
        (let ((props (cons end-or-prop (cons prop-or-sub rest))))
          ;; Filter to only include valid property symbols (not nil)
          (dolist (prop (cl-remove-if-not #'symbolp props))
            (remove-text-properties start end (list prop nil) str))))
       ;; (tp-remove str '(face :underline)) - nested property spec
       ((listp end-or-prop)
        (tp--remove-property start end end-or-prop str)))
      str))
   ;; First arg is a number - buffer region
   ((numberp start-or-string)
    (let* ((start start-or-string)
           (end end-or-prop)
           (property prop-or-sub)
           (object (car rest)))
      (tp--remove-property start end property object)
      nil))
   (t (error "Invalid arguments to tp-remove"))))

;;;###autoload
(defun tp-clear (&optional start end object)
  "Clear all text properties from START to END in OBJECT.
If START and END are not provided, clear the entire buffer."
  (interactive)
  (let ((beg (or start (point-min)))
        (finish (or end (point-max))))
    (set-text-properties beg finish nil object)))

;;;============================================================================
;;; Layer 5: High-Level API - Pattern Matching (match/regexp)
;;;============================================================================

(defun tp--match-apply-single (pattern properties apply-fn object)
  "Apply APPLY-FN to matches of single PATTERN in OBJECT."
  (cond
   ;; String object
   ((stringp object)
    (let ((pos 0))
      (while (string-match (regexp-quote pattern) object pos)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when properties
            (funcall apply-fn beg end properties object))
          (setq pos (if (= beg end) (1+ beg) end))))
      object))
   ;; Buffer or nil (current buffer)
   (t
    (let ((buf (or object (current-buffer))))
      (tp-with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let (regions)
            (while (search-forward pattern nil t)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when properties
                  (funcall apply-fn beg end properties buf))
                (push (cons beg end) regions)))
            (nreverse regions))))))))

(defun tp--match-apply (pattern properties apply-fn &optional object)
  "Internal function to apply APPLY-FN to matches of PATTERN.
PATTERN can be a string or a list of strings (multiple patterns).
When PATTERN is a list, each element is a pattern to match.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
Returns modified object or list of regions."
  (let ((patterns (if (listp pattern) pattern (list pattern))))
    (cond
     ;; String object
     ((stringp object)
      (dolist (p patterns)
        (tp--match-apply-single p properties apply-fn object))
      object)
     ;; Buffer or nil (current buffer)
     (t
      (let ((all-regions nil))
        (dolist (p patterns)
          (let ((regions (tp--match-apply-single p properties apply-fn object)))
            (setq all-regions (append all-regions regions))))
        all-regions)))))

(defun tp--regexp-apply-single (pattern properties apply-fn object)
  "Apply APPLY-FN to regexp matches of single PATTERN in OBJECT.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
Returns modified object or list of regions."
  (cond
   ;; String object
   ((stringp object)
    (let ((pos 0))
      (while (string-match pattern object pos)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when properties
            (funcall apply-fn beg end properties object))
          (setq pos (if (= beg end) (1+ beg) end))))
      object))
   ;; Buffer or nil (current buffer)
   (t
    (let ((buf (or object (current-buffer))))
      (tp-with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (let (regions)
            (while (re-search-forward pattern nil t)
              (let ((beg (match-beginning 0))
                    (end (match-end 0)))
                (when properties
                  (funcall apply-fn beg end properties buf))
                (push (cons beg end) regions)))
            (nreverse regions))))))))

(defun tp--regexp-apply (pattern properties apply-fn &optional object)
  "Internal function to apply APPLY-FN to regexp matches of PATTERN.
PATTERN can be a string (single regexp) or a list of strings (multiple regexps).
When PATTERN is a list, each element is a regexp to match.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
Returns modified object or list of regions."
  (let ((patterns (if (listp pattern) pattern (list pattern))))
    (cond
     ;; String object
     ((stringp object)
      (dolist (p patterns)
        (tp--regexp-apply-single p properties apply-fn object))
      object)
     ;; Buffer or nil (current buffer)
     (t
      (let ((all-regions nil))
        (dolist (p patterns)
          (let ((regions (tp--regexp-apply-single p properties apply-fn object)))
            (setq all-regions (append all-regions regions))))
        all-regions)))))

(defun tp--deep-merge-apply (start end props obj)
  "Apply PROPS to OBJ from START to END with deep merge.
Merges nested plists instead of replacing them."
  (let ((pos start))
    (while (< pos end)
      (let* ((current-props (text-properties-at pos obj))
             (next-pos (or (next-property-change pos obj end) end)))
        (cl-loop for (key val) on props by #'cddr
                 do (let* ((current-val (plist-get current-props key))
                           (new-val
                            (cond
                             ((and (listp val) (keywordp (car-safe val))
                                   (listp current-val)
                                   (keywordp (car-safe current-val)))
                              (tp--deep-merge-plist current-val val))
                             (t val))))
                      (put-text-property pos next-pos key new-val obj)))
        (setq pos next-pos)))))

(defun tp-match-set (pattern plist &optional object)
  "Set properties on all occurrences of PATTERN.

  (tp-match-set PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Returns:
- For strings: the modified string
- For buffers: list of (START . END) pairs for all matches."
  (tp--match-apply pattern (tp--ensure-props plist) #'tp-set object))

(defun tp-match-reset (pattern plist &optional object)
  "Reset (completely replace) properties on all occurrences of PATTERN.

  (tp-match-reset PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-match-set', this completely replaces all existing properties."
  (tp--match-apply pattern (tp--ensure-props plist)
                   (lambda (start end props obj)
                     (set-text-properties start end props obj))
                   object))

(defun tp-match-add (pattern plist &optional object)
  "Add/update properties on all occurrences of PATTERN.

  (tp-match-add PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-match-set', this deeply merges nested properties."
  (tp--match-apply pattern (tp--ensure-props plist) #'tp--deep-merge-apply object))

(defun tp-regexp-set (pattern plist &optional object)
  "Set properties on all matches of PATTERN (regexp).

  (tp-regexp-set PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Returns:
- For strings: the modified string
- For buffers: list of (START . END) pairs for all matches."
  (tp--regexp-apply pattern (tp--ensure-props plist) #'tp-set object))

(defun tp-regexp-reset (pattern plist &optional object)
  "Reset (completely replace) properties on all regexp matches of PATTERN.

  (tp-regexp-reset PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-regexp-set', this completely replaces all existing properties."
  (tp--regexp-apply pattern (tp--ensure-props plist)
                    (lambda (start end props obj)
                      (set-text-properties start end props obj))
                    object))

(defun tp-regexp-add (pattern plist &optional object)
  "Add/update properties on all regexp matches of PATTERN.

  (tp-regexp-add PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-regexp-set', this deeply merges nested properties."
  (tp--regexp-apply pattern (tp--ensure-props plist) #'tp--deep-merge-apply object))

;;;============================================================================
;;; Layer 5: High-Level API - Search and Navigation
;;;============================================================================

(defun tp-search-forward (property &optional value predicate not-current)
  "Search forward for text with PROPERTY.
Wraps `text-property-search-forward'."
  (text-property-search-forward property value predicate not-current))

(defun tp-search-backward (property &optional value predicate not-current)
  "Search backward for text with PROPERTY.
Wraps `text-property-search-backward'."
  (text-property-search-backward property value predicate not-current))

(defun tp-forward (property &optional value object n)
  "Search forward N times for text with PROPERTY.
Returns prop-match for buffers or list of (START END VALUE) for strings."
  (let ((count (or n 1)))
    (cond
     ;; String object - use tp-search
     ((stringp object)
      (let ((matches (tp-search object property value)))
        (seq-take matches count)))
     ;; Buffer or nil
     (t
      (let ((result nil)
            (buf (or object (current-buffer))))
        (tp-with-current-buffer buf
          (dotimes (_ count)
            (setq result (tp-search-forward property value t))))
        result)))))

(defun tp-backward (property &optional value object n)
  "Search backward N times for text with PROPERTY.

N is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.

For buffers, returns the prop-match object from the last successful search.
For strings, returns a list of (START END VALUE) for the last N matches
in reverse order (from end to start).

Uses `tp-search-backward' for buffers and `tp-search' for strings."
  (let ((count (or n 1)))
    (cond
     ;; String object - use tp-search and reverse
     ((stringp object)
      (let ((matches (nreverse (tp-search object property value))))
        (seq-take matches count)))
     ;; Buffer or nil
     (t
      (let ((result nil)
            (buf (or object (current-buffer))))
        (tp-with-current-buffer buf
          (dotimes (_ count)
            (setq result (tp-search-backward property value))))
        result)))))

(defun tp--forward-do (function property &optional value object times start end)
  "Internal: Search forward TIMES for PROPERTY and apply FUNCTION to the last match.

FUNCTION receives two arguments: the prop-match object (or list for strings)
and OBJECT.
TIMES is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.

Returns the number of successful matches."
  (let ((count (or times 1)))
    (cond
     ;; String object
     ((stringp object)
      (let* ((start-pos (or start 0))
             (end-pos (or end (length object)))
             (all-matches (tp-search object property value))
             (filtered-matches (seq-filter (lambda (m)
                                             (and (>= (car m) start-pos)
                                                  (<= (cadr m) end-pos)))
                                           all-matches))
             (matches (seq-take filtered-matches count)))
        (when matches
          (funcall function (car (last matches)) object))
        (length matches)))
     ;; Buffer or nil
     (t
      (let* ((buf (or object (current-buffer)))
             (matches 0))
        (tp-with-current-buffer buf
          (let ((search-start (or start (point-min)))
                (search-end (or end (point-max))))
            (save-excursion
              (goto-char search-start)
              (dotimes (i count)
                (when-let ((match (tp-search-forward property value t)))
                  (when (<= (prop-match-end match) search-end)
                    (when (= i (1- count))
                      (funcall function match buf))
                    (cl-incf matches)))))))
        matches)))))

(defun tp-forward-do (function property &optional value object times start end)
  "Search forward for text with PROPERTY and apply FUNCTION to the last match.

FUNCTION receives (TEXT &optional START END) where TEXT is the matched text,
START and END are the positions of the match.  The return value of FUNCTION
replaces the matched text in the string or buffer.

PROPERTY is the text property to search for.
VALUE is the optional value to match; nil means search for PROPERTY without
matching value.
OBJECT can be a buffer or string; nil defaults to current buffer.
TIMES is the number of searches, defaulting to 1.  The function searches
TIMES times but only applies FUNCTION to the last (Nth) match found.
START and END define the search range; defaults are object start and end.

Returns the number of successful matches.

Note: For string objects, the replacement text must have the same length
as the original matched text, since strings have fixed length in Emacs.
If the replacement is shorter, only that portion will be replaced.
If the replacement is longer, it will be truncated.

Example:
  ;; Upcase only the last (2nd) match
  (setq my-string (copy-sequence \"hello world hello\"))
  (tp-set 0 5 \\='(marker t) my-string)
  (tp-set 12 17 \\='(marker t) my-string)
  (tp-forward-do #\\='upcase \\='marker nil my-string 2)
  ;; => \"hello world HELLO\" - only the 2nd match is upcased

  ;; Use start and end positions in function
  (tp-forward-do (lambda (txt start end) (format \"[%d-%d]%s\" start end txt))
                 \\='marker nil my-string 2)

  ;; Search within a range
  (tp-forward-do #\\='upcase \\='marker nil my-string 1 0 10)"
  (let ((arity (func-arity function)))
    (tp--forward-do
     (lambda (match obj)
       (let* ((m-start (if (listp match) (car match) (prop-match-beginning match)))
              (m-end (if (listp match) (cadr match) (prop-match-end match)))
              (text (if (stringp obj)
                        (substring obj m-start m-end)
                      (buffer-substring m-start m-end)))
              (max-arity (cdr arity))
              (can-accept-start (or (eq max-arity 'many)
                                    (and (numberp max-arity) (>= max-arity 2))))
              (can-accept-end (or (eq max-arity 'many)
                                  (and (numberp max-arity) (>= max-arity 3))))
              (new-text (cond
                         (can-accept-end (funcall function text m-start m-end))
                         (can-accept-start (funcall function text m-start))
                         (t (funcall function text)))))
         (when (stringp new-text)
           (if (stringp obj)
               ;; For strings: copy text content and properties separately
               (let ((len (min (length new-text) (- m-end m-start))))
                 ;; Copy text content
                 (store-substring obj m-start new-text)
                 ;; Copy properties from new-text to obj
                 (let ((pos 0))
                   (while (< pos len)
                     (let* ((props (text-properties-at pos new-text))
                            (next-change (or (next-property-change pos new-text) len)))
                       (when props
                         (set-text-properties (+ m-start pos)
                                              (+ m-start (min next-change len))
                                              props
                                              obj))
                       (setq pos next-change)))))
             ;; For buffers, delete and insert
             (unless (equal new-text text)
               (save-excursion
                 (delete-region m-start m-end)
                 (goto-char m-start)
                 (insert new-text)))))))
     property value object times start end)))

(defun tp--backward-do (function property &optional value object times start end)
  "Internal: Search backward TIMES for PROPERTY and apply FUNCTION to the last match.

FUNCTION receives two arguments: the prop-match object (or list for strings)
and OBJECT.
TIMES is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.

Returns the number of successful matches."
  (let ((count (or times 1)))
    (cond
     ;; String object - reverse the matches
     ((stringp object)
      (let* ((start-pos (or start 0))
             (end-pos (or end (length object)))
             (all-matches (tp-search object property value))
             (filtered-matches
              (seq-filter (lambda (m)
                            (and (>= (car m) start-pos)
                                 (<= (cadr m) end-pos)))
                          all-matches))
             (matches (seq-take (nreverse filtered-matches) count)))
        (when matches
          (funcall function (car (last matches)) object))
        (length matches)))
     ;; Buffer or nil
     (t
      (let* ((buf (or object (current-buffer)))
             (matches 0))
        (tp-with-current-buffer buf
          (let ((search-start (or start (point-min)))
                (search-end (or end (point-max))))
            (save-excursion
              (goto-char search-end)
              (dotimes (i count)
                (when-let ((match (tp-search-backward property value)))
                  (when (>= (prop-match-beginning match) search-start)
                    (when (= i (1- count))
                      (funcall function match buf))
                    (cl-incf matches)))))))
        matches)))))

(defun tp-backward-do (function property &optional value object times start end)
  "Search backward for text with PROPERTY and apply FUNCTION to the last match.

FUNCTION receives (TEXT &optional START END) where TEXT is the matched text,
START and END are the positions of the match.  The return value of FUNCTION
replaces the matched text in the string or buffer.

PROPERTY is the text property to search for.
VALUE is the optional value to match; nil means search for PROPERTY without
matching value.
OBJECT can be a buffer or string; nil defaults to current buffer.
TIMES is the number of searches, defaulting to 1.  The function searches
TIMES times but only applies FUNCTION to the last (Nth) match found.
START and END define the search range; defaults are object start and end.

Returns the number of successful matches.

Note: For string objects, the replacement text must have the same length
as the original matched text, since strings have fixed length in Emacs.
If the replacement is shorter, only that portion will be replaced.
If the replacement is longer, it will be truncated.

Example:
  ;; Upcase only the last (2nd) match
  (setq my-string (copy-sequence \"hello world hello\"))
  (tp-set 0 5 \\='(marker t) my-string)
  (tp-set 12 17 \\='(marker t) my-string)
  (tp-backward-do #\\='upcase \\='marker nil my-string 2)
  ;; => \"HELLO world hello\" - only the 2nd (last) match is upcased

  ;; Use start and end positions in function
  (tp-backward-do (lambda (txt start end) (format \"[%d-%d]%s\" start end txt))
                  \\='marker nil my-string 2)

  ;; Search within a range
  (tp-backward-do #\\='upcase \\='marker nil my-string 1 0 10)"
  (let ((arity (func-arity function)))
    (tp--backward-do
     (lambda (match obj)
       (let* ((m-start (if (listp match) (car match) (prop-match-beginning match)))
              (m-end (if (listp match) (cadr match) (prop-match-end match)))
              (text (if (stringp obj)
                        (substring obj m-start m-end)
                      (buffer-substring m-start m-end)))
              (max-arity (cdr arity))
              (can-accept-start (or (eq max-arity 'many)
                                    (and (numberp max-arity) (>= max-arity 2))))
              (can-accept-end (or (eq max-arity 'many)
                                  (and (numberp max-arity) (>= max-arity 3))))
              (new-text (cond
                         (can-accept-end (funcall function text m-start m-end))
                         (can-accept-start (funcall function text m-start))
                         (t (funcall function text)))))
         (when (stringp new-text)
           (if (stringp obj)
               ;; For strings: copy text content and properties separately
               (let ((len (min (length new-text) (- m-end m-start))))
                 ;; Copy text content
                 (store-substring obj m-start new-text)
                 ;; Copy properties from new-text to obj
                 (let ((pos 0))
                   (while (< pos len)
                     (let* ((props (text-properties-at pos new-text))
                            (next-change (or (next-property-change pos new-text) len)))
                       (when props
                         (set-text-properties (+ m-start pos)
                                              (+ m-start (min next-change len))
                                              props
                                              obj))
                       (setq pos next-change)))))
             ;; For buffers, delete and insert
             (unless (equal new-text text)
               (save-excursion
                 (delete-region m-start m-end)
                 (goto-char m-start)
                 (insert new-text)))))))
     property value object times start end)))

(defun tp-search (start-or-string
                  &optional end-or-property property-or-value value object)
  "Search for all text with PROPERTY in a buffer/string range or entire string.

This function supports two calling conventions:

1. Buffer/string region:
   (tp-search START END PROPERTY &optional VALUE OBJECT)

2. Entire string:
   (tp-search STRING PROPERTY &optional VALUE)

Returns a list of (START END VALUE) lists for all matching regions.
Each element contains the start position, end position, and property value."
  (cond
   ;; Entire string form: (tp-search string property &optional value)
   ((stringp start-or-string)
    (let* ((str start-or-string)
           (property end-or-property)
           (value property-or-value)
           (results nil)
           (pos 0)
           (len (length str)))
      (while (< pos len)
        (let* ((props (text-properties-at pos str))
               (has-prop (plist-member props property))
               (prop-val (plist-get props property)))
          (if (and has-prop
                   (or (null value)
                       (equal prop-val value)))
              ;; Find the extent of this property
              (let ((next-change
                     (or (next-single-property-change
                          pos property str len)
                         len)))
                (push (list pos next-change prop-val) results)
                (setq pos next-change))
            ;; No match, move to next change
            (setq pos (or (next-single-property-change
                           pos property str len)
                          len)))))
      (nreverse results)))
   ;; Buffer/string region form: (tp-search start end property &optional value object)
   ((numberp start-or-string)
    (let* ((start start-or-string)
           (end end-or-property)
           (property property-or-value)
           (value value)
           (obj (or object (current-buffer)))
           (results nil)
           (pos start))
      (if (stringp obj)
          ;; String object
          (while (< pos end)
            (let* ((props (text-properties-at pos obj))
                   (has-prop (plist-member props property))
                   (prop-val (plist-get props property)))
              (if (and has-prop
                       (or (null value)
                           (equal prop-val value)))
                  (let ((next-change
                         (or (next-single-property-change
                              pos property obj end)
                             end)))
                    (push (list pos next-change prop-val) results)
                    (setq pos next-change))
                (setq pos (or (next-single-property-change
                               pos property obj end)
                              end)))))
        ;; Buffer object
        (tp-with-current-buffer obj
          (while (< pos end)
            (let* ((props (text-properties-at pos))
                   (has-prop (plist-member props property))
                   (prop-val (plist-get props property)))
              (if (and has-prop
                       (or (null value)
                           (equal prop-val value)))
                  (let ((next-change
                         (or (next-single-property-change
                              pos property nil end)
                             end)))
                    (push (list pos next-change prop-val) results)
                    (setq pos next-change))
                (setq pos (or (next-single-property-change
                               pos property nil end)
                              end)))))))
      (nreverse results)))
   (t (error "Invalid first argument: %S" start-or-string))))

(defun tp--search-do (function property &optional value object start end)
  "Internal: Execute FUNCTION on all matches of PROPERTY.

Signature: (tp--search-do FUNCTION PROPERTY &optional VALUE OBJECT START END)

FUNCTION receives two arguments: the prop-match (list of START END VALUE) and OBJECT.
PROPERTY is the text property to search for.
VALUE is the optional value to match; nil means search for PROPERTY without matching value.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.

Returns the number of matches processed."
  (let* ((obj (or object (current-buffer)))
         (all-matches (if (stringp obj)
                          (tp-search obj property value)
                        (let ((s (or start (point-min)))
                              (e (or end (point-max))))
                          (tp-search s e property value obj))))
         (filtered-matches
          (if (and (not (stringp obj)) start end)
              (seq-filter (lambda (m)
                            (and (>= (car m) start)
                                 (<= (cadr m) end)))
                          all-matches)
            (if (stringp obj)
                (let ((s (or start 0))
                      (e (or end (length obj))))
                  (seq-filter (lambda (m)
                                (and (>= (car m) s)
                                     (<= (cadr m) e)))
                              all-matches))
              all-matches))))
    (dolist (match filtered-matches)
      (funcall function match obj))
    (length filtered-matches)))

(defun tp-search-map (function property &optional value object start end)
  "Apply FUNCTION to all matches of PROPERTY in OBJECT.

Signature: (tp-search-map FUNCTION PROPERTY &optional VALUE OBJECT START END)

FUNCTION receives (TEXT &optional START END IDX) where:
- TEXT is the matched text
- START and END are the positions of the match
- IDX is the 0-based index of the current match

FUNCTION can either:
- Return a new/modified string to replace the matched text
- Modify the text properties of the argument and return it
- Return nil to skip replacement

PROPERTY is the text property to search for.
VALUE is the optional value to match; nil means search for PROPERTY without
matching value.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.

Returns the number of matches processed.

Note: For string objects, replacement text must have the same length
as the original matched text, since strings have fixed length in Emacs.
If the replacement is shorter, only that portion will be replaced.
If the replacement is longer, it will be truncated.

Example:
  ;; Upcase all matched text
  (tp-search-map #\\='upcase \\='marker nil my-string)

  ;; Add properties to matched text
  (tp-search-map (lambda (txt) (tp-add txt \\='face \\='bold)) \\='marker nil str)

  ;; Use start, end, and index
  (tp-search-map (lambda (txt start end idx)
                   (format \"[%d:%d-%d]%s\" idx start end txt))
                 \\='marker nil str)

  ;; Search within a range
  (tp-search-map #\\='upcase \\='marker nil my-string 0 10)"
  (let* ((obj (or object (current-buffer)))
         (idx 0)
         (arity (func-arity function)))
    (tp--search-do
     (lambda (match obj)
       (let* ((m-start (car match))
              (m-end (cadr match))
              (text (if (stringp obj)
                        (substring obj m-start m-end)
                      (buffer-substring m-start m-end)))
              (max-arity (cdr arity))
              (can-accept-start (or (eq max-arity 'many)
                                    (and (numberp max-arity) (>= max-arity 2))))
              (can-accept-end (or (eq max-arity 'many)
                                  (and (numberp max-arity) (>= max-arity 3))))
              (can-accept-idx (or (eq max-arity 'many)
                                  (and (numberp max-arity) (>= max-arity 4))))
              (new-text (cond
                         (can-accept-idx (funcall function text m-start m-end idx))
                         (can-accept-end (funcall function text m-start m-end))
                         (can-accept-start (funcall function text m-start))
                         (t (funcall function text)))))
         (setq idx (1+ idx))
         (when (stringp new-text)
           (if (stringp obj)
               ;; For strings: copy text content and properties separately
               (let ((len (min (length new-text) (- m-end m-start))))
                 ;; Copy text content
                 (store-substring obj m-start new-text)
                 ;; Copy properties from new-text to obj
                 (let ((pos 0))
                   (while (< pos len)
                     (let* ((props (text-properties-at pos new-text))
                            (next-change (or (next-property-change pos new-text) len)))
                       (when props
                         (set-text-properties (+ m-start pos)
                                              (+ m-start (min next-change len))
                                              props
                                              obj))
                       (setq pos next-change)))))
             ;; For buffers, delete and insert
             (unless (equal new-text text)
               (save-excursion
                 (delete-region m-start m-end)
                 (goto-char m-start)
                 (insert new-text)))))))
     property value object start end)))

;;;============================================================================
;;; Layer 3-4: Layer Definition and Management
;;;============================================================================

(defun tp--parse-define-layer-args (args)
  "Parse ARGS for tp--define-layer-internal function.
Returns plist with keys :props, :data, :watch, :compute, :transform.
- Keyword arguments: :props PLIST [:data DATA] [:watch WATCH] [:compute COMPUTE] [:transform FN]"
  (let (props data watch compute transform has-keywords)
    (cond
     ;; Check for keyword arguments format
     ((and (keywordp (car args))
           (memq (car args) '(:props :data :watch :compute :transform)))
      (setq has-keywords t)
      ;; Parse keyword arguments
      (let ((rest args))
        (while rest
          (pcase (car rest)
            (:props (setq props (cadr rest) rest (cddr rest)))
            (:data (setq data (cadr rest) rest (cddr rest)))
            (:watch (setq watch (cadr rest) rest (cddr rest)))
            (:compute (setq compute (cadr rest) rest (cddr rest)))
            (:transform (setq transform (cadr rest) rest (cddr rest)))
            (_ (error "Unknown keyword in tp--define-layer-internal: %s" (car rest))))))
      ;; Validate: if :watch, :compute, or :data present, :props must be present
      (when (and (or watch compute data) (null props))
        (error "When using :watch, :compute, or :data, :props must be explicitly specified")))
     ;; Format 1: single plist (the plist directly as first arg)
     ((and (= (length args) 1)
           (listp (car args)))
      (setq props (car args)))
     (t (error "Invalid tp--define-layer-internal format")))
    (list :props props :data data :watch watch :compute compute :transform transform)))

(defun tp--define-layer-internal (name &rest args)
  "Define a single text property layer named NAME.

This function supports two formats:

Format 1 - Direct plist (no :watch/:compute/:data/:transform support):
  (tp--define-layer-internal \\='layer-name
    \\='(display \"🌑\" face (:height 1.0)))

Format 2 - With :props, :data, :watch, :compute, and/or :transform (Vue 3 style reactivity):
  (tp--define-layer-internal \\='layer-name
    ;; props: $-prefixed symbols are reactive variables; auto-defined if not bound
    :props \\='(face (:foreground $my-color) help-echo $full-name)
    ;; data: additional reactive variables not used in props; auto-defined if not bound
    :data \\='((first-name . \"John\") (last-name . \"Doe\"))
    ;; compute: list of (VAR-NAME FUNCTION) - compute reactive variable values
    :compute \\='((full-name (lambda () (concat first-name \" \" last-name))))
    ;; watch: list of (VAR-NAME CALLBACK) - side effects when vars change
    :watch \\='((my-color (lambda (new old layer)
                        (message \"Color changed from %s to %s\" old new))))
    ;; transform: function to transform tp-text values before display
    :transform (lambda (text) (upcase text)))

Reactive Variables:
  If any symbol in :props starts with $, it is treated as a reactive variable.
  Variables in :data are also reactive. All reactive variables are automatically
  defined as global variables if they are not already bound.

:data - A list of variable symbols or cons cells (SYMBOL . INITIAL-VALUE)
  for additional reactive state not in :props.

:compute - A list of (VAR-SYMBOL COMPUTE-FN) pairs. COMPUTE-FN is evaluated
  to compute the value of VAR-SYMBOL. Can reference other reactive variables
  from both :props and :data.

:watch - A list of (VAR-SYMBOL CALLBACK) pairs. CALLBACK is called when
  VAR-SYMBOL changes, receiving (NEW-VALUE OLD-VALUE LAYER-NAME).

:transform - A function that receives the tp-text value and returns a
  transformed string. Useful for formatting numbers, dates, or other values
  before display. Example: (lambda (text) (format \"$%.2f\" (string-to-number text)))

Note: When using :watch, :compute, or :data, you MUST use :props to specify
the text properties explicitly.

If a layer with the same NAME already exists, it will be overwritten.
The layer is stored in `tp-layer-alist'."
  (declare (indent defun))
  (let* ((parsed (tp--parse-define-layer-args args))
         (properties (plist-get parsed :props))
         (data (plist-get parsed :data))
         (watch (plist-get parsed :watch))
         (compute (plist-get parsed :compute))
         (transform (plist-get parsed :transform))
         (reactive-syms (tp--collect-reactive-symbols properties))
         ;; Collect computed variable names (they become reactive too)
         (computed-vars (when compute (mapcar #'car compute)))
         ;; All variables that need to be reactive
         (all-reactive-syms (delete-dups (append reactive-syms)))
         ;; Variables from :props that need to be defined (without initial values)
         (props-vars (mapcar #'tp--reactive-var-symbol reactive-syms))
         ;; All variables to ensure are defined:
         ;; - :data entries (may have initial values as cons cells)
         ;; - :props reactive symbols (no initial values)
         ;; - :compute variable names (no initial values)
         (all-vars-to-define (delete-dups
                              (append data
                                      props-vars
                                      computed-vars))))
    ;; Register or unregister transform function
    (if transform
        (if (assoc name tp-layer-transforms)
            (setcdr (assoc name tp-layer-transforms) transform)
          (push (cons name transform) tp-layer-transforms))
      ;; Remove any existing transform when redefining without one
      (setq tp-layer-transforms (assq-delete-all name tp-layer-transforms)))
    (if (or all-reactive-syms data compute)
        ;; Has reactive features - register dependencies and resolve at runtime
        (progn
          ;; Clean up old reactive dependencies, watchers, computed properties, and data (for re-definition)
          (tp--unregister-reactive-deps name)
          ;; Ensure all reactive variables are defined
          (tp--ensure-reactive-variables all-vars-to-define)
          ;; Register data variables
          (when data
            (tp--register-layer-data name data))
          ;; Register computed variable definitions
          (when compute
            (tp--register-layer-computed name compute)
            ;; Apply initial computed values
            (tp--apply-initial-computed compute))
          ;; Register reactive dependencies
          (tp--register-reactive-deps name all-reactive-syms properties)
          ;; Register watchers
          (when watch
            (tp--register-layer-watchers name watch))
          ;; Set layer properties with resolved values
          (let ((resolved-props (tp--resolve-reactive-symbols properties)))
            (tp--set-layer-props name resolved-props))
          ;; Update any text regions that already have this layer applied
          ;; This ensures re-definition immediately updates applied text
          (tp--update-layer-regions name)
          (assoc name tp-layer-alist))
      ;; No reactive symbols - use static properties
      (progn
        ;; Clean up old reactive dependencies, watchers, computed properties, and data (for re-definition)
        (tp--unregister-reactive-deps name)
        (tp--set-layer-props name properties)
        ;; Update any text regions that already have this layer applied
        (tp--update-layer-regions name)
        (assoc name tp-layer-alist)))))

(defmacro define-tp (name arglist &rest body)
  "Define a text property layer named NAME.

This macro supports four formats:

Format 1 - Non-parameterized simple (empty arglist, simple body):
  (define-tp tp-bold ()
    \\='(face bold))

Format 2 - Parameterized simple (single argument, simple body):
  (define-tp tp-space (pixel)
    \\=`(display (space :width (,pixel))))

Format 3 - Non-parameterized with reactive features:
  (define-tp my-layer ()
    :props \\='(face (:foreground $my-color))
    :data \\='((my-color . \"red\"))
    :compute \\='((full-name (lambda () (concat first-name \" \" last-name))))
    :watch \\='((my-color (lambda (new old layer) (message \"Color changed!\"))))
    :transform (lambda (text) (upcase text)))

Format 4 - Parameterized with reactive features:
  (define-tp my-color-layer (color)
    :props \\=`(face (:foreground ,color))
    :data \\='((my-var . \"value\"))
    :compute ...)

Usage:
  (tp-set \"emacs\" \\='tp-bold t)
  (tp-set 0 5 \\='(tp-bold t) \"emacs\")
  ;; => #(\"emacs\" 0 5 (tp-name tp-bold face bold))

ARGLIST must be either:
- An empty list () for non-parameterized layers
- A list containing exactly one symbol for parameterized layers

BODY is either:
- A single property list expression (simple format)
- Keyword arguments starting with :props, :data, :compute, :watch, or :transform
  (reactive format)

Note: NAME cannot be a built-in Emacs text property name like `face',
`display', `invisible', etc. See `tp--builtin-text-properties' for the
complete list of reserved names."
  (declare (indent defun))
  (unless (listp arglist)
    (error "define-tp ARGLIST must be a list"))
  ;; Check for built-in text property name conflict
  (when (tp--builtin-text-property-p name)
    (error "define-tp: '%s' is a built-in Emacs text property name and cannot be used as a layer name" name))
  ;; Check if body starts with keyword (reactive format)
  (let ((first-elem (car body)))
    (if (and (keywordp first-elem)
             (memq first-elem '(:props :data :compute :watch :transform)))
        ;; Reactive format
        (if arglist
            ;; Parameterized reactive: store as function that calls tp--define-layer-internal
            (let ((arg (car arglist)))
              `(tp--define-layer-parameterized-reactive
                ',name ',arglist
                (lambda (,arg) (list ,@body))))
          ;; Non-parameterized reactive: use tp--define-layer-internal directly
          `(tp--define-layer-internal ',name ,@body))
      ;; Simple format (original behavior)
      (let ((simple-body (car body)))
        (cond
         ;; Non-parameterized: empty arglist - store as (LAYER-NAME nil BODY-FORM)
         ((null arglist)
          `(tp--define-layer-unified ',name nil ,simple-body))
         ;; Parameterized: single argument - store as (LAYER-NAME ARGLIST BODY-FORM)
         ((and (= (length arglist) 1)
               (symbolp (car arglist)))
          `(tp--define-layer-unified ',name ',arglist ',simple-body))
         (t
          (error "define-tp ARGLIST must be empty or contain exactly one symbol")))))))

(defun tp--define-layer-parameterized-reactive (name arglist body-fn)
  "Define a parameterized layer NAME with ARGLIST and reactive BODY-FN.
BODY-FN is a function that takes the parameter and returns a keyword plist
like (:props ... :data ... :compute ... :watch ... :transform ...).
The layer is stored and evaluated when tp-layer-props-with-arg is called."
  ;; Store the parameterized reactive definition
  ;; Format: (ARGLIST BODY-FN) where BODY-FN returns keyword plist
  (let ((entry (list arglist body-fn)))
    (if (assoc name tp-layer-alist)
        (setf (cdr (assoc name tp-layer-alist)) entry)
      (push (cons name entry) tp-layer-alist)))
  (assoc name tp-layer-alist))

(defun tp--define-layer-unified (name arglist body)
  "Define a layer NAME with ARGLIST and BODY using unified structure.
For non-parameterized layers, ARGLIST is nil and BODY is the evaluated plist.
For parameterized layers, ARGLIST contains one symbol and BODY is the unevaluated form.
Stores the layer in `tp-layer-alist' with format: (LAYER-NAME ARGLIST BODY-FORM).

For non-parameterized layers, if BODY contains reactive symbols ($-prefixed),
delegates to `tp--define-layer-internal' for proper reactive handling."
  (if arglist
      ;; Parameterized - store for later evaluation
      (let ((entry (list arglist body)))
        (if (assoc name tp-layer-alist)
            (setf (cdr (assoc name tp-layer-alist)) entry)
          (push (cons name entry) tp-layer-alist))
        (assoc name tp-layer-alist))
    ;; Non-parameterized - check for reactive symbols
    (let ((reactive-syms (tp--collect-reactive-symbols body)))
      (if reactive-syms
          ;; Has reactive symbols - use tp--define-layer-internal for proper handling
          (tp--define-layer-internal name body)
        ;; No reactive symbols - store as static layer
        ;; Clean up old reactive dependencies if the layer was previously reactive
        (tp--unregister-reactive-deps name)
        (let ((entry (list nil `',body)))
          (if (assoc name tp-layer-alist)
              (setf (cdr (assoc name tp-layer-alist)) entry)
            (push (cons name entry) tp-layer-alist))
          (assoc name tp-layer-alist))))))

(defun tp--layer-group-element-format (element)
  "Determine the format type of ELEMENT.
Returns 'symbol, 'format-1, 'format-2, 'format-3, 'format-4, or nil if invalid."
  (cond
   ;; Symbol - reference to existing layer
   ((symbolp element) 'symbol)
   ;; Format 4 - ("name" :props (plist...) [:data ...] [:watch ...] [:compute ...])
   ;; Named layer with :props and optional :data/:watch/:compute
   ((and (listp element)
         (> (length element) 3)
         (stringp (car element))
         (eq (cadr element) :props)
         (listp (caddr element))
         ;; Must have additional keywords after :props
         (let ((rest (cdddr element)))
           (and rest (keywordp (car rest)))))
    'format-4)
   ;; Format 3 - ("name" :props (plist...))
   ((and (listp element)
         (= (length element) 3)
         (stringp (car element))
         (eq (cadr element) :props)
         (listp (caddr element)))
    'format-3)
   ;; Format 2 - ("name" . (plist...)) - cons cell with proper list cdr
   ((and (consp element)
         (stringp (car element))
         (listp (cdr element))
         (not (eq (cadr element) :props)))  ; Distinguish from format-3
    'format-2)
   ;; Format 1 - (plist...) - anonymous, must start with a symbol
   ((and (listp element)
         (symbolp (car element)))
    'format-1)
   (t nil)))

(defun tp--parse-layer-group-element (group-name element idx)
  "Parse a layer group element and return (layer-name . properties) or extended form.
GROUP-NAME is the name of the layer group.
ELEMENT is the element to parse (can be anonymous plist, cons-cell, or :props form).
IDX is the index for anonymous elements.

Returns a cons cell (LAYER-NAME . PROPERTIES) or a symbol if ELEMENT
references an already-defined layer.
For format-4 elements, returns (LAYER-NAME :props PROPS :data DATA :watch WATCH :compute COMPUTE)."
  (let ((format (tp--layer-group-element-format element)))
    (pcase format
      ('symbol element)
      ('format-4
       ;; Parse named layer with :props and optional :data/:watch/:compute
       (let* ((layer-suffix (car element))
              (layer-name (intern (format "%s-%s" group-name layer-suffix)))
              (rest (cdr element))
              (props nil)
              (data nil)
              (watch nil)
              (compute nil))
         ;; Parse keyword arguments
         (while rest
           (pcase (car rest)
             (:props (setq props (cadr rest) rest (cddr rest)))
             (:data (setq data (cadr rest) rest (cddr rest)))
             (:watch (setq watch (cadr rest) rest (cddr rest)))
             (:compute (setq compute (cadr rest) rest (cddr rest)))
             (_ (setq rest (cdr rest)))))
         (list layer-name :props props :data data :watch watch :compute compute)))
      ('format-3
       (let* ((layer-suffix (car element))
              (layer-name (intern (format "%s-%s" group-name layer-suffix)))
              (props (caddr element)))
         (cons layer-name props)))
      ('format-2
       (let* ((layer-suffix (car element))
              (layer-name (intern (format "%s-%s" group-name layer-suffix)))
              (props (cdr element)))
         (cons layer-name props)))
      ('format-1
       (let ((layer-name (intern (format "%s-%d" group-name idx))))
         (cons layer-name element)))
      (_ (error "Invalid layer group element: %S" element)))))

(defun tp--define-layer-from-parsed (layer-name props data watch compute)
  "Internal helper to define a layer from parsed components.
LAYER-NAME is the symbol name for the layer.
PROPS is the property list.
DATA is the list of data variables.
WATCH is the list of watcher definitions.
COMPUTE is the list of computed variable definitions."
  (let* ((reactive-syms (tp--collect-reactive-symbols props))
         (computed-vars (when compute (mapcar #'car compute)))
         (all-reactive-syms (delete-dups reactive-syms))
         (props-vars (mapcar #'tp--reactive-var-symbol reactive-syms))
         (all-vars-to-define (delete-dups
                              (append data
                                      props-vars
                                      computed-vars))))
    (if (or all-reactive-syms data compute)
        ;; Has reactive features - register dependencies and resolve at runtime
        (progn
          (tp--unregister-reactive-deps layer-name)
          (tp--ensure-reactive-variables all-vars-to-define)
          (when data
            (tp--register-layer-data layer-name data))
          (when compute
            (tp--register-layer-computed layer-name compute)
            (tp--apply-initial-computed compute))
          (tp--register-reactive-deps layer-name all-reactive-syms props)
          (when watch
            (tp--register-layer-watchers layer-name watch))
          (let ((resolved-props (tp--resolve-reactive-symbols props)))
            (tp--set-layer-props layer-name resolved-props))
          (tp--update-layer-regions layer-name))
      ;; No reactive symbols - use static properties
      (progn
        (tp--unregister-reactive-deps layer-name)
        (tp--set-layer-props layer-name props)
        (tp--update-layer-regions layer-name)))
    layer-name))

(defun tp--define-layer-internal-group (name &rest elements)
  "Define a layer group named NAME containing multiple layers.

This function accepts a list of layer definitions in ELEMENTS.
Each element in ELEMENTS should be one of:

- A symbol: reference to an existing layer
- A plist: anonymous layer (named as NAME-0, NAME-1, etc.)
- A cons cell (\"suffix\" . plist): named layer (named as NAME-suffix)
- A list (\"suffix\" :props plist [:data data] [:watch watch] [:compute compute]):
  named layer with reactive features

All property lists should be evaluated (quoted in the call).

Example:
  (tp--define-layer-internal-group \\='my-group
    \\='existing-layer
    \\='(face bold)
    \\='(\"named\" . (face italic))
    \\='(\"reactive\" :props (face (:foreground $color))
                   :data ((color . \"red\"))))

If a layer group with the same NAME already exists, it will be overwritten.
Individual layers created by the group are stored in `tp-layer-alist',
and the group itself is stored in `tp-layer-groups'."
  (declare (indent defun))
  (let ((layer-names nil)
        (idx 0))
    (dolist (element elements)
      (let ((parsed (tp--parse-layer-group-element name element idx)))
        (cond
         ;; Reference to existing layer (symbol)
         ((symbolp parsed)
          (push parsed layer-names))
         ;; Extended format with :data/:watch/:compute (format-4)
         ((and (listp parsed) (plist-get (cdr parsed) :props))
          (let* ((layer-name (car parsed))
                 (props (plist-get (cdr parsed) :props))
                 (data (plist-get (cdr parsed) :data))
                 (watch (plist-get (cdr parsed) :watch))
                 (compute (plist-get (cdr parsed) :compute)))
            (tp--define-layer-from-parsed layer-name props data watch compute)
            (push layer-name layer-names)))
         ;; Simple format (cons cell of name . props)
         ((consp parsed)
          (let* ((layer-name (car parsed))
                 (props (cdr parsed)))
            (tp--define-layer-from-parsed layer-name props nil nil nil)
            (push layer-name layer-names)
            ;; Only increment idx for anonymous (Format 1) elements
            (when (eq (tp--layer-group-element-format element) 'format-1)
              (cl-incf idx)))))))
    (setq layer-names (nreverse layer-names))
    (tp--set-group-layers name layer-names)
    (assoc name tp-layer-groups)))

(defun tp--define-layer-group-internal (name arglist elements)
  "Internal function for define-tps with ARGLIST and ELEMENTS.
NAME is the group name symbol.
ARGLIST is nil for non-parameterized groups, or a list with one symbol.
ELEMENTS is the list of layer definitions."
  (if arglist
      ;; Parameterized group - store for later evaluation
      (let ((entry (list arglist elements)))
        (if (assoc name tp-layer-groups)
            (setf (cdr (assoc name tp-layer-groups)) entry)
          (push (cons name entry) tp-layer-groups))
        (assoc name tp-layer-groups))
    ;; Non-parameterized - define immediately using tp--define-layer-internal-group
    (apply #'tp--define-layer-internal-group name elements)))

(defun tp--define-layer-group-unified (name arglist body-form)
  "Define a parameterized layer group NAME with ARGLIST and BODY-FORM.
Stores the group in `tp-layer-groups' with format: (GROUP-NAME ARGLIST BODY-FORM)."
  (let ((entry (list arglist body-form)))
    (if (assoc name tp-layer-groups)
        (setf (cdr (assoc name tp-layer-groups)) entry)
      (push (cons name entry) tp-layer-groups)))
  (assoc name tp-layer-groups))

(defmacro define-tps (name arglist &rest body)
  "Define a text property group named NAME.

This macro defines a group of text properties (layers) that can be used together.
It follows the same format as `define-tp' for consistency.

ARGLIST must be either:
- An empty list () for non-parameterized groups
- A list containing exactly one symbol for parameterized groups

BODY contains the layer definitions, which should be quoted lists.

Format 1 - Non-parameterized (empty arglist):
  (define-tps my-moon-phases ()
    \\='(display \"🌑\")
    \\='(display \"🌕\"))

Format 2 - Parameterized (with argument):
  (define-tps my-status (color)
    \\=`((face (:foreground ,color)))
    \\='(face (:weight bold)))

Supported formats for each element in BODY:

Format 1 - Existing layer reference:
  \\='existing-layer-name

Format 2 - Anonymous layer (named as NAME-0, NAME-1, etc.):
  \\='(display \"🌑\" face (:height 1.0))

Format 3 - Named layer with cons-cell (named as NAME-suffix):
  \\='(\"新月\" . (display \"🌑\" face (:height 1.0)))

Format 4 - Named layer with :props keyword (named as NAME-suffix):
  \\='(\"新月\" :props (display \"🌑\" face (:height 1.0)))

Format 5 - Named layer with :props, :data, :watch, and/or :compute:
  \\='(\"reactive\" :props (face (:foreground $my-color))
               :data ((my-color . \"red\"))
               :watch ((my-color (lambda (new old layer) (message \"Changed!\")))))

Note: NAME cannot be a built-in Emacs text property name like `face',
`display', `invisible', etc. See `tp--builtin-text-properties' for the
complete list of reserved names."
  (declare (indent defun))
  (unless (listp arglist)
    (error "define-tps ARGLIST must be a list"))
  ;; Check for built-in text property name conflict
  (when (tp--builtin-text-property-p name)
    (error "define-tps: '%s' is a built-in Emacs text property name and cannot be used as a group name" name))
  (cond
   ;; Non-parameterized: empty arglist
   ((null arglist)
    `(tp--define-layer-group-internal ',name nil (list ,@body)))
   ;; Parameterized: single argument
   ((and (= (length arglist) 1)
         (symbolp (car arglist)))
    `(tp--define-layer-group-unified ',name ',arglist '(list ,@body)))
   (t
    (error "define-tps ARGLIST must be empty or contain exactly one symbol"))))

;; For backward compatibility, keep define-tp-group as an alias
(defalias 'define-tp-group 'define-tps
  "Alias for `define-tps' for backward compatibility.")

(defun tp--set-layer-props (layer-name properties)
  "Set PROPERTIES for layer LAYER-NAME in `tp-layer-alist'.
If the layer already exists, updates its properties; otherwise creates it.
Stores as (LAYER-NAME . PROPERTIES) for backward compatibility with reactive layers.
This is an internal function used by layer definition macros and reactive updates."
  (if (assoc layer-name tp-layer-alist)
      (setf (cdr (assoc layer-name tp-layer-alist)) properties)
    (push (cons layer-name properties) tp-layer-alist)))

(defun tp--set-group-layers (group-name layer-names)
  "Set LAYER-NAMES for group GROUP-NAME in `tp-layer-groups'.
If the group already exists, updates its layer list; otherwise creates it.
This is an internal function used by group definition macros."
  (if (assoc group-name tp-layer-groups)
      (setf (cdr (assoc group-name tp-layer-groups)) layer-names)
    (push (cons group-name layer-names) tp-layer-groups)))

(defun tp-layer-props (layer-name &optional include-tp-name)
  "Return properties for layer LAYER-NAME from `tp-layer-alist'.
If INCLUDE-TP-NAME is non-nil, appends 'tp-name property to identify the layer.
Also includes tp-name automatically if the layer has reactive dependencies registered.
Handles two storage formats:
1. Old format (from tp--set-layer-props): (LAYER-NAME . PLIST) - flat plist
2. Unified format (from define-tp): (LAYER-NAME ARGLIST BODY-FORM)
For parameterized layers (ARGLIST non-nil), returns nil - use `tp-layer-props-with-arg'.
Recursively expands any nested layer names in the returned plist."
  (when-let ((entry (cdr (assoc layer-name tp-layer-alist))))
    ;; Auto-include tp-name for layers with reactive deps
    (let ((needs-tp-name (or include-tp-name
                              (tp--layer-has-reactive-deps-p layer-name))))
      (cond
       ;; Unified format: entry is (ARGLIST BODY-FORM) where first elem is nil or a list
       ;; Check: exactly 2 elements and first is nil or a list of symbols
       ((and (= (length entry) 2)
             (or (null (car entry))
                 (and (listp (car entry))
                      (cl-every #'symbolp (car entry)))))
        (let ((arglist (car entry))
              (body (cadr entry)))
          (if arglist
              ;; Parameterized - needs argument, return nil
              nil
            ;; Non-parameterized - evaluate body and return props
            (let ((plist (eval body)))
              (when plist
                ;; Recursively expand nested layer names
                (when (tp--plist-has-layer-key-p plist)
                  (setq plist (tp--expand-layer-in-plist plist)))
                (if needs-tp-name
                    (append plist (list 'tp-name layer-name))
                  plist))))))
       ;; Old format: entry is just a flat plist
       (t
        (let ((plist entry))
          ;; Recursively expand nested layer names
          (when (tp--plist-has-layer-key-p plist)
            (setq plist (tp--expand-layer-in-plist plist)))
          (if needs-tp-name
              (append plist (list 'tp-name layer-name))
            plist)))))))

(defun tp-layer-parameterized-p (layer-name)
  "Return non-nil if LAYER-NAME is a parameterized layer.
Parameterized layers are stored in unified format (LAYER-NAME ARGLIST BODY-FORM)
where ARGLIST is a non-nil list of argument symbols."
  (when-let ((entry (cdr (assoc layer-name tp-layer-alist))))
    ;; Unified format: entry is (ARGLIST BODY-FORM) with exactly 2 elements
    ;; and first element is a non-nil list of symbols
    (and (= (length entry) 2)
         (listp (car entry))
         (not (null (car entry)))
         (cl-every #'symbolp (car entry)))))

(defun tp-layer-props-with-arg (layer-name arg &optional include-tp-name)
  "Return properties for parameterized layer LAYER-NAME with ARG.
Evaluates the body form with the argument bound to the parameter.
If INCLUDE-TP-NAME is non-nil, appends 'tp-name property to identify the layer.
Recursively expands any nested layer names in the returned plist."
  (when-let ((entry (cdr (assoc layer-name tp-layer-alist))))
    ;; entry is (ARGLIST BODY-FORM)
    (let ((arglist (car entry))
          (body (cadr entry)))
      (when arglist  ; Only for parameterized layers
        (let* ((arg-sym (car arglist))
               ;; Evaluate the body with the argument bound
               (plist (eval `(let ((,arg-sym ',arg)) ,body))))
          (when plist
            ;; Recursively expand nested layer names
            (when (tp--plist-has-layer-key-p plist)
              (setq plist (tp--expand-layer-in-plist plist)))
            (if include-tp-name
                (append plist (list 'tp-name layer-name))
              plist)))))))

(defun tp-group-props (group-name &optional include-tp-name)
  "Return list of properties for all layers in GROUP-NAME.
If INCLUDE-TP-NAME is non-nil, each layer's props will include tp-name.
Handles both old format (list of layer names) and new unified format
from `define-tps` (parameterized groups store ARGLIST and BODY-FORM)."
  (when-let ((entry (cdr (assoc group-name tp-layer-groups))))
    ;; Check if it's the unified format from define-tps (ARGLIST BODY-FORM)
    ;; Unified format: (ARGLIST BODY-FORM) where ARGLIST is a list of symbols or nil
    ;; Old format: (layer1 layer2 ...) where each element is a symbol referring to a layer
    (cond
     ;; Unified parameterized format: (ARGLIST BODY-FORM) with non-nil ARGLIST
     ((and (= (length entry) 2)
           (listp (car entry))
           (not (null (car entry)))
           (cl-every #'symbolp (car entry)))
      ;; Parameterized group - can't get props without argument
      nil)
     ;; Old format or non-parameterized define-tps: list of layer names
     (t
      (mapcar (lambda (layer)
                (tp-layer-props layer include-tp-name))
              entry)))))

(defun tp--is-layer-name-p (sym)
  "Return non-nil if SYM is a defined layer, parameterized layer, or group name."
  (and (symbolp sym)
       (or (assoc sym tp-layer-alist)
           (assoc sym tp-layer-groups))))

(defun tp--plist-has-layer-key-p (plist)
  "Return non-nil if PLIST contains any layer names as keys."
  (cl-loop for (key _val) on plist by #'cddr
           thereis (tp--is-layer-name-p key)))

(defun tp--expand-layer-in-plist (props)
  "Expand any layer names found in PROPS plist.
Scans through PROPS treating it as a plist (key value pairs).
When a key is a layer/group name, expands it with its properties.
Recursively expands until no more layer names are found in the result.
Does NOT add tp-name - this is for direct property setting (tp-set/add/reset).
Returns the expanded plist."
  (let ((result nil)
        (remaining props))
    (while remaining
      (let ((key (car remaining))
            (val (cadr remaining)))
        (cond
         ;; Key is a layer/parameterized layer/group name - expand it
         ((tp--is-layer-name-p key)
          (let ((layer-props
                 (cond
                  ;; Parameterized layer - evaluate with the argument (val)
                  ((tp-layer-parameterized-p key)
                   (tp-layer-props-with-arg key val nil))  ; no tp-name
                  ;; Non-parameterized layer - val should be t
                  ((assoc key tp-layer-alist)
                   (tp-layer-props key nil))  ; no tp-name
                  ;; Layer group - merge all layers' properties for direct setting
                  ((assoc key tp-layer-groups)
                   (when-let ((layer-props-list (tp-group-props key)))
                     ;; For direct property setting, merge all layers' properties
                     ;; without the tp-layers structure.
                     ;; Reverse so first layer's properties are applied last (take precedence)
                     (apply #'append (reverse layer-props-list)))))))
            (when layer-props
              ;; Recursively expand if the layer props contain more layer names
              (when (tp--plist-has-layer-key-p layer-props)
                (setq layer-props (tp--expand-layer-in-plist layer-props)))
              (setq result (append result layer-props)))))
         ;; Regular property - keep as-is
         (t
          (setq result (append result (list key val)))))
        (setq remaining (cddr remaining))))
    ;; Merge duplicate keys in the expanded result
    ;; Use (cdddr result) for O(1) check - need at least 4 elements (2 key-value pairs) for possible duplicates
    (if (cdddr result)
        (tp--merge-duplicate-keys result)
      result)))

(defun tp--resolve-props (props)
  "Resolve PROPS to a property list with layer metadata.
PROPS can be:
- A symbol (layer name from `tp-layer-alist' or group name from `tp-layer-groups')
- A two-element list (LAYER-NAME ARG) where LAYER-NAME is a defined layer
  and ARG is either `t' for non-parameterized layers or the argument value
  for parameterized layers
- A list starting with (LAYER-NAME ARG EXTRA-PROPS...) where extra properties
  are merged with the layer properties
- A plist with layer names at any position - they will be expanded inline
- A plist (handles anonymous layers with reactive variables)

If PROPS is a symbol:
- First checks `tp-layer-alist' and returns the layer properties WITH `tp-name'
- Then checks `tp-layer-groups' and returns properties WITH `tp-layers'

If PROPS is (LAYER-NAME ARG) or (LAYER-NAME ARG EXTRA-PROPS...):
- For non-parameterized layers: if ARG is t, returns the layer properties
- For parameterized layers: evaluates the body with ARG and returns the result
- Extra properties after ARG are appended to the layer properties

If PROPS is a plist with layer names at any position:
- Layer names are expanded inline with their properties
- Other properties are preserved in order

If PROPS is a plist:
- If it contains reactive variables ($...), generates a UUID for `tp-name',
  registers reactive dependencies, and returns the resolved props with `tp-name'.
  If the plist already has a `tp-name', uses that instead of generating a new one.
- If no reactive variables, returns props as-is (no tp-name added).

Returns nil if PROPS is a symbol but no matching layer/group is found.

For layer names, includes `tp-name' property for reactive text property support.
For group names, includes `tp-layers' property with the full layer stack."
  (cond
   ;; Already a plist - check for reactive variables and add tp-name
   ((listp props)
    (let ((first-elem (car-safe props))
          (second-elem (cadr props))
          (extra-props (cddr props)))
      (cond
       ;; Handle (layer-name arg ...) format for defined layers at the START
       ;; This includes both (layer-name arg) and (layer-name arg extra-prop val ...)
       ((and (>= (length props) 2)
             (tp--is-layer-name-p first-elem))
        (let ((layer-props
               (cond
                ;; Parameterized layer - evaluate with the argument
                ((tp-layer-parameterized-p first-elem)
                 (tp-layer-props-with-arg first-elem second-elem nil))  ; no tp-name
                ;; Non-parameterized layer - arg should be t, return the layer props
                ;; (silently ignore non-t values for flexibility)
                ((assoc first-elem tp-layer-alist)
                 (tp-layer-props first-elem nil))  ; no tp-name
                ;; Layer group - merge all layers' properties for direct setting
                ((assoc first-elem tp-layer-groups)
                 (when-let ((layer-props-list (tp-group-props first-elem)))
                   ;; For direct property setting, merge all layers' properties
                   ;; without the tp-layers structure.
                   ;; Reverse so first layer's properties are applied last (take precedence)
                   (apply #'append (reverse layer-props-list)))))))
          ;; Recursively resolve extra properties (they may also contain layer names)
          (let ((expanded-props
                 (if (and layer-props extra-props)
                     (let* ((resolved-extra (tp--expand-layer-in-plist extra-props))
                            (combined (append layer-props resolved-extra)))
                       ;; Merge duplicate keys after combining layer props with extra props
                       ;; Need at least 4 elements (2 key-value pairs) for possible duplicates
                       (if (cdddr combined)
                           (tp--merge-duplicate-keys combined)
                         combined))
                   layer-props)))
            ;; After expansion, check for reactive symbols in the merged props
            ;; (original props may contain $vars that need reactive tracking)
            (let ((reactive-syms (tp--collect-reactive-symbols props)))
              (if reactive-syms
                  ;; Has reactive symbols - need anonymous tp-name for reactive tracking
                  (let* ((existing-tp-name (plist-get props 'tp-name))
                         (layer-name (or existing-tp-name
                                         (tp--generate-anonymous-layer-name)))
                         ;; Resolve reactive symbols in expanded props
                         (resolved-props (tp--resolve-reactive-symbols expanded-props)))
                    ;; Register reactive dependencies
                    (tp--set-layer-props layer-name resolved-props)
                    (tp--register-reactive-deps layer-name reactive-syms props)
                    (append resolved-props (list 'tp-name layer-name)))
                ;; No reactive symbols - return expanded props as-is (no tp-name)
                expanded-props)))))
       
       ;; Handle single-element list containing a layer/group name symbol.
       ;; This can happen when tp-set is called with string form: (tp-set str 'layer-name)
       ;; which produces props = (layer-name) in tp--parse-args.
       ((and (= (length props) 1)
             (symbolp first-elem)
             (or (assoc first-elem tp-layer-alist)
                 (assoc first-elem tp-layer-groups)))
        ;; It's a layer/group name wrapped in a list - recurse with the symbol
        (tp--resolve-props first-elem))
       
       ;; Check if any key in the plist is a layer name (layer at any position)
       ((cl-some #'tp--is-layer-name-p
                 (cl-loop for (key _val) on props by #'cddr collect key))
        ;; Expand all layer names in the plist
        (let ((expanded-props (tp--expand-layer-in-plist props)))
          ;; After expansion, check for reactive symbols in the original props
          ;; (they may contain $vars that need reactive tracking)
          (let ((reactive-syms (tp--collect-reactive-symbols props)))
            (if reactive-syms
                ;; Has reactive symbols - need anonymous tp-name for reactive tracking
                (let* ((existing-tp-name (plist-get props 'tp-name))
                       (layer-name (or existing-tp-name
                                       (tp--generate-anonymous-layer-name)))
                       ;; Resolve reactive symbols in expanded props
                       (resolved-props (tp--resolve-reactive-symbols expanded-props)))
                  ;; Register reactive dependencies
                  (tp--set-layer-props layer-name resolved-props)
                  (tp--register-reactive-deps layer-name reactive-syms props)
                  (append resolved-props (list 'tp-name layer-name)))
              ;; No reactive symbols - return expanded props as-is (no tp-name)
              expanded-props))))
       
       ;; Normal plist processing
       (t
        (let* ((existing-tp-name (plist-get props 'tp-name))
               (reactive-syms (tp--collect-reactive-symbols props)))
          (if reactive-syms
              ;; Has reactive symbols - need to handle as anonymous reactive layer
              (let* ((layer-name (or existing-tp-name
                                     (tp--generate-anonymous-layer-name)))
                     ;; Resolve reactive symbols to get current values
                     (resolved-props (tp--resolve-reactive-symbols props)))
                ;; Register this anonymous layer in tp-layer-alist with resolved props
                (tp--set-layer-props layer-name resolved-props)
                ;; Register reactive dependencies with the original props
                (tp--register-reactive-deps layer-name reactive-syms props)
                ;; Return resolved props with tp-name for reactive tracking
                (append resolved-props (list 'tp-name layer-name)))
            ;; No reactive symbols - return props as-is (no tp-name needed)
            ;; This preserves the native text property behavior for non-reactive plists
            props))))))
   ;; Symbol - check if it's a layer or group name
   ((symbolp props)
    (cond
     ;; Parameterized layer without argument - cannot resolve, return nil
     ((tp-layer-parameterized-p props)
      nil)
     ;; Check layer - get props without tp-name for direct property setting
     ((assoc props tp-layer-alist)
      (tp-layer-props props nil))  ; no tp-name
     ;; Check group - merge all layers' properties for direct setting
     ((assoc props tp-layer-groups)
      (when-let ((layer-props-list (tp-group-props props)))
        ;; For direct property setting, merge all layers' properties
        ;; without the tp-layers structure.
        ;; Reverse so first layer's properties are applied last (take precedence)
        (apply #'append (reverse layer-props-list))))
     ;; Not found - return nil (let caller decide how to handle)
     (t nil)))
   (t nil)))

(defun tp--ensure-props (plist)
  "Ensure PLIST is a property list, resolving layer names and handling reactive vars.
If PLIST is a symbol, resolve it via `tp--resolve-props'.
If PLIST is a plist, also process it via `tp--resolve-props' to handle
anonymous reactive layers.
If resolution fails, return PLIST unchanged (for backward compatibility)."
  (or (tp--resolve-props plist) plist))

(defun tp-layer-reset ()
  "Reset all layer definitions.
Clears both `tp-layer-alist' and `tp-layer-groups'.
Also resets all reactive text property watchers, dependencies, and transforms."
  (interactive)
  (tp-reactive-reset)
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
  (setq tp-layer-transforms nil))

(defun tp-undefine-layer (name)
  "Remove layer NAME from `tp-layer-alist'.
Also unregisters any reactive dependencies and transforms for this layer."
  (tp--unregister-reactive-deps name)
  (setq tp-layer-alist (assq-delete-all name tp-layer-alist))
  (setq tp-layer-transforms (assq-delete-all name tp-layer-transforms)))

(defun tp-undefine-group (name)
  "Remove layer group NAME from `tp-layer-groups'."
  (setq tp-layer-groups (assq-delete-all name tp-layer-groups)))

;;;============================================================================
;;; Layer 3: Layer Stack Operations
;;;============================================================================

(defun tp-intervals-map (function start end &optional object)
  "Apply FUNCTION to all intervals between START and END in OBJECT.
FUNCTION receives (i-start i-end top-props below-props-lst)."
  (remove
   nil
   (mapcar
    (lambda (tp)
      (let* ((interval-start (nth 0 tp)) ;; start from 0
             (interval-end (nth 1 tp))
             (interval-props (nth 2 tp))
             (top-props
              (if-let ((idx (-elem-index 'tp-layers interval-props)))
                  (-remove-at-indices (list idx (1+ idx)) interval-props)
                interval-props))
             (below-props-lst (plist-get interval-props 'tp-layers)))
        (funcall function
                 interval-start interval-end
                 top-props below-props-lst)))
    (tp-intervals start end object))))

(defun tp-region-layer-props (start end layer-name &optional object)
  "Return layer properties for LAYER-NAME in region from START to END.
OBJECT defaults to current buffer.
Returns a list of (START END PROPERTIES) for matching intervals."
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (when-let ((props (seq-find
                        (lambda (props)
                          (equal layer-name
                                 (plist-get props 'tp-name)))
                        (append (list top) belows))))
       (list (+ start i-start) (+ start i-end) props)))
   start end object))

;;; --- Layer Stack Utilities ---

(defun tp--normalize-layer-spec (layer-spec)
  "Normalize LAYER-SPEC to a plist with tp-name.
Used by layer stack functions that need tp-name for identification.

LAYER-SPEC can be:
- A symbol (non-parameterized layer name from define-tp or tp--define-layer-internal)
- A list (LAYER-NAME ARG) for parameterized layers from define-tp
- A plist for inline layer definition
- A list (NAME &rest PLIST) for named inline layer"
  (cond
   ;; Symbol - look up in tp-layer-alist (non-parameterized layer)
   ((symbolp layer-spec)
    (cond
     ;; Parameterized layer symbol without arg - error
     ((tp-layer-parameterized-p layer-spec)
      (error "Parameterized layer %S requires an argument, use '(%S arg)" 
             layer-spec layer-spec))
     ;; Non-parameterized layer or old-format layer
     ((assoc layer-spec tp-layer-alist)
      (or (tp-layer-props layer-spec t)  ; include tp-name for layer stack
          (error "Layer %S not found in tp-layer-alist" layer-spec)))
     (t (error "Layer %S not found in tp-layer-alist" layer-spec))))
   
   ;; List starting with symbol - check if it's a parameterized layer
   ((and (listp layer-spec)
         (symbolp (car layer-spec))
         (not (keywordp (car layer-spec))))
    (let ((name (car layer-spec))
          (rest (cdr layer-spec)))
      (cond
       ;; Parameterized layer: (LAYER-NAME ARG)
       ((and (tp-layer-parameterized-p name)
             (= (length rest) 1))
        (or (tp-layer-props-with-arg name (car rest) t)  ; include tp-name
            (error "Failed to resolve parameterized layer %S with arg %S" 
                   name (car rest))))
       ;; Named inline layer: (NAME &rest PLIST)
       (rest
        (append rest (list 'tp-name name)))
       ;; Just a symbol in a list - treat as non-parameterized layer
       ((null rest)
        (or (tp-layer-props name t)
            (error "Layer %S not found in tp-layer-alist" name))))))
   
   ;; Plist (starts with keyword or property name)
   ((and (listp layer-spec) layer-spec)
    layer-spec)
   (t (error "Invalid layer spec: %S" layer-spec))))

(defun tp--get-layer-stack (pos object)
  "Get the layer stack at POS in OBJECT as a list.
Returns (TOP-PROPS . BELOW-PROPS-LIST)."
  (let* ((props (text-properties-at pos object))
         (tp-layers-idx (-elem-index 'tp-layers props))
         (top-props (if tp-layers-idx
                        (-remove-at-indices
                         (list tp-layers-idx (1+ tp-layers-idx)) props)
                      props))
         (below-props (plist-get props 'tp-layers)))
    (cons top-props below-props)))

(defun tp--build-layer-props (layer-list)
  "Build text properties from LAYER-LIST.
First element is top layer, rest are in tp-layers."
  (if (null layer-list)
      nil
    (append (car layer-list)
            (list 'tp-layers (cdr layer-list)))))

(defun tp--layer-stack-to-list (top belows)
  "Convert TOP and BELOWS to a flat list of layers."
  (if top
      (cons top belows)
    belows))

(defun tp--get-layer-by-idx-or-name (layers idx-or-name)
  "Find layer in LAYERS by IDX-OR-NAME.
Returns (index . layer-props) or nil."
  (cond
   ((integerp idx-or-name)
    (let ((actual-idx (if (< idx-or-name 0)
                          (+ (length layers) idx-or-name)
                        idx-or-name)))
      (when (and (>= actual-idx 0) (< actual-idx (length layers)))
        (cons actual-idx (nth actual-idx layers)))))
   ((symbolp idx-or-name)
    (cl-loop for layer in layers
             for i from 0
             when (equal idx-or-name (plist-get layer 'tp-name))
             return (cons i layer)))
   (t nil)))

(defun tp--parse-layer-args (args)
  "Parse flexible layer function arguments.
Returns (START END LAYER-SPEC IDX OBJECT) for buffer/string range,
or (STRING LAYER-SPEC IDX nil nil) for entire string."
  (cond
   ;; First arg is a string - apply to entire string
   ;; (tp-put-layer string layer idx)
   ((stringp (car args))
    (list (car args) (cadr args) (caddr args) nil nil))
   ;; First arg is a number - buffer/string region
   ;; (tp-put-layer start end layer idx object)
   ((numberp (car args))
    (list (car args) (cadr args) (caddr args) (cadddr args) (nth 4 args)))
   (t (error "Invalid arguments: %S" args))))

(defun tp-put-layer (start-or-string &optional end-or-layer layer-or-idx idx-or-object object)
  "Set layer(s) at a specific index position.

Calling conventions:
1. Buffer/string region:
   (tp-put-layer START END LAYER IDX OBJECT)
   
2. Entire string:
   (tp-put-layer STRING LAYER IDX)

LAYER can be:
- A symbol (layer name from tp-layer-alist or tp-layer-groups)
- A plist (inline layer definition)
- A list (NAME &rest PLIST) for named inline layer
- A list of the above for multiple layers

IDX specifies where to insert:
- 0 means top (visible layer)
- -1 means bottom
- Other values insert at that position

OBJECT defaults to current buffer for region form."
  (let (start end layer-spec idx obj)
    (cond
     ;; Entire string form: (tp-put-layer string layer idx)
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            layer-spec end-or-layer
            idx (or layer-or-idx 0)))
     ;; Region form: (tp-put-layer start end layer idx object)
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-layer
            layer-spec layer-or-idx
            idx (or idx-or-object 0)
            obj object)))
    
    ;; Normalize layer-spec to a list of layer property lists
    (let ((layers-to-add
           (cond
            ;; Check if it's a group name
            ((and (symbolp layer-spec)
                  (assoc layer-spec tp-layer-groups))
             (tp-group-props layer-spec t))  ; include tp-name for layer stack
            ;; Single layer spec
            ((or (symbolp layer-spec)
                 (and (listp layer-spec)
                      (or (keywordp (car layer-spec))
                          (and (symbolp (car layer-spec))
                               (cdr layer-spec)
                               (not (listp (cadr layer-spec)))))))
             (list (tp--normalize-layer-spec layer-spec)))
            ;; List of layer specs (multiple layers)
            ((and (listp layer-spec)
                  (listp (car layer-spec)))
             (mapcar #'tp--normalize-layer-spec layer-spec))
            (t (list (tp--normalize-layer-spec layer-spec))))))
      
      ;; Apply layers at specified index
      (if (tp-empty-p (or obj (current-buffer)))
          ;; No existing properties
          (set-text-properties start end
                               (tp--build-layer-props layers-to-add)
                               obj)
        ;; Has existing properties
        (tp-intervals-map
         (lambda (i-start i-end top belows)
           (let* ((current-stack (tp--layer-stack-to-list top belows))
                  (actual-idx (cond
                               ((= idx 0) 0)
                               ((< idx 0) (max 0 (+ (length current-stack) 1 idx)))
                               (t (min idx (length current-stack)))))
                  ;; Insert new layers at the specified position
                  (new-stack (append (seq-take current-stack actual-idx)
                                     layers-to-add
                                     (seq-drop current-stack actual-idx))))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))
         start end obj)))
    (or obj (cons start end))))

(defun tp-push-layer (start-or-string &optional end-or-layer layer-or-object object)
  "Push layer(s) to the top of the layer stack.

This is equivalent to (tp-put-layer ... LAYER 0 ...).

Calling conventions:
1. Buffer/string region:
   (tp-push-layer START END LAYER OBJECT)
   
2. Entire string:
   (tp-push-layer STRING LAYER)"
  (cond
   ((stringp start-or-string)
    (tp-put-layer start-or-string end-or-layer 0))
   ((numberp start-or-string)
    (tp-put-layer start-or-string end-or-layer layer-or-object 0 object))))

(defun tp-delete-layer (start-or-string &optional end-or-idx idx-or-object object)
  "Delete layer by name or index.

Calling conventions:
1. Buffer/string region:
   (tp-delete-layer START END LAYER-NAME/IDX OBJECT)
   
2. Entire string:
   (tp-delete-layer STRING LAYER-NAME/IDX)

LAYER-NAME/IDX can be:
- A symbol (layer name)
- An integer (layer index, 0=top, -1=bottom)"
  (let (start end layer-id obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            layer-id end-or-idx))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-idx
            layer-id idx-or-object
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (found (tp--get-layer-by-idx-or-name current-stack layer-id)))
         (when found
           (let ((new-stack (-remove-at (car found) current-stack)))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
     start end obj)
    nil))

(defun tp-pop-layer (start-or-string &optional end-or-object object)
  "Pop the top layer from the layer stack.

This is equivalent to (tp-delete-layer ... 0 ...).

Calling conventions:
1. Buffer/string region:
   (tp-pop-layer START END OBJECT)
   
2. Entire string:
   (tp-pop-layer STRING)"
  (cond
   ((stringp start-or-string)
    (tp-delete-layer start-or-string 0))
   ((numberp start-or-string)
    (tp-delete-layer start-or-string end-or-object 0 object))))

(defun tp--move-layer-in-stack (stack from-id to-idx)
  "Move layer at FROM-ID to TO-IDX position in STACK.
FROM-ID can be an integer index or a layer name symbol.
TO-IDX must be an integer index.
Both indices refer to positions before the move and can be negative (counting from end).
TO-IDX is clamped to valid range (0 to stack length - 1) if out of bounds.
Returns the new stack, or nil if FROM-ID is invalid."
  (let* ((len (length stack))
         ;; Resolve from-id to actual index
         (found (tp--get-layer-by-idx-or-name stack from-id))
         (actual-from (when found (car found)))
         ;; Normalize to-idx
         (actual-to (if (< to-idx 0)
                        (+ len to-idx)
                      to-idx)))
    ;; Only proceed if from-id is valid
    (when actual-from
      (let* ((layer-props (cdr found))
             (stack-without (-remove-at actual-from stack))
             ;; Clamp to-idx to valid range for insertion
             (clamped-to (max 0 (min actual-to (length stack-without)))))
        (append (seq-take stack-without clamped-to)
                (list layer-props)
                (seq-drop stack-without clamped-to))))))

(defun tp--raise-layer-in-stack (stack from-id n)
  "Raise layer at FROM-ID by N positions in STACK.
FROM-ID can be an integer index or a layer name symbol.
Positive N moves the layer up (toward top/visible).
Negative N moves the layer down (toward bottom).
The resulting position is clamped to valid range (0 to stack length - 1).
Returns the new stack, or nil if FROM-ID is invalid."
  (let* ((found (tp--get-layer-by-idx-or-name stack from-id))
         (actual-from (when found (car found))))
    (when actual-from
      (let* ((len (length stack))
             ;; Calculate new position: subtracting N because lower index = higher in stack
             (new-idx (max 0 (min (1- len) (- actual-from n)))))
        (tp--move-layer-in-stack stack actual-from new-idx)))))

(defun tp--switch-layers-in-stack (stack id1 id2)
  "Swap layers at ID1 and ID2 positions in STACK.
ID1 and ID2 can be integer indices or layer name symbols.
Returns the new stack, or nil if either ID is invalid."
  (let* ((found1 (tp--get-layer-by-idx-or-name stack id1))
         (found2 (tp--get-layer-by-idx-or-name stack id2)))
    (when (and found1 found2)
      (let* ((idx1 (car found1))
             (idx2 (car found2))
             (props1 (cdr found1))
             (props2 (cdr found2))
             (new-stack (copy-sequence stack)))
        (setf (nth idx1 new-stack) props2)
        (setf (nth idx2 new-stack) props1)
        new-stack))))

(defun tp-move-layer (start-or-string &optional end-or-from from-or-to to-or-object object)
  "Move a layer from one position to another in the layer stack.

Calling conventions:
1. Buffer/string region:
   (tp-move-layer START END FROM-ID TO-IDX OBJECT)

2. Entire string:
   (tp-move-layer STRING FROM-ID TO-IDX)

FROM-ID identifies the layer to move:
- An integer index (0 = top, 1 = second from top, -1 = bottom, etc.)
- A layer name symbol

TO-IDX is the target position (integer index):
- 0 means top (visible)
- Positive integers count from top
- -1 means bottom
- Negative integers count from bottom

Both indices refer to positions before the move.
The layer at FROM-ID is removed and inserted at TO-IDX position.
OBJECT defaults to current buffer for region form."
  (let (start end from-id to-idx obj)
    (cond
     ;; Entire string form: (tp-move-layer string from-id to-idx)
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            from-id end-or-from
            to-idx from-or-to))
     ;; Region form: (tp-move-layer start end from-id to-idx object)
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-from
            from-id from-or-to
            to-idx to-or-object
            obj object)))

    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (new-stack (tp--move-layer-in-stack current-stack from-id to-idx)))
         (when new-stack
           (set-text-properties
            (+ start i-start) (+ start i-end)
            (tp--build-layer-props new-stack)
            obj))))
     start end obj)
    nil))

(defun tp-raise-layer (start-or-string &optional end-or-idx idx-or-n n-or-object object)
  "Raise a layer by N positions in the stack.

Calling conventions:
1. Buffer/string region:
   (tp-raise-layer START END IDX/LAYER-NAME N OBJECT)
   
2. Entire string:
   (tp-raise-layer STRING IDX/LAYER-NAME N)

Positive N moves the layer up (toward top/visible).
Negative N moves the layer down (toward bottom).

Uses `tp--raise-layer-in-stack' internally, which is built on `tp--move-layer-in-stack'."
  (let (start end layer-id n obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            layer-id end-or-idx
            n (or idx-or-n 1)))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-idx
            layer-id idx-or-n
            n (or n-or-object 1)
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (new-stack (tp--raise-layer-in-stack current-stack layer-id n)))
         (when new-stack
           (set-text-properties
            (+ start i-start) (+ start i-end)
            (tp--build-layer-props new-stack)
            obj))))
     start end obj)
    nil))

(defun tp-rotate-layer (start-or-string &optional end-or-object object)
  "Rotate layers, moving top layer to bottom.

Calling conventions:
1. Buffer/string region:
   (tp-rotate-layer START END OBJECT)
   
2. Entire string:
   (tp-rotate-layer STRING)

Uses `tp-move-layer' internally to move layer at index 0 to index -1."
  (cond
   ((stringp start-or-string)
    (tp-move-layer start-or-string 0 -1))
   ((numberp start-or-string)
    (tp-move-layer start-or-string end-or-object 0 -1 object))))

(defun tp-pin-layer (start-or-string &optional end-or-idx idx-or-object object)
  "Pin a layer to the top (make it visible).

Calling conventions:
1. Buffer/string region:
   (tp-pin-layer START END IDX/LAYER-NAME OBJECT)
   
2. Entire string:
   (tp-pin-layer STRING IDX/LAYER-NAME)

Uses `tp-move-layer' internally to move the specified layer to index 0 (top)."
  (cond
   ((stringp start-or-string)
    (tp-move-layer start-or-string end-or-idx 0))
   ((numberp start-or-string)
    (tp-move-layer start-or-string end-or-idx idx-or-object 0 object))))

(defun tp-switch-layer (start-or-string &optional end-or-id1 id1-or-id2 id2-or-object object)
  "Switch between two layers by name or index.

Calling conventions:
1. Buffer/string region:
   (tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)
   
2. Entire string:
   (tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)

Uses `tp--switch-layers-in-stack' internally."
  (let (start end id1 id2 obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            id1 end-or-id1
            id2 id1-or-id2))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-id1
            id1 id1-or-id2
            id2 id2-or-object
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (new-stack (tp--switch-layers-in-stack current-stack id1 id2)))
         (when new-stack
           (set-text-properties
            (+ start i-start) (+ start i-end)
            (tp--build-layer-props new-stack)
            obj))))
     start end obj)
    nil))

(defun tp-merge-layers (start-or-string &optional end-or-name name-or-ids ids-or-object object)
  "Merge specified layers into a new layer.

Calling conventions:
1. Buffer/string region:
   (tp-merge-layers START END NEW-LAYER-NAME \\='(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)
   
2. Entire string:
   (tp-merge-layers STRING NEW-LAYER-NAME \\='(IDX1 LAYER-NAME1 IDX2 ...))"
  (let (start end new-name layer-ids obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            new-name end-or-name
            layer-ids name-or-ids))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-name
            new-name name-or-ids
            layer-ids ids-or-object
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              ;; Find all layers to merge
              (layers-to-merge
               (cl-loop for id in layer-ids
                        for found = (tp--get-layer-by-idx-or-name current-stack id)
                        when found collect found))
              ;; Sort by index (descending) to remove from end first
              (sorted-layers (sort (copy-sequence layers-to-merge)
                                   (lambda (a b) (> (car a) (car b))))))
         (when layers-to-merge
           ;; Merge properties (earlier in list takes precedence)
           (let* ((merged-props
                   (cl-reduce (lambda (acc layer)
                                (let ((props (cdr layer)))
                                  (cl-loop for (key val) on props by #'cddr
                                           do (unless (plist-get acc key)
                                                (setq acc (plist-put acc key val))))
                                  acc))
                              layers-to-merge
                              :initial-value (list 'tp-name new-name)))
                  ;; Remove old layers from stack
                  (indices-to-remove (mapcar #'car sorted-layers))
                  (new-stack current-stack))
             (dolist (idx indices-to-remove)
               (setq new-stack (-remove-at idx new-stack)))
             ;; Add merged layer at top
             (setq new-stack (cons merged-props new-stack))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
     start end obj)
    nil))

(defun tp-flatten-layers (start-or-string &optional end-or-name name-or-object object)
  "Flatten all layers into a single layer.

Calling conventions:
1. Buffer/string region:
   (tp-flatten-layers START END NAME OBJECT)
   
2. Entire string:
   (tp-flatten-layers STRING NAME)

NAME can be nil for an unnamed layer."
  (let (start end name obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)
            name end-or-name))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-name
            name name-or-object
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (layer-count (length current-stack)))
         (when (> layer-count 0)
           ;; Create list of all indices
           (let ((all-ids (cl-loop for i from 0 below layer-count collect i)))
             ;; Use merge with all layers
             (let* ((layers-to-merge
                     (cl-loop for id in all-ids
                              for found = (tp--get-layer-by-idx-or-name
                                           current-stack id)
                              when found collect found))
                    (merged-props
                     (cl-reduce (lambda (acc layer)
                                  (let ((props (cdr layer)))
                                    (cl-loop for (key val) on props by #'cddr
                                             unless (eq key 'tp-name)
                                             do (unless (plist-get acc key)
                                                  (setq acc (plist-put acc key val))))
                                    acc))
                                layers-to-merge
                                :initial-value (if name (list 'tp-name name) nil))))
               (set-text-properties
                (+ start i-start) (+ start i-end)
                merged-props
                obj))))))
     start end obj)
    nil))

;;; --- Layer Query Functions ---

(defun tp-layer-list (start end &optional object)
  "Return list of all layer names in region from START to END."
  (let ((layers nil))
    (tp-intervals-map
     (lambda (_i-start _i-end top belows)
       (when-let ((name (plist-get top 'tp-name)))
         (cl-pushnew name layers :test #'equal))
       (dolist (below belows)
         (when-let ((name (plist-get below 'tp-name)))
           (cl-pushnew name layers :test #'equal))))
     start end object)
    (nreverse layers)))

(defun tp-layer-count (start end &optional object)
  "Return number of layers in region from START to END.
OBJECT defaults to current buffer."
  (let ((max-count 0))
    (tp-intervals-map
     (lambda (_i-start _i-end top belows)
       (let ((count (+ (if top 1 0) (length belows))))
         (when (> count max-count)
           (setq max-count count))))
     start end object)
    max-count))

(defun tp-layer-exists-p (start end name &optional object)
  "Return t if layer NAME exists in region from START to END.
OBJECT defaults to current buffer."
  (not (null (tp-region-layer-props start end name object))))

(defun tp-layer-top (start end &optional object)
  "Return the name of the top layer at START in OBJECT.
OBJECT defaults to current buffer."
  (when-let ((intervals (tp-intervals start end object)))
    (plist-get (nth 2 (car intervals)) 'tp-name)))

;;; --- Layer Property Manipulation Functions ---

(defun tp-add-to-layers (idx-or-layer-name-list start-or-string &optional end-or-plist plist-or-object &rest rest)
  "Add/merge properties to specified layers.

IDX-OR-LAYER-NAME-LIST is a list of layer indices (integers) or
layer names (symbols) specifying which layers to add properties to.
For indices: 0 means top layer, -1 means bottom layer.

For region form, PLIST is a property list to merge into the specified layers.
For string form, PROP VAL ... are property-value pairs to merge.
Properties are deeply merged (nested plists are merged, not replaced).

OBJECT defaults to current buffer for region form.

Returns the modified object (string) or nil for buffer operations."
  (let (start end plist obj layer-ids)
    (setq layer-ids idx-or-layer-name-list)
    (cond
     ;; Entire string form: (tp-add-to-layers ids string prop val ...)
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string))
      ;; Construct plist from end-or-plist, plist-or-object, and rest
      (when end-or-plist
        (setq plist (cons end-or-plist
                          (if plist-or-object
                              (cons plist-or-object rest)
                            rest)))))
     ;; Region form: (tp-add-to-layers ids start end plist object)
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-plist
            plist plist-or-object
            obj (car rest))))

    ;; Handle plist wrapped in a list (from region form)
    (when (and (listp plist)
               (not (keywordp (car-safe plist)))
               (listp (car-safe plist)))
      (setq plist (car plist)))

    ;; Process each interval
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let* ((current-stack (tp--layer-stack-to-list top belows))
              (modified-stack
               (cl-loop for layer in current-stack
                        for i from 0
                        collect
                        (if (cl-some
                             (lambda (id)
                               (let ((found (tp--get-layer-by-idx-or-name
                                             current-stack id)))
                                 (and found (= (car found) i))))
                             layer-ids)
                            ;; Merge plist into this layer
                            (tp--deep-merge-plist layer plist)
                          ;; Keep layer unchanged
                          layer))))
         (set-text-properties
          (+ start i-start) (+ start i-end)
          (tp--build-layer-props modified-stack)
          obj)))
     start end obj)
    (if (stringp obj) obj nil)))

(defun tp-add-to-all-layers (start-or-string &optional end-or-plist plist-or-object &rest rest)
  "Add/merge properties to all layers.

This function supports two calling conventions:

1. Buffer/string region:
   (tp-add-to-all-layers START END PLIST OBJECT)

2. Entire string:
   (tp-add-to-all-layers STRING PROP VAL ...)

For region form, PLIST is a property list to merge into all layers.
For string form, PROP VAL ... are property-value pairs to merge.
Properties are deeply merged (nested plists are merged, not replaced).

OBJECT defaults to current buffer for region form.

This function uses `tp-add-to-layers' internally, collecting all
layer indices and passing them to add the plist to every layer.

Returns the modified object (string) or nil for buffer operations."
  (let (start end plist obj)
    (cond
     ;; Entire string form: (tp-add-to-all-layers string prop val ...)
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string))
      ;; Construct plist from end-or-plist, plist-or-object, and rest
      (when end-or-plist
        (setq plist (cons end-or-plist
                          (if plist-or-object
                              (cons plist-or-object rest)
                            rest)))))
     ;; Region form: (tp-add-to-all-layers start end plist object)
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-plist
            plist plist-or-object
            obj (car rest))))

    ;; Handle plist wrapped in a list (from region form)
    (when (and (listp plist)
               (not (keywordp (car-safe plist)))
               (listp (car-safe plist)))
      (setq plist (car plist)))

    ;; Get the maximum layer count in the region to build a list of all indices
    (let ((max-count (tp-layer-count start end obj)))
      (when (> max-count 0)
        (let ((all-indices (cl-loop for i from 0 below max-count collect i)))
          (tp-add-to-layers all-indices start end plist obj))))
    (if (stringp obj) obj nil)))

(defmacro tp-pop-to-buffer (buffer-or-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-or-name)))
     (tp-with-current-buffer buffer
       (erase-buffer)
       ,@body         
       (read-only-mode 1))
     (pop-to-buffer buffer)))

;;; Utilities

(defun tp-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun tp-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun tp-parse-color (color)
  "e.g.1 (tp-parse-color \"red\")
e.g.2 (tp-parse-color '(\"red\" . \"green\"))
e.g.3 (tp-parse-color '(:light \"red\" :dark \"green\"))"
  (cond ((stringp color) color)
        ((and (consp color)
              (stringp (car color))
              (stringp (cdr color)))
         (cond
          ((tp-theme-light-p) (car color))
          ((tp-theme-dark-p) (cdr color))))
        ((and (plistp color)
              (or (plist-member color :light)
                  (plist-member color :dark)))
         (cond
          ((tp-theme-light-p) (plist-get color :light))
          ((tp-theme-dark-p) (plist-get color :dark))))
        ((null color) nil)
        (t (error "Invalid format of color %S" color))))

(require 'tp-palette)

;;; Built-in text properties

(define-tp tp-palette (palette)
  `(face (,@(when-let ((color (tp-palette-fg-color palette)))
              `(:foreground ,color))
          ,@(when-let ((color (tp-palette-bg-color palette)))
              `(:background ,color))
          ,@(when-let ((color (tp-palette-border-color palette)))
              `(:box (:color ,color))))))

(define-tp tp-fg (color)
  `(face (:foreground ,color)))

(define-tp tp-bg (color)
  `(face (:background ,color)))

(define-tp tp-button (plist)
  (let ((palette (plist-get plist :palette))
        (action (plist-get plist :action)))
    `( tp-palette ,palette 
       keymap ,(let ((keymap (make-sparse-keymap)))
                 (define-key keymap (kbd "<RET>") action)
                 (define-key keymap [mouse-1] action)
                 keymap))))

(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

(define-tp tp-headline (props)
  (let (height boldp)
    (cond ((floatp props)
           (setq height props boldp t))
          ((plistp props)
           (setq height (plist-get props :height)
                 boldp (plist-get props :bold))))
    `(face (:height ,height
                    ,@(when boldp '(:weight bold))))))

(provide 'tp)
;;; tp.el ends here
