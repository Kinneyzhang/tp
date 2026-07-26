;;; tp-layer.el --- Layer definition and registry for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; The layer registry: `define-tp' / `define-tps' and all machinery to
;; define, store, resolve and expand named property layers and groups,
;; plus the layer-stack representation helpers shared with tp-stack.el.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tp-core)
(require 'tp-reactive)

(defvar tp--layer-refresh-function nil
  "Function re-rendering regions that carry a given layer, or nil.
Installed by tp-render.el.  Called with (LAYER-NAME) after a layer is
redefined, so text that already uses the layer picks up the new
definition.  When nil, redefinition only updates the registry.")

(defun tp--layer-refresh (layer-name)
  "Re-render regions carrying LAYER-NAME via `tp--layer-refresh-function'."
  (when tp--layer-refresh-function
    (funcall tp--layer-refresh-function layer-name)))

(defvar tp-layer-alist nil
  "Alist of layer definitions: (LAYER-NAME . PROPERTIES).")

(defvar tp-layer-groups nil
  "Alist of layer groups: (GROUP-NAME . (LAYER-NAME1 LAYER-NAME2 ...)).")

(defvar tp-layer-transforms nil
  "Alist of layer transforms: (LAYER-NAME . TRANSFORM-FN).
TRANSFORM-FN receives the value and returns the transformed value.
Used for tp-text transformations like formatting numbers or dates.")

(defun tp--expand-layer-to-props-list (layer-name str start)
  "Expand LAYER-NAME to a list of property keys it contributes.
If LAYER-NAME is a layer defined in `tp-layer-alist', returns a list
of the property keys that the layer adds, plus 'tp-name.
STR and START are used to get the argument value for parameterized layers.
For non-layer symbols, returns a list containing just that symbol."
  (if (tp--is-layer-name-p layer-name)
      (let* ((existing-props (text-properties-at start str))
             (existing-tp-name (plist-get existing-props 'tp-name))
             (layer-prop-value (plist-get existing-props layer-name))
             ;; Proceed if tp-name matches OR if the layer property exists
             ;; (for cases where layer was used in mixed syntax without tp-name)
             (layer-props
              (cond
               ;; tp-name matches - traditional layer application
               ((eq existing-tp-name layer-name)
                (cond
                 ;; Parameterized layer - get property keys it would produce
                 ;; We pass a dummy arg (t) since we only need the key names, not values
                 ((tp-layer-parameterized-p layer-name)
                  (tp-layer-props-with-arg layer-name t nil)) ; arg=t, include-tp-name=nil
                 ;; Non-parameterized layer
                 ((assoc layer-name tp-layer-alist)
                  (tp-layer-props layer-name nil)) ; include-tp-name=nil
                 ;; Layer group
                 ((assoc layer-name tp-layer-groups)
                  (when-let ((layer-props-list (tp-group-props layer-name t)))
                    (tp--build-layer-props layer-props-list)))))
               ;; Layer property exists (mixed syntax like `tp-set str 'face 'bold 'layer arg`)
               ;; In this case, the layer's face properties are merged into face
               (layer-prop-value
                (cond
                 ((tp-layer-parameterized-p layer-name)
                  (tp-layer-props-with-arg layer-name layer-prop-value nil))
                 ((assoc layer-name tp-layer-alist)
                  (tp-layer-props layer-name nil))
                 ((assoc layer-name tp-layer-groups)
                  (when-let ((layer-props-list (tp-group-props layer-name t)))
                    (tp--build-layer-props layer-props-list))))))))
        (if layer-props
            ;; Return all property keys from the layer plus tp-name and the layer itself
            (let ((keys (cl-loop for (key _val) on layer-props by #'cddr
                                 collect key)))
              (unless (memq 'tp-name keys)
                (push 'tp-name keys))
              (unless (memq layer-name keys)
                (push layer-name keys))
              keys)
          ;; Layer name doesn't match tp-name and layer property doesn't exist
          ;; Just remove the literal symbol
          (list layer-name)))
    ;; Not a layer name, just return the symbol itself
    (list layer-name)))

(defun tp--expand-props-to-remove (props-to-remove str start)
  "Expand PROPS-TO-REMOVE list, expanding any layer names to their property keys.
STR and START are used to determine context for parameterized layers."
  (let ((result nil))
    (dolist (prop props-to-remove)
      (dolist (expanded (tp--expand-layer-to-props-list prop str start))
        (unless (memq expanded result)
          (push expanded result))))
    (nreverse result)))

(defun tp--get-layer-face-contribution (layer-name layer-prop-value)
  "Get the face contribution from LAYER-NAME.
LAYER-PROP-VALUE is the value of the layer property (the argument passed to it).
Returns the face value that the layer adds, or nil if no face contribution."
  (when (tp--is-layer-name-p layer-name)
    (let ((layer-props
           (cond
            ((tp-layer-parameterized-p layer-name)
             (tp-layer-props-with-arg layer-name layer-prop-value nil))
            ((assoc layer-name tp-layer-alist)
             (tp-layer-props layer-name nil))
            ((assoc layer-name tp-layer-groups)
             (when-let ((layer-props-list (tp-group-props layer-name t)))
               (tp--build-layer-props layer-props-list))))))
      (when layer-props
        (plist-get layer-props 'face)))))

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
          (tp--layer-refresh name)
          (assoc name tp-layer-alist))
      ;; No reactive symbols - use static properties
      (progn
        ;; Clean up old reactive dependencies, watchers, computed properties, and data (for re-definition)
        (tp--unregister-reactive-deps name)
        (tp--set-layer-props name properties)
        ;; Update any text regions that already have this layer applied
        (tp--layer-refresh name)
        (assoc name tp-layer-alist)))))

(defmacro define-tp (name arglist &rest body)
  "Define a text property layer named NAME.

This macro supports three formats:

Format 1 - Non-parameterized simple (empty arglist, simple body):
  (define-tp tp-bold ()
    \\='(face bold))

Format 2 - Parameterized simple (single argument, simple body):
  (define-tp tp-space (pixel)
    \\=`(display (space :width (,pixel))))

Format 3 - Non-parameterized with reactive features (requires $-prefixed variables):
  (define-tp my-layer ()
    :props \\='(face (:foreground $my-color))
    :data \\='((my-color . \"red\"))
    :compute \\='((full-name (lambda () (concat first-name \" \" last-name))))
    :watch \\='((my-color (lambda (new old layer) (message \"Color changed!\"))))
    :transform (lambda (text) (upcase text)))

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
  (reactive format - only for non-parameterized layers with $-prefixed variables)

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
        ;; Reactive format - only allowed for non-parameterized layers
        (if arglist
            (error "define-tp: reactive keywords (:props, :data, :compute, :watch, :transform) are only supported for non-parameterized layers (empty arglist)")
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
          (tp--layer-refresh layer-name))
      ;; No reactive symbols - use static properties
      (progn
        (tp--unregister-reactive-deps layer-name)
        (tp--set-layer-props layer-name props)
        (tp--layer-refresh layer-name)))
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

(defun tp-group-parameterized-p (group-name)
  "Return non-nil if GROUP-NAME is a parameterized group.
Parameterized groups are stored in format (GROUP-NAME ARGLIST BODY-FORM)
where ARGLIST is a non-nil list of argument symbols."
  (when-let ((entry (cdr (assoc group-name tp-layer-groups))))
    ;; Check for unified format: (ARGLIST BODY-FORM) with non-nil ARGLIST
    (and (= (length entry) 2)
         (listp (car entry))
         (not (null (car entry)))
         (cl-every #'symbolp (car entry)))))

(defun tp-group-props-with-arg (group-name arg &optional include-tp-name)
  "Return list of properties for parameterized group GROUP-NAME with ARG.
Evaluates the body form with the argument bound to the parameter.
Each evaluated element is a layer reference like (layer-name arg) or just layer-name.
Returns a list of property lists for each layer in the group."
  (when-let ((entry (cdr (assoc group-name tp-layer-groups))))
    ;; entry is (ARGLIST BODY-FORM)
    (let ((arglist (car entry))
          (body-form (cadr entry)))
      (when arglist  ; Only for parameterized groups
        (let* ((arg-sym (car arglist))
               ;; Evaluate the body with the argument bound - returns list of layer specs
               (layer-specs (eval `(let ((,arg-sym ',arg)) ,body-form))))
          ;; Convert layer specs to property lists
          (mapcar (lambda (spec)
                    (cond
                     ;; spec is a symbol - just a layer name
                     ((symbolp spec)
                      (tp-layer-props spec include-tp-name))
                     ;; spec is (layer-name arg) - parameterized layer
                     ((and (listp spec) (symbolp (car spec)))
                      (let ((layer-name (car spec))
                            (layer-arg (cadr spec)))
                        (if (tp-layer-parameterized-p layer-name)
                            (tp-layer-props-with-arg layer-name layer-arg include-tp-name)
                          ;; Non-parameterized layer - arg should be t or ignored
                          (tp-layer-props layer-name include-tp-name))))
                     (t nil)))
                  layer-specs))))))

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
                  ;; Parameterized layer group - evaluate with the argument (val)
                  ((tp-group-parameterized-p key)
                   (when-let ((layer-props-list (tp-group-props-with-arg key val t)))
                     ;; Build layered structure: first layer at top, rest in tp-layers
                     (tp--build-layer-props layer-props-list)))
                  ;; Non-parameterized layer group - build layered structure
                  ((assoc key tp-layer-groups)
                   (when-let ((layer-props-list (tp-group-props key t)))
                     ;; Build layered structure: first layer at top, rest in tp-layers
                     (tp--build-layer-props layer-props-list))))))
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
                ;; Parameterized layer group - evaluate with the argument
                ((tp-group-parameterized-p first-elem)
                 (when-let ((layer-props-list (tp-group-props-with-arg first-elem second-elem t)))
                   ;; Build layered structure: first layer at top, rest in tp-layers
                   (tp--build-layer-props layer-props-list)))
                ;; Non-parameterized layer group - build layered structure
                ((assoc first-elem tp-layer-groups)
                 (when-let ((layer-props-list (tp-group-props first-elem t)))
                   ;; Build layered structure: first layer at top, rest in tp-layers
                   (tp--build-layer-props layer-props-list))))))
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
     ;; Check group - build layered structure with tp-layers
     ((assoc props tp-layer-groups)
      (when-let ((layer-props-list (tp-group-props props t)))  ; include tp-name
        ;; Build layered structure: first layer at top, rest in tp-layers
        (tp--build-layer-props layer-props-list)))
     ;; Parameterized group without argument - cannot resolve, return nil
     ((tp-group-parameterized-p props)
      nil)
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

(provide 'tp-layer)
;;; tp-layer.el ends here
