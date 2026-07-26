;;; tp-ops.el --- Core text property operations for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; The public property primitives: `tp-set', `tp-reset', `tp-add',
;; `tp-get', `tp-at', `tp-remove', `tp-clear', built on the shared
;; argument parser.  Layer names in property specs are resolved through
;; tp-layer.el.  The reactive `tp-text' property is handled through
;; `tp--tp-text-handler-function', installed by tp-render.el.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tp-core)
(require 'tp-layer)

(defvar tp--tp-text-handler-function nil
  "Function that handles the reactive `tp-text' property, or nil.
Installed by tp-render.el.  Called with (START END PROPS OBJECT
PRESERVE-PROPS MERGE-MODE) and must return (PROPS NEW-END NEW-OBJECT).
When nil, `tp-text' is treated as an ordinary text property.")

(defun tp--handle-tp-text (start end props object preserve-props merge-mode)
  "Dispatch `tp-text' handling for PROPS between START and END in OBJECT.
PRESERVE-PROPS and MERGE-MODE are forwarded to the installed handler.
Returns (PROPS NEW-END NEW-OBJECT); a pass-through when no handler is
installed (see `tp--tp-text-handler-function')."
  (if tp--tp-text-handler-function
      (funcall tp--tp-text-handler-function
               start end props object preserve-props merge-mode)
    (list props end object)))

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
       ;; Always include props-or-val even if it's nil, to handle (tp-set "str" 'prop nil)
       (t
        (setq props (if end-or-prop
                        (cons end-or-prop (cons props-or-val rest))
                      nil)))))
     ;; First arg is a number - region convention
     ((numberp start-or-string)
      (setq start start-or-string
            finish end-or-prop
            props props-or-val)
      ;; Check if 4th arg (first of rest) is a buffer or string
      (let ((extra rest))
        (when (and extra (or (bufferp (car extra))
                             (stringp (car extra))))
          (setq object (car extra)
                extra (cdr extra)))
        ;; Anything left over is not a valid region-form argument.
        ;; In particular, flat PROP/VAL pairs like (tp-set 1 4 'face 'bold)
        ;; are only supported in the whole-string form; region form takes
        ;; a plist.  Signal immediately instead of silently discarding.
        (when extra
          (error "Region form takes a properties plist: (tp-set START END '(PROP VAL ...) &optional OBJECT); flat PROP/VAL arguments like %S are only supported in the whole-string form"
                 (car extra)))))
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

(defun tp--apply-props-to-string (str start end props &optional merge-mode)
  "Apply PROPS to string STR from START to END, returning a NEW string.
This function does not modify the original string.
Preserves the original text property intervals.

MERGE-MODE controls how properties are applied:
  nil or :set - Set properties, preserving existing unspecified ones
  :reset - Completely replace all properties
  :add - Merge properties deeply (for face, prepend symbols)

Returns a new propertized string."
  (let* ((len (length str))
         ;; Ensure bounds are valid
         (start (max 0 start))
         (end (min end len)))
    (cond
     ;; :reset - completely replace properties in the range
     ((eq merge-mode :reset)
      (let ((result (copy-sequence str)))
        (set-text-properties start end props result)
        result))
     ;; :add - deep merge with face prepending
     ((eq merge-mode :add)
      (let ((result (copy-sequence str)))
        (cl-loop
         for (key val) on props by #'cddr
         do (let ((pos start))
              (while (< pos end)
                (let* ((current-val (get-text-property pos key result))
                       (new-val (cond
                                 ((memq key tp-face-properties)
                                  (tp--prepend-face val current-val))
                                 ((and (listp val) (keywordp (car-safe val))
                                       (listp current-val) (keywordp (car-safe current-val)))
                                  (tp--deep-merge-plist current-val val))
                                 (t val)))
                       (next-change (or (next-single-property-change pos key result end) end)))
                  (put-text-property pos next-change key new-val result)
                  (setq pos next-change)))))
        result))
     ;; nil/:set - set properties while preserving existing ones
     ;; If applying to the entire string, use propertize for efficiency
     ;; Otherwise, use copy-sequence + put-text-property to apply to specific range
     (t
      (if (and (= start 0) (= end len))
          ;; Entire string: use propertize which creates a new copy and preserves existing properties
          (apply #'propertize str props)
        ;; Partial range: copy string and apply properties to the range
        (let ((result (copy-sequence str)))
          (cl-loop for (key val) on props by #'cddr
                   do (put-text-property start end key val result))
          result))))))

(defun tp-set (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Set text properties on string or buffer region.

Supports four calling conventions:
1. (tp-set START END PROPS) - current buffer
2. (tp-set START END PROPS BUFFER/STRING) - specific object
3. (tp-set STRING PROP VAL ...) - entire string

PROPS can be a plist or a layer/group name symbol.
Preserves existing properties not specified in PROPS.
For tp-text, props override embedded text properties.

**String Modification Behavior:**
- Entire string form (tp-set STRING ...): Returns a NEW propertized string
  (original is not modified). Uses `propertize' internally.
- Region form with string (tp-set START END PROPS STRING): Modifies the
  original string in-place using `put-text-property'.
- Buffer forms: Always modify in-place.

Returns: For buffers, (START . END) cons. For strings, the result string."
  ;; Determine if this is the "entire string" form (first arg is a string)
  (let ((entire-string-form (stringp start-or-string)))
    (pcase-let ((`(,object ,start ,finish ,props)
                 (tp--parse-args start-or-string end-or-prop props-or-val rest)))
      ;; Handle tp-text property specially - :override means props override embedded props
      (pcase-let ((`(,new-props ,new-finish ,new-object)
                   (tp--handle-tp-text start finish props object t :override)))
        (setq props new-props finish new-finish object new-object)
        (when (and (stringp object) (plist-member props 'tp-text))
          (setq start 0)))
      (cond
       ;; Entire string form: create a new propertized string (non-destructive)
       ((and (stringp object) entire-string-form)
        (tp--apply-props-to-string object start finish props nil))
       ;; Region form with string object: modify in-place
       ((stringp object)
        (let ((has-existing-props (text-properties-at start object)))
          (if (and (not has-existing-props)
                   (= start (or (next-single-property-change start nil object finish) finish)))
              (set-text-properties start finish props object)
            (cl-loop for (key val) on props by #'cddr
                     do (put-text-property start finish key val object))))
        object)
       ;; Buffer: modify in place
       (t
        (let ((has-existing-props (text-properties-at start object)))
          (if (and (not has-existing-props)
                   (= start (or (next-single-property-change start nil object finish) finish)))
              (set-text-properties start finish props object)
            (cl-loop for (key val) on props by #'cddr
                     do (put-text-property start finish key val object))))
        (cons start finish))))))

(defun tp-reset (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Completely replace all text properties with PROPS.
Like `tp-set' but replaces ALL existing properties.
For tp-text, embedded text properties are preserved (props override if there's a conflict).

**String Modification Behavior:**
- Entire string form (tp-reset STRING ...): Returns a NEW propertized string
  (original is not modified). Uses `propertize' internally.
- Region form with string (tp-reset START END PROPS STRING): Modifies the
  original string in-place using `set-text-properties'.
- Buffer forms: Always modify in-place.

Returns: For buffers, (START . END) cons. For strings, the result string."
  ;; Determine if this is the "entire string" form (first arg is a string)
  (let ((entire-string-form (stringp start-or-string)))
    (pcase-let ((`(,object ,start ,finish ,props)
                 (tp--parse-args start-or-string end-or-prop props-or-val rest)))
      ;; Handle tp-text property - :reset means only use props, ignore embedded props
      (pcase-let ((`(,new-props ,new-finish ,new-object)
                   (tp--handle-tp-text start finish props object nil :reset)))
        (setq props new-props finish new-finish object new-object)
        (when (and (stringp object) (plist-member props 'tp-text))
          (setq start 0)))
      (cond
       ;; Entire string form: create a new propertized string (non-destructive)
       ((and (stringp object) entire-string-form)
        (tp--apply-props-to-string object start finish props :reset))
       ;; Region form with string object: modify in-place
       ((stringp object)
        (set-text-properties start finish props object)
        object)
       ;; Buffer: modify in place
       (t
        (set-text-properties start finish props object)
        (cons start finish))))))

(defun tp-add (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Add or update text properties with deep merging.
Unlike `tp-set', deeply merges nested properties.
For face-family properties (see `tp-face-properties': face,
font-lock-face, mouse-face), symbol faces are prepended to the
existing face list and face plists are deep-merged.
For tp-text, embedded text properties are merged with props.

**String Modification Behavior:**
- Entire string form (tp-add STRING ...): Returns a NEW propertized string
  (original is not modified). Uses `propertize' internally.
- Region form with string (tp-add START END PROPS STRING): Modifies the
  original string in-place using `put-text-property'.
- Buffer forms: Always modify in-place.

Returns: For buffers, (START . END) cons. For strings, the result string."
  ;; Determine if this is the "entire string" form (first arg is a string)
  (let ((entire-string-form (stringp start-or-string)))
    (pcase-let ((`(,object ,start ,finish ,props)
                 (tp--parse-args start-or-string end-or-prop props-or-val rest)))
      ;; Handle tp-text property - :merge means embedded props are merged with props
      (let ((has-tp-text (plist-member props 'tp-text)))
        (pcase-let ((`(,new-props ,new-finish ,new-object)
                     (tp--handle-tp-text start finish props object t :merge)))
          (setq props new-props finish new-finish object new-object)
          (when (and (stringp object) has-tp-text)
            (setq start 0))))
      (cond
       ;; Entire string form: create a new propertized string (non-destructive)
       ((and (stringp object) entire-string-form)
        (if (plist-member props 'tp-text)
            ;; For tp-text: tp--handle-tp-text-property has already merged embedded
            ;; properties with props (in :merge mode above). The new-object is a
            ;; fresh string with tp-text content, and new-props contains all merged
            ;; properties. We use :reset mode here to simply apply these final
            ;; merged properties to the new string, without re-merging with any
            ;; (non-existent) existing properties on the new string.
            (tp--apply-props-to-string object start finish props :reset)
          ;; Otherwise use :add mode for deep merging with any existing properties
          (tp--apply-props-to-string object start finish props :add)))
       ;; Region form with string object: modify in-place with deep merging
       ((stringp object)
        (let ((pos start))
          (while (< pos finish)
            (let* ((current-props (text-properties-at pos object))
                   (next-pos (or (next-property-change pos object finish) finish)))
              (cl-loop
               for (key val) on props by #'cddr
               do (let* ((current-val (plist-get current-props key))
                         (new-val (cond
                                   ((memq key tp-face-properties)
                                    (tp--prepend-face val current-val))
                                   ((and (listp val) (keywordp (car-safe val))
                                         (listp current-val) (keywordp (car-safe current-val)))
                                    (tp--deep-merge-plist current-val val))
                                   (t val))))
                    (put-text-property pos next-pos key new-val object)))
              (setq pos next-pos))))
        object)
       ;; Buffer: modify in place with deep merging
       (t
        (let ((pos start))
          (while (< pos finish)
            (let* ((current-props (text-properties-at pos object))
                   (next-pos (or (next-property-change pos object finish) finish)))
              (cl-loop
               for (key val) on props by #'cddr
               do (let* ((current-val (plist-get current-props key))
                         (new-val (cond
                                   ((memq key tp-face-properties)
                                    (tp--prepend-face val current-val))
                                   ((and (listp val) (keywordp (car-safe val))
                                         (listp current-val) (keywordp (car-safe current-val)))
                                    (tp--deep-merge-plist current-val val))
                                   (t val))))
                    (put-text-property pos next-pos key new-val object)))
              (setq pos next-pos))))
        (cons start finish))))))

(defun tp-get (start-or-string &optional end-or-property &rest args)
  "Get text property value(s) with support for nested sub-properties.
Returns list of (START END VALUE) intervals.
Use `tp-at' for single position queries.
OBJECT defaults to current buffer.

Calling conventions:
  (tp-get STRING [PROPERTY [SUB-KEYS...]])   - entire string
  (tp-get STRING START END [PROPERTY ...])   - range within STRING
  (tp-get START END [PROPERTY ...] [OBJECT]) - region form
String positions are 0-based; buffer positions are 1-based."
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
       ;; (tp-get str START END [PROPERTY [SUB-KEYS...]]) - range within
       ;; the string, consistent with the buffer region form.  Positions
       ;; are 0-based as everywhere else for strings.
       ((numberp end-or-property)
        (let ((range-start end-or-property)
              (range-end (car args)))
          (unless (numberp range-end)
            (error "tp-get: string range form requires a numeric END after START, got %S"
                   range-end))
          ;; Delegate to the region form with the string as OBJECT.
          (apply #'tp-get range-start range-end
                 (append (cdr args) (list str)))))
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
PROPERTY can be a symbol (including layer names) or a list for nested removal.
If PROPERTY is a layer name, all properties added by that layer are removed."
  (cond
   ;; Simple property removal (or layer name)
   ((symbolp property)
    ;; Check if this is a layer name
    (if (tp--is-layer-name-p property)
        ;; Layer name - need to remove all properties added by the layer
        (let ((pos start))
          (while (< pos end)
            (let* ((tp-name-at-pos (get-text-property pos 'tp-name object))
                   (next-pos (or (next-single-property-change pos 'tp-name object end) end)))
              (when (eq tp-name-at-pos property)
                ;; This region has the layer applied - get the layer's property keys
                ;; For parameterized layers, we pass a dummy arg (t) since we only need key names
                (let* ((layer-props
                        (cond
                         ((tp-layer-parameterized-p property)
                          (tp-layer-props-with-arg property t nil)) ; arg=t, include-tp-name=nil
                         ((assoc property tp-layer-alist)
                          (tp-layer-props property nil)) ; include-tp-name=nil
                         ((assoc property tp-layer-groups)
                          (when-let ((layer-props-list (tp-group-props property t)))
                            (tp--build-layer-props layer-props-list)))))
                       (props-to-remove
                        (when layer-props
                          (cl-loop for (key _val) on layer-props by #'cddr
                                   collect key into keys
                                   finally return (if (memq 'tp-name keys)
                                                      keys
                                                    (cons 'tp-name keys))))))
                  (dolist (prop-key (or props-to-remove (list property 'tp-name)))
                    (remove-text-properties pos next-pos (list prop-key nil) object))))
              (setq pos next-pos))))
      ;; Regular property removal
      (remove-text-properties start end (list property nil) object)))
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

**String Modification Behavior:**
- Entire string form (tp-remove STRING ...): Returns a NEW string with
  properties removed (original is not modified). Uses `propertize' internally.
- Region form with string (tp-remove START END PROP STRING): Modifies the
  original string in-place using `remove-text-properties'.
- Buffer forms: Always modify in-place.

Returns: For buffers, nil. For entire string forms, a new string."
  (cond
   ;; First arg is a string - apply to entire string, non-destructively
   ((stringp start-or-string)
    (let* ((str start-or-string)
           (start 0)
           (end (length str)))
      (cond
       ;; (tp-remove str 'face :underline '(:style :position)) - nested sub-property removal with list
       ((and (symbolp end-or-prop)
             (keywordp prop-or-sub)
             rest
             (listp (car rest)))
        (tp--remove-property-from-string str start end (list end-or-prop prop-or-sub (car rest))))
       ;; (tp-remove str 'face :underline :position :style ...) - nested sub-property removal with keywords
       ((and (symbolp end-or-prop)
             (keywordp prop-or-sub)
             rest
             (keywordp (car rest)))
        (tp--remove-property-from-string str start end (list end-or-prop prop-or-sub rest)))
       ;; (tp-remove str 'face :underline) - sub-property removal
       ((and (symbolp end-or-prop) (keywordp prop-or-sub))
        (tp--remove-sub-from-string str start end end-or-prop prop-or-sub))
       ;; (tp-remove str 'face 'help-echo ...) - multiple properties
       ((symbolp end-or-prop)
        ;; Splice REST so the 3rd and later properties are kept, and
        ;; drop nils (nil is a symbol and would otherwise ride along
        ;; when PROP-OR-SUB is not given).
        (let ((props-to-remove (cl-remove-if-not
                                (lambda (p) (and p (symbolp p)))
                                (cons end-or-prop (cons prop-or-sub rest)))))
          (tp--remove-props-from-string str start end props-to-remove)))
       ;; (tp-remove str '(face :underline)) - nested property spec
       ((listp end-or-prop)
        (tp--remove-property-from-string str start end end-or-prop))
       (t str))))
   ;; First arg is a number - buffer region
   ((numberp start-or-string)
    (let* ((start start-or-string)
           (end end-or-prop)
           (property prop-or-sub)
           (object (car rest)))
      (tp--remove-property start end property object)
      nil))
   (t (error "Invalid arguments to tp-remove"))))

(defun tp--remove-props-from-string (str start end props-to-remove)
  "Create a new string from STR with PROPS-TO-REMOVE removed from START to END.
PROPS-TO-REMOVE can include layer names, which will be expanded to include
all properties that the layer adds.
For face properties from layers, subtracts the layer's face contribution
instead of removing the entire face property.
Operates per property interval, so every interval keeps its own
remaining properties (intervals are never overwritten with properties
sampled at START).
Returns a new string (original is not modified)."
  (let ((result (copy-sequence str)))
    (tp--map-intervals
     str start end
     (lambda (istart iend existing-props)
       (let (;; Remaining face after layer subtractions (this interval)
             (remaining-face nil)
             ;; Track if face was modified by layer subtraction
             (face-was-modified nil)
             ;; Collect all properties to remove entirely (non-face or non-layer)
             (props-to-remove-entirely nil))
         ;; Process each property to remove against this interval's props
         (dolist (prop props-to-remove)
           (if (tp--is-layer-name-p prop)
               ;; Layer name - get its face contribution and subtract from face
               (let* ((layer-prop-value (plist-get existing-props prop))
                      (layer-face (tp--get-layer-face-contribution prop layer-prop-value)))
                 ;; Subtract layer's face from the current face
                 (when layer-face
                   (let ((current-face (or remaining-face
                                           (plist-get existing-props 'face))))
                     (setq remaining-face
                           (tp--subtract-face-from-face-value current-face layer-face))
                     ;; Mark that we processed the face (even if result is nil)
                     (setq face-was-modified t)))
                 ;; Add the layer property itself to remove list
                 (push prop props-to-remove-entirely)
                 ;; Also add tp-name if it matches
                 (when (eq (plist-get existing-props 'tp-name) prop)
                   (push 'tp-name props-to-remove-entirely)))
             ;; Non-layer property - remove entirely
             (push prop props-to-remove-entirely)))
         ;; Build this interval's final properties
         (let ((final-props
                (let ((res nil))
                  (cl-loop for (key val) on existing-props by #'cddr
                           do (cond
                               ;; Face property with layer subtraction
                               ((and (eq key 'face) face-was-modified)
                                (when remaining-face
                                  (setq res (plist-put res key remaining-face))))
                               ;; Property to remove entirely
                               ((memq key props-to-remove-entirely)
                                nil) ; skip
                               ;; Keep other properties
                               (t (setq res (plist-put res key val)))))
                  res)))
           (set-text-properties istart iend final-props result)))))
    result))

(defun tp--remove-sub-from-string (str start end property sub-key)
  "Create a new string from STR with SUB-KEY removed from PROPERTY.
Returns a new string (original is not modified).
Handles complex face values that contain a mix of symbols and plists.
Operates per property interval, so every interval keeps its own
remaining properties."
  (let ((result (copy-sequence str)))
    (tp--map-intervals
     str start end
     (lambda (istart iend existing-props)
       (let* ((prop-value (plist-get existing-props property))
              ;; Use the helper to handle complex face values
              (new-value (when prop-value
                           (tp--remove-sub-from-face-value prop-value sub-key)))
              (final-props (let ((res nil))
                             (cl-loop for (key val) on existing-props by #'cddr
                                      do (setq res (plist-put res key
                                                              (if (eq key property)
                                                                  new-value
                                                                val))))
                             res)))
         (set-text-properties istart iend final-props result))))
    result))

(defun tp--remove-property-from-string (str start end property-spec)
  "Create a new string from STR with PROPERTY-SPEC removed from START to END.
PROPERTY-SPEC can be a symbol or a nested spec like (PROPERTY SUB-KEY ...).
Returns a new string (original is not modified)."
  (cond
   ((symbolp property-spec)
    (tp--remove-props-from-string str start end (list property-spec)))
   ((listp property-spec)
    (let ((property (car property-spec))
          (sub-key (cadr property-spec))
          (nested-keys (caddr property-spec)))
      (cond
       ;; Nested sub-property removal - per interval so every interval
       ;; keeps its own remaining properties
       ((and sub-key nested-keys)
        (let ((result (copy-sequence str)))
          (tp--map-intervals
           str start end
           (lambda (istart iend existing-props)
             (let* ((prop-value (plist-get existing-props property))
                    (new-value (if (and prop-value (listp prop-value))
                                   (tp--remove-nested-sub-keys
                                    prop-value sub-key nested-keys)
                                 ;; Not a plist-shaped value (e.g. a bare
                                 ;; face symbol) - the nested spec does
                                 ;; not apply; keep the value unchanged.
                                 prop-value))
                    (final-props (let ((res nil))
                                   (cl-loop for (key val) on existing-props by #'cddr
                                            do (setq res (plist-put res key
                                                                    (if (eq key property)
                                                                        new-value
                                                                      val))))
                                   res)))
               (set-text-properties istart iend final-props result))))
          result))
       ;; Simple sub-property removal
       (sub-key
        (tp--remove-sub-from-string str start end property sub-key))
       ;; Just a property name
       (t
        (tp--remove-props-from-string str start end (list property))))))
   (t str)))

(defun tp--remove-nested-sub-keys (plist sub-key nested-keys)
  "Remove NESTED-KEYS from the SUB-KEY value within PLIST.
Returns a new plist (does not modify the original)."
  (let* ((sub-value (plist-get plist sub-key))
         (keys-to-remove (if (listp nested-keys) nested-keys (list nested-keys)))
         (new-sub-value (when (and sub-value (listp sub-value))
                          (let ((result nil))
                            (cl-loop for (k v) on sub-value by #'cddr
                                     unless (memq k keys-to-remove)
                                     do (setq result (plist-put result k v)))
                            result))))
    (if new-sub-value
        ;; Build a new plist with the updated sub-value
        (let ((result nil))
          (cl-loop for (k v) on plist by #'cddr
                   do (setq result (plist-put result k
                                              (if (eq k sub-key)
                                                  new-sub-value
                                                v))))
          result)
      ;; Remove the sub-key entirely if no value left
      (let ((result nil))
        (cl-loop for (k v) on plist by #'cddr
                 unless (eq k sub-key)
                 do (setq result (plist-put result k v)))
        result))))

;;;###autoload
(defun tp-clear (&optional start end object)
  "Clear all text properties from START to END in OBJECT.
OBJECT is a string or buffer; nil means the current buffer.
If START and END are not provided, they default to the whole of
OBJECT: 0/(length OBJECT) for strings, `point-min'/`point-max' of
OBJECT for buffers (the current buffer when OBJECT is nil)."
  (interactive)
  (let ((beg (or start
                 (cond ((stringp object) 0)
                       ((bufferp object)
                        (with-current-buffer object (point-min)))
                       (t (point-min)))))
        (finish (or end
                    (cond ((stringp object) (length object))
                          ((bufferp object)
                           (with-current-buffer object (point-max)))
                          (t (point-max))))))
    (set-text-properties beg finish nil object)))

(provide 'tp-ops)
;;; tp-ops.el ends here
