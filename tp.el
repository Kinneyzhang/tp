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

;; tp.el provides a convenient wrapper around Emacs text properties,
;; with an innovative layer system that allows setting multiple layers
;; of text properties on the same text region.
;;
;; Features:
;; - Simple API for text property manipulation (similar to ov.el for overlays)
;; - Innovative tp-layer system for multi-layer text properties
;; - Layer groups for defining reusable property sets
;; - Search and navigation functions for text properties
;;
;; Inspired by https://github.com/emacsorphanage/ov
;;
;; Requires Emacs 28.1+ for `object-intervals' function.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)

;;; Variables

(defgroup tp nil
  "Group for tp.el text property manipulation."
  :prefix "tp-"
  :group 'development)

(defvar tp-layer-alist nil
  "Alist where each element is (LAYER-NAME . PROPERTIES).
Stores individual layer definitions.")

(defvar tp-layer-groups nil
  "Alist where each element is (GROUP-NAME . (LAYER-NAME1 LAYER-NAME2 ...)).
Stores layer group definitions, where each group contains multiple layer names.")


;;; Core Property Functions

(defun tp--parse-args (start-or-string end-or-prop props-or-val rest)
  "Parse flexible function arguments and return (OBJECT START END PROPS).
Supports four calling conventions:
1. Buffer region: (START END PROPS)
2. Buffer region with object: (START END PROPS OBJECT)
3. String region: (START END PROPS STRING)
4. Entire string: (STRING PROP VAL ...)"
  (let (object start finish props)
    (cond
     ;; First arg is a string - apply to entire string
     ((stringp start-or-string)
      (setq object start-or-string
            start 0
            finish (length start-or-string)
            props (if end-or-prop
                      (if props-or-val
                          (cons end-or-prop (cons props-or-val rest))
                        (list end-or-prop))
                    nil)))
     ;; First arg is a number - region convention
     ((numberp start-or-string)
      (setq start start-or-string
            finish end-or-prop)
      ;; Check if 4th arg (first of rest) is a buffer or string
      (if (and rest (or (bufferp (car rest)) (stringp (car rest))))
          (setq object (car rest)
                props props-or-val)
        (setq object nil
              props props-or-val)))
     (t (error "Invalid first argument: %S" start-or-string)))
    ;; Handle properties as a list
    (when (listp (car-safe props))
      (setq props (car props)))
    (list object start finish props)))

(defun tp-set (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Set text properties on string or buffer region.

This function supports four calling conventions:

1. Current buffer:
   (tp-set START END \\='(PROPERTY VALUE ...))

2. Specific buffer:
   (tp-set START END \\='(PROPERTY VALUE ...) BUFFER)

3. Specific string (0-indexed positions):
   (tp-set START END \\='(PROPERTY VALUE ...) STRING)

4. Entire string:
   (tp-set STRING PROPERTY VALUE ...)

This replaces only the properties specified, preserving other properties.
Return the modified object (string) or region (START . END) for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Apply properties individually (preserves other properties)
    (let ((len (length props))
          (i 0))
      (while (< i len)
        (put-text-property start finish
                           (nth i props)
                           (nth (1+ i) props)
                           object)
        (setq i (+ i 2))))
    (if (stringp object)
        object
      (cons start finish))))

(defun tp-reset (start-or-string &optional end-or-prop props-or-val &rest rest)
  "Completely replace all text properties with PROPS.

This function supports four calling conventions:

1. Current buffer:
   (tp-reset START END \\='(PROPERTY VALUE ...))

2. Specific buffer:
   (tp-reset START END \\='(PROPERTY VALUE ...) BUFFER)

3. Specific string (0-indexed positions):
   (tp-reset START END \\='(PROPERTY VALUE ...) STRING)

4. Entire string:
   (tp-reset STRING PROPERTY VALUE ...)

Unlike `tp-set', this completely replaces all existing properties.
Return the modified object (string) or region (START . END) for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Completely replace all properties
    (set-text-properties start finish props object)
    (if (stringp object)
        object
      (cons start finish))))

(defun tp--deep-merge-plist (base new)
  "Deep merge NEW plist into BASE plist.
For nested plists (starting with keyword), recursively merge.
NEW values override BASE values."
  (let ((result (copy-sequence base)))
    (cl-loop for (key val) on new by #'cddr
             do (let ((base-val (plist-get result key)))
                  (setq result
                        (plist-put result key
                                   (cond
                                    ;; Both are plists - recursively merge
                                    ((and (listp val) (keywordp (car-safe val))
                                          (listp base-val) (keywordp (car-safe base-val)))
                                     (tp--deep-merge-plist base-val val))
                                    ;; Otherwise use new value
                                    (t val))))))
    result))

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
  "Add or update text properties, preserving existing properties.

This function supports four calling conventions:

1. Current buffer:
   (tp-add START END \\='(PROPERTY VALUE ...))

2. Specific buffer:
   (tp-add START END \\='(PROPERTY VALUE ...) BUFFER)

3. Specific string (0-indexed positions):
   (tp-add START END \\='(PROPERTY VALUE ...) STRING)

4. Entire string:
   (tp-add STRING PROPERTY VALUE ...)

Unlike `tp-set', this deeply merges nested properties.
For example, \\='(face (:underline (:style wave))) will merge with
existing face properties rather than replacing them entirely.

For the `face' property specifically, symbol faces are prepended to
the existing face list rather than replacing.  For example:
  (tp-add str \\='face \\='shadow) with existing face \\='bold
  results in face value \\='(shadow bold).

Return the modified object (string) or region (START . END) for buffer."
  (pcase-let ((`(,object ,start ,finish ,props)
               (tp--parse-args start-or-string end-or-prop props-or-val rest)))
    ;; Process each property with deep merging
    (let ((pos start))
      (while (< pos finish)
        (let* ((current-props (text-properties-at pos object))
               (next-pos (or (next-property-change pos object finish) finish)))
          ;; Merge each property in props
          (cl-loop
           for (key val) on props by #'cddr
           do (let* ((current-val (plist-get current-props key))
                     (new-val
                      (cond
                       ;; Handle face property specially - prepend faces
                       ((eq key 'face)
                        (tp--prepend-face val current-val))
                       ;; Both are plists - deep merge
                       ((and (listp val) (keywordp (car-safe val))
                             (listp current-val) (keywordp (car-safe current-val)))
                        (tp--deep-merge-plist current-val val))
                       ;; Otherwise use new value
                       (t val))))
                (put-text-property pos next-pos key new-val object)))
          (setq pos next-pos))))
    (if (stringp object)
        object
      (cons start finish))))

(defun tp--get-nested (value path)
  "Get nested value from VALUE following PATH.
PATH is a list of keys/symbols to traverse nested structures.
Supports plists, alists, and special display property formats.

If an element in PATH is a list of keys, extract those keys from the
current value and return a plist with those keys.
Example: (tp--get-nested \\='(:a 1 :b 2 :c 3) \\='((:a :b))) => (:a 1 :b 2)"
  (if (null path)
      value
    (let* ((key (car path))
           (rest (cdr path))
           ;; Check if value is a plist-like structure
           ;; A plist starts with keyword, or starts with symbol followed by keywords
           ;; e.g., (:foreground "red") or (shadow :foreground "red")
           (is-plist-like (and (listp value)
                               (or (keywordp (car value))
                                   (and (symbolp (car value))
                                        (cdr value)
                                        (keywordp (cadr value))))))
           (next-value
            (cond
             ;; Key is a list of keys - extract multiple keys from value
             ((and (listp key) (not (null key)))
              (when is-plist-like
                (let ((result nil)
                      (plist-part (if (keywordp (car value))
                                      value
                                    (cdr value))))
                  (dolist (k key)
                    (let ((v (plist-get plist-part k)))
                      (when v
                        (setq result (plist-put result k v)))))
                  result)))
             ;; Value is a plist or plist-like (symbol followed by plist)
             (is-plist-like
              (let ((plist-part (if (keywordp (car value))
                                    value
                                  (cdr value))))
                (plist-get plist-part key)))
             ;; Value is an alist
             ((and (listp value) (consp (car value)))
              (cdr (assoc key value)))
             ;; Value is a list of specs (e.g., display property)
             ((listp value)
              (or (plist-get value key)
                  (cdr (assoc key value))
                  (cl-loop for spec in value
                           when (and (listp spec)
                                     (eq (car spec) key))
                           return (if (listp (cdr spec))
                                      (if (= (length (cdr spec)) 1)
                                          (cadr spec)
                                        (cdr spec))
                                    (cdr spec))
                           when (and (listp spec) (keywordp (car spec)))
                           thereis (plist-get spec key))))
             (t nil))))
      (tp--get-nested next-value rest))))

(defun tp-get (start-or-string &optional end-or-property &rest args)
  "Get text property value(s) with support for nested sub-properties.

This function supports multiple calling conventions:

1. Range with property path as list:
   (tp-get START END \\='(PROPERTY) OBJECT)
   (tp-get START END \\='(PROPERTY SUB-KEY ...) OBJECT)
   (tp-get 5 20 \\='(face) str-or-buffer-or-nil)
   (tp-get 5 20 \\='(face :underline) str-or-buffer-or-nil)
   (tp-get 5 20 \\='(face :underline :style) str-or-buffer-or-nil)

2. Range, single property:
   (tp-get START END PROPERTY)
   (tp-get START END PROPERTY OBJECT)

3. Range, nested sub-property:
   (tp-get START END PROPERTY SUB-KEY ...)

4. Range, all properties:
   (tp-get START END)
   (tp-get START END OBJECT)

5. Entire string, all properties:
   (tp-get STRING)

6. Entire string, single property:
   (tp-get STRING PROPERTY)

7. Entire string, nested sub-property:
   (tp-get STRING PROPERTY SUB-KEY ...)
   (tp-get str \\='face)
   (tp-get str \\='face :underline)
   (tp-get str \\='face :underline :style)

8. Entire string with property path as list:
   (tp-get STRING \\='(PROPERTY SUB-KEY ...))
   (tp-get str \\='(face :foreground))

Returns a list of (START END VALUE) intervals, allowing you to see all
property values across the range.

For single position queries, use `tp-at' instead.

For buffers, positions are 1-indexed.
For strings, positions are 0-indexed.
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
                   (next-pos (or (next-single-property-change pos property str len) len))
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
                   (next-pos (or (next-single-property-change pos property str len) len))
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
                     (next-pos (or (next-single-property-change pos property object end) end))
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

;;; Private functions for fine-grained property manipulation

(defun tp--remove-sub (start end property sub-property &optional object)
  "Remove SUB-PROPERTY from PROPERTY between START and END in OBJECT.
For example, remove :foreground from a face property.
OBJECT defaults to current buffer.
Internal function - use `tp-remove' with nested path for public API."
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
                   (next-pos (or (next-single-property-change pos prop-name object end) end)))
              (when current-value
                (let* ((sub-value (if (and (listp current-value) (keywordp (car current-value)))
                                      (plist-get current-value sub-key)
                                    nil))
                       (new-sub-value (when (and (listp sub-value) (keywordp (car sub-value)))
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
If START and END are not provided, clear the entire buffer.
OBJECT defaults to current buffer."
  (interactive)
  (let ((beg (or start (point-min)))
        (finish (or end (point-max))))
    (set-text-properties beg finish nil object)))

;;; Match and regexp functions

(defun tp--match-apply-single (pattern properties apply-fn object)
  "Apply APPLY-FN to matches of single PATTERN in OBJECT.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
Returns modified object or list of regions."
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
      (with-current-buffer buf
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
      (with-current-buffer buf
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
                                   (listp current-val) (keywordp (car-safe current-val)))
                              (tp--deep-merge-plist current-val val))
                             (t val))))
                      (put-text-property pos next-pos key new-val obj)))
        (setq pos next-pos)))))

(defun tp-match-set (pattern plist &optional object)
  "Set properties on all occurrences of PATTERN.

  (tp-match-set PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Returns:
- For strings: the modified string
- For buffers: list of (START . END) pairs for all matches."
  (tp--match-apply pattern plist #'tp-set object))

(defun tp-match-reset (pattern plist &optional object)
  "Reset (completely replace) properties on all occurrences of PATTERN.

  (tp-match-reset PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-match-set', this completely replaces all existing properties."
  (tp--match-apply pattern plist
                   (lambda (start end props obj)
                     (set-text-properties start end props obj))
                   object))

(defun tp-match-add (pattern plist &optional object)
  "Add/update properties on all occurrences of PATTERN.

  (tp-match-add PATTERN PLIST &optional OBJECT)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-match-set', this deeply merges nested properties."
  (tp--match-apply pattern plist #'tp--deep-merge-apply object))

(defun tp-regexp-set (pattern plist &optional object)
  "Set properties on all matches of PATTERN (regexp).

  (tp-regexp-set PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Returns:
- For strings: the modified string
- For buffers: list of (START . END) pairs for all matches."
  (tp--regexp-apply pattern plist #'tp-set object))

(defun tp-regexp-reset (pattern plist &optional object)
  "Reset (completely replace) properties on all regexp matches of PATTERN.

  (tp-regexp-reset PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-regexp-set', this completely replaces all existing properties."
  (tp--regexp-apply pattern plist
                    (lambda (start end props obj)
                      (set-text-properties start end props obj))
                    object))

(defun tp-regexp-add (pattern plist &optional object)
  "Add/update properties on all regexp matches of PATTERN.

  (tp-regexp-add PATTERN PLIST &optional OBJECT)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\").
OBJECT is a buffer or string; nil means current buffer.

Unlike `tp-regexp-set', this deeply merges nested properties."
  (tp--regexp-apply pattern plist #'tp--deep-merge-apply object))

;;; Search functions

(defun tp-search-forward (property &optional value predicate not-current)
  "Search forward for text with PROPERTY.
VALUE, PREDICATE, and NOT-CURRENT work as in `text-property-search-forward'."
  (text-property-search-forward property value predicate not-current))

(defun tp-search-backward (property &optional value predicate not-current)
  "Search backward for text with PROPERTY.
VALUE, PREDICATE, and NOT-CURRENT work as in `text-property-search-backward'."
  (text-property-search-backward property value predicate not-current))

(defun tp-forward (property &optional value object n)
  "Search forward N times for text with PROPERTY.

N is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.

For buffers, returns the prop-match object from the last successful search.
For strings, returns a list of (START END VALUE) for all matches found.

Uses `tp-search-forward' for buffers and `tp-search' for strings."
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
        (with-current-buffer buf
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
        (with-current-buffer buf
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
        (with-current-buffer buf
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
        (with-current-buffer buf
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
        (with-current-buffer obj
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


;;; Query Functions

;;; Text property intervals
;; Note: Uses `object-intervals' which requires Emacs 28.1+

(defun tp-intervals (start end &optional object)
  "Get all text property intervals from START to END in OBJECT.
OBJECT can be a buffer or string; nil defaults to current buffer.
Returns a list of (START END PROPERTIES) for each interval.
Uses `object-intervals' (Emacs 28.1+)."
  (let ((object (or object (current-buffer))))
    (cond
     ((stringp object)
      (object-intervals (substring object start end)))
     ((bufferp object)
      (with-current-buffer (get-buffer-create object)
        (object-intervals (buffer-substring start end))))
     (t (error "Invalid format of object: %S"
               (type-of object))))))

(defun tp-empty-p (&optional object)
  "Return t if OBJECT has no text properties.
OBJECT can be a string or buffer; nil defaults to current buffer.
Uses `object-intervals' (Emacs 28.1+)."
  (let ((obj (or object (current-buffer))))
    (cond
     ((stringp obj)
      (null (object-intervals obj)))
     ((bufferp obj)
      (with-current-buffer obj
        (null (object-intervals (buffer-substring (point-min) (point-max))))))
     (t (error "Invalid object type: %S" (type-of obj))))))

(defun tp-plist (start-or-string &optional end object)
  "Get the property list of text in a region or string.

This function supports two calling conventions:

1. Buffer/string region:
   (tp-plist START END &optional OBJECT)

2. Entire string:
   (tp-plist STRING)

Returns a plist of all properties in the region or string."
  (let (start finish obj)
    (cond
     ;; Entire string form: (tp-plist string)
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            finish (length start-or-string)))
     ;; Region form: (tp-plist start end &optional object)
     ((numberp start-or-string)
      (setq start start-or-string
            finish end
            obj object)))
    (let ((props nil)
          (pos start))
      (while (< pos finish)
        (let ((current-props (tp-at pos obj)))
          (cl-loop for (key val) on current-props by #'cddr
                   do (unless (plist-member props key)
                        (setq props (plist-put props key val)))))
        (setq pos (next-single-property-change pos nil obj finish)))
      props)))

;;; Layer Definition Functions

(defmacro tp-define-layer (name &rest layers)
  "Define a text property layer or layer group named NAME.

Single layer:
  (tp-define-layer layer-1 \\='(face (:background \"cyan\") line-prefix \">>\"))

Multiple layers (first defined layer is the top layer):
  (tp-define-layer layers-2
    \\='layer-1
    \\='(face (:background \"red\") line-prefix \">>\")
    \\='(face (:background \"green\" :weight bold) line-prefix \"::\"))

LAYERS can be:
- A single plist for a single layer definition
- Multiple items where each can be:
  - A symbol referencing another defined layer
  - A plist defining an anonymous sub-layer

For multiple layers, they are stored as a group in `tp-layer-groups'.
The first layer in the definition is the top layer."
  (declare (indent defun))
  ;; Determine if this is a single layer (one plist argument) or multiple layers
  (let ((first-layer (car layers)))
    (let ((is-single-layer
           (and (= (length layers) 1)
                first-layer
                (listp first-layer)
                ;; A plist has an even number of elements (key-value pairs)
                (cl-evenp (length first-layer))
                ;; The first element is a property name (symbol, not nil)
                (symbolp (car first-layer)))))
      (if is-single-layer
          ;; Single layer: (tp-define-layer name '(plist...))
          (let ((properties first-layer))
            `(progn
               (if (assoc ',name tp-layer-alist)
                   (setf (cdr (assoc ',name tp-layer-alist)) ',properties)
                 (push (cons ',name ',properties) tp-layer-alist))
               (assoc ',name tp-layer-alist)))
      ;; Multiple layers: (tp-define-layer name 'layer1 '(plist1) '(plist2) ...)
      (let ((layer-names nil)
            (idx 0)
            (layer-defs nil))
        (dolist (layer layers)
          (cond
           ;; Reference to existing layer
           ((symbolp layer)
            (push layer layer-names))
           ;; Plist layer - create with auto-generated name
           ((listp layer)
            (let ((sub-name (intern (format "%s-layer-%d" name idx))))
              (push `(if (assoc ',sub-name tp-layer-alist)
                         (setf (cdr (assoc ',sub-name tp-layer-alist)) ',layer)
                       (push (cons ',sub-name ',layer) tp-layer-alist))
                    layer-defs)
              (push sub-name layer-names)
              (cl-incf idx)))))
        (setq layer-names (nreverse layer-names))
        (setq layer-defs (nreverse layer-defs))
        `(progn
           ,@layer-defs
           (if (assoc ',name tp-layer-groups)
               (setf (cdr (assoc ',name tp-layer-groups)) ',layer-names)
             (push (cons ',name ',layer-names) tp-layer-groups))
           (assoc ',name tp-layer-groups)))))))

(defun tp-layer-props (layer-name)
  "Return properties for layer LAYER-NAME from `tp-layer-alist'.
Appends 'tp-name property to identify the layer."
  (when-let ((plist (cdr (assoc layer-name tp-layer-alist))))
    (append plist (list 'tp-name layer-name))))

(defun tp-group-props (group-name)
  "Return list of properties for all layers in GROUP-NAME."
  (when-let ((layers (cdr (assoc group-name tp-layer-groups))))
    (mapcar (lambda (layer)
              (tp-layer-props layer))
            layers)))

(defun tp-layer-reset ()
  "Reset all layer definitions.
Clears both `tp-layer-alist' and `tp-layer-groups'."
  (interactive)
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil))

(defun tp-undefine-layer (name)
  "Remove layer NAME from `tp-layer-alist'."
  (setq tp-layer-alist (assq-delete-all name tp-layer-alist)))

(defun tp-undefine-group (name)
  "Remove layer group NAME from `tp-layer-groups'."
  (setq tp-layer-groups (assq-delete-all name tp-layer-groups)))

(defun tp-intervals-map (function start end &optional object)
  "Apply FUNCTION to all intervals between START and END in OBJECT.
FUNCTION receives four arguments: interval-start, interval-end,
top-props (the visible layer properties), and below-props-lst (list of hidden layers).
OBJECT can be a buffer or string; nil defaults to current buffer."
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

;;; New Layer API Functions

(defun tp--normalize-layer-spec (layer-spec)
  "Normalize LAYER-SPEC to a plist with tp-name.
LAYER-SPEC can be:
- A symbol (layer name from tp-layer-alist)
- A plist (inline layer definition)
- A list (name &rest plist) for named inline layer."
  (cond
   ;; Symbol - look up in tp-layer-alist
   ((symbolp layer-spec)
    (or (tp-layer-props layer-spec)
        (error "Layer %S not found in tp-layer-alist" layer-spec)))
   ;; List starting with symbol followed by plist - named inline layer (name &rest plist)
   ((and (listp layer-spec)
         (symbolp (car layer-spec))
         (not (keywordp (car layer-spec)))
         (cdr layer-spec))
    (let ((name (car layer-spec))
          (props (cdr layer-spec)))
      (append props (list 'tp-name name))))
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
                        (-remove-at-indices (list tp-layers-idx (1+ tp-layers-idx)) props)
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
             (tp-group-props layer-spec))
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

(defun tp-raise-layer (start-or-string &optional end-or-idx idx-or-n n-or-object object)
  "Raise a layer by N positions in the stack.

Calling conventions:
1. Buffer/string region:
   (tp-raise-layer START END IDX/LAYER-NAME N OBJECT)
   
2. Entire string:
   (tp-raise-layer STRING IDX/LAYER-NAME N)

Positive N moves the layer up (toward top/visible).
Negative N moves the layer down (toward bottom)."
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
              (found (tp--get-layer-by-idx-or-name current-stack layer-id)))
         (when found
           (let* ((old-idx (car found))
                  (layer-props (cdr found))
                  (new-idx (max 0 (min (- (length current-stack) 1)
                                       (- old-idx n))))
                  (stack-without (-remove-at old-idx current-stack))
                  (new-stack (append (seq-take stack-without new-idx)
                                     (list layer-props)
                                     (seq-drop stack-without new-idx))))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
     start end obj)
    nil))

(defun tp-rotate-layer (start-or-string &optional end-or-object object)
  "Rotate layers, moving top layer to bottom.

Calling conventions:
1. Buffer/string region:
   (tp-rotate-layer START END OBJECT)
   
2. Entire string:
   (tp-rotate-layer STRING)"
  (let (start end obj)
    (cond
     ((stringp start-or-string)
      (setq obj start-or-string
            start 0
            end (length start-or-string)))
     ((numberp start-or-string)
      (setq start start-or-string
            end end-or-object
            obj object)))
    
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (let ((current-stack (tp--layer-stack-to-list top belows)))
         (when (> (length current-stack) 1)
           (let ((new-stack (append (cdr current-stack)
                                    (list (car current-stack)))))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
     start end obj)
    nil))

(defun tp-pin-layer (start-or-string &optional end-or-idx idx-or-object object)
  "Pin a layer to the top (make it visible).

Calling conventions:
1. Buffer/string region:
   (tp-pin-layer START END IDX/LAYER-NAME OBJECT)
   
2. Entire string:
   (tp-pin-layer STRING IDX/LAYER-NAME)"
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
         (when (and found (> (car found) 0))
           (let* ((layer-props (cdr found))
                  (stack-without (-remove-at (car found) current-stack))
                  (new-stack (cons layer-props stack-without)))
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
     start end obj)
    nil))

(defun tp-switch-layer (start-or-string &optional end-or-id1 id1-or-id2 id2-or-object object)
  "Switch between two layers by name or index.

Calling conventions:
1. Buffer/string region:
   (tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)
   
2. Entire string:
   (tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)"
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
              (found1 (tp--get-layer-by-idx-or-name current-stack id1))
              (found2 (tp--get-layer-by-idx-or-name current-stack id2)))
         (when (and found1 found2)
           (let* ((idx1 (car found1))
                  (idx2 (car found2))
                  (props1 (cdr found1))
                  (props2 (cdr found2))
                  ;; Swap the layers
                  (new-stack (copy-sequence current-stack)))
             (setf (nth idx1 new-stack) props2)
             (setf (nth idx2 new-stack) props1)
             (set-text-properties
              (+ start i-start) (+ start i-end)
              (tp--build-layer-props new-stack)
              obj)))))
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
                              for found = (tp--get-layer-by-idx-or-name current-stack id)
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

;;; Layer query functions

(defun tp-layer-list (start end &optional object)
  "Return list of all layer names in region from START to END.
OBJECT defaults to current buffer."
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

;;; Layer property manipulation functions

(defun tp-add-to-layers (idx-or-layer-name-list start-or-string &optional end-or-plist plist-or-object &rest rest)
  "Add/merge properties to specified layers.

This function supports two calling conventions:

1. Buffer/string region:
   (tp-add-to-layers IDX-OR-LAYER-NAME-LIST START END PLIST OBJECT)

2. Entire string:
   (tp-add-to-layers IDX-OR-LAYER-NAME-LIST STRING PROP VAL ...)

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
    (when (and (listp plist) (not (keywordp (car-safe plist))) (listp (car-safe plist)))
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
                               (let ((found (tp--get-layer-by-idx-or-name current-stack id)))
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
    (when (and (listp plist) (not (keywordp (car-safe plist))) (listp (car-safe plist)))
      (setq plist (car plist)))

    ;; Get the maximum layer count in the region to build a list of all indices
    (let ((max-count (tp-layer-count start end obj)))
      (when (> max-count 0)
        (let ((all-indices (cl-loop for i from 0 below max-count collect i)))
          (tp-add-to-layers all-indices start end plist obj))))
    (if (stringp obj) obj nil)))


(provide 'tp)
;;; tp.el ends here
