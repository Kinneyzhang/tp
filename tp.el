;;; tp.el --- Text Properties manipulation library for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Version: 0.1.0
;; Keywords: convenience text-properties
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

;;; tp layer define

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

(defmacro tp-layer-define (name properties)
  "Define a text property layer named NAME with PROPERTIES.
The layer is stored in `tp-layer-alist'.
PROPERTIES should be a plist of text properties."
  (declare (indent defun))
  `(progn
     (if (assoc ',name tp-layer-alist)
         (setf (cdr (assoc ',name tp-layer-alist)) ,properties)
       (push (cons ',name ,properties) tp-layer-alist))
     (assoc ',name tp-layer-alist)))

(defmacro tp-group-define (name &rest layers)
  "Define a layer group named NAME containing LAYERS.
LAYERS are specified as alternating NAME PROPERTIES pairs.
The first layer in the definition is the top layer (visible by default).
All layers are stored in `tp-layer-alist' and the group in `tp-layer-groups'."
  (declare (indent defun))
  `(let ((layer-names
          (nreverse
           (-map (lambda (lst)
                   (let ((layer-name (car lst)))
                     (eval `(tp-layer-define ,layer-name ,(cadr lst)))
                     layer-name))
                 (-partition 2 ',layers)))))
     (if (assoc ',name tp-layer-groups)
         (setf (cdr (assoc ',name tp-layer-groups)) layer-names)
       (push (cons ',name layer-names) tp-layer-groups))
     (assoc ',name tp-layer-groups)))

(defalias 'tp-layer-group-define 'tp-group-define
  "Alias for `tp-group-define'.")

(defun tp-layer-props (layer-name)
  "Return properties for layer LAYER-NAME from `tp-layer-alist'.
Appends 'tp-name property to identify the layer."
  (when-let ((plist (cdr (assoc layer-name tp-layer-alist))))
    (append plist (list 'tp-name layer-name))))

(defalias 'tp-layer-properties 'tp-layer-props
  "Alias for `tp-layer-props'.")

(defun tp-group-props (group-name)
  "Return list of properties for all layers in GROUP-NAME."
  (when-let ((layers (cdr (assoc group-name tp-layer-groups))))
    (mapcar (lambda (layer)
              (tp-layer-props layer))
            layers)))

(defalias 'tp-layer-group-properties 'tp-group-props
  "Alias for `tp-group-props'.")

;;; Basic text property functions (similar to ov.el)

(defun tp-put (start end &rest properties)
  "Set text PROPERTIES from START to END in current buffer.
PROPERTIES is a plist of property-value pairs.
Return the modified region as (START . END)."
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (let ((len (length properties))
        (i 0))
    (while (< i len)
      (put-text-property start end
                         (nth i properties)
                         (nth (1+ i) properties))
      (setq i (+ i 2))))
  (cons start end))

(defalias 'tp-set 'tp-put
  "Alias for `tp-put'.")

(defun tp-get (position property &optional object)
  "Get the value of PROPERTY at POSITION in OBJECT.
OBJECT defaults to current buffer."
  (get-text-property position property object))

(defun tp-remove (start end property &optional object)
  "Remove PROPERTY from text between START and END in OBJECT.
OBJECT defaults to current buffer."
  (remove-text-properties start end (list property nil) object))

(defun tp-remove-list (start end properties &optional object)
  "Remove list of PROPERTIES from text between START and END in OBJECT.
PROPERTIES should be a list of property names."
  (let ((plist (mapcan (lambda (p) (list p nil)) properties)))
    (remove-text-properties start end plist object)))

;;;###autoload
(defun tp-clear (&optional start end object)
  "Clear all text properties from START to END in OBJECT.
If START and END are not provided, clear the entire buffer.
OBJECT defaults to current buffer."
  (interactive)
  (let ((beg (or start (point-min)))
        (finish (or end (point-max))))
    (set-text-properties beg finish nil object)))

(defun tp-at (&optional point object)
  "Get all text properties at POINT in OBJECT.
POINT defaults to current point.
OBJECT defaults to current buffer."
  (text-properties-at (or point (point)) object))

(defun tp-plist (start end &optional object)
  "Get the property list of text at START to END in OBJECT.
Returns a plist of all properties in the region."
  (let ((props nil)
        (pos start))
    (while (< pos end)
      (let ((current-props (tp-at pos object)))
        (cl-loop for (key val) on current-props by #'cddr
                 do (unless (plist-member props key)
                      (setq props (plist-put props key val)))))
      (setq pos (next-single-property-change pos nil object end)))
    props))

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

(defun tp-empty-p (object)
  "Return t if OBJECT has no text properties.
Uses `object-intervals' (Emacs 28.1+)."
  (null (object-intervals object)))

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

(defun tp-layer-set (start end name &optional object)
  "Set NAME as the layer name for text from START to END in OBJECT.
This names the current visible layer without adding new properties.
OBJECT defaults to current buffer."
  (if (tp-empty-p (or object (current-buffer)))
      (add-text-properties start end (list 'tp-name name) object)
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (plist-put top 'tp-name name)
                (list 'tp-layers belows))
        object))
     start end object))
  object)

;;;###autoload
(defun tp-layer-push (start end name &optional object)
  "Push layer NAME to top of the layer stack from START to END.
Uses properties from `tp-layer-alist' if NAME is defined there.
OBJECT defaults to current buffer.
Signals an error if layer NAME already exists in the region."
  (declare (indent defun))
  (when (tp-region-layer-props start end name object)
    (error "Already exist layer named %S" name))
  (let ((props (tp-layer-props name)))
    (if (tp-empty-p (or object (current-buffer)))
        ;; No existing properties, just set the layer properties
        (set-text-properties start end
                             (append props (list 'tp-layers nil))
                             object)
      ;; Has existing properties, push to layer stack
      (tp-intervals-map
       (lambda (i-start i-end top belows)
         (set-text-properties
          (+ start i-start) (+ start i-end)
          (append props
                  (list 'tp-layers (append (list top) belows)))
          object))
       start end object)))
  object)

(defun tp-layer-delete (start end name &optional object)
  "Delete layer NAME from the layer stack between START and END.
If NAME is the top layer, the next layer becomes visible.
OBJECT defaults to current buffer."
  (declare (indent defun))
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (set-text-properties
      (+ start i-start) (+ start i-end)
      ;; If NAME is the top layer, promote the next layer
      (if (equal name (plist-get top 'tp-name))
          (append (nth 0 belows)
                  (list 'tp-layers (seq-drop belows 1)))
        ;; NAME is not the top layer, remove from belows
        (append top
                (list 'tp-layers
                      (-remove (lambda (props)
                                 (equal name (plist-get props 'tp-name)))
                               belows))))
      object))
   start end object)
  nil)

(defun tp-layer-rotate (start end &optional object)
  "Rotate layers from START to END, moving top layer to bottom.
This cycles through the layer stack, making each layer visible in turn.
OBJECT defaults to current buffer."
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (when belows
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (nth 0 belows)
                (list 'tp-layers
                      (append (seq-drop belows 1)
                              (list top))))
        object)))
   start end object)
  nil)

(defun tp-layer-pin (start end name &optional object)
  "Pin layer NAME to the top of the layer stack from START to END.
Moves the layer named NAME to the top, making it visible.
OBJECT defaults to current buffer.
Signals an error if layer NAME does not exist in the region."
  (unless (tp-region-layer-props start end name object)
    (error "Doesn't exist a layer named %S" name))
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     ;; Only do something if NAME is not already at top
     (unless (equal (plist-get top 'tp-name) name)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (let ((new-top
               ;; Find the layer to promote
               (seq-find (lambda (props)
                           (equal (plist-get props 'tp-name) name))
                         belows))
              ;; Remove the promoted layer from belows
              (rest-belows
               (-remove (lambda (props)
                          (equal (plist-get props 'tp-name) name))
                        belows)))
          (append new-top
                  (list 'tp-layers
                        (append (list top) rest-belows))))
        object)))
   start end object)
  nil)

;;; Propertize string functions

(defun tp-propertize (string &rest properties)
  "Return a copy of STRING with PROPERTIES applied.
PROPERTIES should be a plist of property-value pairs."
  (declare (indent defun))
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (apply #'propertize string properties))

(defun tp-layer-propertize (string layer)
  "Return STRING with properties from LAYER applied.
LAYER must be defined in `tp-layer-alist'."
  (if-let ((layer-info (assoc layer tp-layer-alist)))
      (apply #'propertize string (cdr layer-info))
    (error "Layer %S doesn't exist!" layer)))

(defun tp-group-propertize (string layer-group)
  "Return STRING with all layers from LAYER-GROUP applied.
LAYER-GROUP must be defined in `tp-layer-groups'.
Layers are applied in order, with later layers on top."
  (if-let* ((group-info (assoc layer-group tp-layer-groups))
            (layers (cdr group-info)))
      (let ((result string))
        ;; Apply base layer first
        (when-let ((first-layer (car layers)))
          (setq result (tp-layer-propertize result first-layer)))
        ;; Apply additional layers using the layer system
        (dolist (layer (cdr layers))
          (when-let ((props (tp-layer-props layer)))
            (set-text-properties 0 (length result)
                                 (append props
                                         (list 'tp-layers
                                               (list (tp-at 0 result))))
                                 result)))
        result)
    (error "Layer group %S doesn't exist!" layer-group)))

(defalias 'tp-layer-group-propertize 'tp-group-propertize
  "Alias for `tp-group-propertize'.")

;;; Search functions

(defun tp-forward (property &optional value predicate not-current)
  "Search forward for text with PROPERTY.
VALUE, PREDICATE, and NOT-CURRENT work as in `text-property-search-forward'."
  (text-property-search-forward property value predicate not-current))

(defun tp-backward (property &optional value predicate not-current)
  "Search backward for text with PROPERTY.
VALUE, PREDICATE, and NOT-CURRENT work as in `text-property-search-backward'."
  (text-property-search-backward property value predicate not-current))

(defun tp-forward-do (function property &optional value predicate not-current)
  "Search forward for PROPERTY and apply FUNCTION to the match.
FUNCTION receives three arguments: START, END, and VALUE."
  (when-let* ((match (tp-forward property value predicate not-current))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (val (prop-match-value match)))
    (funcall function start end val)))

(defun tp-backward-do (function property &optional value predicate not-current)
  "Search backward for PROPERTY and apply FUNCTION to the match.
FUNCTION receives three arguments: START, END, and VALUE."
  (when-let* ((match (tp-backward property value predicate not-current))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (val (prop-match-value match)))
    (funcall function start end val)))

(defun tp-regions-map (function property &optional value predicate collect)
  "Apply FUNCTION to all regions with PROPERTY in current buffer.
FUNCTION receives three arguments: START, END, and INDEX.
If COLLECT is non-nil, return list of results."
  (save-excursion
    (goto-char (point-min))
    (let ((idx 0) lst)
      (while-let ((match (tp-forward property value predicate))
                  (start (prop-match-beginning match))
                  (end (prop-match-end match)))
        (let ((res (funcall function start end idx)))
          (when collect (push res lst)))
        (cl-incf idx 1))
      (nreverse lst))))

(defun tp-strings-map (function property &optional value predicate collect)
  "Apply FUNCTION to all strings with PROPERTY in current buffer.
FUNCTION receives two arguments: STRING and INDEX."
  (tp-regions-map
   (lambda (start end idx)
     (funcall function (buffer-substring start end) idx))
   property value predicate collect))

;;; Match and regexp functions (similar to ov-match and ov-regexp)

(defun tp-match (string &rest properties)
  "Set PROPERTIES on all occurrences of STRING in current buffer.
Returns list of (START . END) pairs for all matches."
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (save-excursion
    (goto-char (point-min))
    (let (regions)
      (while (search-forward string nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when properties
            (tp-put beg end properties))
          (push (cons beg end) regions)))
      (nreverse regions))))

(defun tp-regexp (regexp &rest properties)
  "Set PROPERTIES on all matches of REGEXP in current buffer.
Returns list of (START . END) pairs for all matches."
  (when (listp (car-safe properties))
    (setq properties (car properties)))
  (save-excursion
    (goto-char (point-min))
    (let (regions)
      (while (re-search-forward regexp nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when properties
            (tp-put beg end properties))
          (push (cons beg end) regions)))
      (nreverse regions))))

;;; Layer list and query functions

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

;;; Layer visibility functions

(defun tp-layer-hide (start end name &optional object)
  "Hide layer NAME by moving it below all other layers.
OBJECT defaults to current buffer."
  (unless (tp-region-layer-props start end name object)
    (error "Doesn't exist a layer named %S" name))
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (if (equal (plist-get top 'tp-name) name)
         ;; NAME is top, move it to bottom
         (when belows
           (set-text-properties
            (+ start i-start) (+ start i-end)
            (append (nth 0 belows)
                    (list 'tp-layers
                          (append (seq-drop belows 1)
                                  (list top))))
            object))
       ;; NAME is in belows, move it to bottom
       (let ((layer (seq-find (lambda (p)
                                (equal name (plist-get p 'tp-name)))
                              belows)))
         (when layer
           (set-text-properties
            (+ start i-start) (+ start i-end)
            (append top
                    (list 'tp-layers
                          (append (-remove (lambda (p)
                                             (equal name (plist-get p 'tp-name)))
                                           belows)
                                  (list layer))))
            object)))))
   start end object)
  nil)

(defun tp-layer-show (start end name &optional object)
  "Show layer NAME by moving it to the top.
Alias for `tp-layer-pin'.
OBJECT defaults to current buffer."
  (tp-layer-pin start end name object))

;;; Layer merge function

(defun tp-layer-merge (start end layer1 layer2 new-name &optional object)
  "Merge LAYER1 and LAYER2 into a new layer named NEW-NAME.
Properties from LAYER1 take precedence over LAYER2.
OBJECT defaults to current buffer."
  (let ((props1 (tp-region-layer-props start end layer1 object))
        (props2 (tp-region-layer-props start end layer2 object)))
    (unless (and props1 props2)
      (error "Both layers must exist in the region"))
    ;; Get the properties from both layers
    (let* ((layer1-props (nth 2 (car props1)))
           (layer2-props (nth 2 (car props2)))
           ;; Merge properties (layer1 takes precedence)
           (merged-props
            (let ((result (copy-sequence layer2-props)))
              (cl-loop for (key val) on layer1-props by #'cddr
                       do (setq result (plist-put result key val)))
              (plist-put result 'tp-name new-name))))
      ;; Delete old layers and push merged layer
      (tp-layer-delete start end layer1 object)
      (tp-layer-delete start end layer2 object)
      ;; Define the new merged layer
      (if (assoc new-name tp-layer-alist)
          (setf (cdr (assoc new-name tp-layer-alist)) merged-props)
        (push (cons new-name merged-props) tp-layer-alist))
      ;; Apply the merged layer
      (tp-layer-push start end new-name object)))
  nil)

;;; Utility functions

(defun tp-in (property &optional value start end)
  "Get all regions with PROPERTY in current buffer.
If VALUE is specified, only return regions where PROPERTY equals VALUE.
If START and END are specified, limit search to that region.
Returns list of (START END PROPERTIES) for each match."
  (let ((beg (or start (point-min)))
        (finish (or end (point-max)))
        (regions nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) finish)
        (let* ((props (tp-at (point)))
               (prop-val (plist-get props property)))
          (when (and prop-val
                     (or (null value)
                         (equal prop-val value)))
            (let ((region-start (point))
                  (region-end (next-single-property-change (point) property nil finish)))
              (push (list region-start region-end props) regions)
              (goto-char region-end)))
          (goto-char (next-single-property-change (point) property nil finish)))))
    (nreverse regions)))

(defun tp-all (&optional start end)
  "Get all regions with any text properties in current buffer.
If START and END are specified, limit search to that region.
Returns list of (START END PROPERTIES)."
  (let ((beg (or start (point-min)))
        (finish (or end (point-max)))
        (regions nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) finish)
        (let* ((props (tp-at (point)))
               (region-start (point))
               (region-end (next-property-change (point) nil finish)))
          (when props
            (push (list region-start region-end props) regions))
          (goto-char (or region-end finish)))))
    (nreverse regions)))

(defun tp-next (&optional point property value)
  "Get the next position with text properties after POINT.
If PROPERTY is specified, find next position with that property.
If VALUE is also specified, the property must equal that value."
  (let ((pos (or point (point))))
    (if property
        (save-excursion
          (goto-char pos)
          (when-let ((match (tp-forward property value)))
            (prop-match-beginning match)))
      (next-property-change pos))))

(defun tp-prev (&optional point property value)
  "Get the previous position with text properties before POINT.
If PROPERTY is specified, find previous position with that property.
If VALUE is also specified, the property must equal that value."
  (let ((pos (or point (point))))
    (if property
        (save-excursion
          (goto-char pos)
          (when-let ((match (tp-backward property value)))
            (prop-match-beginning match)))
      (previous-property-change pos))))

(defun tp-goto-next (&optional property value)
  "Move point to next text with PROPERTY (optionally equal to VALUE)."
  (interactive)
  (when-let ((pos (tp-next (point) property value)))
    (goto-char pos)))

(defun tp-goto-prev (&optional property value)
  "Move point to previous text with PROPERTY (optionally equal to VALUE)."
  (interactive)
  (when-let ((pos (tp-prev (point) property value)))
    (goto-char pos)))

;;; Layer reset functions

(defun tp-layer-reset ()
  "Reset all layer definitions.
Clears both `tp-layer-alist' and `tp-layer-groups'."
  (interactive)
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil))

(defun tp-layer-undefine (name)
  "Remove layer NAME from `tp-layer-alist'."
  (setq tp-layer-alist (assq-delete-all name tp-layer-alist)))

(defun tp-group-undefine (name)
  "Remove layer group NAME from `tp-layer-groups'."
  (setq tp-layer-groups (assq-delete-all name tp-layer-groups)))

(defalias 'tp-layer-group-undefine 'tp-group-undefine
  "Alias for `tp-group-undefine'.")

(provide 'tp)
;;; tp.el ends here
