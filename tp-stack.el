;;; tp-stack.el --- Layer stack operations for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Photoshop-style layer stack operations on text regions: put/push/
;; delete/pop/move/raise/rotate/pin/switch/merge/flatten, stack queries,
;; and bulk layer property manipulation.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tp-core)
(require 'tp-layer)
(require 'tp-ops)

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
      ;; Always include plist-or-object even if nil, to handle (... 'prop nil)
      (when end-or-plist
        (setq plist (cons end-or-plist (cons plist-or-object rest)))))
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
      ;; Always include plist-or-object even if nil, to handle (... 'prop nil)
      (when end-or-plist
        (setq plist (cons end-or-plist (cons plist-or-object rest)))))
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

(provide 'tp-stack)
;;; tp-stack.el ends here
