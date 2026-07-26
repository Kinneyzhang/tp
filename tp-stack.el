;;; tp-stack.el --- Layer stack operations for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Photoshop-style layer stack operations on text regions: put/push/
;; delete/pop/move/raise/lower/rotate/pin/switch/hide/show/merge/
;; flatten, stack queries, and bulk layer property manipulation.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'tp-core)
(require 'tp-reactive)
(require 'tp-layer)
(require 'tp-ops)

;;; Shared argument parsing and region iteration

(defun tp--parse-layer-args (start-or-string rest n)
  "Normalize a layer operation's positional arguments.

START-OR-STRING is the caller's first positional argument and REST the
list of its remaining positional arguments, in order.  N is the number
of operation-specific arguments the caller takes (for example 2 for
`tp-put-layer's LAYER and IDX).

Two calling conventions are supported:
- (STRING ARG1 ... ARGN): operate on the whole STRING.
- (START END ARG1 ... ARGN OBJECT): operate on a region of OBJECT,
  where nil means the current buffer.

Returns the list (START END OBJECT ARG1 ... ARGN) with START/END in
OBJECT's native coordinates (0-based for strings, 1-based for
buffers)."
  (cond
   ((stringp start-or-string)
    (append (list 0 (length start-or-string) start-or-string)
            (seq-take rest n)))
   ((numberp start-or-string)
    (append (list start-or-string (car rest) (nth (1+ n) rest))
            (seq-take (cdr rest) n)))
   (t (error "Invalid layer arguments: %S" (cons start-or-string rest)))))

(defun tp--plist-remove (plist key)
  "Return a copy of PLIST without KEY and its value.
Comparison uses `eq'.  PLIST itself is not modified."
  (cl-loop for (k v) on plist by #'cddr
           unless (eq k key) append (list k v)))

(defun tp--stack-map-region (start end object function)
  "Call FUNCTION over each property run of [START, END) in OBJECT.

OBJECT is a string, a buffer, or nil for the current buffer.
FUNCTION receives (ABS-START ABS-END STACK): the run's bounds, clipped
to [START, END) and expressed in OBJECT's native coordinates (0-based
for strings, 1-based for buffers), and the run's layer stack as a list
of layer plists, top layer first (empty for bare text).  Hidden layers
\(see `tp-hide-layer') are included at their stack position.

Returns the list of FUNCTION's non-nil results, in order.

Unlike `tp-intervals-map', runs never extend beyond the requested
region, positions are absolute for strings as well as buffers, and
bare text is visited (with an empty STACK) so layers can be applied to
previously property-less text."
  (delq nil
        (tp--map-intervals
         object start end
         (lambda (i-start i-end props)
           (funcall function i-start i-end
                    (tp--stack-props-to-list props))))))

(defun tp--stack-register-layers (stack object)
  "Register OBJECT in the reactive buffer registry for every layer in STACK.
STACK is a list of layer plists as stored by the stack operations.
When OBJECT is a buffer or nil (the current buffer), every plist
carrying a `tp-name' - buried and hidden layers included - registers
that buffer via `tp-reactive--register-layer-buffer', so reactive
updates and the anonymous-layer GC keep seeing buffers whose layers
were written by stack mutators rather than by `tp-set'.  String
OBJECTs are not registered; see `tp-reactive-layer-buffers' for that
gap.  Registration is idempotent, so calling this once per rewritten
run is cheap."
  (when (or (null object) (bufferp object))
    (let ((buf (or object (current-buffer))))
      (dolist (layer stack)
        (when-let ((name (plist-get layer 'tp-name)))
          (tp-reactive--register-layer-buffer name buf))))))

;;; Queries

(defun tp-region-layer-props (start end layer-name &optional object)
  "Return layer properties for LAYER-NAME in region from START to END.
OBJECT defaults to current buffer.
Returns a list of (START END PROPERTIES) for matching intervals, with
positions in OBJECT's native coordinates (0-based for strings, 1-based
for buffers) and clipped to the requested region."
  (tp--stack-map-region
   start end object
   (lambda (abs-start abs-end stack)
     (when-let ((props (seq-find
                        (lambda (props)
                          (equal layer-name
                                 (plist-get props 'tp-name)))
                        stack)))
       (list abs-start abs-end props)))))

(defun tp-layer-list (start end &optional object)
  "Return list of all layer names in region from START to END."
  (let ((layers nil))
    (tp--stack-map-region
     start end object
     (lambda (_abs-start _abs-end stack)
       (dolist (layer stack)
         (when-let ((name (plist-get layer 'tp-name)))
           (cl-pushnew name layers :test #'equal)))))
    (nreverse layers)))

(defun tp-layer-count (start end &optional object)
  "Return number of layers in region from START to END.
OBJECT defaults to current buffer."
  (let ((max-count 0))
    (tp--stack-map-region
     start end object
     (lambda (_abs-start _abs-end stack)
       (setq max-count (max max-count (length stack)))))
    max-count))

(defun tp-layer-exists-p (start end name &optional object)
  "Return t if layer NAME exists in region from START to END.
OBJECT defaults to current buffer."
  (not (null (tp-region-layer-props start end name object))))

(defun tp-layer-top (start end &optional object)
  "Return the name of the topmost named layer in START..END of OBJECT.
Scans the region's property runs in order and returns the `tp-name'
of the first top layer that has one, so bare or unnamed runs (for
example before a layer that starts mid-region) do not hide layers
later in the region.  Returns nil when no run in the region has a
named top layer.  OBJECT defaults to current buffer.

The topmost layer is reported in stack order even when it is hidden
\(see `tp-hide-layer'); use `tp-layer-stack-at' to distinguish hidden
layers from visible ones."
  (car (tp--stack-map-region
        start end object
        (lambda (_abs-start _abs-end stack)
          (plist-get (car stack) 'tp-name)))))

(defun tp-layer-stack-at (pos &optional object)
  "Return the full ordered layer stack at POS in OBJECT.

The result is a list with one element per layer, topmost layer first
and bottommost last, where each element is a cons (NAME . PROPS):
- NAME is the layer's `tp-name' symbol, or nil for an unnamed layer.
- PROPS is the layer's property plist without its `tp-name' entry.
  A hidden layer (see `tp-hide-layer') is distinguishable by the
  entry `tp-hidden' with value t in PROPS; visible layers never
  carry a `tp-hidden' entry.

Hidden layers are included at their stack position.  Returns nil for
bare text.  POS is in OBJECT's native coordinates (0-based for
strings, 1-based for buffers).  OBJECT is a string, a buffer, or nil
for the current buffer."
  (mapcar (lambda (layer)
            (cons (plist-get layer 'tp-name)
                  (tp--plist-remove layer 'tp-name)))
          (tp--stack-props-to-list (text-properties-at pos object))))

;;; Layer spec normalization for tp-put-layer

(defun tp--put-layer-specs (layer-spec)
  "Normalize LAYER-SPEC into a list of layer plists for `tp-put-layer'.

LAYER-SPEC can be:
- a layer name or group name (symbol);
- (LAYER-NAME ARG) or (GROUP-NAME ARG) for parameterized layers/groups;
- an inline plist, e.g. (face bold) or (:foreground \"red\");
- (NAME PROP VAL ...) for a named inline layer;
- a list of any of the above.

An inline plist is recognized by its even length together with a head
that is a keyword or an ordinary property symbol (one that is not a
defined layer or group name); a named inline layer has odd length
\(NAME plus prop/value pairs)."
  (cond
   ;; Group name symbol.
   ((and (symbolp layer-spec)
         (assoc layer-spec tp-layer-groups))
    (if (tp-group-parameterized-p layer-spec)
        (error "Parameterized group %S requires an argument, use '(%S ARG)"
               layer-spec layer-spec)
      (tp-group-props layer-spec t)))   ; include tp-name for layer stack
   ;; Any other symbol: a single layer name.
   ((symbolp layer-spec)
    (list (tp--normalize-layer-spec layer-spec)))
   ;; (GROUP-NAME ARG1 ... ARGN) or (GROUP-NAME (ARG1 ... ARGN)):
   ;; multi-argument parameterized group (arity >= 2).  Checked before
   ;; the single-arg forms so the wrapped variant is not mistaken for
   ;; one list-valued argument.
   ((and (consp layer-spec)
         (symbolp (car layer-spec))
         (proper-list-p layer-spec)
         (let ((arity (length (tp--group-arglist (car layer-spec)))))
           (and (>= arity 2)
                (or (= (length (cdr layer-spec)) arity)
                    (and (= (length (cdr layer-spec)) 1)
                         (proper-list-p (cadr layer-spec))
                         (= (length (cadr layer-spec)) arity))))))
    (let* ((arity (length (tp--group-arglist (car layer-spec))))
           (args (if (= (length (cdr layer-spec)) arity)
                     (cdr layer-spec)
                   (cadr layer-spec))))
      (tp--group-props-with-args (car layer-spec) args t)))
   ;; (LAYER-NAME ARG1 ... ARGN) or (LAYER-NAME (ARG1 ... ARGN)):
   ;; multi-argument parameterized layer (arity >= 2).
   ((and (consp layer-spec)
         (symbolp (car layer-spec))
         (proper-list-p layer-spec)
         (let ((arity (length (tp-layer-arglist (car layer-spec)))))
           (and (>= arity 2)
                (or (= (length (cdr layer-spec)) arity)
                    (and (= (length (cdr layer-spec)) 1)
                         (proper-list-p (cadr layer-spec))
                         (= (length (cadr layer-spec)) arity))))))
    (let* ((arity (length (tp-layer-arglist (car layer-spec))))
           (args (if (= (length (cdr layer-spec)) arity)
                     (cdr layer-spec)
                   (cadr layer-spec))))
      (list (tp--normalize-layer-spec (cons (car layer-spec) args)))))
   ;; (GROUP-NAME ARG): parameterized group.
   ((and (consp layer-spec)
         (symbolp (car layer-spec))
         (= (safe-length layer-spec) 2)
         (tp-group-parameterized-p (car layer-spec)))
    (tp-group-props-with-arg (car layer-spec) (cadr layer-spec) t))
   ;; (LAYER-NAME ARG): parameterized layer.
   ((and (consp layer-spec)
         (symbolp (car layer-spec))
         (= (safe-length layer-spec) 2)
         (tp-layer-parameterized-p (car layer-spec)))
    (list (tp--normalize-layer-spec layer-spec)))
   ;; Keyword-headed plist: a single inline layer.
   ((and (consp layer-spec) (keywordp (car layer-spec)))
    (list (tp--normalize-layer-spec layer-spec)))
   ;; Even-length plist headed by an ordinary (non-layer) property
   ;; symbol, e.g. (face bold): a single inline layer.
   ((and (consp layer-spec)
         (car layer-spec)
         (symbolp (car layer-spec))
         (not (tp--is-layer-name-p (car layer-spec)))
         (proper-list-p layer-spec)
         (cl-evenp (length layer-spec)))
    (list layer-spec))
   ;; List whose every element is itself a spec (a layer/group name or
   ;; a list): multiple layers.
   ((and (consp layer-spec)
         (proper-list-p layer-spec)
         (cl-every (lambda (el)
                     (or (consp el) (tp--is-layer-name-p el)))
                   layer-spec))
    (apply #'append (mapcar #'tp--put-layer-specs layer-spec)))
   ;; Anything else, including (NAME PROP VAL ...) named inline
   ;; layers; tp--normalize-layer-spec signals on invalid specs.
   (t
    (list (tp--normalize-layer-spec layer-spec)))))

;;; Mutators

(defun tp-put-layer (start-or-string &optional end-or-layer layer-or-idx idx-or-object object noerror)
  "Set layer(s) at a specific index position.

Calling conventions:
1. Buffer/string region:
   (tp-put-layer START END LAYER IDX OBJECT NOERROR)

2. Entire string:
   (tp-put-layer STRING LAYER IDX NOERROR)

LAYER can be:
- A symbol (layer name from `tp-layer-alist' or `tp-layer-groups')
- A list (LAYER-NAME ARG) or (GROUP-NAME ARG) for parameterized
  layers or groups
- A plist (inline layer definition), e.g. (face bold)
- A list (NAME &rest PLIST) for named inline layer
- A list of the above for multiple layers

IDX specifies where to insert:
- 0 means top (visible layer)
- -1 means bottom
- Other values insert at that position

OBJECT defaults to current buffer for region form.  Only text inside
\[START, END) is modified.

A LAYER naming an undefined layer or group normally signals an
error.  If NOERROR is non-nil, return nil instead of signaling when
LAYER cannot be resolved; nothing is modified in that case.

Returns OBJECT when one was given (in particular the string in
string forms), otherwise the cons (START . END)."
  (pcase-let ((`(,start ,end ,obj ,layer-spec ,idx)
               (tp--parse-layer-args
                start-or-string
                (list end-or-layer layer-or-idx idx-or-object object) 2)))
    (setq idx (or idx 0))
    (let* ((noerr (if (stringp start-or-string) idx-or-object noerror))
           (layers-to-add
            (if noerr
                (condition-case nil
                    (tp--put-layer-specs layer-spec)
                  (error 'tp--unresolved))
              (tp--put-layer-specs layer-spec))))
      (unless (eq layers-to-add 'tp--unresolved)
        (tp--stack-map-region
         start end obj
         (lambda (abs-start abs-end stack)
           (let* ((actual-idx (if (< idx 0)
                                  (max 0 (+ (length stack) 1 idx))
                                (min idx (length stack))))
                  (new-stack (append (seq-take stack actual-idx)
                                     layers-to-add
                                     (seq-drop stack actual-idx))))
             (set-text-properties abs-start abs-end
                                  (tp--stack-build-props new-stack)
                                  obj)
             (tp--stack-register-layers new-stack obj))))
        (or obj (cons start end))))))

(defun tp-push-layer (start-or-string &optional end-or-layer layer-or-object object noerror)
  "Push layer(s) to the top of the layer stack.

This is equivalent to (tp-put-layer ... LAYER 0 ...).

Calling conventions:
1. Buffer/string region:
   (tp-push-layer START END LAYER OBJECT NOERROR)

2. Entire string:
   (tp-push-layer STRING LAYER NOERROR)

A LAYER naming an undefined layer or group normally signals an
error.  If NOERROR is non-nil, return nil instead of signaling when
LAYER cannot be resolved; nothing is modified in that case.

Returns what `tp-put-layer' returns: OBJECT when one was given (in
particular the string in string forms), otherwise (START . END)."
  (pcase-let ((`(,start ,end ,obj ,layer)
               (tp--parse-layer-args
                start-or-string
                (list end-or-layer layer-or-object object) 1)))
    (let ((noerr (if (stringp start-or-string) layer-or-object noerror)))
      (tp-put-layer start end layer 0 obj noerr))))

(defun tp-delete-layer (start-or-string &optional end-or-idx idx-or-object object)
  "Delete layer by name or index.

Calling conventions:
1. Buffer/string region:
   (tp-delete-layer START END LAYER-NAME/IDX OBJECT)

2. Entire string:
   (tp-delete-layer STRING LAYER-NAME/IDX)

LAYER-NAME/IDX can be:
- A symbol (layer name)
- An integer (layer index, 0=top, -1=bottom)

Only text inside [START, END) is modified.

Returns the number of property runs modified.  A LAYER-NAME/IDX
matching no layer never signals: unmatched runs are silently left
alone and a return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,layer-id)
               (tp--parse-layer-args
                start-or-string
                (list end-or-idx idx-or-object object) 1)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((found (tp--get-layer-by-idx-or-name stack layer-id)))
           (let ((new-stack (-remove-at (car found) stack)))
             (set-text-properties abs-start abs-end
                                  (tp--stack-build-props new-stack)
                                  obj)
             (tp--stack-register-layers new-stack obj))
           (setq count (1+ count)))))
      count)))

(defun tp-pop-layer (start-or-string &optional end-or-object object)
  "Pop the top layer from the layer stack.

This is equivalent to (tp-delete-layer ... 0 ...).

Calling conventions:
1. Buffer/string region:
   (tp-pop-layer START END OBJECT)

2. Entire string:
   (tp-pop-layer STRING)

Returns the number of property runs modified; 0 means no run in the
region had a layer to pop."
  (pcase-let ((`(,start ,end ,obj)
               (tp--parse-layer-args
                start-or-string (list end-or-object object) 0)))
    (tp-delete-layer start end 0 obj)))

(defun tp--move-layer-in-stack (stack from-id to-idx)
  "Move layer at FROM-ID to TO-IDX position in STACK.
FROM-ID can be an integer index or a layer name symbol.
TO-IDX must be an integer index.
Both indices refer to positions before the move and can be negative
\(counting from end).
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
OBJECT defaults to current buffer for region form.

Returns the number of property runs modified.  A FROM-ID matching no
layer never signals: unmatched runs are silently left alone and a
return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,from-id ,to-idx)
               (tp--parse-layer-args
                start-or-string
                (list end-or-from from-or-to to-or-object object) 2)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((new-stack (tp--move-layer-in-stack stack from-id to-idx)))
           (set-text-properties abs-start abs-end
                                (tp--stack-build-props new-stack)
                                obj)
           (tp--stack-register-layers new-stack obj)
           (setq count (1+ count)))))
      count)))

(defun tp-raise-layer (start-or-string &optional end-or-idx idx-or-n n-or-object object)
  "Raise a layer by N positions in the stack.

Calling conventions:
1. Buffer/string region:
   (tp-raise-layer START END IDX/LAYER-NAME N OBJECT)

2. Entire string:
   (tp-raise-layer STRING IDX/LAYER-NAME N)

Positive N moves the layer up (toward top/visible).
Negative N moves the layer down (toward bottom).
N defaults to 1.  The resulting position is clamped to the stack.

Uses `tp--raise-layer-in-stack' internally, which is built on
`tp--move-layer-in-stack'.

Returns the number of property runs modified.  An IDX/LAYER-NAME
matching no layer never signals: unmatched runs are silently left
alone and a return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,layer-id ,n)
               (tp--parse-layer-args
                start-or-string
                (list end-or-idx idx-or-n n-or-object object) 2)))
    (setq n (or n 1))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((new-stack (tp--raise-layer-in-stack stack layer-id n)))
           (set-text-properties abs-start abs-end
                                (tp--stack-build-props new-stack)
                                obj)
           (tp--stack-register-layers new-stack obj)
           (setq count (1+ count)))))
      count)))

(defun tp-lower-layer (start-or-string &optional end-or-idx idx-or-n n-or-object object)
  "Lower a layer by N positions in the stack.

This is the mirror image of `tp-raise-layer': lowering by N is
raising by -N.

Calling conventions:
1. Buffer/string region:
   (tp-lower-layer START END IDX/LAYER-NAME N OBJECT)

2. Entire string:
   (tp-lower-layer STRING IDX/LAYER-NAME N)

IDX/LAYER-NAME identifies the layer: a layer name symbol or an
integer index (0 = top, negative indices count from the bottom, so
-1 = bottom).

Positive N moves the layer down (toward bottom).
Negative N moves the layer up (toward top/visible).
N defaults to 1.  The resulting position is clamped to the stack.

OBJECT defaults to current buffer for region form.

Returns the number of property runs modified.  An IDX/LAYER-NAME
matching no layer never signals: unmatched runs are silently left
alone and a return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,layer-id ,n)
               (tp--parse-layer-args
                start-or-string
                (list end-or-idx idx-or-n n-or-object object) 2)))
    (setq n (or n 1))
    (tp-raise-layer start end layer-id (- n) obj)))

(defun tp-rotate-layer (start-or-string &optional end-or-direction object-or-count direction count)
  "Rotate layers, by default moving the top layer to the bottom.

Calling conventions:
1. Buffer/string region:
   (tp-rotate-layer START END OBJECT DIRECTION COUNT)

2. Entire string:
   (tp-rotate-layer STRING DIRECTION COUNT)

DIRECTION is `down' or nil to move the top layer to the bottom (the
historical behavior), or `up' to move the bottom layer to the top;
any other value signals an error.  COUNT is the number of rotation
steps and defaults to 1; a COUNT below 1 rotates nothing.  Layers
keep their relative order; hidden layers rotate with the rest of the
stack.

OBJECT defaults to current buffer for region form.

Returns the number of property runs modified; 0 means no run in the
region had layers to rotate (or COUNT was below 1)."
  (pcase-let ((`(,start ,end ,obj)
               (tp--parse-layer-args
                start-or-string
                (list end-or-direction object-or-count) 0)))
    (let* ((string-form (stringp start-or-string))
           (dir (or (if string-form end-or-direction direction) 'down))
           (cnt (or (if string-form object-or-count count) 1))
           (applied 0))
      (unless (memq dir '(up down))
        (error "Invalid rotate direction: %S" dir))
      (when (>= cnt 1)
        (tp--stack-map-region
         start end obj
         (lambda (abs-start abs-end stack)
           (when stack
             (let* ((len (length stack))
                    (k (mod (if (eq dir 'up) (- cnt) cnt) len))
                    (new-stack (append (seq-drop stack k)
                                       (seq-take stack k))))
               (set-text-properties abs-start abs-end
                                    (tp--stack-build-props new-stack)
                                    obj)
               (tp--stack-register-layers new-stack obj)
               (setq applied (1+ applied)))))))
      applied)))

(defun tp-pin-layer (start-or-string &optional end-or-idx idx-or-object object)
  "Pin a layer to the top (make it visible).

Calling conventions:
1. Buffer/string region:
   (tp-pin-layer START END IDX/LAYER-NAME OBJECT)

2. Entire string:
   (tp-pin-layer STRING IDX/LAYER-NAME)

Uses `tp-move-layer' internally to move the specified layer to index 0 (top).

Returns the number of property runs modified.  An IDX/LAYER-NAME
matching no layer never signals: unmatched runs are silently left
alone and a return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,layer-id)
               (tp--parse-layer-args
                start-or-string
                (list end-or-idx idx-or-object object) 1)))
    (tp-move-layer start end layer-id 0 obj)))

(defun tp-switch-layer (start-or-string &optional end-or-id1 id1-or-id2 id2-or-object object)
  "Switch between two layers by name or index.

Calling conventions:
1. Buffer/string region:
   (tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)

2. Entire string:
   (tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)

Uses `tp--switch-layers-in-stack' internally.

Returns the number of property runs modified.  When either layer is
missing from a run's stack nothing signals: such runs are silently
left alone and a return value of 0 means nothing matched at all."
  (pcase-let ((`(,start ,end ,obj ,id1 ,id2)
               (tp--parse-layer-args
                start-or-string
                (list end-or-id1 id1-or-id2 id2-or-object object) 2)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((new-stack (tp--switch-layers-in-stack stack id1 id2)))
           (set-text-properties abs-start abs-end
                                (tp--stack-build-props new-stack)
                                obj)
           (tp--stack-register-layers new-stack obj)
           (setq count (1+ count)))))
      count)))

(defun tp-hide-layer (start-or-string &optional end-or-name name-or-object object)
  "Hide layer NAME in region from START to END without removing it.

Calling conventions:
1. Buffer/string region:
   (tp-hide-layer START END NAME OBJECT)

2. Entire string:
   (tp-hide-layer STRING NAME)

NAME identifies the layer: a layer name symbol or an integer index
into the full stack, hidden layers included (0 = top, -1 = bottom).

A hidden layer stays in the stack -- it still counts for
`tp-layer-count', appears in `tp-layer-list' and `tp-layer-stack-at'
and can be moved, raised or lowered -- but it no longer renders: the
text shows the properties of the topmost non-hidden layer instead.
Hiding the currently visible top layer therefore reveals the next
visible layer below it.  When every layer of a run is hidden the text
keeps only the `tp-layers' bookkeeping property (so not even
`tp-name' renders) while all layers stay queryable.  Use
`tp-show-layer' to make a hidden layer render again.

Hiddenness is stored as a `tp-hidden' flag entry inside the layer's
plist in the `tp-layers' stack storage, so `tp-hidden' is a reserved
property name inside layers, like `tp-name'.

OBJECT defaults to current buffer for region form.

Returns the number of property runs modified.  A NAME matching no
layer never signals; runs whose match is already hidden are left
alone as well, so a return value of 0 means nothing changed."
  (pcase-let ((`(,start ,end ,obj ,name)
               (tp--parse-layer-args
                start-or-string
                (list end-or-name name-or-object object) 1)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((found (tp--get-layer-by-idx-or-name stack name)))
           (unless (tp--stack-hidden-p (cdr found))
             (let ((new-stack (-replace-at (car found)
                                           (append (list 'tp-hidden t)
                                                   (cdr found))
                                           stack)))
               (set-text-properties abs-start abs-end
                                    (tp--stack-build-props new-stack)
                                    obj)
               (tp--stack-register-layers new-stack obj)
               (setq count (1+ count)))))))
      count)))

(defun tp-show-layer (start-or-string &optional end-or-name name-or-object object)
  "Show layer NAME in region from START to END, undoing `tp-hide-layer'.

Calling conventions:
1. Buffer/string region:
   (tp-show-layer START END NAME OBJECT)

2. Entire string:
   (tp-show-layer STRING NAME)

NAME identifies the layer: a layer name symbol or an integer index
into the full stack, hidden layers included (0 = top, -1 = bottom).

The layer's `tp-hidden' flag is removed.  When the shown layer sits
above the currently visible top layer it becomes the rendered layer
again, restoring its properties onto the text.

OBJECT defaults to current buffer for region form.

Returns the number of property runs modified.  A NAME matching no
layer never signals; runs whose match is not hidden are left alone
as well, so a return value of 0 means nothing changed."
  (pcase-let ((`(,start ,end ,obj ,name)
               (tp--parse-layer-args
                start-or-string
                (list end-or-name name-or-object object) 1)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when-let ((found (tp--get-layer-by-idx-or-name stack name)))
           (when (tp--stack-hidden-p (cdr found))
             (let ((new-stack (-replace-at (car found)
                                           (tp--plist-remove (cdr found)
                                                             'tp-hidden)
                                           stack)))
               (set-text-properties abs-start abs-end
                                    (tp--stack-build-props new-stack)
                                    obj)
               (tp--stack-register-layers new-stack obj)
               (setq count (1+ count)))))))
      count)))

(defun tp--merge-layer-props (layers initial)
  "Merge the plists of LAYERS into the INITIAL plist and return it.
LAYERS is a list of (INDEX . PROPS) conses as returned by
`tp--get-layer-by-idx-or-name'.  Earlier layers take precedence: a key
already present in the accumulator is never overwritten, and presence
is tested with `plist-member' so an explicit nil value in a higher
layer shadows lower layers' values.  `tp-name' keys of the merged
layers are dropped (INITIAL may seed its own), as are `tp-hidden'
bookkeeping flags (see `tp-hide-layer')."
  (cl-reduce (lambda (acc layer)
               (cl-loop for (key val) on (cdr layer) by #'cddr
                        unless (memq key '(tp-name tp-hidden))
                        do (unless (plist-member acc key)
                             (setq acc (plist-put acc key val))))
               acc)
             layers
             :initial-value initial))

(defun tp-merge-layers (start-or-string &optional end-or-name name-or-ids ids-or-object object)
  "Merge specified layers into a new layer.

Calling conventions:
1. Buffer/string region:
   (tp-merge-layers START END NEW-LAYER-NAME
                    \\='(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)

2. Entire string:
   (tp-merge-layers STRING NEW-LAYER-NAME \\='(IDX1 LAYER-NAME1 IDX2 ...))

Earlier layers in the list take precedence; a property explicitly set
to nil in a higher-precedence layer stays nil in the merged layer.

Hidden matched layers (see `tp-hide-layer') are merged away with the
rest but contribute NO properties to the merged layer, so a merge can
never render what was hidden.  When EVERY matched layer of a run is
hidden, the merged layer keeps their merged properties but carries
the `tp-hidden' flag itself: the data is preserved without un-hiding
anything, and `tp-show-layer' on the merged layer renders it.

Returns the number of property runs modified, counting like
`tp-delete-layer': a run counts when at least one listed layer
matched and the merge rewrote it, and 0 means nothing matched at
all."
  (pcase-let ((`(,start ,end ,obj ,new-name ,layer-ids)
               (tp--parse-layer-args
                start-or-string
                (list end-or-name name-or-ids ids-or-object object) 2)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (let* ((layers-to-merge
                 (cl-loop for id in layer-ids
                          for found = (tp--get-layer-by-idx-or-name stack id)
                          when found collect found))
                ;; Sort by index (descending) to remove from end first
                (sorted-layers (sort (copy-sequence layers-to-merge)
                                     (lambda (a b) (> (car a) (car b))))))
           (when layers-to-merge
             ;; Merge properties (earlier in list takes precedence).
             ;; Hidden layers contribute no props unless ALL matched
             ;; layers are hidden, in which case the merged layer
             ;; keeps their props but stays hidden itself.
             (let* ((visible (seq-remove (lambda (found)
                                           (tp--stack-hidden-p (cdr found)))
                                         layers-to-merge))
                    (merged-props
                     (if visible
                         (tp--merge-layer-props
                          visible (list 'tp-name new-name))
                       (tp--merge-layer-props
                        layers-to-merge
                        (list 'tp-name new-name 'tp-hidden t))))
                    (new-stack stack))
               ;; Remove old layers from stack
               (dolist (idx (mapcar #'car sorted-layers))
                 (setq new-stack (-remove-at idx new-stack)))
               ;; Add merged layer at top
               (setq new-stack (cons merged-props new-stack))
               (set-text-properties abs-start abs-end
                                    (tp--stack-build-props new-stack)
                                    obj)
               (tp--stack-register-layers new-stack obj)
               (setq count (1+ count)))))))
      count)))

(defun tp-flatten-layers (start-or-string &optional end-or-name name-or-object object)
  "Flatten all layers into a single layer.

Calling conventions:
1. Buffer/string region:
   (tp-flatten-layers START END NAME OBJECT)

2. Entire string:
   (tp-flatten-layers STRING NAME)

NAME can be nil for an unnamed layer.  Higher layers take precedence;
a property explicitly set to nil in a higher layer stays nil in the
flattened result.

Hidden layers (see `tp-hide-layer') are DISCARDED, mirroring
image-editor flatten semantics: only the visible layers' properties
merge into the flattened result, so flattening can never render what
was hidden.  When EVERY layer of a run is hidden, the run's
properties are cleared entirely (bare text), consistent with the
all-hidden rendering of `tp-hide-layer'.

Returns the number of property runs modified, counting like
`tp-delete-layer': every run that had layers to flatten counts, and
0 means no run in the region had any layers."
  (pcase-let ((`(,start ,end ,obj ,name)
               (tp--parse-layer-args
                start-or-string
                (list end-or-name name-or-object object) 1)))
    (let ((count 0))
      (tp--stack-map-region
       start end obj
       (lambda (abs-start abs-end stack)
         (when stack
           ;; Hidden layers are discarded; an all-hidden run flattens
           ;; to bare text.
           (let* ((visible (seq-remove #'tp--stack-hidden-p stack))
                  (merged-props
                   (when visible
                     (tp--merge-layer-props
                      (cl-loop for layer in visible
                               for i from 0
                               collect (cons i layer))
                      (when name (list 'tp-name name))))))
             (set-text-properties abs-start abs-end merged-props obj)
             (when merged-props
               (tp--stack-register-layers (list merged-props) obj))
             (setq count (1+ count))))))
      count)))

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
            obj (car rest)))
     (t (error "Invalid layer arguments: %S"
               (cons start-or-string (list end-or-plist plist-or-object)))))

    ;; Handle plist wrapped in a list (from region form)
    (when (and (listp plist)
               (not (keywordp (car-safe plist)))
               (listp (car-safe plist)))
      (setq plist (car plist)))

    ;; Process each interval
    (tp--stack-map-region
     start end obj
     (lambda (abs-start abs-end stack)
       (let ((modified-stack
              (cl-loop for layer in stack
                       for i from 0
                       collect
                       (if (cl-some
                            (lambda (id)
                              (let ((found (tp--get-layer-by-idx-or-name
                                            stack id)))
                                (and found (= (car found) i))))
                            layer-ids)
                           ;; Merge plist into this layer
                           (tp--deep-merge-plist layer plist)
                         ;; Keep layer unchanged
                         layer))))
         (when stack
           (set-text-properties abs-start abs-end
                                (tp--stack-build-props modified-stack)
                                obj)
           (tp--stack-register-layers modified-stack obj)))))
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
            obj (car rest)))
     (t (error "Invalid layer arguments: %S"
               (cons start-or-string (list end-or-plist plist-or-object)))))

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
