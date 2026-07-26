;;; tp-core.el --- Foundation utilities for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Foundation layer of the tp library.  No dependencies on other tp
;; modules.  Provides: debug logging, interval/property inspection
;; (`tp-intervals', `tp-plist', `tp-empty-p', `tp-intervals-map'),
;; plist utilities (deep merge, duplicate-key merge, nested access),
;; the face merge engine, pure reactive-symbol ($var) utilities, and
;; small shared helpers.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'seq)

(defgroup tp nil
  "Group for tp.el text property manipulation."
  :prefix "tp-"
  :group 'development)

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
These property names are reserved and cannot be used as layer names
in `define-tp'.
An error is signaled at macro expansion time (when the `define-tp' form is
evaluated) if a reserved name is used, preventing the layer definition from
being created.")

(defun tp--builtin-text-property-p (name)
  "Return non-nil if NAME is a built-in text property name.
NAME should be a symbol."
  (memq name tp--builtin-text-properties))

(defconst tp-face-properties '(face font-lock-face mouse-face)
  "Text properties whose values follow face merging semantics.
These properties hold face specs (symbols, plists or lists thereof)
and are merged with face-aware logic instead of plain replacement.")

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

;;;###autoload
(defun tp-debug-clear ()
  "Clear the *tp-debug* buffer."
  (interactive)
  (when-let ((buf (get-buffer "*tp-debug*")))
    (with-current-buffer buf
      (erase-buffer))))

;;;###autoload
(defun tp-debug-show ()
  "Show the *tp-debug* buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create "*tp-debug*")))

(defmacro tp-with-current-buffer (buffer-or-name &rest body)
  "Execute BODY in BUFFER-OR-NAME with `inhibit-read-only' bound to t."
  (declare (indent defun))
  `(with-current-buffer ,buffer-or-name
     (let ((inhibit-read-only t))
       ,@body)))

(defun tp-intervals (start end &optional object absolute)
  "Return list of property intervals from START to END in OBJECT.
Each element is (START END PROPERTIES).  OBJECT defaults to current
buffer.
For buffers, positions are by default relative to START (0-based
offsets, the legacy convention).  When ABSOLUTE is non-nil they are
native 1-based buffer positions instead, directly reusable in other
tp calls (`tp-set', `tp-remove', ...) without offset arithmetic.
For strings, positions are always absolute (0-based); ABSOLUTE
changes nothing.
Intervals that extend beyond the requested range are clipped to it, so
returned positions never fall outside [START, END)."
  (let* ((intervals (object-intervals (or object (current-buffer))))
         ;; For buffers, object-intervals returns 0-based positions
         ;; but buffer positions are 1-based, so we need to adjust:
         ;; subtracting (1- start) makes them START-relative, while
         ;; subtracting -1 restores native 1-based positions.
         (offset (cond ((stringp object) 0)
                       (absolute -1)
                       (t (1- start))))
         ;; Filter bounds in 0-based terms for buffers
         (filter-start (if (stringp object) start (1- start)))
         (filter-end (if (stringp object) end (1- end))))
    (mapcar (lambda (tp)
              (let* ((tp-start (- (max (nth 0 tp) filter-start) offset))
                     (tp-end (- (min (nth 1 tp) filter-end) offset))
                     (tp-props (nth 2 tp)))
                (list tp-start tp-end tp-props)))
            (seq-filter (lambda (tp)
                          (and (< (nth 0 tp) filter-end)
                               (> (nth 1 tp) filter-start)))
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

(defun tp--string-has-properties-p (str)
  "Return non-nil if string STR has any text properties.
Scans the entire string, not just position 0."
  (and (stringp str)
       (not (null (object-intervals str)))))

(defun tp--equal-including-string-properties (a b)
  "Compare A and B for equality, considering string text properties.
If both A and B are strings, uses `equal-including-properties' to ensure
text properties are considered in the comparison.
Otherwise, uses standard `equal'."
  (if (and (stringp a) (stringp b))
      (equal-including-properties a b)
    (equal a b)))

(defun tp--parse-face-list (face-list)
  "Parse a mixed face list into symbols and a plist.
FACE-LIST can be a mix of:
- Face symbols (like bold, italic)
- Face plists (like (:foreground \"red\"))
- Inline plist keys and values (like bold :foreground \"green\")

Returns (SYMBOLS . PLIST) where SYMBOLS is a list of face symbols
and PLIST is the merged plist of all face attributes."
  (let ((symbols nil)
        (plist nil)
        (i 0)
        (len (length face-list)))
    (while (< i len)
      (let ((elem (nth i face-list)))
        (cond
         ;; Nested plist like (:foreground "red")
         ((and (listp elem) (keywordp (car-safe elem)))
          (setq plist (if plist (tp--deep-merge-plist plist elem) elem))
          (setq i (1+ i)))
         ;; Inline keyword - consume key and value
         ((keywordp elem)
          (if (< (1+ i) len)
              (let ((key elem)
                    (val (nth (1+ i) face-list)))
                (setq plist (if plist
                                (plist-put plist key val)
                              (list key val)))
                (setq i (+ i 2)))
            ;; Trailing bare keyword with no value: malformed input.
            ;; Ignore it rather than inventing a bogus (KEY nil) pair.
            (setq i (1+ i))))
         ;; Face symbol
         ((symbolp elem)
          (push elem symbols)
          (setq i (1+ i)))
         ;; Something else - skip
         (t (setq i (1+ i))))))
    (cons (nreverse symbols) plist)))

(defun tp--remove-sub-from-face-value (face-value sub-key)
  "Remove SUB-KEY from FACE-VALUE, handling complex face structures.
FACE-VALUE can be:
- A simple plist like (:foreground \"red\" :background \"blue\")
- A symbol like bold
- A mixed list like ((:foreground \"red\") (:strike-through t) bold)

Returns the modified face value with SUB-KEY removed from any plist components.
Returns nil if the result would be empty."
  (cond
   ;; Nil face - nothing to remove
   ((null face-value) nil)
   ;; Symbol face - no sub-key to remove
   ((symbolp face-value) face-value)
   ;; Simple plist - remove the sub-key directly
   ((and (listp face-value) (keywordp (car-safe face-value)))
    (let ((result nil))
      (cl-loop for (k v) on face-value by #'cddr
               unless (eq k sub-key)
               do (setq result (plist-put result k v)))
      result))
   ;; Mixed list - parse and remove from plist component
   ((listp face-value)
    (let* ((parsed (tp--parse-face-list face-value))
           (symbols (car parsed))
           (plist (cdr parsed)))
      (when plist
        ;; Remove sub-key from the merged plist
        (let ((new-plist nil))
          (cl-loop for (k v) on plist by #'cddr
                   unless (eq k sub-key)
                   do (setq new-plist (plist-put new-plist k v)))
          (setq plist new-plist)))
      ;; Reconstruct the face value
      (cond
       ((and symbols plist) (append symbols (list plist)))
       (symbols (if (= (length symbols) 1) (car symbols) symbols))
       (plist plist)
       (t nil))))
   ;; Unknown format - return as-is
   (t face-value)))

(defun tp--subtract-face-from-face-value (face-value face-to-remove)
  "Remove FACE-TO-REMOVE from FACE-VALUE.
FACE-TO-REMOVE is the face contribution to subtract (from a layer).
FACE-VALUE is the current combined face value.
Returns the modified face value with the layer's face contribution removed."
  (cond
   ;; Nothing to remove from
   ((null face-value) nil)
   ;; If face-to-remove is nil, return as-is
   ((null face-to-remove) face-value)
   ;; If they're equal, remove entirely
   ((equal face-value face-to-remove) nil)
   ;; face-to-remove is a plist - remove those keys from face-value
   ((and (listp face-to-remove) (keywordp (car-safe face-to-remove)))
    (let ((keys-to-remove (cl-loop for (k _v) on face-to-remove by #'cddr
                                   collect k)))
      ;; Remove each key
      (dolist (key keys-to-remove)
        (setq face-value (tp--remove-sub-from-face-value face-value key)))
      face-value))
   ;; face-to-remove is a symbol - remove it from face-value
   ((symbolp face-to-remove)
    (cond
     ((eq face-value face-to-remove) nil)
     ((and (listp face-value) (not (keywordp (car-safe face-value))))
      (let ((result (remove face-to-remove face-value)))
        (if (= (length result) 1) (car result) result)))
     (t face-value)))
   ;; face-to-remove is a list - remove each element
   ((listp face-to-remove)
    (dolist (elem face-to-remove)
      (setq face-value (tp--subtract-face-from-face-value face-value elem)))
    face-value)
   ;; Unknown - return as-is
   (t face-value)))

(defun tp--merge-string-props-into-plist (str props)
  "Merge text properties from string STR into PROPS plist.
Properties from PROPS take precedence over those in STR.
Returns the merged plist where new props override embedded props.
For simplicity, only considers properties at position 0 of STR."
  (if (not (tp--string-has-properties-p str))
      props
    (let ((str-props (text-properties-at 0 str))
          (result (copy-sequence props)))
      ;; Merge each property from the string into result
      ;; Props values take precedence over embedded string values
      (cl-loop for (key val) on str-props by #'cddr
               do (let ((existing (plist-get result key)))
                    (if existing
                        ;; Props already has this key - merge with props taking precedence
                        (setq result
                              (plist-put result key
                                         (cond
                                          ;; Face properties need special merging
                                          ;; Pass embedded val as face1 (base), existing as face2 (override)
                                          ((memq key tp-face-properties)
                                           (tp--merge-face-values val existing))
                                          ;; Other properties - props value takes precedence
                                          (t existing))))
                      ;; Props doesn't have this key - add from string
                      (setq result (plist-put result key val)))))
      result)))

(defun tp--merge-face-values (face1 face2)
  "Merge two face values into one.
FACE1 is the earlier value, FACE2 is the later value.
For face plists (like (:foreground \"red\")), merge with later overriding.
For symbol faces, create a list with FACE2 taking precedence.
Returns the merged face value.

Role: this is the merge engine for face values that arrive together in
a SINGLE call's property spec - `tp--merge-duplicate-keys' reduces
repeated face/font-lock-face/mouse-face keys through it, and
`tp--merge-string-props-into-plist' uses it to fold a string's embedded
face into caller props.  Argument order is (EARLIER LATER); LATER wins.

Note: `tp--prepend-face' is a sibling engine used by `tp-add' to merge
an INCOMING face value into one already present on the text.  Its
argument order is swapped ((NEW EXISTING)), and the two engines have
drifted for mixed lists: `tp--prepend-face' parses a mixed
symbol/plist list and merges plist components, whereas this function
conses a plist override onto a non-plist list without parsing.  Do not
substitute one for the other without checking those cases."
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
     ;; face1 is a plist - need to merge any plist in face2 with face1
     ((and (listp face1) (keywordp (car-safe face1)))
      ;; Use tp--parse-face-list to handle mixed formats like (bold :foreground "green")
      (let* ((parsed (tp--parse-face-list face2))
             (symbols (car parsed))
             (plist (cdr parsed)))
        ;; Merge face2's plist with face1, then prepend symbols
        (let ((merged-plist (if plist (tp--deep-merge-plist face1 plist) face1)))
          (if symbols
              (append symbols (list merged-plist))
            merged-plist))))
     ;; Both are lists - parse both, merge plists, combine symbols
     ((listp face1)
      (let* ((parsed1 (tp--parse-face-list face1))
             (symbols1 (car parsed1))
             (plist1 (cdr parsed1))
             (parsed2 (tp--parse-face-list face2))
             (symbols2 (car parsed2))
             (plist2 (cdr parsed2))
             ;; Merge plists with face2's plist taking precedence
             (merged-plist (cond
                            ((and plist1 plist2) (tp--deep-merge-plist plist1 plist2))
                            (plist2 plist2)
                            (plist1 plist1)
                            (t nil)))
             ;; Combine symbols: face2 symbols first, then face1 symbols not in face2
             (merged-symbols (append symbols2
                                     (cl-remove-if (lambda (s) (member s symbols2)) symbols1))))
        ;; Build result: symbols first, then merged plist if any
        (if merged-plist
            (append merged-symbols (list merged-plist))
          merged-symbols)))
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

  (tp--merge-duplicate-keys
   \\='(face (:background \"blue\") face (:foreground \"red\")))
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
                    ((memq key tp-face-properties)
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
If VAL is a plist, recursively extract only the key-value pairs
containing REACTIVE-VAR.
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
For mixed lists containing both symbols and plists, plists are merged correctly.
Duplicate faces are not added.

Role: this is the merge engine `tp-add' uses to fold an INCOMING face
value into the face value already present on the text, for every
property in `tp-face-properties'.  Argument order is (NEW EXISTING);
NEW wins.

Note: `tp--merge-face-values' is a sibling engine (argument order
swapped: (EARLIER LATER)) used when duplicate face keys appear within
a single call's property spec.  The two have drifted for mixed
symbol/plist lists - this function parses such lists and merges their
plist components, `tp--merge-face-values' conses a plist override onto
a non-plist list without parsing.  Do not substitute one for the other
without checking those cases."
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
      ;; Parse existing to extract any plists and merge them
      (let* ((parsed (tp--parse-face-list existing-face))
             (existing-symbols (car parsed))
             (existing-plist (cdr parsed)))
        (if existing-plist
            ;; Merge new-face plist with existing plist, prepend symbols
            (let ((merged-plist (tp--deep-merge-plist existing-plist new-face)))
              (if existing-symbols
                  (append existing-symbols (list merged-plist))
                merged-plist))
          (cons new-face existing-face))))
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
   ;; New face is a list of faces - parse and merge with existing
   ((listp new-face)
    (cond
     ((symbolp existing-face)
      (if (member existing-face new-face)
          new-face
        (append new-face (list existing-face))))
     ((listp existing-face)
      ;; Parse both to extract symbols and plists, then merge appropriately
      (let* ((parsed-new (tp--parse-face-list new-face))
             (new-symbols (car parsed-new))
             (new-plist (cdr parsed-new))
             (parsed-existing (tp--parse-face-list existing-face))
             (existing-symbols (car parsed-existing))
             (existing-plist (cdr parsed-existing))
             ;; Merge plists with new taking precedence
             (merged-plist (cond
                            ((and existing-plist new-plist)
                             (tp--deep-merge-plist existing-plist new-plist))
                            (new-plist new-plist)
                            (existing-plist existing-plist)
                            (t nil)))
             ;; Combine symbols: new symbols first, then existing symbols not in new
             (merged-symbols (append new-symbols
                                     (cl-remove-if (lambda (s) (member s new-symbols))
                                                   existing-symbols))))
        ;; Build result: symbols first, then merged plist if any
        (if merged-plist
            (append merged-symbols (list merged-plist))
          merged-symbols)))
     (t new-face)))
   (t new-face)))

(defun tp--map-intervals (object start end function &optional property)
  "Iterate property intervals of OBJECT between START and END, clipped.

OBJECT is a string, a buffer, or nil for the current buffer.
FUNCTION is called with (ISTART IEND VALUE) for each interval, where
ISTART/IEND are clipped to the [START, END) range and expressed in
OBJECT's native coordinates (0-based for strings, 1-based for
buffers).  START and END may be nil, meaning the object's bounds.

When PROPERTY is nil, intervals are maximal runs with an identical
full property list and VALUE is that plist.  When PROPERTY is
non-nil, intervals are maximal runs of `eq' values of that single
property and VALUE is the property's value (which may be nil).

Unlike `tp-intervals', intervals that extend beyond the requested
range are clipped to it, so FUNCTION never sees positions outside
\[START, END).  Returns the list of FUNCTION's return values, in
order."
  (let* ((is-string (stringp object))
         (buf (unless is-string (or object (current-buffer)))))
    (if is-string
        (let* ((min-pos 0)
               (max-pos (length object))
               (from (max (or start min-pos) min-pos))
               (to (min (or end max-pos) max-pos))
               (pos from)
               (results nil))
          (while (< pos to)
            (let ((next (if property
                            (or (next-single-property-change
                                 pos property object to)
                                to)
                          (or (next-property-change pos object to) to)))
                  (val (if property
                           (get-text-property pos property object)
                         (text-properties-at pos object))))
              (push (funcall function pos next val) results)
              (setq pos next)))
          (nreverse results))
      (with-current-buffer buf
        (let* ((from (max (or start (point-min)) (point-min)))
               (to (min (or end (point-max)) (point-max)))
               (pos from)
               (results nil))
          (while (< pos to)
            (let ((next (if property
                            (or (next-single-property-change pos property nil to)
                                to)
                          (or (next-property-change pos nil to) to)))
                  (val (if property
                           (get-text-property pos property)
                         (text-properties-at pos))))
              (push (funcall function pos next val) results)
              (setq pos next)))
          (nreverse results))))))

(defun tp-intervals-map (function start end &optional object absolute)
  "Apply FUNCTION to each property interval of [START, END) in OBJECT.

FUNCTION is called with (I-START I-END TOP-PROPS BELOW-PROPS-LST) for
every interval `tp-intervals' reports, splitting the layer-stack
bookkeeping out of the raw properties:
- TOP-PROPS is the interval's property plist with the `tp-layers'
  entry removed: the directly rendered properties.
- BELOW-PROPS-LST is the value of the interval's `tp-layers'
  property: the list of stored layer plists (normally the layers
  buried below the rendered top layer; while any layer is hidden it
  holds the whole ordered stack - see `tp-layer-stack-at' for the
  decoded view).  It is nil when the interval carries no layer stack.

I-START/I-END follow `tp-intervals' coordinates: for buffers they
are by default relative to START (0-based offsets, the legacy
convention), or native 1-based buffer positions when ABSOLUTE is
non-nil; for strings they are always absolute 0-based positions.
OBJECT is a string, a buffer, or nil for the current buffer.

Returns the list of FUNCTION's non-nil results, in interval order
\(nil results are dropped)."
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
    (tp-intervals start end object absolute))))

(provide 'tp-core)
;;; tp-core.el ends here
