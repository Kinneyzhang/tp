;;; tp-search.el --- Pattern matching and property search for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Pattern-driven property application (`tp-match-*', `tp-regexp-*')
;; and property-run search/navigation (`tp-search', `tp-search-map',
;; `tp-forward', `tp-backward', `tp-forward-do', `tp-backward-do').

;;; Code:

(require 'cl-lib)
(require 'text-property-search)
(require 'tp-core)
(require 'tp-layer)
(require 'tp-ops)

(defun tp--match-apply-single (pattern properties apply-fn object)
  "Apply APPLY-FN to matches of single PATTERN in OBJECT.
For strings, returns a new string with properties applied (non-destructive).
For buffers, modifies in-place and returns list of regions."
  (cond
   ;; String object
   ((stringp object)
    ;; First, collect all match positions from the original string
    (let ((matches nil)
          (pos 0))
      (while (string-match (regexp-quote pattern) object pos)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (push (cons beg end) matches)
          (setq pos (if (= beg end) (1+ beg) end))))
      ;; Apply function to each match in order (reverse to get correct order)
      ;; Make a copy to ensure original string is not modified
      (let ((result (copy-sequence object)))
        (dolist (match (nreverse matches))
          (when properties
            (setq result (funcall apply-fn (car match) (cdr match) properties result))))
        result)))
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
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, returns list of regions."
  (let ((patterns (if (listp pattern) pattern (list pattern))))
    (cond
     ;; String object
     ((stringp object)
      (let ((result object))
        (dolist (p patterns)
          (setq result (tp--match-apply-single p properties apply-fn result)))
        result))
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
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, modifies in-place and returns list of regions."
  (cond
   ;; String object
   ((stringp object)
    ;; First, collect all match positions from the original string
    (let ((matches nil)
          (pos 0))
      (while (string-match pattern object pos)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (push (cons beg end) matches)
          (setq pos (if (= beg end) (1+ beg) end))))
      ;; Apply function to each match in order (reverse to get correct order)
      ;; Make a copy to ensure original string is not modified
      (let ((result (copy-sequence object)))
        (dolist (match (nreverse matches))
          (when properties
            (setq result (funcall apply-fn
                                  (car match) (cdr match)
                                  properties result))))
        result)))
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
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, returns list of regions."
  (let ((patterns (if (listp pattern) pattern (list pattern))))
    (cond
     ;; String object
     ((stringp object)
      (let ((result object))
        (dolist (p patterns)
          (setq result (tp--regexp-apply-single p properties apply-fn result)))
        result))
     ;; Buffer or nil (current buffer)
     (t
      (let ((all-regions nil))
        (dolist (p patterns)
          (let ((regions (tp--regexp-apply-single p properties apply-fn object)))
            (setq all-regions (append all-regions regions))))
        all-regions)))))

(defun tp--deep-merge-apply (start end props obj)
  "Apply PROPS to OBJ from START to END with deep merge.
Merges nested plists instead of replacing them.
For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place."
  (if (stringp obj)
      ;; For strings: create a new propertized string using tp--apply-props-to-string with :add mode
      (tp--apply-props-to-string obj start end props :add)
    ;; For buffers: modify in-place
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
          (setq pos next-pos))))
    obj))

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

Unlike `tp-match-set', this completely replaces all existing properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of regions."
  (tp--match-apply pattern (tp--ensure-props plist)
                   #'tp--reset-apply
                   object))

(defun tp--reset-apply (start end props obj)
  "Apply PROPS to OBJ from START to END, completely replacing existing properties.
For strings, returns a NEW string.
For buffers, modifies in-place."
  (if (stringp obj)
      (tp--apply-props-to-string obj start end props :reset)
    (set-text-properties start end props obj)
    obj))

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

Unlike `tp-regexp-set', this completely replaces all existing properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of regions."
  (tp--regexp-apply pattern (tp--ensure-props plist)
                    #'tp--reset-apply
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

(provide 'tp-search)
;;; tp-search.el ends here
