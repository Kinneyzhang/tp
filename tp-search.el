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
(require 'tp-reactive)
(require 'tp-layer)
(require 'tp-ops)

(defun tp--search-register-layer-buffer (props object)
  "Record OBJECT in the reactive buffer registry for PROPS's layers.
When OBJECT is a buffer or nil (the current buffer) and the applied
PROPS carry a `tp-name' - directly, or inside a `tp-layers' entry
from a group application - register that buffer under each layer name
via `tp-reactive--register-layer-buffer', so reactive updates keep
visiting buffers written through the pattern-apply paths.  String
OBJECTs are not registered; see `tp-reactive-layer-buffers' for that
gap."
  (when (or (null object) (bufferp object))
    (let ((buf (or object (current-buffer))))
      (when-let ((name (plist-get props 'tp-name)))
        (tp-reactive--register-layer-buffer name buf))
      (dolist (layer (plist-get props 'tp-layers))
        (when-let ((name (plist-get layer 'tp-name)))
          (tp-reactive--register-layer-buffer name buf))))))

(defun tp--pattern-apply-single (pattern properties apply-fn object literal
                                         &optional start end subexp)
  "Apply APPLY-FN to matches of single PATTERN in OBJECT.
When LITERAL is non-nil, PATTERN is matched literally; otherwise it
is a regexp.  APPLY-FN is called with (START END PROPS OBJECT) for
each match.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped (matching the buffer path's historical narrow-to-region
behavior, now uniform across object types).  Matching behaves as if
OBJECT consisted only of that portion (the buffer path narrows, the
string path matches against the substring), so no match crosses the
boundaries.
When SUBEXP is non-nil, it names a capture group of PATTERN: the
properties and returned regions cover (match-beginning SUBEXP) to
\(match-end SUBEXP) of each match, and a match in which that group
does not participate contributes nothing.  The scan still advances
past the whole match.  A SUBEXP larger than PATTERN's group count
\(per `regexp-opt-depth') signals an error instead of silently
matching nothing.
For strings, returns a NEW string with properties applied
\(non-destructive).
For buffers, modifies in-place and returns list of regions.

Zero-width matches (an empty literal pattern, or a regexp that can
match the empty string) are recorded and the scan advances one
position past them, so the search always terminates."
  (let ((regexp (if literal (regexp-quote pattern) pattern)))
    ;; Reversed bounds are swapped, not signaled: the buffer path's
    ;; narrow-to-region always did this, so the string path follows.
    (when (and start end (> start end))
      (cl-rotatef start end))
    ;; A group number beyond the pattern's group count could never
    ;; match; make the typo loud instead of a silent no-op.
    (when (and subexp (> subexp (regexp-opt-depth regexp)))
      (error "Regexp %S has no group %d" pattern subexp))
    (cond
     ;; String object
     ((stringp object)
      ;; First, collect all match positions from the original string.
      ;; Bounded searches run against the substring so matches cannot
      ;; cross the [START, END) boundaries; positions are shifted back
      ;; into whole-string coordinates afterwards.
      (let* ((from (max (or start 0) 0))
             (to (min (or end (length object)) (length object)))
             (searchable (if (and (= from 0) (= to (length object)))
                             object
                           (substring object from to)))
             (matches nil)
             (pos 0)
             (limit (- to from)))
        (while (and (<= pos limit) (string-match regexp searchable pos))
          (let ((beg (match-beginning 0))
                (end (match-end 0))
                (sub-beg (match-beginning (or subexp 0)))
                (sub-end (match-end (or subexp 0))))
            ;; A group that does not participate contributes nothing.
            (when sub-beg
              (push (cons (+ from sub-beg) (+ from sub-end)) matches))
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
            (save-restriction
              (when (or start end)
                (narrow-to-region (max (or start (point-min)) (point-min))
                                  (min (or end (point-max)) (point-max))))
              (goto-char (point-min))
              (let (regions (keep-going t))
                (while (and keep-going (re-search-forward regexp nil t))
                  (let ((beg (match-beginning 0))
                        (end (match-end 0))
                        (sub-beg (match-beginning (or subexp 0)))
                        (sub-end (match-end (or subexp 0))))
                    ;; A group that does not participate contributes nothing.
                    (when sub-beg
                      (when properties
                        (funcall apply-fn sub-beg sub-end properties buf))
                      (push (cons sub-beg sub-end) regions))
                    ;; Guard against zero-width matches looping forever
                    (when (= beg end)
                      (if (eobp)
                          (setq keep-going nil)
                        (forward-char 1)))))
                (nreverse regions))))))))))

(defun tp--pattern-apply (pattern properties apply-fn object literal
                                  &optional start end subexp)
  "Apply APPLY-FN to matches of PATTERN (one pattern or a list).
When LITERAL is non-nil, patterns are matched literally; otherwise
they are regexps.  APPLY-FN is called with (START END PROPS OBJECT)
for each match.
START and END restrict matching to [START, END) in native
coordinates; SUBEXP names a capture group to target (see
`tp--pattern-apply-single').
For strings, returns a NEW string with properties applied
\(non-destructive).
For buffers, returns list of regions."
  (let ((patterns (if (listp pattern) pattern (list pattern))))
    (cond
     ;; String object
     ((stringp object)
      (let ((result object))
        (dolist (p patterns)
          (setq result (tp--pattern-apply-single p properties apply-fn
                                                 result literal
                                                 start end subexp)))
        result))
     ;; Buffer or nil (current buffer)
     (t
      (let ((all-regions nil))
        (dolist (p patterns)
          (let ((regions (tp--pattern-apply-single p properties apply-fn
                                                   object literal
                                                   start end subexp)))
            (setq all-regions (append all-regions regions))))
        all-regions)))))

(defun tp--match-apply-single (pattern properties apply-fn object
                                       &optional start end)
  "Apply APPLY-FN to literal matches of single PATTERN in OBJECT.
START and END restrict matching to [START, END) in native coordinates.
For strings, returns a new string with properties applied (non-destructive).
For buffers, modifies in-place and returns list of regions."
  (tp--pattern-apply-single pattern properties apply-fn object t start end))

(defun tp--match-apply (pattern properties apply-fn &optional object start end)
  "Internal function to apply APPLY-FN to matches of PATTERN.
PATTERN can be a string or a list of strings (multiple patterns).
When PATTERN is a list, each element is a pattern to match.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
START and END restrict matching to [START, END) in native coordinates.
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, returns list of regions."
  (tp--pattern-apply pattern properties apply-fn object t start end))

(defun tp--regexp-apply-single (pattern properties apply-fn object
                                        &optional start end subexp)
  "Apply APPLY-FN to regexp matches of single PATTERN in OBJECT.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
START and END restrict matching to [START, END) in native
coordinates; SUBEXP names a capture group to target.
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, modifies in-place and returns list of regions."
  (tp--pattern-apply-single pattern properties apply-fn object nil
                            start end subexp))

(defun tp--regexp-apply (pattern properties apply-fn
                                 &optional object start end subexp)
  "Internal function to apply APPLY-FN to regexp matches of PATTERN.
PATTERN can be a string (single regexp) or a list of strings (multiple regexps).
When PATTERN is a list, each element is a regexp to match.
APPLY-FN is called with (START END PROPS OBJECT) for each match.
START and END restrict matching to [START, END) in native
coordinates; SUBEXP names a capture group to target.
For strings, returns a NEW string with properties applied (non-destructive).
For buffers, returns list of regions."
  (tp--pattern-apply pattern properties apply-fn object nil start end subexp))

(defun tp--deep-merge-apply (start end props obj)
  "Apply PROPS to OBJ from START to END with deep merge.
Merges nested plists instead of replacing them.
For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place."
  (if (stringp obj)
      ;; For strings: create a new propertized string using tp--apply-props-to-string with :add mode
      (tp--apply-props-to-string obj start end props :add)
    ;; For buffers: modify in-place.  This path stamps `tp-name' for
    ;; resolved layer applications, so the buffer must be registered
    ;; in the reactive registry or later updates would skip it (REG-1).
    (tp--search-register-layer-buffer props obj)
    (let ((pos start))
      (while (< pos end)
        (let* ((current-props (text-properties-at pos obj))
               (next-pos (or (next-property-change pos obj end) end)))
          (cl-loop for (key val) on props by #'cddr
                   do (let* ((current-val (plist-get current-props key))
                             (new-val
                              (cond
                               ;; Face-family properties merge with the
                               ;; incoming face taking precedence, same as
                               ;; the string path (:add mode).
                               ((memq key tp-face-properties)
                                (tp--prepend-face val current-val))
                               ((and (listp val) (keywordp (car-safe val))
                                     (listp current-val)
                                     (keywordp (car-safe current-val)))
                                (tp--deep-merge-plist current-val val))
                               (t val))))
                        (put-text-property pos next-pos key new-val obj)))
          (setq pos next-pos))))
    obj))

(defun tp-match-set (pattern plist &optional object start end)
  "Set properties on all occurrences of PATTERN.

  (tp-match-set PATTERN PLIST &optional OBJECT START END)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.  Matching behaves as if OBJECT consisted only of that
portion, so no match crosses the boundaries.

Returns:
- For strings: a NEW string with properties applied (the original
  string is not modified)
- For buffers: list of (START . END) pairs for all matches."
  (tp--match-apply pattern (tp--ensure-props plist) #'tp-set object
                   start end))

(defun tp-match-reset (pattern plist &optional object start end)
  "Reset (completely replace) properties on all occurrences of PATTERN.

  (tp-match-reset PATTERN PLIST &optional OBJECT START END)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.

Unlike `tp-match-set', this completely replaces all existing properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of (START . END)
regions."
  (tp--match-apply pattern (tp--ensure-props plist)
                   #'tp--reset-apply
                   object start end))

(defun tp--reset-apply (start end props obj)
  "Apply PROPS to OBJ from START to END, completely replacing existing properties.
For strings, returns a NEW string.
For buffers, modifies in-place."
  (if (stringp obj)
      (tp--apply-props-to-string obj start end props :reset)
    (set-text-properties start end props obj)
    ;; A resolved layer application stamps `tp-name': register the
    ;; buffer so reactive updates keep visiting it (REG-1).
    (tp--search-register-layer-buffer props obj)
    obj))

(defun tp-match-add (pattern plist &optional object start end)
  "Add/update properties on all occurrences of PATTERN.

  (tp-match-add PATTERN PLIST &optional OBJECT START END)

PATTERN is a string (single pattern) or list of strings (multiple patterns).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.

Unlike `tp-match-set', this deeply merges nested properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of (START . END)
regions."
  (tp--match-apply pattern (tp--ensure-props plist) #'tp--deep-merge-apply
                   object start end))

(defun tp-regexp-set (pattern plist &optional object start end subexp)
  "Set properties on all matches of PATTERN (regexp).

  (tp-regexp-set PATTERN PLIST &optional OBJECT START END SUBEXP)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
Each pattern will be matched and have properties applied.
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.  Matching behaves as if OBJECT consisted only of that
portion, so no match crosses the boundaries.
When SUBEXP is non-nil, it names a capture group of PATTERN (1 for
the first group, like font-lock highlights): properties apply to that
group of each match instead of the whole match, and a match in which
the group does not participate contributes nothing.  A SUBEXP larger
than PATTERN's group count signals an error.

Returns:
- For strings: a NEW string with properties applied (the original
  string is not modified)
- For buffers: list of (START . END) pairs for all matches."
  (tp--regexp-apply pattern (tp--ensure-props plist) #'tp-set object
                    start end subexp))

(defun tp-regexp-reset (pattern plist &optional object start end subexp)
  "Reset (completely replace) properties on all regexp matches of PATTERN.

  (tp-regexp-reset PATTERN PLIST &optional OBJECT START END SUBEXP)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.
When SUBEXP is non-nil, properties apply to that capture group of
each match instead of the whole match; a match in which the group
does not participate contributes nothing.  A SUBEXP larger than
PATTERN's group count signals an error.

Unlike `tp-regexp-set', this completely replaces all existing properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of (START . END)
regions."
  (tp--regexp-apply pattern (tp--ensure-props plist)
                    #'tp--reset-apply
                    object start end subexp))

(defun tp-regexp-add (pattern plist &optional object start end subexp)
  "Add/update properties on all regexp matches of PATTERN.

  (tp-regexp-add PATTERN PLIST &optional OBJECT START END SUBEXP)

PATTERN is a string (single regexp) or list of strings (multiple regexps).
PLIST is a property list like \\='(face bold help-echo \"tip\"),
or a symbol representing a layer/group name defined by `define-tp'
or `define-tp-group'.
OBJECT is a buffer or string; nil means current buffer.
START and END restrict matching to the [START, END) portion of
OBJECT, in native coordinates (0-based for strings, 1-based for
buffers); nil means the object's bounds.  If START > END the bounds
are swapped.
When SUBEXP is non-nil, properties apply to that capture group of
each match instead of the whole match; a match in which the group
does not participate contributes nothing.  A SUBEXP larger than
PATTERN's group count signals an error.

Unlike `tp-regexp-set', this deeply merges nested properties.

For strings, returns a NEW string (original is not modified).
For buffers, modifies in-place and returns list of (START . END)
regions."
  (tp--regexp-apply pattern (tp--ensure-props plist) #'tp--deep-merge-apply
                    object start end subexp))

(defun tp-search-forward (property &optional value predicate not-current)
  "Search forward from point for text whose PROPERTY matches VALUE.
This is a raw wrapper: PROPERTY, VALUE, PREDICATE and NOT-CURRENT are
passed unchanged to `text-property-search-forward', whose semantics
apply in full - including the primitive's nil-PREDICATE default of
matching values that are non-nil and NOT `equal' to VALUE.  On
success point moves to the end of the matched region and a prop-match
object is returned; otherwise nil.

Obsolete since tp 0.3.0: call `tp-forward' for tp's `equal'-matching
search (which also supports string OBJECTs and repeat counts), or
call the Emacs primitive `text-property-search-forward' directly for
raw use - this wrapper adds nothing to it."
  (text-property-search-forward property value predicate not-current))
(make-obsolete 'tp-search-forward 'tp-forward "0.3.0")

(defun tp-search-backward (property &optional value predicate not-current)
  "Search backward from point for text whose PROPERTY matches VALUE.
This is a raw wrapper: PROPERTY, VALUE, PREDICATE and NOT-CURRENT are
passed unchanged to `text-property-search-backward', whose semantics
apply in full - including the primitive's nil-PREDICATE default of
matching values that are non-nil and NOT `equal' to VALUE.  On
success point moves to the beginning of the matched region and a
prop-match object is returned; otherwise nil.

Obsolete since tp 0.3.0: call `tp-backward' for tp's `equal'-matching
search (which also supports string OBJECTs and repeat counts), or
call the Emacs primitive `text-property-search-backward' directly for
raw use - this wrapper adds nothing to it."
  (text-property-search-backward property value predicate not-current))
(make-obsolete 'tp-search-backward 'tp-backward "0.3.0")

(defun tp--property-match-p (value prop-value predicate)
  "Return non-nil when PROP-VALUE matches VALUE under PREDICATE.
PREDICATE follows the convention tp uses for
`text-property-search-forward': nil and t both mean the values must
be `equal' (tp's 0.2.0 symmetric matching contract); a function is
called with VALUE and PROP-VALUE and matches when it returns
non-nil."
  (if (functionp predicate)
      (funcall predicate value prop-value)
    (equal value prop-value)))

(defun tp--string-property-matches (string property value predicate)
  "Collect PROPERTY runs of STRING matching VALUE under PREDICATE.
Returns a list of (START END VALUE) lists with 0-based positions.  A
run is a maximal stretch with one `eq' PROPERTY value, and it matches
when `tp--property-match-p' accepts that value.  Adjacent matching
runs with different values stay separate entries, mirroring how
`text-property-search-forward' ends a match where the property value
changes when a non-nil predicate is given."
  (let ((results nil))
    (tp--map-intervals
     string 0 (length string)
     (lambda (beg end val)
       (when (tp--property-match-p value val predicate)
         (push (list beg end val) results))
       nil)
     property)
    (nreverse results)))

(defun tp--property-search-backward (property value
                                              &optional predicate not-current)
  "Search backward for the previous region where PROPERTY matches VALUE.

This is the backward mirror of (text-property-search-forward PROPERTY
VALUE t): by default a region matches when its PROPERTY value is
`equal' to VALUE.  It deliberately does not call
`text-property-search-backward' with predicate t, because that
primitive's non-default-predicate branch skips every other property
run when non-matching runs intervene (observed through Emacs 30.2),
silently missing valid matches.

PREDICATE follows `tp--property-match-p': nil and t both mean `equal'
matching (the 0.2.0 contract); a function is called with VALUE and
the region's PROPERTY value.  When NOT-CURRENT is non-nil, the
matching region containing point (or ending exactly at point) is
skipped, mirroring the primitive's NOT-CURRENT argument.

If a matching region is found, move point to its beginning and
return a `prop-match' object whose end is clipped to the starting
point (matching the primitive's behavior when point starts inside a
matching region).  Otherwise return nil and leave point alone."
  (if (bobp)
      nil
    (let ((origin (point))
          (found nil))
      ;; Walk PROPERTY runs before point; remember the last matching one.
      ;; tp--map-intervals clips the run containing ORIGIN to end there.
      (tp--map-intervals
       (current-buffer) (point-min) origin
       (lambda (ibeg iend val)
         (when (and (tp--property-match-p value val predicate)
                    ;; With NOT-CURRENT, the run point is inside (or
                    ;; just after) is not a candidate.
                    (not (and not-current (= iend origin))))
           (setq found (list ibeg iend val)))
         nil)
       property)
      (when found
        (goto-char (car found))
        (make-prop-match :beginning (car found)
                         :end (cadr found)
                         :value (caddr found))))))

(defun tp-forward (property &optional value object n predicate not-current)
  "Search forward N times for text with PROPERTY.

VALUE is the optional value to match; N is the number of searches,
defaulting to 1.
OBJECT can be a buffer or string; nil defaults to current buffer.
PREDICATE customizes matching: nil (the default) and t both keep the
0.2.0 contract where a region matches when its PROPERTY value is
`equal' to VALUE; a function is called with VALUE and the region's
PROPERTY value and matches when it returns non-nil.  For buffers it
is passed to `text-property-search-forward'.
NOT-CURRENT is passed to `text-property-search-forward' and, when
non-nil, makes the search skip a matching region containing point.
It only applies to the buffer path; strings have no point, so it is
ignored there.

For buffers, each search starts from point and each successful one
moves point to the end of its matched region; the return value is
the prop-match object of the N-th search, or nil when that search
found nothing.

For strings, point is not involved at all: the return value is the
list of the FIRST N matching regions counted from position 0 of the
string, each a (START END VALUE) list with 0-based positions - not
the N-th match alone.  Fewer than N matches return however many
exist."
  (let ((count (or n 1)))
    (cond
     ;; String object - use tp-search (or the predicate-aware matcher)
     ((stringp object)
      (let ((matches (if (functionp predicate)
                         (tp--string-property-matches object property
                                                      value predicate)
                       (tp-search object property value))))
        (seq-take matches count)))
     ;; Buffer or nil
     (t
      (let ((result nil)
            (buf (or object (current-buffer))))
        (tp-with-current-buffer buf
          (dotimes (_ count)
            (setq result (text-property-search-forward
                          property value
                          (if (functionp predicate) predicate t)
                          not-current))))
        result)))))

(defun tp-backward (property &optional value object n predicate not-current)
  "Search backward N times for text with PROPERTY.

N is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.
PREDICATE customizes matching: nil (the default) and t both keep the
0.2.0 contract where a region matches when its PROPERTY value is
`equal' to VALUE; a function is called with VALUE and the region's
PROPERTY value and matches when it returns non-nil.
NOT-CURRENT, when non-nil, skips a matching region containing point
\(or ending exactly at point), mirroring
`text-property-search-backward'.  It only applies to the buffer
path; strings have no point, so it is ignored there.

For buffers, returns the prop-match object from the last successful search.
For strings, returns a list of (START END VALUE) for the last N matches
in reverse order (from end to start).

Uses `tp--property-search-backward' for buffers and `tp-search' (or
the predicate-aware matcher) for strings."
  (let ((count (or n 1)))
    (cond
     ;; String object - use tp-search and reverse
     ((stringp object)
      (let ((matches (nreverse (if (functionp predicate)
                                   (tp--string-property-matches
                                    object property value predicate)
                                 (tp-search object property value)))))
        (seq-take matches count)))
     ;; Buffer or nil
     (t
      (let ((result nil)
            (buf (or object (current-buffer))))
        (tp-with-current-buffer buf
          (dotimes (_ count)
            ;; `equal' matching by default, mirroring the predicate t
            ;; that `tp-forward' passes.  The previous code used the
            ;; default nil predicate, which matches values NOT `equal'
            ;; to VALUE and so inverted the match when VALUE was
            ;; non-nil.
            (setq result (tp--property-search-backward
                          property value predicate not-current))))
        result)))))

(defun tp--forward-do (function property &optional value object times
                                start end predicate not-current)
  "Internal: search forward TIMES for PROPERTY, call FUNCTION on last match.

FUNCTION receives two arguments: the prop-match object (or list for strings)
and OBJECT.
TIMES is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.
PREDICATE and NOT-CURRENT are passed to each underlying search (see
`tp-forward'); nil PREDICATE keeps the 0.2.0 `equal' matching.

FUNCTION is called only when the TIMES-th match exists; if fewer
matches are available, nothing is applied.
Returns the number of matches found (at most TIMES)."
  (let ((count (or times 1)))
    (cond
     ;; String object
     ((stringp object)
      (let* ((start-pos (or start 0))
             (end-pos (or end (length object)))
             (all-matches (if (functionp predicate)
                              (tp--string-property-matches object property
                                                           value predicate)
                            (tp-search object property value)))
             (filtered-matches (seq-filter (lambda (m)
                                             (and (>= (car m) start-pos)
                                                  (<= (cadr m) end-pos)))
                                           all-matches))
             (matches (seq-take filtered-matches count)))
        ;; All-or-nothing, mirroring the buffer path: FUNCTION targets
        ;; the TIMES-th match specifically, so when fewer matches exist
        ;; acting on a different one would hit the wrong target.
        (when (= (length matches) count)
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
                (when-let ((match (text-property-search-forward
                                   property value
                                   (if (functionp predicate) predicate t)
                                   not-current)))
                  (when (<= (prop-match-end match) search-end)
                    (when (= i (1- count))
                      (funcall function match buf))
                    (cl-incf matches)))))))
        matches)))))

(defun tp--replace-match-text (function arity match obj &optional idx)
  "Replace the text of MATCH in OBJ with the result of calling FUNCTION.

MATCH is either a (START END VALUE) list (string matches) or a
prop-match struct (buffer matches).  ARITY is the precomputed
\(func-arity FUNCTION); depending on it, FUNCTION is called with
\(TEXT), (TEXT START), (TEXT START END) or - when IDX is non-nil and
FUNCTION accepts a 4th argument - (TEXT START END IDX).

If FUNCTION returns a string, it replaces the matched text:
- For string OBJ the replacement happens in place; since strings have
  fixed length, a longer replacement is truncated to the match length
  and a shorter one only replaces that portion.  The replacement's
  text properties (including their absence) are copied onto the
  replaced portion.
- For buffer OBJ the match is replaced via `delete-region' + `insert'
  \(the buffer may grow or shrink).
Any non-string return value leaves OBJ untouched."
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
         (can-accept-idx (and idx
                              (or (eq max-arity 'many)
                                  (and (numberp max-arity) (>= max-arity 4)))))
         (new-text (cond
                    (can-accept-idx (funcall function text m-start m-end idx))
                    (can-accept-end (funcall function text m-start m-end))
                    (can-accept-start (funcall function text m-start))
                    (t (funcall function text)))))
    (when (stringp new-text)
      (if (stringp obj)
          ;; For strings: copy text content and properties separately.
          ;; A string cannot change length in place, so a replacement
          ;; of a different length would silently corrupt the text
          ;; (truncation or residue); reject it clearly instead.
          (let ((len (- m-end m-start)))
            (unless (= (length new-text) len)
              (error "tp: replacement %S is %d chars but the match is %d; \
strings cannot change length in place -- use a buffer OBJECT for \
length-changing replacements" new-text (length new-text) len))
            (store-substring obj m-start new-text)
            ;; Copy properties from new-text to obj.  Ranges with nil
            ;; properties are copied too, so FUNCTION can REMOVE
            ;; properties by returning a stripped string.
            (let ((pos 0))
              (while (< pos len)
                (let* ((props (text-properties-at pos new-text))
                       (next-change (or (next-property-change pos new-text)
                                        len)))
                  (set-text-properties (+ m-start pos)
                                       (+ m-start (min next-change len))
                                       props
                                       obj)
                  (setq pos next-change)))))
        ;; For buffers, delete and insert
        (unless (equal new-text text)
          (save-excursion
            (delete-region m-start m-end)
            (goto-char m-start)
            (insert new-text)))))))

(defun tp-forward-do (function property &optional value object times
                               start end predicate not-current)
  "Search forward TIMES times for PROPERTY; apply FUNCTION at the Nth match.

Despite the -do suffix this is NOT a for-each: the search advances
through TIMES matches and FUNCTION is applied only to the final
\(TIMES-th) one.  Use `tp-search-map' to apply a function to EVERY
match.

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
PREDICATE customizes matching: nil (the default) and t both keep the
0.2.0 contract where a region matches when its PROPERTY value is
`equal' to VALUE; a function is called with VALUE and the region's
PROPERTY value and matches when it returns non-nil.
NOT-CURRENT is passed to each underlying
`text-property-search-forward' call; it only applies to the buffer
path (strings have no point).

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
       (tp--replace-match-text function arity match obj))
     property value object times start end predicate not-current)))

(defun tp--backward-do (function property &optional value object times
                                 start end predicate not-current)
  "Internal: search backward TIMES for PROPERTY, call FUNCTION on last match.

FUNCTION receives two arguments: the prop-match object (or list for strings)
and OBJECT.
TIMES is the number of searches, defaulting to 1.
VALUE is the optional value to match.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.
PREDICATE and NOT-CURRENT are passed to each underlying search (see
`tp-backward'); nil PREDICATE keeps the 0.2.0 `equal' matching.

FUNCTION is called only when the TIMES-th match exists; if fewer
matches are available, nothing is applied.
Returns the number of matches found (at most TIMES)."
  (let ((count (or times 1)))
    (cond
     ;; String object - reverse the matches
     ((stringp object)
      (let* ((start-pos (or start 0))
             (end-pos (or end (length object)))
             (all-matches (if (functionp predicate)
                              (tp--string-property-matches object property
                                                           value predicate)
                            (tp-search object property value)))
             (filtered-matches
              (seq-filter (lambda (m)
                            (and (>= (car m) start-pos)
                                 (<= (cadr m) end-pos)))
                          all-matches))
             (matches (seq-take (nreverse filtered-matches) count)))
        ;; All-or-nothing; see tp--forward-do.
        (when (= (length matches) count)
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
                ;; `equal' matching by default, same as tp--forward-do's
                ;; predicate t.
                (when-let ((match (tp--property-search-backward
                                   property value predicate not-current)))
                  (when (>= (prop-match-beginning match) search-start)
                    (when (= i (1- count))
                      (funcall function match buf))
                    (cl-incf matches)))))))
        matches)))))

(defun tp-backward-do (function property &optional value object times
                                start end predicate not-current)
  "Search backward TIMES times for PROPERTY; apply FUNCTION at the Nth match.

Despite the -do suffix this is NOT a for-each: the search walks back
through TIMES matches and FUNCTION is applied only to the final
\(TIMES-th) one.  Use `tp-search-map' to apply a function to EVERY
match.

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
PREDICATE customizes matching: nil (the default) and t both keep the
0.2.0 contract where a region matches when its PROPERTY value is
`equal' to VALUE; a function is called with VALUE and the region's
PROPERTY value and matches when it returns non-nil.
NOT-CURRENT, when non-nil, skips a matching region containing point
on each underlying search; it only applies to the buffer path
\(strings have no point).

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
       (tp--replace-match-text function arity match obj))
     property value object times start end predicate not-current)))

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

FUNCTION receives two arguments: the match, as a (START END VALUE)
list, and OBJECT.
PROPERTY is the text property to search for.
VALUE is the optional value to match; nil means search for PROPERTY
without matching value.
OBJECT can be a buffer or string; nil defaults to current buffer.
START and END define the search range; defaults are object start and end.

Returns the number of matches processed.

For buffers, FUNCTION is called with OBJECT as the current buffer, and
the match positions handed to FUNCTION are tracked with markers, so
FUNCTION may safely change the length of earlier matches (e.g. replace
their text): later matches still receive their up-to-date positions."
  (let ((obj (or object (current-buffer))))
    (if (stringp obj)
        (let* ((all-matches (tp-search obj property value))
               (s (or start 0))
               (e (or end (length obj)))
               (filtered-matches
                (seq-filter (lambda (m)
                              (and (>= (car m) s)
                                   (<= (cadr m) e)))
                            all-matches)))
          (dolist (match filtered-matches)
            (funcall function match obj))
          (length filtered-matches))
      ;; Buffer: do all the work with OBJ current, and track match
      ;; positions with markers so length-changing edits made by
      ;; FUNCTION on earlier matches don't invalidate later positions.
      (tp-with-current-buffer obj
        (let* ((s (or start (point-min)))
               (e (or end (point-max)))
               (matches (tp-search s e property value obj))
               (marked (mapcar (lambda (m)
                                 ;; Begin markers advance on insertion at
                                 ;; their position so adjacent runs stay
                                 ;; correct after a replacement.
                                 (list (copy-marker (car m) t)
                                       (copy-marker (cadr m))
                                       (caddr m)))
                               matches)))
          (unwind-protect
              (dolist (m marked)
                (funcall function
                         (list (marker-position (car m))
                               (marker-position (cadr m))
                               (caddr m))
                         obj))
            (dolist (m marked)
              (set-marker (car m) nil)
              (set-marker (cadr m) nil)))
          (length marked))))))

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
  (let ((idx 0)
        (arity (func-arity function)))
    (tp--search-do
     (lambda (match obj)
       (tp--replace-match-text function arity match obj idx)
       (setq idx (1+ idx)))
     property value object start end)))

(provide 'tp-search)
;;; tp-search.el ends here
