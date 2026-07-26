;;; tp-render.el --- Reactive re-rendering engine for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; The reactive update engine: when a reactive variable changes, this
;; module recomputes layer definitions and re-renders every affected
;; buffer region, including live `tp-text' text replacement.  It
;; installs itself into tp-reactive.el (update/flush hooks) and
;; tp-ops.el (`tp-text' handler).

;;; Code:

(require 'cl-lib)
(require 'tp-core)
(require 'tp-reactive)
(require 'tp-layer)
(require 'tp-ops)
(require 'tp-search)

(defun tp--layer-reactive-props (layer-name)
  "Collect LAYER-NAME's unresolved reactive props from `tp-reactive-deps'.
Each dependency entry stores only the portions of the layer's props
that reference one variable; this merges the fragments back into a
single plist with the `$var' markers intact.  Returns nil when the
layer has no reactive props (data-only dependencies store nil)."
  (let ((all nil))
    (dolist (dep tp-reactive-deps)
      (let ((layer-entry (assoc layer-name (cdr dep))))
        (when (and layer-entry (cdr layer-entry))
          (setq all (if all
                        (tp--deep-merge-plist all (cdr layer-entry))
                      (copy-sequence (cdr layer-entry)))))))
    all))

(defun tp--layer-render-props (layer-name override-alist)
  "Return LAYER-NAME's props for re-rendering in the current buffer.
Starts from the stored layer definition and deep-merges the layer's
reactive props re-resolved against the current variable values, so
buffer-local values are honored when the target buffer is current.
OVERRIDE-ALIST maps variables to not-yet-visible new values (the
variable watcher runs before the variable is actually set) and takes
precedence over `symbol-value'.  Returns nil when the layer has no
usable definition."
  (let ((base (tp-layer-props layer-name t)))  ; include tp-name for tracking
    (when base
      (let ((reactive (tp--layer-reactive-props layer-name)))
        (if reactive
            (tp--deep-merge-plist
             base (tp--resolve-reactive-symbols reactive override-alist))
          base)))))

(defun tp--update-layer-computed (layer-name override-alist)
  "Update computed reactive variables for LAYER-NAME with OVERRIDE-ALIST.
Evaluates compute functions and updates the reactive variable values.
A compute function returning nil is a legitimate result and is
propagated; only computes that signal an error are skipped (see
`tp--compute-error').
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
                 tp--compute-error))))
        (unless (eq computed-val tp--compute-error)
          ;; Update the global variable
          (set var-sym computed-val)
          ;; Add to override-alist for property resolution
          (push (cons var-sym computed-val) override-alist)
          ;; Also update the layer properties if the computed var is used in props
          (let ((current-props (cdr (assoc layer-name tp-layer-alist))))
            (when current-props
              (when-let ((all-reactive-props (tp--layer-reactive-props layer-name)))
                (let ((resolved-props (tp--resolve-reactive-symbols
                                       all-reactive-props override-alist)))
                  (when resolved-props
                    ;; Deep-merge the resolved props into the current layer
                    ;; props so sibling static attributes nested in plists
                    ;; (e.g. a :background next to a reactive :foreground)
                    ;; survive the update.
                    (tp--set-layer-props
                     layer-name
                     (tp--deep-merge-plist current-props resolved-props)))))))))))
  override-alist)

(defun tp--render-visit-buffer (buffer fn)
  "Call FN with BUFFER current and `inhibit-read-only' bound to t.
Dead buffers are skipped.  This is the per-buffer seam of the
reactive update walk; tests may advise it to count buffer visits."
  (when (buffer-live-p buffer)
    (tp-with-current-buffer buffer
      (funcall fn))))

(defun tp--map-layer-buffers (layer-name where fn)
  "Run FN in each buffer that may show LAYER-NAME's regions.
A non-nil WHERE (a live buffer, the `setq-local' case) restricts the
walk to that buffer.  Otherwise the walk consults the buffer registry
via `tp-reactive-layer-buffers' and visits only registered live
buffers.  When the registry answers `unknown', the walk falls back to
a full `buffer-list' scan, registering every buffer that actually
contains a region of LAYER-NAME; once at least one buffer is
registered the layer is known and later updates skip the full scan.
A layer found in no buffer at all deliberately stays `unknown', so a
later application through a path that does not register buffers is
still picked up by the next update's full scan."
  (if (and where (bufferp where) (buffer-live-p where))
      (tp--render-visit-buffer where fn)
    (let ((registered (tp-reactive-layer-buffers layer-name)))
      (if (not (eq registered 'unknown))
          (dolist (buf registered)
            (tp--render-visit-buffer buf fn))
        ;; Learning fallback: behave exactly like the historical full
        ;; scan, but record which buffers actually carry the layer.
        (dolist (buf (buffer-list))
          (when (buffer-live-p buf)
            (when (tp--buffer-has-layer-region-p layer-name buf)
              (tp-reactive--register-layer-buffer layer-name buf))
            (tp--render-visit-buffer buf fn)))))))

(defun tp--merge-props-into-stack-entry (entry props)
  "Return stack-storage plist ENTRY with its keys updated from PROPS.
Every key of PROPS except `tp-name' and `tp-layers' replaces ENTRY's
value for that key (or extends ENTRY when the key is new), so ENTRY's
`tp-hidden' flag and identity survive the update.  Returns a fresh
plist; ENTRY itself is not modified."
  (let ((new-entry (copy-sequence entry)))
    (cl-loop for (key val) on props by #'cddr
             unless (memq key '(tp-name tp-layers))
             do (setq new-entry (plist-put new-entry key val)))
    new-entry))

(defun tp--write-layer-through-stack-storage (layer-name props)
  "Write PROPS through to LAYER-NAME's entries in `tp-layers' storage.
A reactive re-render rewrites a layer's direct (rendered) properties,
but the same layer can also sit inside the `tp-layers' stack-storage
property of a run: buried below another layer, or hidden (see
`tp-hide-layer'), in which case the direct properties are only a
render cache and the stored entry is what the next stack operation
rebuilds from.  For every run of the current buffer whose `tp-layers'
holds an entry whose `tp-name' equals LAYER-NAME, replace the layer's
own keys in that entry with their values from PROPS - preserving the
entry's `tp-hidden' flag and stack position - and rewrite the run via
`tp--stack-props-to-list' / `tp--stack-build-props', which also
refreshes the topmost-visible render cache in full-stack storage
mode.  Runs already storing the current values are left untouched, so
an update that changes nothing does not mark the buffer as modified."
  (let ((pos (point-min))
        (max (point-max)))
    (while (< pos max)
      (let ((next (or (next-property-change pos nil max) max))
            (stored (get-text-property pos 'tp-layers)))
        (when (and stored
                   (cl-some (lambda (entry)
                              (equal (plist-get entry 'tp-name) layer-name))
                            stored))
          (let* ((stack (tp--stack-props-to-list (text-properties-at pos)))
                 (new-stack
                  (mapcar (lambda (entry)
                            (if (equal (plist-get entry 'tp-name) layer-name)
                                (tp--merge-props-into-stack-entry entry props)
                              entry))
                          stack)))
            (unless (equal new-stack stack)
              (set-text-properties pos next
                                   (tp--stack-build-props new-stack)))))
        (setq pos next)))))

(defun tp--update-layer-regions (layer-name &optional where override-alist)
  "Update text regions that have LAYER-NAME applied.
Re-applies the layer's current properties to every region tagged with
the layer's `tp-name'.  The layer's OWN property keys are replaced
with their current values (so refresh is idempotent: a face variable
changing from bold to italic yields italic, not (italic bold)), while
properties contributed by other sources are left untouched.

The update also writes through to `tp-layers' stack storage (see
`tp--write-layer-through-stack-storage'): copies of the layer that
are hidden or buried below another layer are refreshed in place, so a
later stack operation or `tp-show-layer' renders current values
instead of a stale snapshot.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update the buffers registered for the layer in
    the reactive buffer registry, falling back to one full
    `buffer-list' scan when the registry has no knowledge of the
    layer (see `tp--map-layer-buffers').

OVERRIDE-ALIST maps reactive variables to their new values when the
watcher fires before the variables are set; layer props are
re-resolved against it in each target buffer, so buffer-local
variable values are honored."
  (let ((update-buffer
         (lambda ()
           (let ((props (tp--layer-render-props layer-name override-alist)))
             (when props
               (save-excursion
                 ;; Callback for tp-search-map: replaces the layer's own
                 ;; property keys on the matched region.  Returns nil to
                 ;; prevent tp-search-map from replacing the text.
                 (tp-search-map
                  (lambda (_text start end)
                    (cl-loop for (key val) on props by #'cddr
                             do (put-text-property start end key val))
                    nil)
                  'tp-name layer-name)
                 ;; Write through to stack storage so hidden or buried
                 ;; copies of the layer do not go stale (HID-1).
                 (tp--write-layer-through-stack-storage layer-name
                                                        props)))))))
    (tp--map-layer-buffers layer-name where update-buffer)))

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

(defun tp--tp-text-transform (layer-name text)
  "Return TEXT transformed by LAYER-NAME's `:transform', or TEXT.
Transform errors are reported and TEXT is returned unchanged; a
non-string transform result is ignored as well."
  (let ((transform-fn (when layer-name
                        (cdr (assoc layer-name tp-layer-transforms)))))
    (if (not transform-fn)
        text
      (condition-case err
          (let ((result (funcall transform-fn text)))
            (tp-debug-log "  Transform %s: %S -> %S" layer-name text result)
            (if (stringp result) result text))
        (error
         (message "tp: transform error for %s: %s" layer-name err)
         text)))))

(defun tp--merge-embedded-props (embedded props)
  "Merge the EMBEDDED string props plist under PROPS; PROPS win.
Like `tp--merge-string-props-into-plist' but takes the embedded plist
directly instead of sampling position 0 of a string, so callers can
merge per property interval.  Face-family values (see
`tp-face-properties') are merged with PROPS taking precedence; other
conflicting keys keep the PROPS value; keys only in EMBEDDED are
added."
  (let ((result (copy-sequence props)))
    (cl-loop for (key val) on embedded by #'cddr
             do (let ((existing (plist-get result key)))
                  (setq result
                        (plist-put result key
                                   (if existing
                                       (if (memq key tp-face-properties)
                                           (tp--merge-face-values val existing)
                                         existing)
                                     val)))))
    result))

(defun tp--put-text-property-unless-equal (start end key val object)
  "Apply KEY -> VAL over [START, END) of OBJECT unless already there.
Like `put-text-property', but when every position of the span already
holds a value `equal' to VAL for KEY the call is skipped, so an
update that changes nothing does not flip the buffer-modified flag.
OBJECT is a string, a buffer, or nil for the current buffer."
  (when (< start end)
    (unless (and (equal (get-text-property start key object) val)
                 (>= (or (next-single-property-change start key object end)
                         end)
                     end))
      (put-text-property start end key val object))))

(defun tp--apply-reactive-text-props (source props offset &optional target)
  "Apply PROPS merged with SOURCE's embedded props to TARGET at OFFSET.
SOURCE is the (possibly propertized) replacement string; TARGET is a
string, or nil for the current buffer.  For every embedded-property
interval of SOURCE the interval's props are merged under PROPS (see
`tp--merge-embedded-props') and the result is applied to the
corresponding span of TARGET shifted by OFFSET.  This keeps
per-interval styling of propertized reactive strings intact instead
of smearing position-0 props across the whole region.  Spans that
already carry an `equal' value are left untouched, so an update that
changes nothing does not mark the buffer as modified."
  (tp--map-intervals
   source nil nil
   (lambda (istart iend str-props)
     (let ((merged (if str-props
                       (tp--merge-embedded-props str-props props)
                     props)))
       (cl-loop for (key val) on merged by #'cddr
                do (tp--put-text-property-unless-equal
                    (+ offset istart) (+ offset iend) key val target))))))

(defun tp--update-reactive-text (layer-name &optional where override-alist)
  "Update text regions that have tp-text property with LAYER-NAME applied.
This is called when a reactive variable bound to tp-text changes.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update the buffers registered for the layer in
    the reactive buffer registry, falling back to one full
    `buffer-list' scan when the registry has no knowledge of the
    layer (see `tp--map-layer-buffers').

OVERRIDE-ALIST maps reactive variables to their new values when the
watcher fires before the variables are set; the layer's props are
re-resolved against it in each target buffer.

If a transform function is registered for LAYER-NAME via `:transform',
it will be applied to the text before updating."
  (let ((update-buffer
         (lambda ()
           (let ((props (tp--layer-render-props layer-name override-alist)))
             (when props
               (let* ((raw-text (plist-get props 'tp-text))
                      ;; Apply transformation if registered
                      (new-text (if (stringp raw-text)
                                    (tp--tp-text-transform layer-name raw-text)
                                  raw-text)))
                 (when (and new-text (stringp new-text))
                   ;; No save-excursion here: the replace function
                   ;; owns point restoration (its clamping semantics
                   ;; would be overridden by save-excursion's own
                   ;; drifting marker).
                   (tp--replace-reactive-text-in-buffer
                    layer-name new-text props))))))))
    (tp--map-layer-buffers layer-name where update-buffer)))

(defun tp--edit-region-minimal-diff (m-start m-end plain-text skip-props)
  "Make [M-START, M-END) of the current buffer read PLAIN-TEXT.
Only the differing span of the region is edited: the common prefix
and suffix of the old and new text are left untouched.  The
replacement is inserted BEFORE the old span is deleted, so markers
sitting in unchanged text keep tracking their characters - including
a marker at the first character of the preserved suffix, which the
old delete-then-insert order collapsed onto the edit start (TXT-1).
Markers whose characters were deleted end up at the end of the edit.
Does nothing when the region already reads PLAIN-TEXT, so an
identical-text update does not mark the buffer as modified.
Properties present at M-START whose keys the plist SKIP-PROPS does
not contain are re-applied over the edited span (a nil SKIP-PROPS
carries every existing property); the untouched prefix and suffix
keep their own properties as is.
Returns the cons (EDIT-START . EDIT-END) of the replaced span in
PRE-edit coordinates - the caller uses it to clamp a remembered
point that sat inside the edit - or nil when nothing was edited."
  (let ((old-text (buffer-substring-no-properties m-start m-end)))
    (unless (equal old-text plain-text)
      ;; Text content differs: trim the common prefix and suffix and
      ;; edit only the span that actually differs, so point and
      ;; markers in the unchanged parts survive the update.
      (let* ((old-len (length old-text))
             (new-len (length plain-text))
             (min-len (min old-len new-len))
             (prefix 0)
             (suffix 0))
        (while (and (< prefix min-len)
                    (eq (aref old-text prefix) (aref plain-text prefix)))
          (setq prefix (1+ prefix)))
        (while (and (< suffix (- min-len prefix))
                    (eq (aref old-text (- old-len suffix 1))
                        (aref plain-text (- new-len suffix 1))))
          (setq suffix (1+ suffix)))
        (let ((edit-start (+ m-start prefix))
              (edit-end (- m-end suffix))
              (insert-text (substring plain-text prefix (- new-len suffix)))
              (existing-props (text-properties-at m-start)))
          ;; Insert first, then delete the (shifted) old span: an
          ;; insertion-type-nil marker at the start of the preserved
          ;; suffix sits strictly after EDIT-START, so the insertion
          ;; shifts it right with its character, and the deletion of
          ;; the old span just before it shifts it back into place.
          (goto-char edit-start)
          (insert insert-text)
          (delete-region (point) (+ (point) (- edit-end edit-start)))
          ;; Carry over existing properties whose keys SKIP-PROPS does
          ;; not name onto the newly inserted span; the untouched
          ;; prefix and suffix keep their own properties as is.
          (let ((mid-end (+ edit-start (length insert-text))))
            (cl-loop for (key val) on existing-props by #'cddr
                     do (unless (plist-member skip-props key)
                          (put-text-property edit-start mid-end key
                                             val))))
          (cons edit-start edit-end))))))

(defun tp--pos-holds-layer-in-storage-only-p (pos layer-name)
  "Return non-nil when POS holds LAYER-NAME only inside `tp-layers'.
True when the `tp-layers' stack-storage property at POS has an entry
whose `tp-name' equals LAYER-NAME while the direct `tp-name' at POS
is a different layer or absent (a hidden layer in all-hidden storage,
or a layer buried below another rendered layer)."
  (and (not (equal (get-text-property pos 'tp-name) layer-name))
       (cl-some (lambda (entry)
                  (equal (plist-get entry 'tp-name) layer-name))
                (get-text-property pos 'tp-layers))
       t))

(defun tp--replace-reactive-text-in-buffer (layer-name new-text props)
  "Replace text in current buffer for reactive text with LAYER-NAME.
NEW-TEXT is the new text to replace with.
PROPS are the properties to apply to the new text.
Only the differing span of each region is edited: the common prefix
and suffix of the old and new text are left untouched, so point and
markers sitting in unchanged text keep their positions (point inside
the edited span ends up at the start of the edit).  An identical-text
update touches no buffer text at all and does not mark the buffer as
modified.
Text properties embedded in NEW-TEXT are merged with PROPS per
embedded interval, so a multi-interval propertized reactive string
keeps its per-character styling.  Existing text properties whose keys
are set neither by PROPS nor by NEW-TEXT's embedded props are
preserved, so one layer's text update does not erase other layers'
contributions on the same region.
Regions where the layer sits only inside `tp-layers' stack storage -
hidden (see `tp-hide-layer') or buried below another rendered layer -
are updated as well: text content is physical (hide/show toggles
properties, never text), so the model value still replaces the text
there, but the layer's props are not applied directly; instead its
stored stack entry, including the refreshed `tp-text', is written
through, so `tp-show-layer' or a reveal by a later stack operation
renders current values.
This function owns point restoration (callers must not wrap it in
`save-excursion', whose own marker would drift): point outside the
edits keeps tracking its character, and point inside an edited span
is clamped to the start of that edit."
  (let ((plain-text (substring-no-properties new-text))
        ;; Remember where the user's point was; the marker tracks all
        ;; edits, and edits that swallow point clamp it explicitly.
        (orig-point (copy-marker (point))))
    (unwind-protect
        (cl-flet ((edit-tracking-point (m-start m-end skip-props)
                    ;; Run the minimal-diff edit; when the remembered
                    ;; point sat inside the replaced span, clamp it to
                    ;; the start of the edit (the documented
                    ;; behavior).
                    (let* ((was (marker-position orig-point))
                           (span (tp--edit-region-minimal-diff
                                  m-start m-end plain-text skip-props)))
                      (when (and span
                                 (>= was (car span))
                                 (< was (cdr span)))
                        (set-marker orig-point (car span))))))
          (goto-char (point-min))
          ;; Pass 1: regions where the layer is the rendered top layer
          ;; (direct `tp-name').
          (let ((match (text-property-search-forward 'tp-name
                                                     layer-name t)))
            (while match
              (let* ((m-start (prop-match-beginning match))
                     (m-end (prop-match-end match)))
                (edit-tracking-point m-start m-end props)
                ;; Apply the layer's props, merged per embedded interval
                ;; of NEW-TEXT.  Keys are replaced (not accumulated);
                ;; unrelated keys are untouched.
                (tp--apply-reactive-text-props new-text props m-start)
                ;; Continue searching after the fully updated region: a
                ;; preserved suffix still carries the layer's `tp-name',
                ;; and restarting the search inside it would re-match
                ;; this region.
                (goto-char (+ m-start (length plain-text))))
              (setq match (text-property-search-forward 'tp-name
                                                        layer-name t))))
          ;; Pass 2: regions where the layer sits only inside stack
          ;; storage.  Replace their text too, carrying ALL existing
          ;; properties (the visible top layer's render cache and the
          ;; `tp-layers' storage) over the edited span; the
          ;; hidden/buried layer's own props are not applied directly.
          (let ((pos (point-min)))
            (while (< pos (point-max))
              (if (tp--pos-holds-layer-in-storage-only-p pos layer-name)
                  (let ((region-end pos))
                    (while (and (< region-end (point-max))
                                (tp--pos-holds-layer-in-storage-only-p
                                 region-end layer-name))
                      (setq region-end (or (next-property-change
                                            region-end)
                                           (point-max))))
                    (edit-tracking-point pos region-end nil)
                    (setq pos (+ pos (length plain-text))))
                (setq pos (or (next-property-change pos) (point-max))))))
          ;; Write the updated props - including the refreshed
          ;; `tp-text' - through to the layer's entries in stack
          ;; storage (HID-1).
          (tp--write-layer-through-stack-storage layer-name props))
      (goto-char orig-point)
      (set-marker orig-point nil))))

(defun tp--tp-text-replace (start end final-text result-props object preserve-props)
  "Replace [START, END) of OBJECT with FINAL-TEXT, handling props.
Implements the text replacement of `tp--handle-tp-text-property' and
returns its (PROPS NEW-END NEW-OBJECT) result.

For a string OBJECT a NEW string is built as prefix + FINAL-TEXT +
suffix, so text outside the region survives.  RESULT-PROPS (merged
per embedded interval of FINAL-TEXT) are applied to the replaced span
here, because callers can only apply props from index 0, which would
smear them over the preserved prefix; the returned NEW-END is 0 so
the caller's own application over [0, NEW-END) is a no-op.

For buffers the region text is replaced in place and the returned
NEW-END is the end of the inserted text; the caller applies
RESULT-PROPS itself.

When PRESERVE-PROPS is non-nil, properties present at START whose
keys RESULT-PROPS does not set are re-applied over the replacement."
  (if (stringp object)
      (let* ((plain (substring-no-properties final-text))
             ;; Splice: keep the string outside [start, end) intact.
             (new-string (concat (substring object 0 start)
                                 plain
                                 (substring object end)))
             (new-end (+ start (length plain)))
             (existing-props (when preserve-props
                               (text-properties-at start object))))
        ;; Preserve non-conflicting existing props of the replaced region
        (cl-loop for (key val) on existing-props by #'cddr
                 do (unless (plist-member result-props key)
                      (put-text-property start new-end key val new-string)))
        ;; Apply the merged props per embedded interval of FINAL-TEXT
        (tp--apply-reactive-text-props final-text result-props start new-string)
        (list result-props 0 new-string))
    ;; Buffer object
    (with-current-buffer (or object (current-buffer))
      (let ((old-text (buffer-substring-no-properties start end)))
        (if (equal old-text (substring-no-properties final-text))
            ;; Same text content, no replacement needed
            (list result-props end object)
          ;; Need to replace text
          (let ((existing-props (when preserve-props
                                  (text-properties-at start)))
                (inhibit-read-only t))
            (save-excursion
              (delete-region start end)
              (goto-char start)
              ;; Insert without properties - the caller applies RESULT-PROPS
              (insert (substring-no-properties final-text)))
            (let ((new-end (+ start (length final-text))))
              ;; Re-apply existing properties to new text region if preserving
              (cl-loop for (key val) on existing-props by #'cddr
                       do (unless (plist-member result-props key)
                            (put-text-property start new-end key val object)))
              (list result-props new-end object))))))))

(defun tp--handle-tp-text-property (start end props object &optional preserve-props merge-mode)
  "Handle tp-text property in PROPS for region from START to END in OBJECT.
If tp-text is nil, initialize it to the current text in the region;
when the layer has a `:transform', the displayed text is the
transformed value (matching later reactive updates) while the model -
the reactive variable and the `tp-text' property - keeps the raw text.
If tp-text is a string different from current text, replace the text.
When PRESERVE-PROPS is non-nil, existing text properties are preserved
on the replaced text (used by tp-set and tp-add).
MERGE-MODE is retained for backward compatibility but no longer
affects behavior.
All modes now preserve embedded text properties from tp-text, with props taking
precedence over embedded props when there's a conflict.
Returns (PROPS NEW-END NEW-OBJECT) where PROPS is the updated props,
NEW-END is the new end position after any text replacement, and
NEW-OBJECT is the new string object (only different for strings whose
text was replaced; see `tp--tp-text-replace' for the string-object
convention of a 0 NEW-END with pre-applied properties)."
  (ignore merge-mode)
  (if (not (plist-member props 'tp-text))
      ;; tp-text not in props - return unchanged
      (list props end object)
    (let ((tp-text-val (plist-get props 'tp-text))
          (layer-name (plist-get props 'tp-name)))
      (cond
       ;; tp-text is nil - initialize it to the current text
       ((null tp-text-val)
        (let ((current-text
               (if (stringp object)
                   (substring-no-properties object start end)
                 (with-current-buffer (or object (current-buffer))
                   (buffer-substring-no-properties start end)))))
          ;; If tp-text uses a reactive variable, update that variable to match
          ;; This ensures the reactive variable and buffer text stay in sync
          (when layer-name
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
          (setq props (plist-put props 'tp-text current-text))
          ;; Apply the layer's :transform to the DISPLAYED text on this first
          ;; render too, so the initial rendering matches later reactive
          ;; updates.  The model value stays the raw text.
          (let ((display-text (tp--tp-text-transform layer-name current-text)))
            (if (equal display-text current-text)
                (list props end object)
              (tp--tp-text-replace
               start end display-text
               (tp--merge-string-props-into-plist display-text props)
               object preserve-props)))))
       ;; tp-text has a string value - replace the text in the region
       ((stringp tp-text-val)
        ;; Apply transform if layer has one registered
        (let* ((final-text (tp--tp-text-transform layer-name tp-text-val))
               ;; Embedded text properties from tp-text are preserved in all
               ;; cases.  The props passed to this function take precedence
               ;; over embedded props when there's a conflict (e.g. both have
               ;; a `face' property).
               (result-props
                (tp--merge-string-props-into-plist final-text props)))
          (tp--tp-text-replace start end final-text result-props
                               object preserve-props)))
       ;; Other types - return unchanged
       (t (list props end object))))))

(defun tp--reactive-apply-update (layer-name reactive-props symbol newval
                                             where override-alist)
  "Recompute LAYER-NAME's definition and re-render affected regions.
REACTIVE-PROPS are the layer's props that reference the changed
variable SYMBOL; NEWVAL is its new value.  WHERE is the buffer for
`setq-local' changes, nil for global ones.  OVERRIDE-ALIST maps SYMBOL
to NEWVAL (the watcher runs before the variable is actually set).

Buffer-local changes (WHERE a buffer) re-render only that buffer,
resolving the layer's props against the buffer-local values, and do
NOT touch the global layer definition, so `setq-local' cannot leak a
buffer's value into other buffers.

When `tp--batch-update-active' is non-nil the buffer update is queued
in `tp--batch-update-pending' instead of applied immediately.  When
this function is re-entered from a nested variable write issued
inside an update (a computed variable being set, or the tp-text
two-way sync), the nested re-render is queued the same way and
flushed once the outermost update completes, instead of recursing.

This is the engine behind `tp--reactive-variable-watcher'; it is
installed as `tp--reactive-update-function'."
  (ignore newval)
  (let ((tp-text-affected (and (plist-member reactive-props 'tp-text) t)))
    (if tp--reactive-updating
        ;; Nested change fired from within an update: queue, don't recurse.
        (tp--queue-batch-update layer-name symbol where tp-text-affected)
      (unwind-protect
          (let ((tp--reactive-updating t))
            ;; Update computed properties for this layer
            (let ((updated-override
                   (tp--update-layer-computed layer-name override-alist)))
              ;; Update only the reactive properties in the layer definition.
              ;; Buffer-local changes must not leak into the global definition;
              ;; the buffer re-render below resolves against the buffer-local
              ;; values instead.
              (when (and reactive-props (not (bufferp where)))
                (let ((resolved-props (tp--resolve-reactive-symbols
                                       reactive-props updated-override))
                      (current-props (cdr (assoc layer-name tp-layer-alist))))
                  (when current-props
                    ;; Deep merge the resolved reactive props into the current
                    ;; layer props to preserve nested plist values (like face)
                    (tp--set-layer-props
                     layer-name
                     (tp--deep-merge-plist current-props resolved-props)))))
              ;; Update text regions with this layer (or defer if batching)
              (if tp--batch-update-active
                  ;; Batching: defer the buffer update
                  (progn
                    (tp-debug-log "  Deferring buffer update for %s (batch mode)"
                                  layer-name)
                    (tp--queue-batch-update layer-name symbol where
                                            tp-text-affected))
                ;; Normal: update immediately
                (tp-debug-log "  Updating layer %s (tp-text affected: %s)"
                              layer-name (if tp-text-affected "yes" "no"))
                (if tp-text-affected
                    (tp--update-reactive-text layer-name where updated-override)
                  (tp--update-layer-regions layer-name where updated-override)))))
        ;; Re-renders queued by nested variable writes during this update
        ;; are flushed now that the outermost update has finished.  The
        ;; flush runs under unwind-protect so an error escaping the
        ;; re-render (for example from a modification hook) cannot strand
        ;; queued entries in the global queue (ARCH-4); the reentrancy
        ;; guard has been unbound by now, so the flush re-renders
        ;; normally.
        (unless tp--batch-update-active
          (when tp--batch-update-pending
            (tp--flush-batch-updates)))))))

(defun tp--reactive-flush-entry (layer-name where tp-text-affected)
  "Re-render LAYER-NAME's regions in WHERE (or all buffers when nil).
TP-TEXT-AFFECTED non-nil means the layer's `tp-text' changed and the
text itself must be replaced.  Runs after the changed variables have
actually been set, so layer props re-resolve against current
\(buffer-local aware) values.  Installed as
`tp--reactive-flush-function'."
  (if tp-text-affected
      (tp--update-reactive-text layer-name where)
    (tp--update-layer-regions layer-name where)))

;; Install the engine into the lower modules.
(setq tp--reactive-update-function #'tp--reactive-apply-update)
(setq tp--reactive-flush-function #'tp--reactive-flush-entry)
(setq tp--tp-text-handler-function #'tp--handle-tp-text-property)
(setq tp--layer-refresh-function #'tp--update-layer-regions)

(provide 'tp-render)
;;; tp-render.el ends here
