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

(defun tp--update-layer-computed (layer-name override-alist)
  "Update computed reactive variables for LAYER-NAME with OVERRIDE-ALIST.
Evaluates compute functions and updates the reactive variable values.
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
                 nil))))
        (when computed-val
          ;; Update the global variable
          (set var-sym computed-val)
          ;; Add to override-alist for property resolution
          (push (cons var-sym computed-val) override-alist)
          ;; Also update the layer properties if the computed var is used in props
          (let ((current-props (cdr (assoc layer-name tp-layer-alist))))
            (when current-props
              ;; Collect all reactive props for this layer from tp-reactive-deps
              (let ((all-reactive-props nil))
                (dolist (dep tp-reactive-deps)
                  (let ((layer-entry (assoc layer-name (cdr dep))))
                    (when (and layer-entry (cdr layer-entry))
                      ;; Merge the reactive props
                      (cl-loop for (key val) on (cdr layer-entry) by #'cddr
                               do (setq all-reactive-props
                                        (plist-put all-reactive-props key val))))))
                (when all-reactive-props
                  (let ((resolved-props (tp--resolve-reactive-symbols
                                         all-reactive-props override-alist)))
                    (when resolved-props
                      (cl-loop for (key val) on resolved-props by #'cddr
                               do (setq current-props (plist-put current-props key val)))
                      (tp--set-layer-props layer-name current-props)))))))))))
  override-alist)

(defun tp--update-layer-regions (layer-name &optional where)
  "Update text regions that have LAYER-NAME applied.
Re-applies the layer properties using tp-search-map and tp-add.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update all buffers that have the text property (setq case)."
  (let ((props (tp-layer-props layer-name t)))  ; include tp-name for reactive tracking
    (when props
      ;; Callback for tp-search-map: applies props to matched region.
      ;; _TEXT is unused (the matched text), START and END are buffer positions.
      ;; Returns nil to prevent tp-search-map from replacing the text.
      (let ((apply-props-fn (lambda (_text start end)
                              (tp-add start end props)
                              nil)))
        (if (and where (bufferp where) (buffer-live-p where))
            ;; setq-local case: only update the specific buffer
            (tp-with-current-buffer where
              (save-excursion
                (tp-search-map apply-props-fn 'tp-name layer-name)))
          ;; setq case: update all buffers that have the text property
          (dolist (buf (buffer-list))
            (when (buffer-live-p buf)
              (tp-with-current-buffer buf
                (save-excursion
                  (tp-search-map apply-props-fn 'tp-name layer-name))))))))))

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

(defun tp--update-reactive-text (layer-name &optional where)
  "Update text regions that have tp-text property with LAYER-NAME applied.
This is called when a reactive variable bound to tp-text changes.

WHERE specifies which buffers to update:
  - If WHERE is a buffer, only update that buffer (setq-local case).
  - If WHERE is nil, update all buffers that have the text property (setq case).

If a transform function is registered for LAYER-NAME via `:transform',
it will be applied to the text before updating."
  (let ((props (tp-layer-props layer-name t)))  ; include tp-name for reactive tracking
    (when props
      (let* ((raw-text (plist-get props 'tp-text))
             ;; Apply transformation if registered
             (transform-fn (cdr (assoc layer-name tp-layer-transforms)))
             (new-text (if (and transform-fn raw-text (stringp raw-text))
                           (condition-case err
                               (let ((result (funcall transform-fn raw-text)))
                                 (tp-debug-log "  Transform %s: %S -> %S"
                                               layer-name raw-text result)
                                 result)
                             (error
                              (message "tp: transform error for %s: %s"
                                       layer-name err)
                              raw-text))
                         raw-text)))
        (when (and new-text (stringp new-text))
          (if (and where (bufferp where) (buffer-live-p where))
              ;; setq-local case: only update the specific buffer
              (tp-with-current-buffer where
                (save-excursion
                  (tp--replace-reactive-text-in-buffer layer-name new-text props)))
            ;; setq case: update all buffers that have the text property
            (dolist (buf (buffer-list))
              (when (buffer-live-p buf)
                (tp-with-current-buffer buf
                  (save-excursion
                    (tp--replace-reactive-text-in-buffer layer-name new-text props)))))))))))

(defun tp--replace-reactive-text-in-buffer (layer-name new-text props)
  "Replace text in current buffer for reactive text with LAYER-NAME.
NEW-TEXT is the new text to replace with.
PROPS are the properties to apply to the new text.
Text properties embedded in NEW-TEXT are merged with PROPS.
The new properties completely reset/replace the old properties."
  (goto-char (point-min))
  (let ((match (text-property-search-forward 'tp-name layer-name t))
        ;; Merge embedded text properties from new-text into props
        (merged-props (tp--merge-string-props-into-plist new-text props)))
    (while match
      (let* ((m-start (prop-match-beginning match))
             (m-end (prop-match-end match))
             (old-text (buffer-substring-no-properties m-start m-end)))
        (if (equal old-text (substring-no-properties new-text))
            ;; Text content is the same, but properties may differ
            ;; Use set-text-properties to reset with new properties
            (set-text-properties m-start m-end merged-props)
          ;; Text content is different - delete old text and insert new
          (delete-region m-start m-end)
          (goto-char m-start)
          (insert (substring-no-properties new-text))
          ;; Apply new properties
          (let ((new-end (+ m-start (length new-text))))
            (set-text-properties m-start new-end merged-props))))
      ;; Search for next match
      (setq match (text-property-search-forward 'tp-name layer-name t)))))

(defun tp--handle-tp-text-property (start end props object &optional preserve-props merge-mode)
  "Handle tp-text property in PROPS for region from START to END in OBJECT.
If tp-text is nil, initialize it to the current text in the region.
If tp-text is a string different from current text, replace the text.
When PRESERVE-PROPS is non-nil, existing text properties are preserved
on the replaced text (used by tp-set and tp-add).
MERGE-MODE is retained for backward compatibility but no longer affects behavior.
All modes now preserve embedded text properties from tp-text, with props taking
precedence over embedded props when there's a conflict.
Returns (PROPS NEW-END NEW-OBJECT) where PROPS is the updated props,
NEW-END is the new end position after any text replacement, and
NEW-OBJECT is the new string object (only different for strings with tp-text)."
  (if (not (plist-member props 'tp-text))
      ;; tp-text not in props - return unchanged
      (list props end object)
    (let ((tp-text-val (plist-get props 'tp-text)))
      (cond
       ;; tp-text is nil - initialize it to the current text
       ((null tp-text-val)
        (let ((current-text
               (if (stringp object)
                   (substring object start end)
                 (if object
                     (with-current-buffer object
                       (buffer-substring-no-properties start end))
                   (buffer-substring-no-properties start end)))))
          ;; If tp-text uses a reactive variable, update that variable to match
          ;; This ensures the reactive variable and buffer text stay in sync
          (when-let ((layer-name (plist-get props 'tp-name)))
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
          (list (plist-put props 'tp-text current-text) end object)))
       ;; tp-text has a string value - replace the text in the region
       ((stringp tp-text-val)
        ;; Apply transform if layer has one registered
        (let* ((layer-name (plist-get props 'tp-name))
               (transform-fn (when layer-name
                               (cdr (assoc layer-name tp-layer-transforms))))
               (final-text
                (if transform-fn
                    (condition-case err
                        (funcall transform-fn tp-text-val)
                      (error
                       (message "tp: transform error for %s: %s" layer-name err)
                       tp-text-val))
                  tp-text-val))
               ;; Embedded text properties from tp-text are now preserved in all cases.
               ;; The props passed to this function take precedence over embedded props
               ;; when there's a conflict (e.g., both have 'face' property).
               ;; The merge-mode parameter is retained for backward compatibility but
               ;; no longer affects behavior in this function - all modes use the same
               ;; merging strategy via tp--merge-string-props-into-plist.
               (result-props
                (tp--merge-string-props-into-plist final-text props)))
          (if (stringp object)
              ;; For strings: create a new string with tp-text content
              ;; Strip properties - result-props will be applied by the caller
              (let ((new-string (substring-no-properties final-text)))
                (list result-props (length new-string) new-string))
            ;; For buffers: replace text and adjust end position
            (let ((old-text (if object
                                (with-current-buffer object
                                  (buffer-substring-no-properties start end))
                              (buffer-substring-no-properties start end))))
              (if (equal old-text (substring-no-properties final-text))
                  ;; Same text content, no replacement needed
                  (list result-props end object)
                ;; Need to replace text
                (let ((existing-props (when preserve-props
                                        (if object
                                            (with-current-buffer object
                                              (text-properties-at start))
                                          (text-properties-at start)))))
                  (save-excursion
                    (if object
                        (with-current-buffer object
                          (let ((inhibit-read-only t))
                            (delete-region start end)
                            (goto-char start)
                            ;; Insert without properties - we'll apply result-props later
                            (insert (substring-no-properties final-text))))
                      (let ((inhibit-read-only t))
                        (delete-region start end)
                        (goto-char start)
                        (insert (substring-no-properties final-text)))))
                  (let ((new-end (+ start (length final-text))))
                    ;; Re-apply existing properties to new text region if preserving
                    (when existing-props
                      (cl-loop for (key val) on existing-props by #'cddr
                               do (unless (plist-member result-props key)
                                    (put-text-property
                                     start new-end key val object))))
                    (list result-props new-end object))))))))
       ;; Other types - return unchanged
       (t (list props end object))))))

(defun tp--reactive-apply-update (layer-name reactive-props symbol newval
                                             where override-alist)
  "Recompute LAYER-NAME's definition and re-render affected regions.
REACTIVE-PROPS are the layer's props that reference the changed
variable SYMBOL; NEWVAL is its new value.  WHERE is the buffer for
`setq-local' changes, nil for global ones.  OVERRIDE-ALIST maps SYMBOL
to NEWVAL (the watcher runs before the variable is actually set).

When `tp--batch-update-active' is non-nil the buffer update is queued
in `tp--batch-update-pending' instead of applied immediately.

This is the engine behind `tp--reactive-variable-watcher'; it is
installed as `tp--reactive-update-function'."
  (let ((tp-text-affected (plist-member reactive-props 'tp-text)))
    ;; Update computed properties for this layer
    (let ((updated-override
           (tp--update-layer-computed layer-name override-alist)))
      (when reactive-props
        ;; Resolve the reactive props with the new value override
        (let ((resolved-props (tp--resolve-reactive-symbols
                               reactive-props updated-override)))
          ;; Update only the reactive properties in the layer definition
          (let ((current-props (cdr (assoc layer-name tp-layer-alist))))
            (when current-props
              ;; Deep merge the resolved reactive props into the current
              ;; layer props to preserve nested plist values (like face)
              (setq current-props (tp--deep-merge-plist current-props
                                                        resolved-props))
              (tp--set-layer-props layer-name current-props))))))
    ;; Update text regions with this layer (or defer if batching)
    (if tp--batch-update-active
        ;; Batching: defer the buffer update
        ;; Pending format: (layer-name symbols-list where tp-text-affected)
        (let ((existing (assoc layer-name tp--batch-update-pending)))
          (tp-debug-log "  Deferring buffer update for %s (batch mode)"
                        layer-name)
          (if existing
              ;; Update existing entry: add symbol if not present
              (let ((symbols (nth 1 existing)))
                (unless (memq symbol symbols)
                  (setf (nth 1 existing) (cons symbol symbols))))
            ;; Create new entry
            (push (list layer-name (list symbol) where tp-text-affected)
                  tp--batch-update-pending)))
      ;; Normal: update immediately
      (tp-debug-log "  Updating layer %s (tp-text affected: %s)"
                    layer-name (if tp-text-affected "yes" "no"))
      (tp--reactive-flush-entry layer-name where tp-text-affected))))

(defun tp--reactive-flush-entry (layer-name where tp-text-affected)
  "Re-render LAYER-NAME's regions in WHERE (or all buffers when nil).
TP-TEXT-AFFECTED non-nil means the layer's `tp-text' changed and the
text itself must be replaced.  Installed as
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
