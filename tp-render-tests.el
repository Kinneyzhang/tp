;;; tp-render-tests.el --- ERT regression tests for tp-render.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for confirmed bugs fixed in the reactive-render
;; module (tp-render.el, with supporting fixes in tp-reactive.el).
;; Each section is tagged with the canonical bug id it guards against.

;;; Code:

(require 'ert)
(require 'tp)

;; Reactive test variables must be dynamically bound so watcher and
;; compute machinery can see them through `symbol-value'.
(defvar tp-rt-b9-var nil)
(defvar tp-rt-b10-data nil)
(defvar tp-rt-b10-full nil)
(defvar tp-rt-b11-face nil)
(defvar tp-rt-b11b-color nil)
(defvar tp-rt-b12-color nil)
(defvar tp-rt-b13-text nil)
(defvar tp-rt-b13b-text nil)
(defvar tp-rt-b14-flag nil)
(defvar tp-rt-b14-inv nil)
(defvar tp-rt-b14b-init nil)
(defvar tp-rt-b16-data nil)
(defvar tp-rt-b16-comp nil)
(defvar tp-rt-b16-count 0)
(defvar tp-rt-b17-color nil)
(defvar tp-rt-b17-text nil)
(defvar tp-rt-b17b-color nil)
(defvar tp-rt-b17b-echo nil)
(defvar tp-rt-b18-text nil)
(defvar tp-rt-b19-amount nil)
(defvar tp-rt-b19s-amount nil)
(defvar tp-rt-r1-color nil)
(defvar tp-rt-r1b-color nil)
(defvar tp-rt-r1c-color nil)
(defvar tp-rt-r1d-color nil)
(defvar tp-rt-r2-text nil)
(defvar tp-rt-r2m-text nil)
(defvar tp-rt-r2n-text nil)
(defvar tp-rt-r2s-text nil)
(defvar tp-rt-r3a-color nil)
(defvar tp-rt-r3b-color nil)
(defvar tp-rt-r3c-color nil)

(defmacro tp-rt-with-cleanup (layers vars &rest body)
  "Run BODY, then undefine LAYERS and reset VARS to nil (teardown)."
  (declare (indent 2))
  `(unwind-protect
       (progn ,@body)
     ,@(mapcar (lambda (l) `(tp-undefine-layer ',l)) layers)
     ,@(mapcar (lambda (v) `(setq ,v nil)) vars)))

;;; B9: sub-region tp-text on a string must splice, not replace the whole string

(ert-deftest tp-render-test-tp-text-string-region-keeps-rest ()
  "Region-form tp-text on a string keeps the text outside the region."
  (let ((result (tp-set 0 1 '(tp-text "X") (copy-sequence "abc"))))
    (should (equal result "Xbc"))
    (should (equal (get-text-property 0 'tp-text result) "X"))
    ;; The preserved suffix must not receive the layer's props
    (should (null (get-text-property 1 'tp-text result)))
    (should (null (get-text-property 2 'tp-text result)))))

(ert-deftest tp-render-test-tp-text-string-mid-region-splices ()
  "A mid-string tp-text region splices prefix + replacement + suffix."
  (let ((result (tp-set 1 2 '(face bold tp-text "XY") (copy-sequence "abc"))))
    (should (equal result "aXYc"))
    ;; Props only on the replaced span [1, 3)
    (should (null (get-text-property 0 'face result)))
    (should (eq (get-text-property 1 'face result) 'bold))
    (should (eq (get-text-property 2 'face result) 'bold))
    (should (null (get-text-property 3 'face result)))))

(ert-deftest tp-render-test-tp-text-string-region-preserves-outside-props ()
  "Splicing keeps the original string's properties outside the region."
  (let* ((source (propertize "abc" 'face 'italic 'my-prop 1))
         (result (tp-set 1 2 '(tp-text "X") source)))
    (should (equal result "aXc"))
    ;; Prefix and suffix keep their original props
    (should (eq (get-text-property 0 'face result) 'italic))
    (should (eq (get-text-property 2 'face result) 'italic))
    ;; Replaced span preserves non-conflicting props (tp-set preserves)
    (should (eq (get-text-property 1 'my-prop result) 1))))

(ert-deftest tp-render-test-tp-text-whole-string-still-replaces ()
  "Whole-string form still returns just the replacement (legacy semantics)."
  (let ((result (tp-set "2" 'face '(:background "green") 'tp-text "6")))
    (should (equal result "6"))
    (should (equal (get-text-property 0 'face result) '(:background "green")))
    (should (equal (get-text-property 0 'tp-text result) "6"))))

;;; B10: computed-variable path must not clobber sibling static attributes

(ert-deftest tp-render-test-computed-update-keeps-static-siblings ()
  "A computed update deep-merges, keeping static nested attributes."
  (tp-rt-with-cleanup (tp-rt-b10-layer) (tp-rt-b10-data tp-rt-b10-full)
    (setq tp-rt-b10-data "red")
    (define-tp tp-rt-b10-layer ()
      :props '(face (:foreground $tp-rt-b10-full :background "green"))
      :data '(tp-rt-b10-data)
      :compute '((tp-rt-b10-full (lambda () (concat "col-" tp-rt-b10-data)))))
    (setq tp-rt-b10-data "blue")
    (let ((face (plist-get (cdr (assoc 'tp-rt-b10-layer tp-layer-alist)) 'face)))
      (should (equal (plist-get face :foreground) "col-blue"))
      ;; The sibling static attribute must survive the update
      (should (equal (plist-get face :background) "green")))))

;;; B11: reactive refresh replaces the layer's own keys instead of accumulating

(ert-deftest tp-render-test-reactive-refresh-replaces-face ()
  "Changing a symbol-valued face variable replaces the face, not stacks it."
  (tp-rt-with-cleanup (tp-rt-b11-layer) (tp-rt-b11-face)
    (setq tp-rt-b11-face 'bold)
    (define-tp tp-rt-b11-layer () '(face $tp-rt-b11-face))
    (with-temp-buffer
      (insert "Hello")
      (tp-set 1 6 'tp-rt-b11-layer)
      (should (eq (get-text-property 1 'face) 'bold))
      (setq tp-rt-b11-face 'italic)
      ;; Must be italic alone, not (italic bold)
      (should (eq (get-text-property 1 'face) 'italic)))))

(ert-deftest tp-render-test-reactive-refresh-keeps-unrelated-props ()
  "Reactive refresh leaves property keys the layer does not own alone."
  (tp-rt-with-cleanup (tp-rt-b11b-layer) (tp-rt-b11b-color)
    (setq tp-rt-b11b-color "red")
    (define-tp tp-rt-b11b-layer () '(face (:foreground $tp-rt-b11b-color)))
    (with-temp-buffer
      (insert "Hello")
      (tp-set 1 6 'tp-rt-b11b-layer)
      (put-text-property 1 6 'help-echo "keep me")
      (setq tp-rt-b11b-color "green")
      (should (equal (plist-get (get-text-property 1 'face) :foreground) "green"))
      (should (equal (get-text-property 1 'help-echo) "keep me")))))

;;; B12: setq-local must not leak into the global layer definition

(ert-deftest tp-render-test-setq-local-does-not-touch-global-def ()
  "A buffer-local change re-renders the buffer but keeps the global def."
  (tp-rt-with-cleanup (tp-rt-b12-layer) ()
    (setq-default tp-rt-b12-color "red")
    (define-tp tp-rt-b12-layer () '(face (:foreground $tp-rt-b12-color)))
    (let ((buf-a (generate-new-buffer " tp-rt-b12-a"))
          (buf-b (generate-new-buffer " tp-rt-b12-b")))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (insert "Hello")
              (tp-set 1 6 'tp-rt-b12-layer))
            (with-current-buffer buf-b
              (insert "Hello")
              (tp-set 1 6 'tp-rt-b12-layer))
            (with-current-buffer buf-a
              (setq-local tp-rt-b12-color "purple"))
            ;; Buffer A is re-rendered with its local value
            (with-current-buffer buf-a
              (should (equal (plist-get (get-text-property 1 'face) :foreground)
                             "purple")))
            ;; The GLOBAL definition must not absorb the local value
            (should (equal (plist-get
                            (plist-get (cdr (assoc 'tp-rt-b12-layer tp-layer-alist))
                                       'face)
                            :foreground)
                           "red"))
            (should (equal (default-value 'tp-rt-b12-color) "red"))
            ;; Other buffers keep rendering the global value
            (with-current-buffer buf-b
              (should (equal (plist-get (get-text-property 1 'face) :foreground)
                             "red"))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)
        (setq-default tp-rt-b12-color nil)))))

;;; B13: reactive tp-text replacement preserves unrelated properties

(ert-deftest tp-render-test-reactive-text-update-preserves-other-props ()
  "Replacing reactive text keeps properties other layers put on the region."
  (tp-rt-with-cleanup (tp-rt-b13-layer) (tp-rt-b13-text)
    (setq tp-rt-b13-text "aaa")
    (define-tp tp-rt-b13-layer () '(face bold tp-text $tp-rt-b13-text))
    (with-temp-buffer
      (insert "Hello")
      (tp-set 1 6 'tp-rt-b13-layer)
      (put-text-property 1 3 'my-other-prop 42)
      (setq tp-rt-b13-text "bbb")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "bbb"))
      ;; The unrelated property survives the text replacement
      (should (eq (get-text-property 1 'my-other-prop) 42))
      ;; The layer's own props are still applied
      (should (eq (get-text-property 1 'face) 'bold)))))

(ert-deftest tp-render-test-reactive-text-same-text-preserves-other-props ()
  "A same-text properties-only update keeps unrelated properties too."
  (tp-rt-with-cleanup (tp-rt-b13b-layer) (tp-rt-b13b-text)
    (setq tp-rt-b13b-text "emacs")
    (define-tp tp-rt-b13b-layer () '(tp-text $tp-rt-b13b-text))
    (with-temp-buffer
      (insert "emacs")
      (tp-set 1 6 'tp-rt-b13b-layer)
      (put-text-property 1 6 'my-other-prop 'yes)
      ;; Same text, new embedded properties
      (setq tp-rt-b13b-text (propertize "emacs" 'face 'bold))
      (should (eq (get-text-property 1 'face) 'bold))
      (should (eq (get-text-property 1 'my-other-prop) 'yes)))))

;;; B14: computed values of nil must propagate

(ert-deftest tp-render-test-computed-nil-propagates-on-update ()
  "A compute function returning nil updates the variable and the layer."
  (tp-rt-with-cleanup (tp-rt-b14-layer) (tp-rt-b14-flag tp-rt-b14-inv)
    (setq tp-rt-b14-flag t)
    (define-tp tp-rt-b14-layer ()
      :props '(invisible $tp-rt-b14-inv)
      :data '(tp-rt-b14-flag)
      :compute '((tp-rt-b14-inv (lambda () tp-rt-b14-flag))))
    (should (eq tp-rt-b14-inv t))
    (setq tp-rt-b14-flag nil)
    ;; nil is a legitimate computed value, not an error sentinel
    (should (eq tp-rt-b14-inv nil))
    (should (eq (plist-get (cdr (assoc 'tp-rt-b14-layer tp-layer-alist))
                           'invisible)
                nil))))

(ert-deftest tp-render-test-computed-nil-applies-initially ()
  "An initial computed value of nil overwrites a stale non-nil value."
  (tp-rt-with-cleanup (tp-rt-b14b-layer) (tp-rt-b14b-init)
    (setq tp-rt-b14b-init 'stale)
    (define-tp tp-rt-b14b-layer ()
      :props '(invisible $tp-rt-b14b-init)
      :compute '((tp-rt-b14b-init (lambda () nil))))
    (should (eq tp-rt-b14b-init nil))))

;;; B16: no watcher recursion from nested variable writes

(ert-deftest tp-render-test-compute-runs-once-per-change ()
  "One data change runs each compute function exactly once (no recursion)."
  (tp-rt-with-cleanup (tp-rt-b16-layer) (tp-rt-b16-data tp-rt-b16-comp)
    (setq tp-rt-b16-data "a" tp-rt-b16-count 0)
    (define-tp tp-rt-b16-layer ()
      :props '(help-echo $tp-rt-b16-comp)
      :data '(tp-rt-b16-data)
      :compute '((tp-rt-b16-comp
                  (lambda ()
                    (setq tp-rt-b16-count (1+ tp-rt-b16-count))
                    (concat tp-rt-b16-data "!")))))
    (with-temp-buffer
      (insert "Hello")
      (tp-set 1 6 'tp-rt-b16-layer)
      (setq tp-rt-b16-count 0)
      (setq tp-rt-b16-data "b")
      ;; The nested (set comp ...) must queue its re-render, not re-enter
      ;; the compute machinery.
      (should (= tp-rt-b16-count 1))
      ;; The nested change's re-render still lands in the buffer
      (should (equal tp-rt-b16-comp "b!"))
      (should (equal (get-text-property 1 'help-echo) "b!")))))

;;; B17: batched entries must union WHERE and the tp-text-affected flag

(ert-deftest tp-render-test-batch-tp-text-flag-is-sticky ()
  "A tp-text change deferred after a non-tp-text change still replaces text."
  (tp-rt-with-cleanup (tp-rt-b17-layer) (tp-rt-b17-color tp-rt-b17-text)
    (setq tp-rt-b17-color "red" tp-rt-b17-text "one")
    (define-tp tp-rt-b17-layer ()
      '(face (:foreground $tp-rt-b17-color) tp-text $tp-rt-b17-text))
    (with-temp-buffer
      (insert "one")
      (tp-set 1 4 'tp-rt-b17-layer)
      (tp-with-batch-updates
        (setq tp-rt-b17-color "blue")   ; first change: no tp-text
        (setq tp-rt-b17-text "two"))    ; second change: tp-text affected
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "two"))
      (should (equal (plist-get (get-text-property 1 'face) :foreground)
                     "blue")))))

(ert-deftest tp-render-test-batch-where-widens-to-all-buffers ()
  "A global change after a buffer-local one must reach other buffers."
  (tp-rt-with-cleanup (tp-rt-b17b-layer) ()
    (setq-default tp-rt-b17b-color "red")
    (setq-default tp-rt-b17b-echo "old")
    (define-tp tp-rt-b17b-layer ()
      '(face (:foreground $tp-rt-b17b-color) help-echo $tp-rt-b17b-echo))
    (let ((buf-a (generate-new-buffer " tp-rt-b17b-a"))
          (buf-b (generate-new-buffer " tp-rt-b17b-b")))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (insert "Hello") (tp-set 1 6 'tp-rt-b17b-layer))
            (with-current-buffer buf-b
              (insert "Hello") (tp-set 1 6 'tp-rt-b17b-layer))
            (with-current-buffer buf-a
              (tp-with-batch-updates
                (setq-local tp-rt-b17b-color "blue") ; WHERE = buf-a
                (setq tp-rt-b17b-echo "new")))       ; WHERE = global
            ;; The global change must not be trapped in buf-a's WHERE
            (with-current-buffer buf-b
              (should (equal (get-text-property 1 'help-echo) "new"))
              (should (equal (plist-get (get-text-property 1 'face) :foreground)
                             "red")))
            ;; buf-a gets both, with its local color honored
            (with-current-buffer buf-a
              (should (equal (get-text-property 1 'help-echo) "new"))
              (should (equal (plist-get (get-text-property 1 'face) :foreground)
                             "blue"))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)
        (setq-default tp-rt-b17b-color nil)
        (setq-default tp-rt-b17b-echo nil)))))

;;; B18: multi-interval reactive strings keep per-interval styling

(ert-deftest tp-render-test-reactive-text-keeps-per-interval-props ()
  "A propertized reactive string renders each interval's own props."
  (tp-rt-with-cleanup (tp-rt-b18-layer) (tp-rt-b18-text)
    (setq tp-rt-b18-text "init")
    (define-tp tp-rt-b18-layer () '(tp-text $tp-rt-b18-text))
    (with-temp-buffer
      (insert "init")
      (tp-set 1 5 'tp-rt-b18-layer)
      (setq tp-rt-b18-text (concat (propertize "AB" 'face 'bold)
                                   (propertize "CD" 'face 'italic)))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "ABCD"))
      ;; Position-0 props must not smear over the whole region
      (should (eq (get-text-property 1 'face) 'bold))
      (should (eq (get-text-property 2 'face) 'bold))
      (should (eq (get-text-property 3 'face) 'italic))
      (should (eq (get-text-property 4 'face) 'italic)))))

;;; B19: :transform applies on the initial nil-tp-text render too

(ert-deftest tp-render-test-transform-applies-on-initial-render ()
  "First render of a nil tp-text layer shows the transformed text."
  (tp-rt-with-cleanup (tp-rt-b19-layer) (tp-rt-b19-amount)
    (setq tp-rt-b19-amount nil)
    (define-tp tp-rt-b19-layer ()
      :props '(face bold tp-text $tp-rt-b19-amount)
      :transform (lambda (s) (concat "$" s)))
    (with-temp-buffer
      (insert "5.00")
      (tp-set 1 5 'tp-rt-b19-layer)
      ;; Initial rendering must match later reactive renderings
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "$5.00"))
      ;; The model (variable and tp-text prop) keeps the raw value
      (should (equal tp-rt-b19-amount "5.00"))
      (should (equal (get-text-property 1 'tp-text) "5.00"))
      (should (eq (get-text-property 1 'face) 'bold))
      ;; And a later update stays consistent
      (setq tp-rt-b19-amount "6.00")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "$6.00")))))

(ert-deftest tp-render-test-transform-applies-on-initial-string-render ()
  "String form of a nil tp-text layer also shows the transformed text."
  (tp-rt-with-cleanup (tp-rt-b19s-layer) (tp-rt-b19s-amount)
    (setq tp-rt-b19s-amount nil)
    (define-tp tp-rt-b19s-layer ()
      :props '(face bold tp-text $tp-rt-b19s-amount)
      :transform (lambda (s) (concat "$" s)))
    (let ((result (tp-set "5.00" 'tp-rt-b19s-layer)))
      (should (equal result "$5.00"))
      ;; Model keeps the raw value
      (should (equal tp-rt-b19s-amount "5.00"))
      (should (equal (get-text-property 0 'tp-text result) "5.00"))
      (should (eq (get-text-property 0 'face result) 'bold)))))

;;; R1 (0.3.0): reactive buffer registry replaces the buffer-list scan

(ert-deftest tp-render-test-registry-update-visits-only-registered ()
  "A reactive update walks only registered buffers, not `buffer-list'."
  (tp-rt-with-cleanup (tp-rt-r1-layer) (tp-rt-r1-color)
    (setq tp-rt-r1-color "red")
    (define-tp tp-rt-r1-layer () '(face (:foreground $tp-rt-r1-color)))
    (let ((buf-a (generate-new-buffer " tp-rt-r1-a"))
          (buf-b (generate-new-buffer " tp-rt-r1-b"))
          (visited nil))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (insert "Hello")
              (tp-set 1 6 'tp-rt-r1-layer))
            (with-current-buffer buf-b (insert "Hello"))
            ;; Applying through tp-ops registered the buffer
            (should (equal (tp-reactive-layer-buffers 'tp-rt-r1-layer)
                           (list buf-a)))
            ;; Count per-buffer visits of the update walk
            (let ((orig (symbol-function 'tp--render-visit-buffer)))
              (cl-letf (((symbol-function 'tp--render-visit-buffer)
                         (lambda (buf fn)
                           (push buf visited)
                           (funcall orig buf fn))))
                (setq tp-rt-r1-color "blue")))
            ;; Only the registered buffer was visited
            (should (equal visited (list buf-a)))
            (with-current-buffer buf-a
              (should (equal (plist-get (get-text-property 1 'face)
                                        :foreground)
                             "blue"))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

(ert-deftest tp-render-test-registry-prunes-on-kill-buffer ()
  "Killing a buffer removes it from the layer-buffer registry."
  (tp-rt-with-cleanup (tp-rt-r1b-layer) (tp-rt-r1b-color)
    (setq tp-rt-r1b-color "red")
    (define-tp tp-rt-r1b-layer () '(face (:foreground $tp-rt-r1b-color)))
    (let ((buf (generate-new-buffer " tp-rt-r1b")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "Hello")
              (tp-set 1 6 'tp-rt-r1b-layer))
            (should (equal (tp-reactive-layer-buffers 'tp-rt-r1b-layer)
                           (list buf)))
            (kill-buffer buf)
            ;; The kill-buffer hook pruned the raw registry entry ...
            (should-not (memq buf (gethash 'tp-rt-r1b-layer
                                           tp--layer-buffers)))
            ;; ... and the accessor answers "known: none", NOT `unknown'.
            (should (null (tp-reactive-layer-buffers 'tp-rt-r1b-layer)))
            (should-not (eq (tp-reactive-layer-buffers 'tp-rt-r1b-layer)
                            'unknown)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(ert-deftest tp-render-test-registry-unknown-full-scan-learns ()
  "An `unknown' layer falls back to a full scan and learns its buffers."
  (tp-rt-with-cleanup (tp-rt-r1c-layer) (tp-rt-r1c-color)
    (setq tp-rt-r1c-color "red")
    (define-tp tp-rt-r1c-layer () '(face (:foreground $tp-rt-r1c-color)))
    (let ((buf (generate-new-buffer " tp-rt-r1c")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "Hello")
              (tp-set 1 6 'tp-rt-r1c-layer))
            ;; Simulate a buffer that got the layer outside the
            ;; registering paths: erase the registry knowledge.
            (remhash 'tp-rt-r1c-layer tp--layer-buffers)
            (should (eq (tp-reactive-layer-buffers 'tp-rt-r1c-layer)
                        'unknown))
            ;; The update still reaches the buffer (conservative fallback)
            (setq tp-rt-r1c-color "blue")
            (with-current-buffer buf
              (should (equal (plist-get (get-text-property 1 'face)
                                        :foreground)
                             "blue")))
            ;; ... and the scan registered the buffer it found (learning)
            (should (equal (tp-reactive-layer-buffers 'tp-rt-r1c-layer)
                           (list buf))))
        (kill-buffer buf)))))

(ert-deftest tp-render-test-track-buffer-closes-string-insert-gap ()
  "`tp-reactive-track-buffer' registers a buffer filled by string insert."
  (tp-rt-with-cleanup (tp-rt-r1d-layer) (tp-rt-r1d-color)
    (setq tp-rt-r1d-color "red")
    (define-tp tp-rt-r1d-layer () '(face (:foreground $tp-rt-r1d-color)))
    (let ((buf-a (generate-new-buffer " tp-rt-r1d-a"))
          (buf-b (generate-new-buffer " tp-rt-r1d-b")))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (insert "Hello")
              (tp-set 1 6 'tp-rt-r1d-layer))
            ;; Inserting an already-propertized STRING bypasses the
            ;; registering buffer operations.
            (let ((s (tp-set "Hi" 'tp-rt-r1d-layer)))
              (with-current-buffer buf-b (insert s)))
            (should-not (memq buf-b
                              (tp-reactive-layer-buffers 'tp-rt-r1d-layer)))
            ;; The layer is known, so buf-b is NOT updated (the gap) ...
            (setq tp-rt-r1d-color "blue")
            (with-current-buffer buf-b
              (should (equal (plist-get (get-text-property 1 'face)
                                        :foreground)
                             "red")))
            ;; ... until tp-reactive-track-buffer closes it.
            (should (equal (with-current-buffer buf-b
                             (tp-reactive-track-buffer))
                           '(tp-rt-r1d-layer)))
            (should (memq buf-b
                          (tp-reactive-layer-buffers 'tp-rt-r1d-layer)))
            (setq tp-rt-r1d-color "green")
            (with-current-buffer buf-b
              (should (equal (plist-get (get-text-property 1 'face)
                                        :foreground)
                             "green")))
            (with-current-buffer buf-a
              (should (equal (plist-get (get-text-property 1 'face)
                                        :foreground)
                             "green"))))
        (kill-buffer buf-a)
        (kill-buffer buf-b)))))

;;; R2 (0.3.0): minimal-diff tp-text replacement

(ert-deftest tp-render-test-minimal-diff-point-in-prefix-stays ()
  "Point in the common prefix survives a reactive text edit unmoved."
  (tp-rt-with-cleanup (tp-rt-r2-layer) (tp-rt-r2-text)
    (setq tp-rt-r2-text "abcdef")
    (define-tp tp-rt-r2-layer () '(tp-text $tp-rt-r2-text))
    (with-temp-buffer
      (insert "abcdef")
      (tp-set 1 7 'tp-rt-r2-layer)
      (goto-char 2)                     ; inside the common prefix "ab"
      (setq tp-rt-r2-text "abXYef")
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "abXYef"))
      (should (= (point) 2)))))

(ert-deftest tp-render-test-minimal-diff-point-in-suffix-stays ()
  "Point in the common suffix stays glued to its character."
  (tp-rt-with-cleanup (tp-rt-r2-layer) (tp-rt-r2-text)
    (setq tp-rt-r2-text "abcdef")
    (define-tp tp-rt-r2-layer () '(tp-text $tp-rt-r2-text))
    (with-temp-buffer
      (insert "abcdef")
      (tp-set 1 7 'tp-rt-r2-layer)
      (goto-char 6)                     ; on the "f" of the suffix "ef"
      ;; Same-length edit: point must not move at all
      (setq tp-rt-r2-text "abXYef")
      (should (= (point) 6))
      (should (eq (char-after) ?f))
      ;; Length-changing edit: point stays glued to its character
      (setq tp-rt-r2-text "abXYZWef")
      (should (= (point) 8))
      (should (eq (char-after) ?f)))))

(ert-deftest tp-render-test-minimal-diff-point-inside-diff-clamps ()
  "Point inside the differing span ends up at the edit start."
  (tp-rt-with-cleanup (tp-rt-r2-layer) (tp-rt-r2-text)
    (setq tp-rt-r2-text "abcdef")
    (define-tp tp-rt-r2-layer () '(tp-text $tp-rt-r2-text))
    (with-temp-buffer
      (insert "abcdef")
      (tp-set 1 7 'tp-rt-r2-layer)
      (goto-char 4)                 ; on "d", inside the "cd" -> "XY" span
      (setq tp-rt-r2-text "abXYef")
      (should (= (point) 3)))))

(ert-deftest tp-render-test-minimal-diff-markers-survive ()
  "Markers in the unchanged prefix and suffix survive a text update."
  (tp-rt-with-cleanup (tp-rt-r2m-layer) (tp-rt-r2m-text)
    (setq tp-rt-r2m-text "abcdef")
    (define-tp tp-rt-r2m-layer () '(tp-text $tp-rt-r2m-text))
    (with-temp-buffer
      (insert "abcdef")
      (tp-set 1 7 'tp-rt-r2m-layer)
      (let ((m-prefix (copy-marker 2))   ; on "b"
            (m-suffix (copy-marker 6)))  ; on "f"
        (setq tp-rt-r2m-text "abXYZef")  ; "cd" -> "XYZ", one char longer
        (should (equal (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       "abXYZef"))
        (should (= (marker-position m-prefix) 2))
        (should (eq (char-after m-prefix) ?b))
        (should (= (marker-position m-suffix) 7))
        (should (eq (char-after m-suffix) ?f))
        (set-marker m-prefix nil)
        (set-marker m-suffix nil)))))

;;; TXT-1: the suffix-boundary marker must track its character

(defun tp-rt--txt1-marker-after-edit (old new marker-offset)
  "Run a minimal-diff replacement of OLD by NEW with a boundary marker.
Insert \"HEAD \" OLD \" TAIL\" in a temp buffer, tag OLD with a
tp-name, put an insertion-type-nil marker at OLD's start plus
MARKER-OFFSET, replace via `tp--replace-reactive-text-in-buffer' and
return (MARKER-POSITION CHAR-AT-MARKER ORIGINAL-CHAR)."
  (with-temp-buffer
    (insert "HEAD ")
    (let ((m-start (point)))
      (insert old " TAIL")
      (put-text-property m-start (+ m-start (length old))
                         'tp-name 'tp-rt-txt1-layer)
      (let* ((mpos (+ m-start marker-offset))
             (mchar (char-after mpos))
             (mk (copy-marker mpos)))
        (tp--replace-reactive-text-in-buffer 'tp-rt-txt1-layer new nil)
        (prog1 (list (marker-position mk) (char-after mk) mchar)
          (set-marker mk nil))))))

(ert-deftest tp-render-test-minimal-diff-suffix-start-marker-tracks ()
  "A marker on the FIRST character of the preserved suffix tracks it.
TXT-1: delete-then-insert collapsed such a marker onto the edit
start, stranding it before the inserted text; insert-then-delete
shifts it right with its character.  Grow, same-length (the clearest
docstring violation) and shrink edits are all covered."
  ;; Grow: "0" -> "42"; marker on the space before "items" (offset 8).
  (pcase-let ((`(,pos ,got ,want)
               (tp-rt--txt1-marker-after-edit
                "count: 0 items" "count: 42 items" 8)))
    (should (eq got want))
    (should (= pos 15)))                ; 14 shifted right by 1
  ;; Same length: "0" -> "9"; the marker's correct position is
  ;; numerically unchanged.
  (pcase-let ((`(,pos ,got ,want)
               (tp-rt--txt1-marker-after-edit
                "count: 0 items" "count: 9 items" 8)))
    (should (eq got want))
    (should (= pos 14)))
  ;; Shrink: "42" -> "0".
  (pcase-let ((`(,pos ,got ,want)
               (tp-rt--txt1-marker-after-edit
                "count: 42 items" "count: 0 items" 9)))
    (should (eq got want))
    (should (= pos 14))))

(ert-deftest tp-render-test-minimal-diff-deleted-char-marker-at-edit-end ()
  "A marker whose character was deleted ends at the END of the edit.
The documented side effect of inserting before deleting; previously
such markers collapsed to the edit start.  Either way they stay
inside the replacement span."
  ;; "100" -> "42": marker on the middle "0" (strictly inside the
  ;; edited span) ends after the inserted "42".
  (pcase-let ((`(,pos ,_got ,_want)
               (tp-rt--txt1-marker-after-edit
                "count: 100 items" "count: 42 items" 8)))
    ;; Edit span starts at buffer position 13 ("100"), insert "42":
    ;; the marker lands at the end of the inserted text.
    (should (= pos 15))))

(ert-deftest tp-render-test-minimal-diff-suffix-marker-real-path ()
  "The suffix-start marker tracks through a real setq-driven update."
  (tp-rt-with-cleanup (tp-rt-r2s-layer) (tp-rt-r2s-text)
    (setq tp-rt-r2s-text "count: 0 items")
    (define-tp tp-rt-r2s-layer () '(tp-text $tp-rt-r2s-text))
    (with-temp-buffer
      (insert "count: 0 items")
      (tp-set 1 15 'tp-rt-r2s-layer)
      (let ((m (copy-marker 9)))        ; the space before "items"
        (setq tp-rt-r2s-text "count: 42 items")
        (should (equal (buffer-substring-no-properties (point-min)
                                                       (point-max))
                       "count: 42 items"))
        (should (eq (char-after m) ?\s))
        (should (= (marker-position m) 10))
        (set-marker m nil)))))

(ert-deftest tp-render-test-minimal-diff-identical-update-is-noop ()
  "An identical-text reactive replacement leaves the buffer unmodified."
  (tp-rt-with-cleanup (tp-rt-r2n-layer) (tp-rt-r2n-text)
    (setq tp-rt-r2n-text "emacs")
    (define-tp tp-rt-r2n-layer () '(face bold tp-text $tp-rt-r2n-text))
    (with-temp-buffer
      (insert "emacs")
      (tp-set 1 6 'tp-rt-r2n-layer)
      (set-buffer-modified-p nil)
      (save-excursion
        (tp--replace-reactive-text-in-buffer
         'tp-rt-r2n-layer "emacs" (tp-layer-props 'tp-rt-r2n-layer t)))
      ;; No text edit and no property churn: the flag must stay clear
      (should-not (buffer-modified-p))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "emacs"))
      (should (eq (get-text-property 1 'face) 'bold)))))

;;; R3 (0.3.0): anonymous-layer garbage collection

(ert-deftest tp-render-test-gc-collects-unreferenced-anonymous-layer ()
  "GC collects an anonymous layer whose only buffer was killed."
  (setq tp-rt-r3a-color "red")
  (let ((buf (generate-new-buffer " tp-rt-r3a"))
        (name nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Hello")
            (tp-set 1 6 '(face (:foreground $tp-rt-r3a-color)))
            (setq name (get-text-property 1 'tp-name)))
          (should name)
          (should (assoc name tp-layer-alist))
          (kill-buffer buf)
          (should (memq name (tp-gc-anonymous-layers)))
          (should-not (assoc name tp-layer-alist))
          (should-not (rassq name tp--anonymous-layer-registry)))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (setq tp-rt-r3a-color nil))))

(ert-deftest tp-render-test-gc-keeps-layer-still-displayed ()
  "GC keeps an anonymous layer that a live buffer still shows."
  (setq tp-rt-r3b-color "red")
  (let ((buf (generate-new-buffer " tp-rt-r3b"))
        (name nil))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Hello")
            (tp-set 1 6 '(face (:foreground $tp-rt-r3b-color)))
            (setq name (get-text-property 1 'tp-name)))
          (should name)
          (should-not (memq name (tp-gc-anonymous-layers)))
          (should (assoc name tp-layer-alist)))
      (kill-buffer buf)
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (setq tp-rt-r3b-color nil))))

(ert-deftest tp-render-test-gc-keeps-unknown-registry-layer ()
  "GC keeps an anonymous layer whose registry state is `unknown'."
  (setq tp-rt-r3c-color "red")
  (let* ((s (tp-set "Hello" '(face (:foreground $tp-rt-r3c-color))))
         (name (get-text-property 0 'tp-name s)))
    (unwind-protect
        (progn
          (should name)
          ;; Applied to a string only: the registry knows nothing
          (should (eq (tp-reactive-layer-buffers name) 'unknown))
          (should-not (memq name (tp-gc-anonymous-layers)))
          (should (assoc name tp-layer-alist)))
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (setq tp-rt-r3c-color nil))))

;;; GC-1: buried and hidden layers are ALIVE for GC and track-buffer

(defvar tp-rt-gc1-color nil)
(defvar tp-rt-gc1b-color nil)
(defvar tp-rt-gc1c-color nil)

(ert-deftest tp-render-test-gc-keeps-layer-buried-under-push ()
  "GC keeps an anonymous layer buried below a pushed top layer.
The buried layer's tp-name lives inside `tp-layers' storage, not as a
direct property; the stack-aware liveness scan must still see it, and
reactivity must survive a later pop (GC-1)."
  (setq tp-rt-gc1-color "blue")
  (let ((buf (generate-new-buffer " tp-rt-gc1"))
        (name nil))
    (unwind-protect
        (progn
          (define-tp tp-rt-gc1-top () '(face bold))
          (with-current-buffer buf
            (insert "0123456789")
            (tp-set 1 6 '(face (:foreground $tp-rt-gc1-color)))
            (setq name (get-text-property 1 'tp-name))
            (should name)
            (tp-push-layer 1 6 'tp-rt-gc1-top)
            ;; Now buried: direct tp-name is the pushed top's.
            (should (eq (get-text-property 1 'tp-name) 'tp-rt-gc1-top))
            ;; The buffer is live and still holds the layer: GC must
            ;; keep it.
            (should-not (memq name (tp-gc-anonymous-layers)))
            (should (assoc name tp-layer-alist))
            ;; Reactivity survives: pop and update.
            (tp-pop-layer 1 6)
            (setq tp-rt-gc1-color "red")
            (should (equal (get-text-property 1 'face)
                           '(:foreground "red")))))
      (kill-buffer buf)
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (tp-undefine-layer 'tp-rt-gc1-top)
      (setq tp-rt-gc1-color nil))))

(ert-deftest tp-render-test-gc-keeps-hidden-layer ()
  "GC keeps an anonymous layer hidden via tp-hide-layer.
An all-hidden run carries no direct tp-name at all; the layer lives
only inside `tp-layers' storage yet is queryable and re-showable, so
GC must not collect it and show+setq must still re-render (GC-1,
XM-02)."
  (setq tp-rt-gc1b-color "green")
  (let ((buf (generate-new-buffer " tp-rt-gc1b"))
        (name nil))
    (unwind-protect
        (with-current-buffer buf
          (insert "abcdefghij")
          (tp-set 1 6 '(face (:foreground $tp-rt-gc1b-color)))
          (setq name (get-text-property 1 'tp-name))
          (should name)
          (tp-hide-layer 1 6 name)
          (should-not (get-text-property 1 'tp-name))
          ;; Live buffer still holds the hidden layer: keep it.
          (should-not (memq name (tp-gc-anonymous-layers)))
          (should (assoc name tp-layer-alist))
          ;; Show and update: reactivity must be intact.
          (tp-show-layer 1 6 name)
          (setq tp-rt-gc1b-color "purple")
          (should (equal (get-text-property 1 'face)
                         '(:foreground "purple"))))
      (kill-buffer buf)
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (setq tp-rt-gc1b-color nil))))

(ert-deftest tp-render-test-track-buffer-finds-buried-and-hidden-layers ()
  "tp-reactive-track-buffer registers layers buried or hidden in storage.
A propertized string carrying a stacked (buried) layer and an
all-hidden string are inserted into a fresh buffer; the track scan
must register every layer name, not just the rendered top ones
\(GC-1, XM-04)."
  (setq tp-rt-gc1c-color "gold")
  (let ((buf (generate-new-buffer " tp-rt-gc1c"))
        (name nil))
    (unwind-protect
        (progn
          (define-tp tp-rt-gc1c-top () '(face bold))
          (define-tp tp-rt-gc1c-hidden () '(face italic))
          (let ((s (with-temp-buffer
                     (insert "trackme")
                     (tp-set 1 6 '(face (:foreground $tp-rt-gc1c-color)))
                     (setq name (get-text-property 1 'tp-name))
                     (tp-push-layer 1 6 'tp-rt-gc1c-top)
                     (buffer-string)))
                (h (let ((h (copy-sequence " hideme")))
                     (tp-push-layer h 'tp-rt-gc1c-hidden)
                     (tp-hide-layer h 'tp-rt-gc1c-hidden)
                     h)))
            (with-current-buffer buf
              (insert s)
              (insert h)
              (let ((found (tp-reactive-track-buffer)))
                ;; Rendered top, buried layer, and all-hidden layer.
                (should (memq 'tp-rt-gc1c-top found))
                (should (memq name found))
                (should (memq 'tp-rt-gc1c-hidden found)))
              (should (memq buf (tp-reactive-layer-buffers name)))
              (should (memq buf (tp-reactive-layer-buffers
                                 'tp-rt-gc1c-hidden))))))
      (kill-buffer buf)
      (when (and name (assoc name tp-layer-alist))
        (tp-undefine-layer name))
      (tp-undefine-layer 'tp-rt-gc1c-top)
      (tp-undefine-layer 'tp-rt-gc1c-hidden)
      (setq tp-rt-gc1c-color nil))))

(provide 'tp-render-tests)
;;; tp-render-tests.el ends here
