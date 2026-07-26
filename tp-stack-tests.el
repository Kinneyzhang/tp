;;; tp-stack-tests.el --- ERT regression tests for tp-stack.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for confirmed bugs fixed in the layer-stack module
;; (tp-stack.el).  Each section is tagged with the canonical bug id it
;; guards against.

;;; Code:

(require 'ert)
(require 'tp)

(defmacro tp-stack-tests--with-env (&rest body)
  "Run BODY in a temp buffer with a clean tp layer state.
Layer registries are reset before BODY and again afterwards so
definitions cannot leak between tests."
  (declare (indent 0))
  `(unwind-protect
       (with-temp-buffer
         (tp-layer-reset)
         ,@body)
     (tp-layer-reset)))

(defun tp-stack-tests--has-prop-p (pos prop &optional object)
  "Return non-nil if PROP is present (even with value nil) at POS of OBJECT."
  (and (plist-member (text-properties-at pos object) prop) t))

;;; B28: region ops must not mutate text outside [START, END)

(ert-deftest tp-stack-test-delete-layer-subregion-keeps-outside ()
  "Deleting a layer on a sub-region leaves the rest of the stack alone."
  (tp-stack-tests--with-env
    (insert "abcdefghij")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 11 'layer1)
    (tp-push-layer 1 11 'layer2)
    (tp-delete-layer 3 6 'layer2)
    ;; Inside [3, 6): layer2 gone, layer1 now on top.
    (should (eq (get-text-property 3 'tp-name) 'layer1))
    (should (eq (get-text-property 5 'tp-name) 'layer1))
    (should-not (tp-layer-exists-p 3 6 'layer2))
    ;; Outside the region: the full 2-layer stack survives.
    (should (eq (get-text-property 1 'tp-name) 'layer2))
    (should (eq (get-text-property 2 'tp-name) 'layer2))
    (should (eq (get-text-property 6 'tp-name) 'layer2))
    (should (eq (get-text-property 10 'tp-name) 'layer2))
    (should (tp-layer-exists-p 1 3 'layer1))
    (should (tp-layer-exists-p 6 11 'layer1))))

(ert-deftest tp-stack-test-push-layer-subregion-keeps-outside ()
  "Pushing onto a sub-region does not smear over the whole interval."
  (tp-stack-tests--with-env
    (insert "abcdefghij")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 4 'layer1)
    (tp-push-layer 3 8 'layer2)
    ;; [1, 3): still only layer1.
    (should (eq (get-text-property 1 'tp-name) 'layer1))
    (should (eq (get-text-property 2 'tp-name) 'layer1))
    (should-not (tp-layer-exists-p 1 3 'layer2))
    ;; [3, 4): layer2 stacked over layer1.
    (should (eq (get-text-property 3 'tp-name) 'layer2))
    (should (tp-layer-exists-p 3 4 'layer1))
    ;; [4, 8): only layer2.
    (should (eq (get-text-property 5 'tp-name) 'layer2))
    (should-not (tp-layer-exists-p 4 8 'layer1))
    ;; [8, 11): untouched bare text.
    (should (null (text-properties-at 8)))
    (should (null (text-properties-at 10)))))

(ert-deftest tp-stack-test-push-layer-subregion-string ()
  "Region-form push on a string only affects the requested sub-range."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp layer1 () '(face bold))
      (tp-put-layer 2 5 'layer1 0 str)
      (should (null (text-properties-at 0 str)))
      (should (null (text-properties-at 1 str)))
      (should (eq (get-text-property 2 'tp-name str) 'layer1))
      (should (eq (get-text-property 4 'tp-name str) 'layer1))
      (should (null (text-properties-at 5 str))))))

;;; B29: tp-put-layer must be region-local, not whole-object

(ert-deftest tp-stack-test-put-layer-bare-region-distant-props ()
  "Putting a layer on a bare region ignores properties elsewhere."
  (tp-stack-tests--with-env
    (insert "abcdefghij")
    (define-tp layer1 () '(face bold))
    (put-text-property 8 10 'help-echo "far")
    (tp-push-layer 1 4 'layer1)
    ;; The layer covers exactly [1, 4).
    (should (eq (get-text-property 1 'tp-name) 'layer1))
    (should (eq (get-text-property 3 'tp-name) 'layer1))
    (should (null (text-properties-at 4)))
    (should (null (text-properties-at 7)))
    ;; The distant properties are untouched.
    (should (equal (get-text-property 8 'help-echo) "far"))
    (should (null (get-text-property 8 'tp-name)))))

(ert-deftest tp-stack-test-put-layer-same-result-with-or-without-distant-props ()
  "Distant unrelated properties do not change what put-layer writes."
  (tp-stack-tests--with-env
    (define-tp layer1 () '(face bold))
    (let (props-bare props-distant)
      (with-temp-buffer
        (insert "abcdefghij")
        (tp-push-layer 1 4 'layer1)
        (setq props-bare (text-properties-at 1)))
      (with-temp-buffer
        (insert "abcdefghij")
        (put-text-property 8 10 'help-echo "far")
        (tp-push-layer 1 4 'layer1)
        (setq props-distant (text-properties-at 1)))
      (should (equal props-bare props-distant)))))

;;; B30: inline plists with ordinary (non-keyword) properties

(ert-deftest tp-stack-test-put-layer-inline-plist-plain ()
  "An inline plist like (face bold) is a valid layer spec."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (tp-put-layer 1 6 '(face bold) 0)
    (should (eq (get-text-property 1 'face) 'bold))))

(ert-deftest tp-stack-test-put-layer-inline-plist-nested ()
  "An inline plist with a nested value list is a valid layer spec."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (tp-put-layer 1 6 '(face (:foreground "red")) 0)
    (should (equal (get-text-property 1 'face) '(:foreground "red")))))

(ert-deftest tp-stack-test-put-layer-inline-plist-multi-pair ()
  "A multi-pair inline plist is applied as one layer."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (tp-put-layer 1 6 '(face bold help-echo "tip") 0)
    (should (eq (get-text-property 1 'face) 'bold))
    (should (equal (get-text-property 1 'help-echo) "tip"))
    (should (= (tp-layer-count 1 6) 1))))

(ert-deftest tp-stack-test-put-layer-named-inline-still-works ()
  "A named inline layer (NAME PROP VAL ...) keeps its old meaning."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (tp-put-layer 1 6 '(mylayer face bold) 0)
    (should (eq (get-text-property 1 'tp-name) 'mylayer))
    (should (eq (get-text-property 1 'face) 'bold))))

;;; B31: list of layer names

(ert-deftest tp-stack-test-put-layer-list-of-names ()
  "A list of defined layer names pushes each as its own layer."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer-a () '(face bold))
    (define-tp layer-b () '(help-echo "b"))
    (tp-put-layer 1 5 '(layer-a layer-b) 0)
    (should (= (tp-layer-count 1 5) 2))
    (should (eq (tp-layer-top 1 5) 'layer-a))
    (should (tp-layer-exists-p 1 5 'layer-a))
    (should (tp-layer-exists-p 1 5 'layer-b))))

(ert-deftest tp-stack-test-put-layer-mixed-list ()
  "A list mixing a layer name and an inline plist works."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer-a () '(face bold))
    (tp-put-layer 1 5 '(layer-a (help-echo "inline")) 0)
    (should (= (tp-layer-count 1 5) 2))
    (should (eq (tp-layer-top 1 5) 'layer-a))))

;;; B32: parameterized groups

(ert-deftest tp-stack-test-put-layer-parameterized-group ()
  "A (GROUP-NAME ARG) spec resolves a parameterized group."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp pcolor (c) `(face (:foreground ,c)))
    (define-tps pgroup (c) `(pcolor ,c))
    (tp-put-layer 1 6 '(pgroup "red") 0)
    (should (equal (get-text-property 1 'face) '(:foreground "red")))
    (should (eq (get-text-property 1 'tp-name) 'pcolor))))

(ert-deftest tp-stack-test-put-layer-parameterized-group-without-arg-errors ()
  "A bare parameterized group name signals instead of silently no-oping."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp pcolor (c) `(face (:foreground ,c)))
    (define-tps pgroup (c) `(pcolor ,c))
    (should-error (tp-put-layer 1 6 'pgroup 0))))

;;; B33: tp-region-layer-props string positions

(ert-deftest tp-stack-test-region-layer-props-string-subrange ()
  "String sub-range queries return absolute in-bounds string positions."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp layer1 () '(face bold))
      (tp-push-layer str 'layer1)
      (let ((result (tp-region-layer-props 2 5 'layer1 str)))
        (should (= (length result) 1))
        (should (= (nth 0 (car result)) 2))
        (should (= (nth 1 (car result)) 5))
        (should (<= (nth 1 (car result)) (length str)))
        (should (eq (plist-get (nth 2 (car result)) 'tp-name) 'layer1))))))

(ert-deftest tp-stack-test-region-layer-props-buffer-subrange ()
  "Buffer queries return 1-based positions clipped to the region."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 7 'layer1)
    (let ((result (tp-region-layer-props 2 4 'layer1)))
      (should (equal (list (nth 0 (car result)) (nth 1 (car result)))
                     '(2 4))))))

;;; string/buffer path convergence for region-form mutators

(ert-deftest tp-stack-test-delete-layer-string-region-form ()
  "Region-form delete on a string works and stays inside the range."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp layer1 () '(face bold))
      (define-tp layer2 () '(face italic))
      (tp-push-layer str 'layer1)
      (tp-push-layer str 'layer2)
      (tp-delete-layer 2 5 'layer2 str)
      (should (eq (get-text-property 2 'tp-name str) 'layer1))
      (should (eq (get-text-property 4 'tp-name str) 'layer1))
      ;; Outside [2, 5) both layers survive.
      (should (eq (get-text-property 0 'tp-name str) 'layer2))
      (should (eq (get-text-property 5 'tp-name str) 'layer2))
      (should (tp-layer-exists-p 0 2 'layer1 str))
      (should (tp-layer-exists-p 5 6 'layer1 str)))))

;;; B34: explicit nil values survive merge/flatten precedence

(ert-deftest tp-stack-test-merge-layers-explicit-nil-wins ()
  "An explicitly-nil value in a higher-precedence layer is kept."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face nil help-echo "u"))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (tp-merge-layers 1 6 'merged '(upper lower))
    (should (tp-stack-tests--has-prop-p 1 'face))
    (should (null (get-text-property 1 'face)))
    (should (equal (get-text-property 1 'help-echo) "u"))
    (should (eq (get-text-property 1 'tp-name) 'merged))))

(ert-deftest tp-stack-test-flatten-layers-explicit-nil-wins ()
  "Flattening keeps an explicit nil from a higher layer over lower values."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold help-echo "low"))
    (define-tp upper () '(face nil))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (tp-flatten-layers 1 6 'flat)
    (should (tp-stack-tests--has-prop-p 1 'face))
    (should (null (get-text-property 1 'face)))
    (should (equal (get-text-property 1 'help-echo) "low"))
    (should (eq (get-text-property 1 'tp-name) 'flat))))

;;; B35: no garbage (tp-layers nil) on single-layer stacks

(ert-deftest tp-stack-test-single-layer-no-tp-layers-prop ()
  "Pushing one layer does not leave a (tp-layers nil) property behind."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 6 'layer1)
    (should-not (tp-stack-tests--has-prop-p 1 'tp-layers))
    (should (eq (get-text-property 1 'face) 'bold))))

(ert-deftest tp-stack-test-delete-to-single-layer-no-tp-layers-prop ()
  "Deleting down to one layer drops the tp-layers property entirely."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; With two layers the below-stack is a real, non-nil list.
    (should (get-text-property 1 'tp-layers))
    (tp-delete-layer 1 6 'layer2)
    (should-not (tp-stack-tests--has-prop-p 1 'tp-layers))
    (should (eq (get-text-property 1 'tp-name) 'layer1))))

(ert-deftest tp-stack-test-pop-to-single-layer-no-tp-layers-prop ()
  "Popping down to one layer drops the tp-layers property entirely."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp layer1 () '(face bold))
      (define-tp layer2 () '(face italic))
      (tp-push-layer str 'layer1)
      (tp-push-layer str 'layer2)
      (tp-pop-layer str)
      (should-not (plist-member (text-properties-at 0 str) 'tp-layers))
      (should (eq (get-text-property 0 'tp-name str) 'layer1)))))

(ert-deftest tp-stack-test-absent-tp-layers-tolerated-by-stack-ops ()
  "Stacks without a tp-layers property still work with every operation."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 6 'layer1)          ; single layer, no tp-layers prop
    (should (= (tp-layer-count 1 6) 1))
    (should (equal (tp-layer-list 1 6) '(layer1)))
    (should (tp-layer-exists-p 1 6 'layer1))
    (should (eq (tp-layer-top 1 6) 'layer1))
    (tp-push-layer 1 6 'layer2)          ; stacking on top still works
    (should (= (tp-layer-count 1 6) 2))
    (should (eq (tp-layer-top 1 6) 'layer2))
    (should (tp-layer-exists-p 1 6 'layer1))))

;;; B36: tp-layer-top respects the whole region

(ert-deftest tp-stack-test-layer-top-mid-region-layer ()
  "A layer starting after bare text is still found by tp-layer-top."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 3 6 'layer1)
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-stack-test-layer-top-respects-end ()
  "tp-layer-top does not report layers that lie beyond END."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 4 6 'layer1)
    (should (null (tp-layer-top 1 3)))))

(ert-deftest tp-stack-test-layer-top-first-named-run-wins ()
  "The first run with a named top layer determines the result."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer-a () '(face bold))
    (define-tp layer-b () '(face italic))
    (tp-push-layer 1 3 'layer-a)
    (tp-push-layer 3 6 'layer-b)
    (should (eq (tp-layer-top 1 6) 'layer-a))
    (should (eq (tp-layer-top 3 6) 'layer-b))))

;;; Shared argument normalizer: both calling conventions still work

(ert-deftest tp-stack-test-normalizer-string-forms ()
  "Whole-string forms of the routed mutators behave as before."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp layer1 () '(face bold))
      (define-tp layer2 () '(face italic))
      (define-tp layer3 () '(face underline))
      (tp-push-layer str 'layer1)
      (tp-push-layer str 'layer2)
      (tp-push-layer str 'layer3)
      (should (eq (tp-layer-top 0 6 str) 'layer3))
      (tp-rotate-layer str)
      (should (eq (tp-layer-top 0 6 str) 'layer2))
      (tp-pin-layer str 'layer1)
      (should (eq (tp-layer-top 0 6 str) 'layer1))
      (tp-pop-layer str)
      (should (eq (tp-layer-top 0 6 str) 'layer2))
      (tp-delete-layer str 'layer3)
      (should (equal (tp-layer-list 0 6 str) '(layer2)))
      (should (eq (tp-push-layer str 'layer1) str)))))

(ert-deftest tp-stack-test-normalizer-invalid-first-arg-signals ()
  "A non-string, non-number first argument signals a clear error."
  (tp-stack-tests--with-env
    (define-tp layer1 () '(face bold))
    (should-error (tp-push-layer nil 'layer1))
    (should-error (tp-delete-layer 'not-a-position 5 'layer1))))

(provide 'tp-stack-tests)
;;; tp-stack-tests.el ends here
