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

;;; 0.3.0 S1: layer visibility (tp-hide-layer / tp-show-layer)

(ert-deftest tp-stack-test-hide-top-reveals-next-visible ()
  "Hiding the top layer renders the next visible layer's properties."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face italic))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (should (= (tp-hide-layer 1 6 'upper) 1))
    ;; The text now renders the lower layer.
    (should (eq (get-text-property 1 'face) 'bold))
    (should (eq (get-text-property 1 'tp-name) 'lower))
    ;; The hidden layer is still in the stack for the queries.
    (should (= (tp-layer-count 1 6) 2))
    (should (equal (tp-layer-list 1 6) '(upper lower)))
    (should (tp-layer-exists-p 1 6 'upper))))

(ert-deftest tp-stack-test-show-restores-hidden-top ()
  "Showing a hidden top layer restores its properties onto the text."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face italic))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (tp-hide-layer 1 6 'upper)
    (should (= (tp-show-layer 1 6 'upper) 1))
    (should (eq (get-text-property 1 'face) 'italic))
    (should (eq (get-text-property 1 'tp-name) 'upper))
    ;; No bookkeeping flag leaks into the rendered properties.
    (should-not (tp-stack-tests--has-prop-p 1 'tp-hidden))))

(ert-deftest tp-stack-test-hide-all-layers-contract ()
  "With every layer hidden only the tp-layers bookkeeping remains."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face italic))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (should (= (tp-hide-layer 1 6 'upper) 1))
    (should (= (tp-hide-layer 1 6 'lower) 1))
    ;; No layer props render, not even tp-name.
    (should (null (get-text-property 1 'face)))
    (should (null (get-text-property 1 'tp-name)))
    (should (tp-stack-tests--has-prop-p 1 'tp-layers))
    ;; The whole stack stays queryable.
    (should (= (tp-layer-count 1 6) 2))
    (should (equal (tp-layer-list 1 6) '(upper lower)))
    ;; Showing one layer again renders it.
    (should (= (tp-show-layer 1 6 'lower) 1))
    (should (eq (get-text-property 1 'face) 'bold))
    (should (eq (get-text-property 1 'tp-name) 'lower))))

(ert-deftest tp-stack-test-hide-missing-name-is-silent-noop ()
  "Hiding or showing a non-existent layer returns 0 without signaling."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 6 'layer1)
    (let ((before (text-properties-at 1)))
      (should (= (tp-hide-layer 1 6 'nope) 0))
      (should (= (tp-show-layer 1 6 'nope) 0))
      (should (equal (text-properties-at 1) before)))))

(ert-deftest tp-stack-test-hide-already-hidden-returns-zero ()
  "Hiding an already-hidden layer (or showing a visible one) counts 0."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face italic))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (should (= (tp-show-layer 1 6 'upper) 0))   ; visible already
    (should (= (tp-hide-layer 1 6 'upper) 1))
    (should (= (tp-hide-layer 1 6 'upper) 0))   ; hidden already
    (should (eq (get-text-property 1 'face) 'bold))))

(ert-deftest tp-stack-test-hide-string-forms ()
  "Whole-string and region-on-string forms of hide/show work 0-based."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp lower () '(face bold))
      (define-tp upper () '(face italic))
      (tp-push-layer str 'lower)
      (tp-push-layer str 'upper)
      (should (= (tp-hide-layer str 'upper) 1))
      (should (eq (get-text-property 0 'tp-name str) 'lower))
      (should (= (tp-show-layer 0 6 'upper str) 1))
      (should (eq (get-text-property 0 'tp-name str) 'upper))
      ;; Region form only touches [2, 5).
      (should (= (tp-hide-layer 2 5 'upper str) 1))
      (should (eq (get-text-property 0 'tp-name str) 'upper))
      (should (eq (get-text-property 2 'tp-name str) 'lower))
      (should (eq (get-text-property 5 'tp-name str) 'upper)))))

(ert-deftest tp-stack-test-show-layer-above-visible-top ()
  "Showing a hidden layer above the visible top makes it render again."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)
    (tp-hide-layer 1 6 'lc)
    (tp-hide-layer 1 6 'lb)
    (should (eq (get-text-property 1 'tp-name) 'la))
    ;; lc sits above the visible top (la); showing it wins again.
    (should (= (tp-show-layer 1 6 'lc) 1))
    (should (eq (get-text-property 1 'tp-name) 'lc))
    (should (eq (get-text-property 1 'face) 'underline))))

(ert-deftest tp-stack-test-hidden-layer-can-be-raised ()
  "A hidden layer can be moved in the stack and shown later."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-hide-layer 1 6 'la)               ; hide the bottom layer
    (should (= (tp-raise-layer 1 6 'la 1) 1))
    ;; la is now on top but hidden, so lb still renders.
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(la lb)))
    (should (eq (get-text-property 1 'tp-name) 'lb))
    (should (= (tp-show-layer 1 6 'la) 1))
    (should (eq (get-text-property 1 'tp-name) 'la))
    (should (eq (get-text-property 1 'face) 'bold))))

(ert-deftest tp-stack-test-hide-show-roundtrip-restores-storage ()
  "A hide/show roundtrip restores the exact original properties."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 6 'layer1)
    (let ((before (text-properties-at 1)))
      (tp-hide-layer 1 6 'layer1)
      ;; All layers hidden: only bookkeeping remains.
      (should (null (get-text-property 1 'tp-name)))
      (tp-show-layer 1 6 'layer1)
      (should (equal (text-properties-at 1) before))
      (should-not (tp-stack-tests--has-prop-p 1 'tp-layers)))))

(ert-deftest tp-stack-test-flatten-drops-tp-hidden-flag ()
  "Flattening a stack with a hidden layer never leaks the tp-hidden flag."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp lower () '(face bold))
    (define-tp upper () '(face italic))
    (tp-push-layer 1 6 'lower)
    (tp-push-layer 1 6 'upper)
    (tp-hide-layer 1 6 'upper)
    (tp-flatten-layers 1 6 'flat)
    (should (eq (get-text-property 1 'tp-name) 'flat))
    (should-not (tp-stack-tests--has-prop-p 1 'tp-hidden))
    (should-not (tp-stack-tests--has-prop-p 1 'tp-layers))))

;;; 0.3.0 S2: tp-lower-layer and extended tp-rotate-layer

(ert-deftest tp-stack-test-lower-layer-moves-down ()
  "Lowering by 1 swaps the layer with the one below it."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)               ; top->bottom: lc lb la
    (should (= (tp-lower-layer 1 6 'lc 1) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lb lc la)))
    (should (eq (get-text-property 1 'tp-name) 'lb))))

(ert-deftest tp-stack-test-lower-layer-mirrors-raise ()
  "Lowering then raising by the same N restores the stack order."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)
    (let ((before (mapcar #'car (tp-layer-stack-at 1))))
      (tp-lower-layer 1 6 'lc 2)
      (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lb la lc)))
      (tp-raise-layer 1 6 'lc 2)
      (should (equal (mapcar #'car (tp-layer-stack-at 1)) before)))))

(ert-deftest tp-stack-test-lower-layer-clamps-and-negates ()
  "Lowering clamps at the bottom; a negative N raises instead."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)
    (should (= (tp-lower-layer 1 6 'lc 99) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lb la lc)))
    (should (= (tp-lower-layer 1 6 'lc -2) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lc lb la)))))

(ert-deftest tp-stack-test-lower-layer-defaults-and-index ()
  "N defaults to 1 and integer indexes address the stack (0 = top)."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp la () '(face bold))
      (define-tp lb () '(face italic))
      (tp-push-layer str 'la)
      (tp-push-layer str 'lb)             ; top->bottom: lb la
      (should (= (tp-lower-layer str 0) 1))
      (should (equal (mapcar #'car (tp-layer-stack-at 0 str)) '(la lb)))
      (should (eq (get-text-property 0 'tp-name str) 'la)))))

(ert-deftest tp-stack-test-lower-layer-missing-returns-zero ()
  "Lowering a non-existent layer is a silent no-op returning 0."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (tp-push-layer 1 6 'la)
    (let ((before (text-properties-at 1)))
      (should (= (tp-lower-layer 1 6 'nope 1) 0))
      (should (equal (text-properties-at 1) before)))))

(ert-deftest tp-stack-test-rotate-layer-default-unchanged ()
  "With no new arguments rotate still moves the top layer to bottom."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)               ; top->bottom: lc lb la
    (should (= (tp-rotate-layer 1 6) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lb la lc)))
    (should (eq (get-text-property 1 'tp-name) 'lb))))

(ert-deftest tp-stack-test-rotate-layer-up-inverts-down ()
  "Rotating up moves the bottom layer to the top; up undoes down."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)
    (should (= (tp-rotate-layer 1 6 nil 'up) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(la lc lb)))
    (should (= (tp-rotate-layer 1 6 nil 'down) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lc lb la)))))

(ert-deftest tp-stack-test-rotate-layer-count-and-wraparound ()
  "COUNT rotates several steps; a full cycle restores the order."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (define-tp lc () '(face underline))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-push-layer 1 6 'lc)
    (should (= (tp-rotate-layer 1 6 nil 'down 2) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(la lc lb)))
    (should (= (tp-rotate-layer 1 6 nil 'up 2) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lc lb la)))
    (should (= (tp-rotate-layer 1 6 nil 'down 3) 1))
    (should (equal (mapcar #'car (tp-layer-stack-at 1)) '(lc lb la)))))

(ert-deftest tp-stack-test-rotate-layer-string-form-direction ()
  "String form accepts DIRECTION and COUNT right after the string."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp la () '(face bold))
      (define-tp lb () '(face italic))
      (tp-push-layer str 'la)
      (tp-push-layer str 'lb)             ; top->bottom: lb la
      (should (= (tp-rotate-layer str 'up) 1))
      (should (equal (mapcar #'car (tp-layer-stack-at 0 str)) '(la lb)))
      (should (= (tp-rotate-layer str 'down 1) 1))
      (should (equal (mapcar #'car (tp-layer-stack-at 0 str)) '(lb la))))))

(ert-deftest tp-stack-test-rotate-layer-edge-arguments ()
  "Invalid DIRECTION signals; COUNT below 1 and bare text return 0."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (tp-push-layer 1 4 'la)
    (should-error (tp-rotate-layer 1 4 nil 'sideways))
    (should (= (tp-rotate-layer 1 4 nil 'down 0) 0))
    (should (= (tp-rotate-layer 4 6) 0))
    (should (eq (get-text-property 1 'tp-name) 'la))))

;;; 0.3.0 S3: tp-layer-stack-at

(ert-deftest tp-stack-test-layer-stack-at-shape ()
  "The stack at a position is (NAME . PROPS) conses, top first."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (should (equal (tp-layer-stack-at 1)
                   '((lb . (face italic))
                     (la . (face bold)))))))

(ert-deftest tp-stack-test-layer-stack-at-hidden-marker ()
  "Hidden layers carry a tp-hidden t entry in their PROPS."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (tp-hide-layer 1 6 'lb)
    (let ((stack (tp-layer-stack-at 1)))
      (should (equal (mapcar #'car stack) '(lb la)))
      (should (eq (plist-get (cdr (nth 0 stack)) 'tp-hidden) t))
      (should-not (plist-member (cdr (nth 1 stack)) 'tp-hidden)))))

(ert-deftest tp-stack-test-layer-stack-at-string-positions ()
  "String positions are 0-based; outside the layer the stack is nil."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp la () '(face bold))
      (tp-put-layer 2 5 'la 0 str)
      (should (null (tp-layer-stack-at 0 str)))
      (should (equal (tp-layer-stack-at 2 str) '((la . (face bold)))))
      (should (null (tp-layer-stack-at 5 str))))))

(ert-deftest tp-stack-test-layer-stack-at-unnamed-and-bare ()
  "Unnamed layers report a nil NAME; bare text reports nil."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (tp-push-layer 1 4 '(face bold))
    (should (equal (tp-layer-stack-at 1) '((nil . (face bold)))))
    (should (null (tp-layer-stack-at 5)))))

;;; 0.3.0 S4: modified-interval counts and NOERROR

(ert-deftest tp-stack-test-delete-layer-returns-run-count ()
  "Delete returns how many property runs matched; 0 when none did."
  (tp-stack-tests--with-env
    (insert "abcdefghij")
    (define-tp la () '(face bold))
    (tp-push-layer 1 4 'la)
    (tp-push-layer 6 9 'la)
    (should (= (tp-delete-layer 1 9 'nope) 0))
    (should (= (tp-delete-layer 1 9 'la) 2))
    (should-not (tp-layer-exists-p 1 9 'la))))

(ert-deftest tp-stack-test-pop-layer-returns-run-count ()
  "Pop returns the number of runs that had a layer to pop."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp la () '(face bold))
      (tp-put-layer 0 3 'la 0 str)
      (should (= (tp-pop-layer 0 6 str) 1))
      (should (= (tp-pop-layer 0 6 str) 0)))))

(ert-deftest tp-stack-test-movement-ops-return-run-counts ()
  "Move, raise, pin and switch return matched-run counts."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (define-tp lb () '(face italic))
    (tp-push-layer 1 6 'la)
    (tp-push-layer 1 6 'lb)
    (should (= (tp-raise-layer 1 6 'nope 1) 0))
    (should (= (tp-raise-layer 1 6 'la 1) 1))
    (should (= (tp-pin-layer 1 6 'lb) 1))
    (should (= (tp-move-layer 1 6 'la 0) 1))
    (should (= (tp-move-layer 1 6 'nope 0) 0))
    (should (= (tp-switch-layer 1 6 'la 'lb) 1))
    (should (= (tp-switch-layer 1 6 'la 'nope) 0))))

(ert-deftest tp-stack-test-put-layer-noerror ()
  "With NOERROR an unresolvable LAYER returns nil and writes nothing."
  (tp-stack-tests--with-env
    (insert "abcdef")
    (define-tp la () '(face bold))
    (should-error (tp-put-layer 1 6 'undefined-x 0))
    (should (null (tp-put-layer 1 6 'undefined-x 0 nil t)))
    (should (null (text-properties-at 1)))
    ;; A resolvable layer with NOERROR still applies normally.
    (should (tp-put-layer 1 6 'la 0 nil t))
    (should (eq (get-text-property 1 'tp-name) 'la))))

(ert-deftest tp-stack-test-push-layer-noerror-both-forms ()
  "NOERROR works for push in region and string forms."
  (tp-stack-tests--with-env
    (let ((str (copy-sequence "abcdef")))
      (define-tp la () '(face bold))
      (should-error (tp-push-layer str 'undefined-x))
      (should (null (tp-push-layer str 'undefined-x t)))
      (should (null (tp-put-layer str 'undefined-x 0 t)))
      (should (null (text-properties-at 0 str)))
      ;; The string form still returns the string on success.
      (should (eq (tp-push-layer str 'la t) str))
      (should (eq (get-text-property 0 'tp-name str) 'la)))
    (insert "abcdef")
    (should (null (tp-push-layer 1 6 'undefined-x nil t)))
    (should (null (text-properties-at 1)))))

;;; Multi-argument parameterized specs through tp-put-layer

(ert-deftest tp-stack-test-put-layer-multiarg-layer-flat ()
  "tp-put-layer accepts flat (LAYER ARG1 ARG2) for a 2-arity layer."
  (tp-layer-reset)
  (define-tp tp-st-colors (fg bg)
    `(face (:foreground ,fg :background ,bg)))
  (with-temp-buffer
    (insert "Hello")
    (tp-put-layer 1 5 '(tp-st-colors "red" "blue") 0)
    (should (equal (tp-at 1 'face)
                   '(:foreground "red" :background "blue")))))

(ert-deftest tp-stack-test-put-layer-multiarg-layer-wrapped ()
  "tp-put-layer accepts wrapped (LAYER (ARG1 ARG2)) for a 2-arity layer."
  (tp-layer-reset)
  (define-tp tp-st-colors2 (fg bg)
    `(face (:foreground ,fg :background ,bg)))
  (with-temp-buffer
    (insert "Hello")
    (tp-put-layer 1 5 '(tp-st-colors2 ("green" "black")) 0)
    (should (equal (tp-at 1 'face)
                   '(:foreground "green" :background "black")))))

(ert-deftest tp-stack-test-put-layer-multiarg-layer-symbol-args ()
  "Multi-arg specs are not misread as a list of layer names.
Arguments that are themselves defined layer names used to be
intercepted by the list-of-specs branch."
  (tp-layer-reset)
  (define-tp tp-st-a () '(help-echo "a"))
  (define-tp tp-st-b () '(help-echo "b"))
  (define-tp tp-st-pair (x y)
    `(display (,x . ,y)))
  (with-temp-buffer
    (insert "Hello")
    (tp-put-layer 1 5 '(tp-st-pair tp-st-a tp-st-b) 0)
    (should (equal (tp-at 1 'display) '(tp-st-a . tp-st-b)))
    (should (null (tp-at 1 'help-echo)))))

(ert-deftest tp-stack-test-put-layer-multiarg-group ()
  "tp-put-layer accepts (GROUP ARG1 ARG2) for a 2-arity group."
  (tp-layer-reset)
  (define-tps tp-st-duo (fg bg)
    `(face (:foreground ,fg))
    `(face (:background ,bg)))
  (with-temp-buffer
    (insert "Hello")
    (tp-put-layer 1 5 '(tp-st-duo "red" "blue") 0)
    (should (equal (tp-at 1 'face) '(:foreground "red")))
    (should (= (tp-layer-count 1 5) 2))))

(ert-deftest tp-stack-test-remove-multiarg-layer-by-name ()
  "tp-remove removes a multi-arg parameterized layer's props by name.
Applied via `tp-put-layer' so the region carries the layer's
`tp-name' (the `tp-set' plist forms do not stamp `tp-name' for
parameterized layers, so name-based removal cannot see those).
The key-extraction path must bind all parameters (dummy args),
not just the first."
  (tp-layer-reset)
  (define-tp tp-st-colors3 (fg bg)
    `(face (:foreground ,fg :background ,bg)))
  (with-temp-buffer
    (insert "Hello")
    (tp-put-layer 1 5 '(tp-st-colors3 "red" "blue") 0)
    (put-text-property 1 5 'help-echo "tip")
    (should (tp-at 1 'face))
    (should (eq (tp-at 1 'tp-name) 'tp-st-colors3))
    (tp-remove 1 5 'tp-st-colors3)
    (should (null (tp-at 1 'face)))
    (should (equal (tp-at 1 'help-echo) "tip"))))

(provide 'tp-stack-tests)
;;; tp-stack-tests.el ends here
