;;; tp-ert-tests.el --- ERT tests for tp.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive test suite for tp.el using ERT (Emacs Lisp Regression Testing).
;; Run with: emacs --batch -l tp.el -l tp-ert-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load tp.el from the same directory
(let ((tp-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path tp-dir)
  (require 'tp))

;;; ============================================================
;;; Test Utilities
;;; ============================================================

(defmacro tp-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with tp.el loaded."
  (declare (indent 0))
  `(with-temp-buffer
     (setq tp-layer-alist nil)
     (setq tp-layer-groups nil)
     ,@body))

;;; ============================================================
;;; Basic Text Property Functions Tests
;;; ============================================================

(ert-deftest tp-test-put-and-get ()
  "Test tp-set and tp-get basic functionality."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; Set a single property
    (tp-set 1 6 '(face bold))
    (should (eq (tp-at 1 'face) 'bold))
    (should (eq (tp-at 3 'face) 'bold))
    (should (null (tp-at 7 'face)))
    ;; Set multiple properties
    (tp-set 7 12 '(face italic help-echo "test"))
    (should (eq (tp-at 7 'face) 'italic))
    (should (equal (tp-at 7 'help-echo) "test"))))

(ert-deftest tp-test-put-with-list ()
  "Test tp-set accepts properties as a list."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "greeting"))
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "greeting"))))

(ert-deftest tp-test-put-returns-region ()
  "Test tp-set returns the modified region."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (let ((result (tp-set 1 6 '(face bold))))
      (should (equal result '(1 . 6))))))

(ert-deftest tp-test-remove ()
  "Test tp-remove removes a specific property."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    (should (eq (tp-at 1 'face) 'bold))
    (tp-remove 1 6 'face)
    (should (null (tp-at 1 'face)))
    (should (equal (tp-at 1 'help-echo) "test"))))

(ert-deftest tp-test-clear ()
  "Test tp-clear removes all properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    (tp-set 7 12 '(face italic))
    (tp-clear 1 12)
    (should (null (tp-at 1 'face)))
    (should (null (tp-at 7 'face)))))

(ert-deftest tp-test-clear-defaults-to-buffer ()
  "Test tp-clear defaults to entire buffer."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 12 '(face bold))
    (tp-clear)
    (should (null (tp-at 1 'face)))
    (should (null (tp-at 7 'face)))))

(ert-deftest tp-test-at ()
  "Test tp-at returns all properties at point."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    (let ((props (tp-at 1)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "test")))))

(ert-deftest tp-test-at-with-property ()
  "Test tp-at returns specific property at point."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "test"))
    (should (null (tp-at 1 'mouse-face)))))

(ert-deftest tp-test-at-with-object ()
  "Test tp-at with string object."
  (let ((str (copy-sequence "Hello World")))
    (tp-set 0 5 '(face bold help-echo "greeting") str)
    ;; All properties at position
    (let ((props (tp-at 0 str)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "greeting")))
    ;; Specific property at position
    (should (eq (tp-at 0 'face str) 'bold))
    (should (equal (tp-at 0 'help-echo str) "greeting"))))

(ert-deftest tp-test-at-with-nested-path ()
  "Test tp-at with nested property path."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (put-text-property 1 6 'face '(:foreground "red" :box (:color "blue" :line-width 2)))
    (should (equal (tp-at 1 '(face :foreground)) "red"))
    (should (equal (tp-at 1 '(face :box)) '(:color "blue" :line-width 2)))
    (should (equal (tp-at 1 '(face :box :color)) "blue"))
    (should (equal (tp-at 1 '(face :box :line-width)) 2))))

(ert-deftest tp-test-at-with-nested-path-on-string ()
  "Test tp-at with nested property path on string."
  (let ((str (copy-sequence "Hello World")))
    (put-text-property 0 5 'face '(:foreground "red" :underline (:style wave)) str)
    (should (equal (tp-at 0 '(face :foreground) str) "red"))
    (should (equal (tp-at 0 '(face :underline :style) str) 'wave))))

(ert-deftest tp-test-at-defaults-to-point ()
  "Test tp-at works with current point."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold))
    (goto-char 3)
    (should (eq (plist-get (tp-at (point)) 'face) 'bold))))

(ert-deftest tp-test-plist ()
  "Test tp-plist merges properties from region."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; Put both properties on the same overlapping region for proper merging
    (tp-set 1 12 '(face bold))
    (tp-set 1 12 '(help-echo "test"))
    (let ((props (tp-plist 1 12)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "test")))))

(ert-deftest tp-test-plist-on-string ()
  "Test tp-plist works on entire string."
  (let ((str (tp-set "Hello World" 'face 'bold 'help-echo "test")))
    (let ((props (tp-plist str)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "test")))))

(ert-deftest tp-test-plist-on-string-range ()
  "Test tp-plist works on string range with object parameter."
  (let ((str (copy-sequence "Hello World")))
    (tp-set 0 5 '(face bold) str)
    (tp-set 6 11 '(help-echo "test") str)
    (let ((props-start (tp-plist 0 5 str))
          (props-end (tp-plist 6 11 str)))
      (should (eq (plist-get props-start 'face) 'bold))
      (should (equal (plist-get props-end 'help-echo) "test")))))

;;; ============================================================
;;; Text Property Interval Tests
;;; ============================================================

(ert-deftest tp-test-empty-p ()
  "Test tp-empty-p detects empty properties."
  (should (tp-empty-p "plain string"))
  (should-not (tp-empty-p (propertize "styled" 'face 'bold))))

(ert-deftest tp-test-empty-p-with-nil ()
  "Test tp-empty-p with nil (current buffer)."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; Empty buffer (no properties)
    (should (tp-empty-p nil))
    (should (tp-empty-p))
    ;; Add properties
    (tp-set 1 6 '(face bold))
    (should-not (tp-empty-p nil))
    (should-not (tp-empty-p))))

(ert-deftest tp-test-empty-p-with-buffer ()
  "Test tp-empty-p with explicit buffer object."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (let ((buf (current-buffer)))
      ;; Empty (no properties)
      (should (tp-empty-p buf))
      ;; Add properties
      (tp-set 1 6 '(face bold))
      (should-not (tp-empty-p buf)))))

(ert-deftest tp-test-intervals ()
  "Test tp-intervals returns property intervals."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    (tp-set 7 12 '(face italic))
    (let ((intervals (tp-intervals 1 12)))
      (should (>= (length intervals) 2)))))

;;; ============================================================
;;; Layer Definition Tests
;;; ============================================================

(ert-deftest tp-test-define-layer ()
  "Test tp-define-layer creates a layer."
  (tp-test-with-temp-buffer
    (tp-define-layer test-layer (face bold help-echo "test"))
    (should (assoc 'test-layer tp-layer-alist))
    (should (equal (cdr (assoc 'test-layer tp-layer-alist))
                   '(face bold help-echo "test")))))

(ert-deftest tp-test-define-layer-updates-existing ()
  "Test tp-define-layer updates existing layer."
  (tp-test-with-temp-buffer
    (tp-define-layer test-layer (face bold))
    (tp-define-layer test-layer (face italic))
    (should (equal (cdr (assoc 'test-layer tp-layer-alist))
                   '(face italic)))))

(ert-deftest tp-test-layer-props ()
  "Test tp-layer-props returns properties with tp-name."
  (tp-test-with-temp-buffer
    (tp-define-layer my-layer (face bold))
    (let ((props (tp-layer-props 'my-layer)))
      (should (eq (plist-get props 'face) 'bold))
      (should (eq (plist-get props 'tp-name) 'my-layer)))))

(ert-deftest tp-test-layer-props-returns-nil-for-undefined ()
  "Test tp-layer-props returns nil for undefined layer."
  (tp-test-with-temp-buffer
    (should (null (tp-layer-props 'undefined-layer)))))

(ert-deftest tp-test-layer-undefine ()
  "Test tp-undefine-layer removes layer definition."
  (tp-test-with-temp-buffer
   (tp-define-layer test-layer (face bold))
   (should (assoc 'test-layer tp-layer-alist))
   (tp-undefine-layer 'test-layer)
   (should-not (assoc 'test-layer tp-layer-alist))))

;;; ============================================================
;;; Layer Group Tests (using tp-define-layer with multiple layers)
;;; ============================================================

(ert-deftest tp-test-define-layer-multiple ()
  "Test tp-define-layer creates a layer group with multiple layers."
  (tp-test-with-temp-buffer
    (tp-define-layer layer1 (face bold))
    (tp-define-layer my-group
      layer1
      (face italic)
      (face underline))
    (should (assoc 'my-group tp-layer-groups))
    ;; Check all layers are present in the group
    (let ((layers (cdr (assoc 'my-group tp-layer-groups))))
      (should (= (length layers) 3))
      (should (memq 'layer1 layers)))))

(ert-deftest tp-test-group-props ()
  "Test tp-group-props returns all layer properties."
  (tp-test-with-temp-buffer
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer my-group layer1 layer2)
    (let ((props-list (tp-group-props 'my-group)))
      (should (= (length props-list) 2))
      ;; Check that both layers are present
      (let ((faces (mapcar (lambda (p) (plist-get p 'face)) props-list)))
        (should (memq 'bold faces))
        (should (memq 'italic faces))))))

(ert-deftest tp-test-group-undefine ()
  "Test tp-undefine-group removes group definition."
  (tp-test-with-temp-buffer
   (tp-define-layer layer1 (face bold))
   (tp-define-layer my-group layer1)
   (should (assoc 'my-group tp-layer-groups))
   (tp-undefine-group 'my-group)
   (should-not (assoc 'my-group tp-layer-groups))))

(ert-deftest tp-test-layer-reset ()
  "Test tp-layer-reset clears all definitions."
  (tp-test-with-temp-buffer
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer group1 layer1 layer2)
    (should tp-layer-alist)
    (should tp-layer-groups)
    (tp-layer-reset)
    (should-not tp-layer-alist)
    (should-not tp-layer-groups)))

;;; ============================================================
;;; Layer Stack Operations Tests (New API)
;;; ============================================================

(ert-deftest tp-test-push-layer ()
  "Test tp-push-layer adds layer to stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-push-layer 1 6 'layer1)
    (should (eq (tp-at 1 'face) 'bold))
    (should (eq (tp-at 1 'tp-name) 'layer1))))

(ert-deftest tp-test-push-layer-multiple ()
  "Test pushing multiple layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; layer2 should be on top (visible)
    (should (eq (tp-at 1 'face) 'italic))
    (should (eq (tp-at 1 'tp-name) 'layer2))
    ;; layer1 should be in the stack below
    (should (tp-at 1 'tp-layers))))

(ert-deftest tp-test-delete-layer ()
  "Test tp-delete-layer removes layer from stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Delete top layer
    (tp-delete-layer 1 6 'layer2)
    ;; layer1 should now be visible
    (should (eq (tp-at 1 'face) 'bold))
    (should (eq (tp-at 1 'tp-name) 'layer1))))

(ert-deftest tp-test-delete-layer-from-middle ()
  "Test deleting layer from middle of stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Delete middle layer
    (tp-delete-layer 1 6 'layer2)
    ;; Top layer should still be visible
    (should (eq (tp-at 1 'tp-name) 'layer3))
    ;; layer2 should not exist anymore
    (should-not (tp-layer-exists-p 1 6 'layer2))))

(ert-deftest tp-test-pop-layer ()
  "Test tp-pop-layer removes top layer."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Pop top layer
    (tp-pop-layer 1 6)
    ;; layer1 should now be visible
    (should (eq (tp-at 1 'tp-name) 'layer1))))

(ert-deftest tp-test-rotate-layer ()
  "Test tp-rotate-layer cycles layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; layer3 is on top
    (should (eq (tp-layer-top 1 6) 'layer3))
    ;; Rotate once - layer2 should be on top
    (tp-rotate-layer 1 6)
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Rotate again - layer1 should be on top
    (tp-rotate-layer 1 6)
    (should (eq (tp-layer-top 1 6) 'layer1))
    ;; Rotate again - layer3 should be on top (cycled back)
    (tp-rotate-layer 1 6)
    (should (eq (tp-layer-top 1 6) 'layer3))))

(ert-deftest tp-test-pin-layer ()
  "Test tp-pin-layer brings layer to top."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Pin layer1 to top
    (tp-pin-layer 1 6 'layer1)
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-raise-layer ()
  "Test tp-raise-layer moves layer up."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; layer3 is at idx 0, layer2 at 1, layer1 at 2
    ;; Raise layer1 by 2 (move to top)
    (tp-raise-layer 1 6 'layer1 2)
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-switch-layer ()
  "Test tp-switch-layer swaps two layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; layer2 is on top
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Switch layer1 and layer2
    (tp-switch-layer 1 6 'layer1 'layer2)
    ;; layer1 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-put-layer-at-idx ()
  "Test tp-put-layer inserts layer at specified index."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Insert layer3 at index 1 (between layer2 and layer1)
    (tp-put-layer 1 6 'layer3 1)
    ;; layer2 should still be on top
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Should have 3 layers
    (should (= (tp-layer-count 1 6) 3))))

(ert-deftest tp-test-merge-layers ()
  "Test tp-merge-layers merges specified layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (help-echo "test"))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Merge layer1 and layer2 into merged-layer
    (tp-merge-layers 1 6 'merged-layer '(layer1 layer2))
    ;; Should have 1 layer now
    (should (= (tp-layer-count 1 6) 1))
    ;; The merged layer should have properties from both
    (should (eq (tp-at 1 'tp-name) 'merged-layer))))

(ert-deftest tp-test-flatten-layers ()
  "Test tp-flatten-layers flattens all layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (help-echo "test"))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Flatten all layers into flat-layer
    (tp-flatten-layers 1 6 'flat-layer)
    ;; Should have 1 layer now
    (should (= (tp-layer-count 1 6) 1))
    (should (eq (tp-at 1 'tp-name) 'flat-layer))))

;;; ============================================================
;;; Layer Query Tests
;;; ============================================================

(ert-deftest tp-test-layer-list ()
  "Test tp-layer-list returns all layer names."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    (let ((layers (tp-layer-list 1 6)))
      (should (= (length layers) 3))
      (should (memq 'layer1 layers))
      (should (memq 'layer2 layers))
      (should (memq 'layer3 layers)))))

(ert-deftest tp-test-layer-count ()
  "Test tp-layer-count returns correct count."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (should (= (tp-layer-count 1 6) 1))
    (tp-push-layer 1 6 'layer2)
    (should (= (tp-layer-count 1 6) 2))))

(ert-deftest tp-test-layer-exists-p ()
  "Test tp-layer-exists-p correctly detects layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-push-layer 1 6 'layer1)
    (should (tp-layer-exists-p 1 6 'layer1))
    (should-not (tp-layer-exists-p 1 6 'layer2))))

(ert-deftest tp-test-layer-top ()
  "Test tp-layer-top returns top layer name."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (should (eq (tp-layer-top 1 6) 'layer1))
    (tp-push-layer 1 6 'layer2)
    (should (eq (tp-layer-top 1 6) 'layer2))))

;;; ============================================================
;;; Match and Regexp Tests
;;; ============================================================

(ert-deftest tp-test-match-set ()
  "Test tp-match-set sets properties on string matches."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (let ((regions (tp-match-set "Hello" '(face bold))))
      (should (= (length regions) 2))
      (should (eq (tp-at 1 'face) 'bold))
      (should (eq (tp-at 13 'face) 'bold)))))

(ert-deftest tp-test-match-set-returns-regions ()
  "Test tp-match-set returns correct region pairs."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (let ((regions (tp-match-set "Hello" nil)))
      (should (= (length regions) 2))
      (should (equal (car regions) '(1 . 6)))
      (should (equal (cadr regions) '(13 . 18))))))

(ert-deftest tp-test-regexp-set ()
  "Test tp-regexp-set sets properties on regexp matches."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (let ((regions (tp-regexp-set "[0-9]+" '(face bold))))
      (should (= (length regions) 2))
      (should (eq (tp-at 5 'face) 'bold))
      (should (eq (tp-at 13 'face) 'bold)))))

(ert-deftest tp-test-regexp-set-returns-regions ()
  "Test tp-regexp-set returns correct region pairs."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (let ((regions (tp-regexp-set "[0-9]+" nil)))
      (should (= (length regions) 2)))))

;;; ============================================================
;;; Search and Navigation Tests
;;; ============================================================

(ert-deftest tp-test-forward ()
  "Test tp-forward finds next property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 7 12 '(face bold))
    (goto-char 1)
    ;; text-property-search-forward may not exist in all Emacs versions
    (skip-unless (fboundp 'text-property-search-forward))
    (let ((match (tp-forward 'face)))
      (should match)
      (should (= (prop-match-beginning match) 7)))))

(ert-deftest tp-test-forward-on-string ()
  "Test tp-forward works on string objects."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (let ((matches (tp-forward 'marker nil str 2)))
      (should (= (length matches) 2))
      (should (equal (car matches) '(0 5 t)))
      (should (equal (cadr matches) '(12 17 t))))))

(ert-deftest tp-test-forward-with-n ()
  "Test tp-forward with N parameter."
  (tp-test-with-temp-buffer
    (insert "Hello World Test Again")
    (tp-set 1 6 '(face bold))
    (tp-set 7 12 '(face italic))
    (tp-set 13 17 '(face bold))
    (goto-char 1)
    (skip-unless (fboundp 'text-property-search-forward))
    ;; Search twice should find third match
    (let ((match (tp-forward 'face nil nil 2)))
      (should match))))

(ert-deftest tp-test-backward ()
  "Test tp-backward finds previous property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    (goto-char 12)
    ;; text-property-search-backward may not exist in all Emacs versions
    ;; Skip test if function is not available
    (skip-unless (fboundp 'text-property-search-backward))
    (let ((match (tp-backward 'face)))
      (should match)
      (should (= (prop-match-beginning match) 1)))))

(ert-deftest tp-test-backward-on-string ()
  "Test tp-backward works on string objects."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (let ((matches (tp-backward 'marker nil str 2)))
      (should (= (length matches) 2))
      ;; Backward returns matches in reverse order
      (should (equal (car matches) '(12 17 t)))
      (should (equal (cadr matches) '(0 5 t))))))

;;; tp-forward-do / tp-backward-do tests (new API)

(ert-deftest tp-test-forward-do-on-string ()
  "Test tp-forward-do on string: applies function to the last match."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Search 2 times, function only applied to the last match
    (let ((count (tp-forward-do #'upcase 'marker nil str 2)))
      (should (= count 2))
      ;; First match should NOT be upcased
      (should (equal (substring str 0 5) "hello"))
      ;; Only the last (2nd) match should be upcased
      (should (equal (substring str 12 17) "HELLO")))))

(ert-deftest tp-test-forward-do-on-string-with-range ()
  "Test tp-forward-do on string with start/end range."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Search only in range 6-17 (after first match)
    (let ((count (tp-forward-do #'upcase 'marker nil str 2 6 17)))
      (should (= count 1))  ; Only one match in range 6-17
      ;; First match should NOT be upcased
      (should (equal (substring str 0 5) "hello"))
      ;; Second match should be upcased
      (should (equal (substring str 12 17) "HELLO")))))

(ert-deftest tp-test-forward-do-function-receives-start-end ()
  "Test tp-forward-do passes start and end to function."
  (let ((str (copy-sequence "hello World hello"))
        (starts nil)
        (ends nil))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Function accepts text, start, end
    (let ((count (tp-forward-do (lambda (txt start end)
                                  (push start starts)
                                  (push end ends)
                                  (upcase txt))
                                'marker nil str 2)))
      (should (= count 2))
      ;; Only the last match positions were passed to function
      (should (equal starts '(12)))
      (should (equal ends '(17)))
      ;; Only the last match should be upcased
      (should (equal (substring str 0 5) "hello"))
      (should (equal (substring str 12 17) "HELLO")))))

(ert-deftest tp-test-forward-do-single-arg-function ()
  "Test tp-forward-do with single-argument function."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Use #'upcase which only takes one argument
    (tp-forward-do #'upcase 'marker nil str 2)
    ;; Only the last match should be upcased
    (should (equal (substring str 0 5) "hello"))
    (should (equal (substring str 12 17) "HELLO"))))

(ert-deftest tp-test-backward-do-on-string ()
  "Test tp-backward-do on string: applies function to the last match."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Search backward 2 times, function only applied to the last match
    (let ((count (tp-backward-do #'upcase 'marker nil str 2)))
      (should (= count 2))
      ;; Only the last (2nd) match should be upcased (first in order)
      (should (equal (substring str 0 5) "HELLO"))
      ;; First match (searched backward) should NOT be upcased
      (should (equal (substring str 12 17) "hello")))))

(ert-deftest tp-test-backward-do-on-string-with-range ()
  "Test tp-backward-do on string with start/end range."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Search only in range 0-10 (before second match)
    (let ((count (tp-backward-do #'upcase 'marker nil str 2 0 10)))
      (should (= count 1))  ; Only one match in range 0-10
      ;; First match should be upcased
      (should (equal (substring str 0 5) "HELLO"))
      ;; Second match should NOT be upcased
      (should (equal (substring str 12 17) "hello")))))

(ert-deftest tp-test-backward-do-function-receives-start-end ()
  "Test tp-backward-do passes start and end to function."
  (let ((str (copy-sequence "hello World hello"))
        (starts nil)
        (ends nil))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Function accepts text, start, end
    (let ((count (tp-backward-do (lambda (txt start end)
                                   (push start starts)
                                   (push end ends)
                                   (upcase txt))
                                 'marker nil str 2)))
      (should (= count 2))
      ;; Only the last match positions were passed to function
      (should (equal starts '(0)))
      (should (equal ends '(5)))
      ;; Only the last match should be upcased
      (should (equal (substring str 0 5) "HELLO"))
      (should (equal (substring str 12 17) "hello")))))

(ert-deftest tp-test-backward-do-single-arg-function ()
  "Test tp-backward-do with single-argument function."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Use #'upcase which only takes one argument
    (tp-backward-do #'upcase 'marker nil str 2)
    ;; Only the last match should be upcased
    (should (equal (substring str 0 5) "HELLO"))
    (should (equal (substring str 12 17) "hello"))))

(ert-deftest tp-test-search-on-string ()
  "Test tp-search finds all matching properties in a string."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (let ((matches (tp-search str 'marker)))
      (should (= (length matches) 2))
      (should (equal (car matches) '(0 5 t)))
      (should (equal (cadr matches) '(12 17 t))))))

(ert-deftest tp-test-search-on-string-with-value ()
  "Test tp-search filters by value in a string."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(type heading) str)
    (tp-set 6 11 '(type paragraph) str)
    (tp-set 12 17 '(type heading) str)
    (let ((matches (tp-search str 'type 'heading)))
      (should (= (length matches) 2))
      (should (equal (caddr (car matches)) 'heading))
      (should (equal (caddr (cadr matches)) 'heading)))))

(ert-deftest tp-test-search-in-range ()
  "Test tp-search finds all matching properties in a buffer range."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(marker t))
    (tp-set 13 18 '(marker t))
    (let ((matches (tp-search 1 18 'marker)))
      (should (= (length matches) 2))
      (should (equal (car matches) '(1 6 t)))
      (should (equal (cadr matches) '(13 18 t))))))

(ert-deftest tp-test--search-do-on-string ()
  "Test tp--search-do applies function to all matches in a string (internal API)."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (let ((result nil))
      (tp--search-do
       (lambda (match obj)
         (push (car match) result))
       'marker nil str)
      (should (= (length result) 2))
      (should (member 0 result))
      (should (member 12 result)))))

(ert-deftest tp-test--search-do-in-range ()
  "Test tp--search-do applies function to all matches in a buffer range (internal API)."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(marker t))
    (tp-set 13 18 '(marker t))
    (let ((result nil))
      (tp--search-do
       (lambda (match obj)
         (push (car match) result))
       'marker nil nil 1 18)
      (should (= (length result) 2))
      (should (member 1 result))
      (should (member 13 result)))))

(ert-deftest tp-test-search-map-on-string ()
  "Test tp-search-map applies function to matched text in a string."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (let ((count (tp-search-map #'upcase 'marker nil str)))
      (should (= count 2))
      ;; Check that text was upcased
      (should (equal (substring str 0 5) "HELLO"))
      (should (equal (substring str 12 17) "HELLO")))))

(ert-deftest tp-test-search-map-in-range ()
  "Test tp-search-map applies function to matched text in a buffer range."
  (tp-test-with-temp-buffer
    (insert "hello World hello")
    (tp-set 1 6 '(marker t))
    (tp-set 13 18 '(marker t))
    (let ((count (tp-search-map #'upcase 'marker nil nil 1 18)))
      (should (= count 2))
      ;; Check that text was upcased
      (should (equal (buffer-substring 1 6) "HELLO"))
      (should (equal (buffer-substring 13 18) "HELLO")))))

(ert-deftest tp-test-search-map-property-modification ()
  "Test tp-search-map applies property modifications to matched text."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; First upcase the text
    (tp-search-map #'upcase 'marker nil str)
    ;; Then add face property
    (tp-search-map (lambda (txt)
                     (tp-add txt 'face '(:background "orange")))
                   'marker nil str)
    ;; Check text was upcased
    (should (equal (substring str 0 5) "HELLO"))
    (should (equal (substring str 12 17) "HELLO"))
    ;; Check face property was added
    (let ((props-0 (text-properties-at 0 str))
          (props-12 (text-properties-at 12 str)))
      (should (equal (plist-get (plist-get props-0 'face) :background) "orange"))
      (should (equal (plist-get (plist-get props-12 'face) :background) "orange")))))

(ert-deftest tp-test-search-map-with-start-end-idx ()
  "Test tp-search-map passes start, end, and index to function."
  (let ((str (copy-sequence "aaa bbb ccc"))
        (positions nil))
    (tp-set 0 3 '(marker t) str)
    (tp-set 4 7 '(marker t) str)
    (tp-set 8 11 '(marker t) str)
    ;; Use a function that accepts text, start, end, idx
    (tp-search-map (lambda (txt start end idx)
                     (push (list start end idx) positions)
                     (upcase txt))
                   'marker nil str)
    ;; Check positions and indices were passed in order (reversed due to push)
    (should (equal (reverse positions) '((0 3 0) (4 7 1) (8 11 2))))
    ;; Check text was transformed (uppercased)
    (should (equal (substring str 0 3) "AAA"))
    (should (equal (substring str 4 7) "BBB"))
    (should (equal (substring str 8 11) "CCC"))))

(ert-deftest tp-test-search-map-with-start-end-in-buffer ()
  "Test tp-search-map passes start and end to function in buffer range."
  (tp-test-with-temp-buffer
    (insert "aaa bbb ccc")
    (tp-set 1 4 '(marker t))
    (tp-set 5 8 '(marker t))
    (tp-set 9 12 '(marker t))
    (let ((positions nil))
      (tp-search-map (lambda (txt start end idx)
                       (push (list start end idx) positions)
                       (format "[%d]" idx))
                     'marker nil nil 1 12)
      ;; Check positions and indices were passed in order
      (should (equal (reverse positions) '((1 4 0) (5 8 1) (9 12 2))))
      ;; Check text was replaced with index markers
      (should (string-match-p "\\[0\\]" (buffer-string)))
      (should (string-match-p "\\[1\\]" (buffer-string)))
      (should (string-match-p "\\[2\\]" (buffer-string))))))

(ert-deftest tp-test-search-map-backward-compat ()
  "Test tp-search-map still works with single-argument functions."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t) str)
    ;; Use #'upcase which only takes one argument
    (tp-search-map #'upcase 'marker nil str)
    (should (equal (substring str 0 5) "HELLO"))))

(ert-deftest tp-test-search-map-with-range ()
  "Test tp-search-map with start and end range."
  (let ((str (copy-sequence "hello World hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    ;; Only search in range 0-10 (first match only)
    (let ((count (tp-search-map #'upcase 'marker nil str 0 10)))
      (should (= count 1))
      ;; First match should be upcased
      (should (equal (substring str 0 5) "HELLO"))
      ;; Second match should NOT be upcased
      (should (equal (substring str 12 17) "hello")))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

;; Tests for tp-search are in Search and Navigation Tests section above

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(ert-deftest tp-test-empty-region ()
  "Test operations on empty buffer."
  (tp-test-with-temp-buffer
   (should (null (tp-at 1)))
   (should (tp-empty-p))))

(ert-deftest tp-test-overlapping-regions ()
  "Test overlapping property regions."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 8 '(prop1 val1))
    (tp-set 5 12 '(prop2 val2))
    (should (eq (tp-at 1 'prop1) 'val1))
    (should (null (tp-at 1 'prop2)))
    (should (eq (tp-at 6 'prop1) 'val1))
    (should (eq (tp-at 6 'prop2) 'val2))
    (should (null (tp-at 10 'prop1)))
    (should (eq (tp-at 10 'prop2) 'val2))))

(ert-deftest tp-test-single-char-region ()
  "Test operations on single character."
  (tp-test-with-temp-buffer
    (insert "H")
    (tp-set 1 2 '(face bold))
    (should (eq (tp-at 1 'face) 'bold))))

(ert-deftest tp-test-layer-on-string ()
  "Test layer operations on string object."
  (let ((str (copy-sequence "Hello")))
    (set-text-properties 0 5 nil str)
    (should (tp-empty-p str))))

;;; ============================================================
;;; Object Parameter Support Tests
;;; ============================================================

(ert-deftest tp-test-put-on-string ()
  "Test tp-set works on string objects."
  (let ((str (copy-sequence "Hello World")))
    (tp-set 0 5 '(face bold) str)
    (should (eq (get-text-property 0 'face str) 'bold))
    (should (null (get-text-property 6 'face str)))))

(ert-deftest tp-test-put-on-string-returns-string ()
  "Test tp-set returns the modified string."
  (let* ((str (copy-sequence "Hello"))
         (result (tp-set 0 5 '(face bold) str)))
    (should (stringp result))
    (should (eq (get-text-property 0 'face result) 'bold))))

(ert-deftest tp-test-put-entire-string ()
  "Test tp-set applies to entire string with flat properties."
  (let* ((str (copy-sequence "Hello"))
         (result (tp-set str 'face 'bold 'help-echo "test")))
    (should (stringp result))
    (should (eq (get-text-property 0 'face result) 'bold))
    (should (equal (get-text-property 0 'help-echo result) "test"))
    (should (eq (get-text-property 4 'face result) 'bold))))

(ert-deftest tp-test-match-set-on-string ()
  "Test tp-match-set works on string objects."
  (let* ((str (copy-sequence "Hello World Hello"))
         (result (tp-match-set "Hello" '(face bold) str)))
    (should (stringp result))
    (should (eq (get-text-property 0 'face result) 'bold))
    (should (eq (get-text-property 12 'face result) 'bold))
    (should (null (get-text-property 6 'face result)))))

(ert-deftest tp-test-regexp-set-on-string ()
  "Test tp-regexp-set works on string objects."
  (let* ((str (copy-sequence "abc 123 def 456"))
         (result (tp-regexp-set "[0-9]+" '(face bold) str)))
    (should (stringp result))
    (should (eq (get-text-property 4 'face result) 'bold))
    (should (eq (get-text-property 12 'face result) 'bold))
    (should (null (get-text-property 0 'face result)))))

;;; ============================================================
;;; Enhanced tp-get Tests
;;; ============================================================

(ert-deftest tp-test-get-single-position ()
  "Test tp-get with single position."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold))
    (should (eq (tp-at 1 'face) 'bold))
    (should (eq (tp-at 3 'face) 'bold))))

(ert-deftest tp-test-get-range-property ()
  "Test tp-get with range and specific property.
Returns list of (START END VALUE) intervals."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    (should (equal (tp-get 1 6 'face) '((1 6 bold))))
    (should (null (tp-get 7 12 'face)))))

(ert-deftest tp-test-get-range-all-properties ()
  "Test tp-get with range returns all property intervals."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold help-echo "test"))
    (let ((intervals (tp-get 1 6)))
      (should (= (length intervals) 1))
      (let ((props (caddr (car intervals))))
        (should (eq (plist-get props 'face) 'bold))
        (should (equal (plist-get props 'help-echo) "test"))))))

(ert-deftest tp-test-get-range-on-string ()
  "Test tp-get with range on string object.
Returns list of (START END VALUE) intervals."
  (let ((str (copy-sequence "Hello World")))
    (tp-set 0 5 '(face bold) str)
    (should (equal (tp-get 0 5 'face str) '((0 5 bold))))
    (should (null (tp-get 6 11 'face str)))))

;;; ============================================================
;;; New API Tests (tp-reset, tp-set, tp-set-face, tp-set-display, tp-add)
;;; ============================================================

(ert-deftest tp-test-reset ()
  "Test tp-reset completely replaces all properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    ;; tp-reset should completely replace
    (tp-reset 1 6 '(mouse-face highlight))
    (should (eq (tp-at 1 'mouse-face) 'highlight))
    (should (null (tp-at 1 'face)))
    (should (null (tp-at 1 'help-echo)))))

(ert-deftest tp-test-reset-on-string ()
  "Test tp-reset on string."
  (let ((str (copy-sequence "Hello World")))
    (tp-set 0 5 '(face bold help-echo "test") str)
    (tp-reset 0 5 '(mouse-face highlight) str)
    (should (eq (get-text-property 0 'mouse-face str) 'highlight))
    (should (null (get-text-property 0 'face str)))))

(ert-deftest tp-test-reset-entire-string ()
  "Test tp-reset on entire string."
  (let* ((str (tp-set "Hello" 'face 'bold 'help-echo "test"))
         (result (tp-reset str 'mouse-face 'highlight)))
    (should (eq (get-text-property 0 'mouse-face result) 'highlight))
    (should (null (get-text-property 0 'face result)))))

(ert-deftest tp-test-set-preserves-other-properties ()
  "Test tp-set preserves unspecified properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    ;; tp-set should only replace specified properties
    (tp-set 1 6 '(face italic))
    (should (eq (tp-at 1 'face) 'italic))
    (should (equal (tp-at 1 'help-echo) "test"))))

(ert-deftest tp-test-add ()
  "Test tp-add adds/updates properties without replacing."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face bold help-echo "test"))
    (tp-add 1 6 '(mouse-face highlight))
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "test"))
    (should (eq (tp-at 1 'mouse-face) 'highlight))))

(ert-deftest tp-test-add-deep-merge ()
  "Test tp-add deeply merges nested properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-set 1 6 '(face (:foreground "red" :weight bold)))
    (tp-add 1 6 '(face (:background "blue")))
    (let ((face (tp-at 1 'face)))
      (should (equal (plist-get face :foreground) "red"))
      (should (eq (plist-get face :weight) 'bold))
      (should (equal (plist-get face :background) "blue")))))

(ert-deftest tp-test-add-on-string ()
  "Test tp-add on string."
  (let ((str (copy-sequence "Hello")))
    (tp-set 0 5 '(face bold) str)
    (tp-add 0 5 '(help-echo "test") str)
    (should (eq (get-text-property 0 'face str) 'bold))
    (should (equal (get-text-property 0 'help-echo str) "test"))))

;;; ============================================================
;;; Enhanced tp-at Tests
;;; ============================================================

(ert-deftest tp-test-at-nested-sub-property ()
  "Test tp-at with nested sub-properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (put-text-property 1 6 'face '(:foreground "red" :box (:color "blue" :line-width 2)))
    (should (equal (tp-at 1 '(face :foreground)) "red"))
    (should (equal (tp-at 1 '(face :box :color)) "blue"))
    (should (equal (tp-at 1 '(face :box :line-width)) 2))))

(ert-deftest tp-test-at-display-sub-property ()
  "Test tp-at with display sub-properties that are plists."
  (tp-test-with-temp-buffer
    (insert "Hello")
    ;; Use a plist-style display property
    (put-text-property 1 6 'display '(:height 1.5 :width 10))
    (should (equal (tp-at 1 '(display :height)) 1.5))
    (should (equal (tp-at 1 '(display :width)) 10))))

;;; ============================================================
;;; Enhanced tp-remove Tests
;;; ============================================================

(ert-deftest tp-test-remove-sub-property-with-path ()
  "Test tp-remove with sub-property path."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (put-text-property 1 6 'face '(:foreground "red" :underline (:style wave :color "blue")))
    ;; Remove just :underline from face
    (tp-remove 1 6 '(face :underline))
    (let ((face (tp-at 1 'face)))
      (should (equal (plist-get face :foreground) "red"))
      (should (null (plist-get face :underline))))))

(ert-deftest tp-test-remove-nested-sub-properties ()
  "Test tp-remove with nested sub-properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (put-text-property 1 6 'face '(:foreground "red" :underline (:style wave :position t :color "blue")))
    ;; Remove :style and :position from :underline, keep :color
    (tp-remove 1 6 '(face :underline (:style :position)))
    (let* ((face (tp-at 1 'face))
           (underline (plist-get face :underline)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get underline :color) "blue"))
      (should (null (plist-get underline :style)))
      (should (null (plist-get underline :position))))))

;;; ============================================================
;;; Match Pattern Format Tests
;;; ============================================================

(ert-deftest tp-test-match-set-multiple-patterns ()
  "Test tp-match-set with multiple patterns (list of patterns)."
  (tp-test-with-temp-buffer
    (insert "Hello world, Hello again")
    ;; Match both "world" and "Hello" - both should get properties applied
    (let ((regions (tp-match-set '("world" "Hello") '(face bold))))
      ;; Should find 3 matches: "Hello", "world", "Hello"
      (should (= (length regions) 3))
      ;; Check that "Hello" at position 1 has face bold
      (should (eq (tp-at 1 'face) 'bold))
      ;; Check that "world" at position 7 has face bold
      (should (eq (tp-at 7 'face) 'bold))
      ;; Check that "Hello" at position 14 has face bold
      (should (eq (tp-at 14 'face) 'bold)))))

(ert-deftest tp-test-match-set-multiple-patterns-on-string ()
  "Test tp-match-set with multiple patterns on string."
  (let* ((str (copy-sequence "Hello world, Hello again"))
         (result (tp-match-set '("world" "Hello") '(face bold) str)))
    (should (stringp result))
    ;; Check that "Hello" at position 0 has face bold
    (should (eq (get-text-property 0 'face result) 'bold))
    ;; Check that "world" at position 6 has face bold
    (should (eq (get-text-property 6 'face result) 'bold))
    ;; Check that "Hello" at position 13 has face bold
    (should (eq (get-text-property 13 'face result) 'bold))))

(ert-deftest tp-test-match-reset ()
  "Test tp-match-reset completely replaces properties."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(help-echo "original"))
    (tp-match-reset "Hello" '(face bold))
    (should (eq (tp-at 1 'face) 'bold))
    ;; Properties should be completely replaced
    (should (null (tp-at 1 'help-echo)))))

(ert-deftest tp-test-match-add ()
  "Test tp-match-add adds/updates properties."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(help-echo "original"))
    (tp-match-add "Hello" '(face bold))
    (should (eq (tp-at 1 'face) 'bold))
    ;; Original properties should be preserved
    (should (equal (tp-at 1 'help-echo) "original"))))

(ert-deftest tp-test-regexp-reset ()
  "Test tp-regexp-reset completely replaces properties."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (tp-set 5 8 '(help-echo "original"))
    (tp-regexp-reset "[0-9]+" '(face bold))
    (should (eq (tp-at 5 'face) 'bold))
    ;; Properties should be completely replaced
    (should (null (tp-at 5 'help-echo)))))

(ert-deftest tp-test-regexp-add ()
  "Test tp-regexp-add adds/updates properties."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (tp-set 5 8 '(help-echo "original"))
    (tp-regexp-add "[0-9]+" '(face bold))
    (should (eq (tp-at 5 'face) 'bold))
    ;; Original properties should be preserved
    (should (equal (tp-at 5 'help-echo) "original"))))

(ert-deftest tp-test-match-reset-on-string ()
  "Test tp-match-reset on string."
  (let* ((str (copy-sequence "Hello World Hello"))
         (result (tp-match-reset "Hello" '(face bold) str)))
    (should (eq (get-text-property 0 'face result) 'bold))
    (should (eq (get-text-property 12 'face result) 'bold))))

(ert-deftest tp-test-regexp-add-on-string ()
  "Test tp-regexp-add on string."
  (let ((str (copy-sequence "abc 123 def 456")))
    (tp-set 4 7 '(help-echo "original") str)
    (tp-regexp-add "[0-9]+" '(face bold) str)
    (should (eq (get-text-property 4 'face str) 'bold))
    (should (equal (get-text-property 4 'help-echo str) "original"))))

(ert-deftest tp-test-match-set-string-as-last-arg ()
  "Test tp-match-set with string as last argument."
  (let ((str (copy-sequence "Hello World Hello")))
    (let ((result (tp-match-set "Hello" '(face bold) str)))
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) 'bold))
      (should (eq (get-text-property 12 'face result) 'bold))
      (should (null (get-text-property 6 'face result))))))

(ert-deftest tp-test-regexp-set-string-as-last-arg ()
  "Test tp-regexp-set with string as last argument."
  (let ((str (copy-sequence "abc 123 def 456")))
    (let ((result (tp-regexp-set "[0-9]+" '(face italic) str)))
      (should (stringp result))
      (should (eq (get-text-property 4 'face result) 'italic))
      (should (eq (get-text-property 12 'face result) 'italic))
      (should (null (get-text-property 0 'face result))))))

(ert-deftest tp-test-regexp-set-multiple-patterns ()
  "Test tp-regexp-set with multiple patterns (list of regexps)."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456 ghi")
    ;; Match both numbers and "abc" - all should get properties applied
    (let ((regions (tp-regexp-set '("[0-9]+" "abc") '(face bold))))
      ;; Should find 3 matches: "abc", "123", "456"
      (should (= (length regions) 3))
      ;; Check that "abc" at position 1 has face bold
      (should (eq (tp-at 1 'face) 'bold))
      ;; Check that "123" at position 5 has face bold
      (should (eq (tp-at 5 'face) 'bold))
      ;; Check that "456" at position 13 has face bold
      (should (eq (tp-at 13 'face) 'bold))
      ;; Check that "def" does NOT have face bold
      (should (null (tp-at 9 'face))))))

(ert-deftest tp-test-regexp-set-multiple-patterns-on-string ()
  "Test tp-regexp-set with multiple patterns on string."
  (let* ((str (copy-sequence "abc 123 def 456"))
         (result (tp-regexp-set '("[0-9]+" "abc") '(face italic) str)))
    (should (stringp result))
    ;; Check that "abc" at position 0 has face italic
    (should (eq (get-text-property 0 'face result) 'italic))
    ;; Check that "123" at position 4 has face italic
    (should (eq (get-text-property 4 'face result) 'italic))
    ;; Check that "456" at position 12 has face italic
    (should (eq (get-text-property 12 'face result) 'italic))
    ;; Check that "def" does NOT have face italic
    (should (null (get-text-property 8 'face result)))))

(ert-deftest tp-test-get-range-multiple-intervals ()
  "Test tp-get returns all property intervals in a range."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(face bold) str)
    (tp-set 12 17 '(face italic) str)
    (let ((intervals (tp-get 0 17 'face str)))
      (should (= (length intervals) 2))
      (should (equal (car intervals) '(0 5 bold)))
      (should (equal (cadr intervals) '(12 17 italic))))))

;;; ============================================================
;;; New API Tests - Issue 1: tp-add face prepending
;;; ============================================================

(ert-deftest tp-test-add-face-prepend-symbol ()
  "Test tp-add prepends face symbol to existing face."
  (let ((str (copy-sequence "Hello")))
    (tp-set 0 5 '(face bold) str)
    (tp-add 0 5 '(face shadow) str)
    (let ((face (get-text-property 0 'face str)))
      ;; New face should be prepended, creating a list
      (should (equal face '(shadow bold))))))

(ert-deftest tp-test-add-face-prepend-to-list ()
  "Test tp-add prepends face to existing face list."
  (let ((str (copy-sequence "Hello")))
    (tp-set 0 5 '(face (bold italic)) str)
    (tp-add 0 5 '(face shadow) str)
    (let ((face (get-text-property 0 'face str)))
      ;; New face should be prepended
      (should (equal face '(shadow bold italic))))))

(ert-deftest tp-test-add-face-plist-merge ()
  "Test tp-add merges face plist with existing face."
  (let ((str (copy-sequence "Hello")))
    (tp-set 0 5 '(face (:foreground "red")) str)
    (tp-add 0 5 '(face (:background "blue")) str)
    (let ((face (get-text-property 0 'face str)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get face :background) "blue")))))

(ert-deftest tp-test-add-face-symbol-no-dup ()
  "Test tp-add doesn't duplicate faces."
  (let ((str (copy-sequence "Hello")))
    (tp-set 0 5 '(face bold) str)
    (tp-add 0 5 '(face bold) str)
    (let ((face (get-text-property 0 'face str)))
      ;; Should not duplicate
      (should (eq face 'bold)))))

;;; ============================================================
;;; New API Tests - Issue 2: tp-remove for strings
;;; ============================================================

(ert-deftest tp-test-remove-entire-string-single-prop ()
  "Test tp-remove removes single property from entire string."
  (let ((str (tp-set "Hello" 'face 'bold 'help-echo "test")))
    (tp-remove str 'face)
    (should (null (get-text-property 0 'face str)))
    (should (equal (get-text-property 0 'help-echo str) "test"))))

(ert-deftest tp-test-remove-entire-string-multiple-props ()
  "Test tp-remove removes multiple properties from entire string."
  (let ((str (tp-set "Hello" 'face 'bold 'help-echo "test" 'mouse-face 'highlight)))
    (tp-remove str 'face 'help-echo)
    (should (null (get-text-property 0 'face str)))
    (should (null (get-text-property 0 'help-echo str)))
    (should (eq (get-text-property 0 'mouse-face str) 'highlight))))

(ert-deftest tp-test-remove-entire-string-sub-prop ()
  "Test tp-remove removes sub-property from entire string."
  (let ((str (copy-sequence "Hello")))
    (put-text-property 0 5 'face '(:foreground "red" :underline t) str)
    (tp-remove str 'face :underline)
    (let ((face (get-text-property 0 'face str)))
      (should (equal (plist-get face :foreground) "red"))
      (should (null (plist-get face :underline))))))

(ert-deftest tp-test-remove-entire-string-nested-sub-prop ()
  "Test tp-remove removes nested sub-properties from entire string."
  (let ((str (copy-sequence "Hello")))
    (put-text-property 0 5 'face '(:foreground "red" :underline (:style wave :color "blue")) str)
    (tp-remove str 'face :underline '(:style))
    (let* ((face (get-text-property 0 'face str))
           (underline (plist-get face :underline)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get underline :color) "blue"))
      (should (null (plist-get underline :style))))))

(ert-deftest tp-test-remove-entire-string-single-nested-key ()
  "Test tp-remove removes a single nested key from a sub-property.
This tests the fix for the bug where (tp-remove str 'face :underline :position)
was removing the entire :underline instead of just :position."
  (let ((str (copy-sequence "happy hacking emacs")))
    (tp-set str 'face '(:foreground "red" :underline (:position t :color "green"))
            'line-prefix ">> " 'other "other")
    (tp-remove str 'face :underline :position)
    (let* ((face (get-text-property 0 'face str))
           (underline (plist-get face :underline)))
      ;; :foreground should be preserved
      (should (equal (plist-get face :foreground) "red"))
      ;; :underline should still exist but without :position
      (should underline)
      (should (equal (plist-get underline :color) "green"))
      (should (null (plist-get underline :position)))
      ;; Other properties should be preserved
      (should (equal (get-text-property 0 'line-prefix str) ">> "))
      (should (equal (get-text-property 0 'other str) "other")))))

;;; ============================================================
;;; New API Tests - Issue 3 & 4: tp-get for strings and new API
;;; ============================================================

(ert-deftest tp-test-get-entire-string-all-props ()
  "Test tp-get returns all property intervals from entire string."
  (let ((str (tp-set "Hello" 'face 'bold 'help-echo "test")))
    (let ((intervals (tp-get str)))
      (should (= (length intervals) 1))
      (let ((props (caddr (car intervals))))
        (should (eq (plist-get props 'face) 'bold))
        (should (equal (plist-get props 'help-echo) "test"))))))

(ert-deftest tp-test-get-entire-string-single-prop ()
  "Test tp-get returns single property intervals from entire string."
  (let ((str (tp-set "Hello" 'face 'bold 'help-echo "test")))
    (let ((face-intervals (tp-get str 'face))
          (help-intervals (tp-get str 'help-echo)))
      (should (= (length face-intervals) 1))
      (should (eq (caddr (car face-intervals)) 'bold))
      (should (= (length help-intervals) 1))
      (should (equal (caddr (car help-intervals)) "test")))))

(ert-deftest tp-test-get-entire-string-nested-prop ()
  "Test tp-get returns nested property intervals from entire string."
  (let ((str (copy-sequence "Hello")))
    (put-text-property 0 5 'face '(:foreground "red" :box (:color "blue" :line-width 2)) str)
    (let ((fg-intervals (tp-get str 'face :foreground))
          (box-color-intervals (tp-get str 'face :box :color))
          (box-width-intervals (tp-get str 'face :box :line-width)))
      (should (= (length fg-intervals) 1))
      (should (equal (caddr (car fg-intervals)) "red"))
      (should (= (length box-color-intervals) 1))
      (should (equal (caddr (car box-color-intervals)) "blue"))
      (should (= (length box-width-intervals) 1))
      (should (equal (caddr (car box-width-intervals)) 2)))))

(ert-deftest tp-test-get-range-with-list-prop-path ()
  "Test tp-get with property path as list.
Returns list of (START END VALUE) intervals."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (put-text-property 1 6 'face '(:foreground "red" :underline (:style wave)) nil)
    ;; Get with list path - returns intervals
    (should (equal (tp-get 1 6 '(face)) '((1 6 (:foreground "red" :underline (:style wave))))))
    (should (equal (tp-get 1 6 '(face :foreground)) '((1 6 "red"))))
    (should (equal (tp-get 1 6 '(face :underline :style)) '((1 6 wave))))))

(ert-deftest tp-test-get-range-with-list-prop-path-on-string ()
  "Test tp-get with property path as list on string.
Returns list of (START END VALUE) intervals."
  (let ((str (copy-sequence "Hello World")))
    (put-text-property 0 5 'face '(:foreground "red" :underline (:style wave)) str)
    ;; Get with list path and object - returns intervals
    (should (equal (tp-get 0 5 '(face) str) '((0 5 (:foreground "red" :underline (:style wave))))))
    (should (equal (tp-get 0 5 '(face :foreground) str) '((0 5 "red"))))
    (should (equal (tp-get 0 5 '(face :underline :style) str) '((0 5 wave))))))

(ert-deftest tp-test-get-entire-string-with-list-prop-path ()
  "Test tp-get with property path as list on entire string.
Returns list of (START END VALUE) intervals."
  (let ((str (copy-sequence "Hello World Hello")))
    (put-text-property 0 5 'face '(:foreground "red") str)
    (put-text-property 12 17 'face '(:foreground "blue") str)
    ;; Get with list path for entire string
    (let ((intervals (tp-get str '(face :foreground))))
      (should (= (length intervals) 2))
      (should (equal (car intervals) '(0 5 "red")))
      (should (equal (cadr intervals) '(12 17 "blue"))))))

(ert-deftest tp-test-get-entire-string-multiple-intervals ()
  "Test tp-get returns multiple intervals from entire string."
  (let ((str (copy-sequence "Hello World Hello")))
    (tp-set 0 5 '(face bold) str)
    (tp-set 12 17 '(face italic) str)
    (let ((intervals (tp-get str 'face)))
      (should (= (length intervals) 2))
      (should (equal (car intervals) '(0 5 bold)))
      (should (equal (cadr intervals) '(12 17 italic))))))

(ert-deftest tp-test-get-deeply-nested-property ()
  "Test tp-get with deeply nested property path."
  (let ((str (copy-sequence "Hello World")))
    (put-text-property 0 5 'face '(:foreground "red" :underline (:color "green" :style wave)) str)
    (put-text-property 6 11 'face '(:foreground "blue" :underline (:color "yellow" :style line)) str)
    ;; Test deeply nested single key from entire string
    (let ((intervals (tp-get str 'face :underline :color)))
      (should (= (length intervals) 2))
      (should (equal (caddr (car intervals)) "green"))
      (should (equal (caddr (cadr intervals)) "yellow")))
    ;; Test range with deeply nested key
    (let ((intervals (tp-get 0 7 '(face :underline :color) str)))
      (should (= (length intervals) 2))
      (should (equal (caddr (car intervals)) "green")))))

(ert-deftest tp-test-get-multiple-nested-keys ()
  "Test tp-get with multiple keys from nested property."
  (let ((str (copy-sequence "Hello World")))
    (put-text-property 0 5 'face '(:foreground "red" :underline (:color "green" :style wave)) str)
    (put-text-property 6 11 'face '(:foreground "blue" :underline (:color "yellow" :style line)) str)
    ;; Test extracting multiple keys from entire string
    (let ((intervals (tp-get str 'face :underline '(:color :style))))
      (should (= (length intervals) 2))
      (let ((val1 (caddr (car intervals)))
            (val2 (caddr (cadr intervals))))
        (should (equal (plist-get val1 :color) "green"))
        (should (eq (plist-get val1 :style) 'wave))
        (should (equal (plist-get val2 :color) "yellow"))
        (should (eq (plist-get val2 :style) 'line))))
    ;; Test range with multiple keys
    (let ((intervals (tp-get 0 7 '(face :underline (:color :style)) str)))
      (should (= (length intervals) 2)))))

;;; ============================================================
;;; tp-add-to-layers and tp-add-to-all-layers Tests
;;; ============================================================

(ert-deftest tp-test-add-to-layers-buffer ()
  "Test tp-add-to-layers adds properties to specified layers in buffer."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Add help-echo to layer1 and layer3
    (tp-add-to-layers '(layer1 layer3) 1 6 '(help-echo "test"))
    ;; layer3 is on top, should have help-echo
    (should (equal (tp-at 1 'help-echo) "test"))
    ;; Check layer1 also got help-echo
    (let ((layer1-props (car (tp-region-layer-props 1 6 'layer1))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "test")))
    ;; layer2 should NOT have help-echo
    (let ((layer2-props (car (tp-region-layer-props 1 6 'layer2))))
      (should (null (plist-get (caddr layer2-props) 'help-echo))))))

(ert-deftest tp-test-add-to-layers-by-index ()
  "Test tp-add-to-layers with layer indices."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Stack is: layer3 (0), layer2 (1), layer1 (2)
    ;; Add help-echo to indices 0 and 2 (layer3 and layer1)
    (tp-add-to-layers '(0 2) 1 6 '(help-echo "indexed"))
    ;; layer3 (top) should have help-echo
    (should (equal (tp-at 1 'help-echo) "indexed"))
    ;; Check layer1 also got help-echo
    (let ((layer1-props (car (tp-region-layer-props 1 6 'layer1))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "indexed")))
    ;; layer2 (index 1) should NOT have help-echo
    (let ((layer2-props (car (tp-region-layer-props 1 6 'layer2))))
      (should (null (plist-get (caddr layer2-props) 'help-echo))))))

(ert-deftest tp-test-add-to-layers-string ()
  "Test tp-add-to-layers works on entire string."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer str 'layer1)
    (tp-push-layer str 'layer2)
    ;; Add help-echo to layer1
    (tp-add-to-layers '(layer1) str 'help-echo "test")
    ;; Check layer1 got help-echo
    (let ((layer1-props (car (tp-region-layer-props 0 5 'layer1 str))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "test")))
    ;; layer2 (top) should NOT have help-echo
    (should (null (tp-at 0 'help-echo str)))))

(ert-deftest tp-test-add-to-layers-deep-merge ()
  "Test tp-add-to-layers deeply merges properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face (:foreground "red")))
    (tp-push-layer 1 6 'layer1)
    ;; Add background to layer1 - should merge with existing face
    (tp-add-to-layers '(layer1) 1 6 '(face (:background "blue")))
    (let ((face (tp-at 1 'face)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get face :background) "blue")))))

(ert-deftest tp-test-add-to-all-layers-buffer ()
  "Test tp-add-to-all-layers adds properties to all layers in buffer."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-define-layer layer3 (face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Add help-echo to all layers
    (tp-add-to-all-layers 1 6 '(help-echo "all"))
    ;; layer3 (top) should have help-echo
    (should (equal (tp-at 1 'help-echo) "all"))
    ;; Check all layers got help-echo
    (let ((layer1-props (car (tp-region-layer-props 1 6 'layer1)))
          (layer2-props (car (tp-region-layer-props 1 6 'layer2)))
          (layer3-props (car (tp-region-layer-props 1 6 'layer3))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "all"))
      (should (equal (plist-get (caddr layer2-props) 'help-echo) "all"))
      (should (equal (plist-get (caddr layer3-props) 'help-echo) "all")))))

(ert-deftest tp-test-add-to-all-layers-string ()
  "Test tp-add-to-all-layers works on entire string."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer str 'layer1)
    (tp-push-layer str 'layer2)
    ;; Add help-echo to all layers
    (tp-add-to-all-layers str 'help-echo "all")
    ;; Check all layers got help-echo
    (let ((layer1-props (car (tp-region-layer-props 0 5 'layer1 str)))
          (layer2-props (car (tp-region-layer-props 0 5 'layer2 str))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "all"))
      (should (equal (plist-get (caddr layer2-props) 'help-echo) "all")))))

(ert-deftest tp-test-add-to-all-layers-deep-merge ()
  "Test tp-add-to-all-layers deeply merges properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face (:foreground "red")))
    (tp-define-layer layer2 (face (:foreground "blue")))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Add background to all layers
    (tp-add-to-all-layers 1 6 '(face (:background "green")))
    ;; Top layer (layer2) should have merged face
    (let ((face (tp-at 1 'face)))
      (should (equal (plist-get face :foreground) "blue"))
      (should (equal (plist-get face :background) "green")))
    ;; layer1 should also have merged face
    (let* ((layer1-props (car (tp-region-layer-props 1 6 'layer1)))
           (face (plist-get (caddr layer1-props) 'face)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get face :background) "green")))))

(ert-deftest tp-test-add-to-layers-negative-index ()
  "Test tp-add-to-layers with negative index (-1 means bottom)."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-define-layer layer1 (face bold))
    (tp-define-layer layer2 (face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; Stack is: layer2 (0), layer1 (1)
    ;; Add help-echo to index -1 (bottom = layer1)
    (tp-add-to-layers '(-1) 1 6 '(help-echo "bottom"))
    ;; layer2 (top) should NOT have help-echo
    (should (null (tp-at 1 'help-echo)))
    ;; layer1 (bottom) should have help-echo
    (let ((layer1-props (car (tp-region-layer-props 1 6 'layer1))))
      (should (equal (plist-get (caddr layer1-props) 'help-echo) "bottom")))))

(ert-deftest tp-test-add-to-layers-returns-string ()
  "Test tp-add-to-layers returns the modified string."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (tp-define-layer layer1 (face bold))
    (tp-push-layer str 'layer1)
    (let ((result (tp-add-to-layers '(layer1) str 'help-echo "test")))
      (should (stringp result))
      (should (eq result str)))))

(ert-deftest tp-test-add-to-all-layers-returns-string ()
  "Test tp-add-to-all-layers returns the modified string."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (tp-define-layer layer1 (face bold))
    (tp-push-layer str 'layer1)
    (let ((result (tp-add-to-all-layers str 'help-echo "test")))
      (should (stringp result))
      (should (eq result str)))))

(provide 'tp-ert-tests)
;;; tp-ert-tests.el ends here
