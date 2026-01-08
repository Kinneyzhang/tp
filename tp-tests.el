;;; tp-ert-tests.el --- ERT tests for tp.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; Comprehensive test suite for tp.el using ERT (Emacs Lisp Regression Testing).
;; Run with: emacs --batch -l tp.el -l tp-ert-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'tp-palette)

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
     (tp-reactive-reset)
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
;;; Layer Definition Tests (using define-tp)
;;; ============================================================

(ert-deftest tp-test-layer-props ()
  "Test tp-layer-props returns properties, and tp-name when requested."
  (tp-test-with-temp-buffer
    (define-tp my-layer ()
      '(face bold))
    ;; Without tp-name (default for direct property setting)
    (let ((props (tp-layer-props 'my-layer)))
      (should (eq (plist-get props 'face) 'bold))
      (should-not (plist-get props 'tp-name)))
    ;; With tp-name (for layer stack functions)
    (let ((props (tp-layer-props 'my-layer t)))
      (should (eq (plist-get props 'face) 'bold))
      (should (eq (plist-get props 'tp-name) 'my-layer)))))

(ert-deftest tp-test-layer-props-returns-nil-for-undefined ()
  "Test tp-layer-props returns nil for undefined layer."
  (tp-test-with-temp-buffer
    (should (null (tp-layer-props 'undefined-layer)))))

(ert-deftest tp-test-layer-undefine ()
  "Test tp-undefine-layer removes layer definition."
  (tp-test-with-temp-buffer
   (define-tp test-layer ()
     '(face bold))
   (should (assoc 'test-layer tp-layer-alist))
   (tp-undefine-layer 'test-layer)
   (should-not (assoc 'test-layer tp-layer-alist))))

;;; ============================================================
;;; Layer Group Tests (using define-tps)
;;; ============================================================

(ert-deftest tp-test-group-props ()
  "Test tp-group-props returns all layer properties."
  (tp-test-with-temp-buffer
    (define-tp layer1 ()
      '(face bold))
    (define-tp layer2 ()
      '(face italic))
    (define-tps my-group ()
      'layer1
      'layer2)
    (let ((props-list (tp-group-props 'my-group)))
      (should (= (length props-list) 2))
      ;; Check that both layers are present
      (let ((faces (mapcar (lambda (p) (plist-get p 'face)) props-list)))
        (should (memq 'bold faces))
        (should (memq 'italic faces))))))

(ert-deftest tp-test-group-undefine ()
  "Test tp-undefine-group removes group definition."
  (tp-test-with-temp-buffer
   (define-tp layer1 ()
     '(face bold))
   (define-tps my-group ()
     'layer1)
   (should (assoc 'my-group tp-layer-groups))
   (tp-undefine-group 'my-group)
   (should-not (assoc 'my-group tp-layer-groups))))

(ert-deftest tp-test-layer-reset ()
  "Test tp-layer-reset clears all definitions."
  (tp-test-with-temp-buffer
    (define-tp layer1 ()
      '(face bold))
    (define-tp layer2 ()
      '(face italic))
    (define-tps group1 ()
      'layer1
      'layer2)
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
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 6 'layer1)
    (should (eq (tp-at 1 'face) 'bold))
    (should (eq (tp-at 1 'tp-name) 'layer1))))

(ert-deftest tp-test-push-layer-multiple ()
  "Test pushing multiple layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    ;; layer2 is on top
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Switch layer1 and layer2
    (tp-switch-layer 1 6 'layer1 'layer2)
    ;; layer1 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-move-layer-by-index ()
  "Test tp-move-layer moves layer by index."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Stack: layer3 (0), layer2 (1), layer1 (2)
    (should (eq (tp-layer-top 1 6) 'layer3))
    ;; Move layer at index 2 (layer1) to index 0 (top)
    (tp-move-layer 1 6 2 0)
    ;; layer1 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-move-layer-by-name ()
  "Test tp-move-layer moves layer by name."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Stack: layer3 (0), layer2 (1), layer1 (2)
    (should (eq (tp-layer-top 1 6) 'layer3))
    ;; Move layer1 to index 0 (top)
    (tp-move-layer 1 6 'layer1 0)
    ;; layer1 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-move-layer-negative-index ()
  "Test tp-move-layer with negative indices."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
    (tp-push-layer 1 6 'layer1)
    (tp-push-layer 1 6 'layer2)
    (tp-push-layer 1 6 'layer3)
    ;; Stack: layer3 (0/-3), layer2 (1/-2), layer1 (2/-1)
    (should (eq (tp-layer-top 1 6) 'layer3))
    ;; Move top layer (0) to bottom (-1)
    (tp-move-layer 1 6 0 -1)
    ;; layer2 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer2))))

(ert-deftest tp-test-move-layer-on-string ()
  "Test tp-move-layer works on strings."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer str 'layer1)
    (tp-push-layer str 'layer2)
    ;; layer2 is on top
    (should (eq (tp-at 0 'tp-name str) 'layer2))
    ;; Move layer1 to top
    (tp-move-layer str 'layer1 0)
    ;; layer1 should now be on top
    (should (eq (tp-at 0 'tp-name str) 'layer1))))

(ert-deftest tp-test-put-layer-at-idx ()
  "Test tp-put-layer inserts layer at specified index."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(help-echo "test"))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(help-echo "test"))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (tp-push-layer 1 6 'layer1)
    (should (= (tp-layer-count 1 6) 1))
    (tp-push-layer 1 6 'layer2)
    (should (= (tp-layer-count 1 6) 2))))

(ert-deftest tp-test-layer-exists-p ()
  "Test tp-layer-exists-p correctly detects layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (tp-push-layer 1 6 'layer1)
    (should (tp-layer-exists-p 1 6 'layer1))
    (should-not (tp-layer-exists-p 1 6 'layer2))))

(ert-deftest tp-test-layer-top ()
  "Test tp-layer-top returns top layer name."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (let ((match (tp-forward 'face)))
      (should match)
      (should (= (prop-match-end match) 7)))))

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
    ;; Search twice should find third match
    (let ((match (tp-forward 'face nil nil 2)))
      (should match))))

(ert-deftest tp-test-backward ()
  "Test tp-backward finds previous property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    (goto-char 12)
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

(ert-deftest tp-test-add-face-subprop-override ()
  "Test tp-add correctly merges face sub-properties.
Later values should override earlier values for the same sub-property."
  ;; The original issue: (tp-add (tp-add (tp-set \"emacs\" 'face 'bold) 
  ;;   'face '(:foreground \"red\")) 'face '(bold (:foreground \"green\")))
  ;; should result in :foreground \"green\", not both \"red\" and \"green\"
  (let* ((base (tp-set "emacs" 'face 'bold))
         (with-red (tp-add base 'face '(:foreground "red")))
         (with-green (tp-add with-red 'face '(bold (:foreground "green")))))
    ;; Final result should have only one :foreground which is "green"
    (let ((face3 (get-text-property 0 'face with-green)))
      (should (listp face3))
      (should (member 'bold face3))
      ;; Extract the plist part
      (let ((plist-part (cl-find-if (lambda (f)
                                      (and (listp f) (keywordp (car-safe f))))
                                    face3)))
        (should plist-part)
        (should (equal (plist-get plist-part :foreground) "green"))
        ;; Ensure there's no duplicate :foreground
        (let ((plist-count (cl-count-if (lambda (f)
                                          (and (listp f) (keywordp (car-safe f))))
                                        face3)))
          (should (= plist-count 1)))))))

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
  "Test tp-regexp-add on string.
For strings, returns a NEW string (original is not modified)."
  (let ((str (copy-sequence "abc 123 def 456")))
    (tp-set 4 7 '(help-echo "original") str)
    (let ((result (tp-regexp-add "[0-9]+" '(face bold) str)))
      ;; Result should have both properties (face added, help-echo preserved)
      (should (eq (get-text-property 4 'face result) 'bold))
      (should (equal (get-text-property 4 'help-echo result) "original"))
      ;; Original should NOT have face property added by tp-regexp-add
      (should (null (get-text-property 4 'face str))))))

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
  (let* ((str (tp-set "Hello" 'face 'bold 'help-echo "test"))
         (result (tp-remove str 'face)))
    (should (null (get-text-property 0 'face result)))
    (should (equal (get-text-property 0 'help-echo result) "test"))))

(ert-deftest tp-test-remove-entire-string-multiple-props ()
  "Test tp-remove removes multiple properties from entire string."
  (let* ((str (tp-set "Hello" 'face 'bold 'help-echo "test" 'mouse-face 'highlight))
         (result (tp-remove str 'face 'help-echo)))
    (should (null (get-text-property 0 'face result)))
    (should (null (get-text-property 0 'help-echo result)))
    (should (eq (get-text-property 0 'mouse-face result) 'highlight))))

(ert-deftest tp-test-remove-entire-string-sub-prop ()
  "Test tp-remove removes sub-property from entire string."
  (let* ((str (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'face '(:foreground "red" :underline t) str))
         (result (tp-remove str 'face :underline)))
    (let ((face (get-text-property 0 'face result)))
      (should (equal (plist-get face :foreground) "red"))
      (should (null (plist-get face :underline))))))

(ert-deftest tp-test-remove-entire-string-nested-sub-prop ()
  "Test tp-remove removes nested sub-properties from entire string."
  (let* ((str (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'face '(:foreground "red" :underline (:style wave :color "blue")) str))
         (result (tp-remove str 'face :underline '(:style))))
    (let* ((face (get-text-property 0 'face result))
           (underline (plist-get face :underline)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get underline :color) "blue"))
      (should (null (plist-get underline :style))))))

(ert-deftest tp-test-remove-entire-string-single-nested-key ()
  "Test tp-remove removes a single nested key from a sub-property.
This tests the fix for the bug where (tp-remove str 'face :underline :position)
was removing the entire :underline instead of just :position."
  (let* ((str (tp-set "happy hacking emacs"
                      'face '(:foreground "red" :underline (:position t :color "green"))
                      'line-prefix ">> " 'other "other"))
         (result (tp-remove str 'face :underline :position)))
    (let* ((face (get-text-property 0 'face result))
           (underline (plist-get face :underline)))
      ;; :foreground should be preserved
      (should (equal (plist-get face :foreground) "red"))
      ;; :underline should still exist but without :position
      (should underline)
      (should (equal (plist-get underline :color) "green"))
      (should (null (plist-get underline :position)))
      ;; Other properties should be preserved
      (should (equal (get-text-property 0 'line-prefix result) ">> "))
      (should (equal (get-text-property 0 'other result) "other")))))

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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face (:foreground "red")))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
    (define-tp layer3 () '(face underline))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face (:foreground "red")))
    (define-tp layer2 () '(face (:foreground "blue")))
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
    (define-tp layer1 () '(face bold))
    (define-tp layer2 () '(face italic))
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
    (define-tp layer1 () '(face bold))
    (tp-push-layer str 'layer1)
    (let ((result (tp-add-to-layers '(layer1) str 'help-echo "test")))
      (should (stringp result))
      (should (eq result str)))))

(ert-deftest tp-test-add-to-all-layers-returns-string ()
  "Test tp-add-to-all-layers returns the modified string."
  (let ((str (copy-sequence "Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (define-tp layer1 () '(face bold))
    (tp-push-layer str 'layer1)
    (let ((result (tp-add-to-all-layers str 'help-echo "test")))
      (should (stringp result))
      (should (eq result str)))))

;;; ============================================================
;;; Reactive Text Properties Tests
;;; ============================================================

(ert-deftest tp-test-reactive-symbol-p ()
  "Test tp--reactive-symbol-p detects $-prefixed symbols."
  (should (tp--reactive-symbol-p '$foo))
  (should (tp--reactive-symbol-p '$my-color))
  (should-not (tp--reactive-symbol-p 'foo))
  (should-not (tp--reactive-symbol-p "string"))
  (should-not (tp--reactive-symbol-p 42)))

(ert-deftest tp-test-reactive-var-symbol ()
  "Test tp--reactive-var-symbol converts $foo to foo."
  (should (eq (tp--reactive-var-symbol '$foo) 'foo))
  (should (eq (tp--reactive-var-symbol '$my-color) 'my-color))
  (should (null (tp--reactive-var-symbol 'foo)))
  (should (null (tp--reactive-var-symbol "string"))))

(ert-deftest tp-test-collect-reactive-symbols ()
  "Test tp--collect-reactive-symbols finds all $-prefixed symbols."
  (should (equal (tp--collect-reactive-symbols '$foo) '($foo)))
  (should (equal (tp--collect-reactive-symbols '(face (:foreground $color)))
                 '($color)))
  (should (equal (tp--collect-reactive-symbols '(face (:foreground $color :background $bg)))
                 '($color $bg)))
  (should (null (tp--collect-reactive-symbols '(face bold)))))

(ert-deftest tp-test-extract-reactive-props ()
  "Test tp--extract-reactive-props extracts only properties using a reactive var."
  ;; Single reactive property
  (should (equal (tp--extract-reactive-props '(help-echo "test" face (:foreground $color)) '$color)
                 '(face (:foreground $color))))
  ;; Multiple properties, only one uses the variable - should extract only reactive sub-props
  (should (equal (tp--extract-reactive-props '(help-echo "test" face (:foreground $color :background "green")) '$color)
                 '(face (:foreground $color))))
  ;; Nested plist with reactive variable - should extract only reactive nested sub-props
  (should (equal (tp--extract-reactive-props
                  '(face (:foreground $color1 :underline (:style wave :color $color2 :position t)))
                  '$color2)
                 '(face (:underline (:color $color2)))))
  ;; No properties use the variable
  (should (null (tp--extract-reactive-props '(help-echo "test" face bold) '$color))))

(ert-deftest tp-test-resolve-reactive-symbols ()
  "Test tp--resolve-reactive-symbols replaces $foo with variable values."
  ;; Use defvar to create dynamically-bound variables
  (defvar tp-test-my-color "red" "Test color variable.")
  (defvar tp-test-my-bg "blue" "Test background variable.")
  (unwind-protect
      (progn
        (should (equal (tp--resolve-reactive-symbols '$tp-test-my-color) "red"))
        (should (equal (tp--resolve-reactive-symbols '(face (:foreground $tp-test-my-color)))
                       '(face (:foreground "red"))))
        (should (equal (tp--resolve-reactive-symbols '(face (:foreground $tp-test-my-color :background $tp-test-my-bg)))
                       '(face (:foreground "red" :background "blue")))))
    ;; Cleanup
    (makunbound 'tp-test-my-color)
    (makunbound 'tp-test-my-bg)))

(ert-deftest tp-test-define-layer-with-reactive ()
  "Test define-tp with reactive variables."
  (tp-test-with-temp-buffer
    (defvar tp-test-var-color "red" "Test color variable.")
    (unwind-protect
        (progn
          (define-tp test-reactive-layer () '(face (:foreground $tp-test-var-color)))
          ;; Check the layer is defined with resolved value
          (let ((props (cdr (assoc 'test-reactive-layer tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "red")))
          ;; Check the dependency is registered with only reactive props
          (should (assoc 'tp-test-var-color tp-reactive-deps))
          ;; Check the stored reactive props only contain the face property
          (let* ((deps (cdr (assoc 'tp-test-var-color tp-reactive-deps)))
                 (layer-dep (assoc 'test-reactive-layer deps)))
            (should layer-dep)
            ;; The stored props should be just the reactive portion
            (should (plist-get (cdr layer-dep) 'face))))
      ;; Cleanup
      (makunbound 'tp-test-var-color))))

(ert-deftest tp-test-reactive-update-on-variable-change ()
  "Test that changing a reactive variable updates the layer."
  (tp-test-with-temp-buffer
    (defvar tp-test-reactive-color nil "Test variable for reactive properties.")
    (setq tp-test-reactive-color "red")
    (unwind-protect
        (progn
          (define-tp test-reactive-update () '(face (:foreground $tp-test-reactive-color)))
          ;; Verify initial value
          (let ((props (cdr (assoc 'test-reactive-update tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "red")))
          ;; Change the variable
          (setq tp-test-reactive-color "blue")
          ;; Verify the layer definition is updated
          (let ((props (cdr (assoc 'test-reactive-update tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "blue"))))
      ;; Cleanup
      (makunbound 'tp-test-reactive-color))))

(ert-deftest tp-test-reactive-update-text-regions ()
  "Test that changing a reactive variable updates applied text regions."
  (tp-test-with-temp-buffer
    (defvar tp-test-region-color nil "Test variable for reactive regions.")
    (setq tp-test-region-color "red")
    (unwind-protect
        (progn
          (define-tp test-reactive-region () '(face (:foreground $tp-test-region-color)))
          (insert "Hello World")
          ;; Apply the layer to text
          (tp-push-layer 1 6 'test-reactive-region)
          ;; Verify initial properties
          (should (equal (plist-get (tp-at 1 'face) :foreground) "red"))
          ;; Change the variable
          (setq tp-test-region-color "green")
          ;; Verify the text is updated
          (should (equal (plist-get (tp-at 1 'face) :foreground) "green")))
      ;; Cleanup
      (makunbound 'tp-test-region-color))))

(ert-deftest tp-test-reactive-reset ()
  "Test tp-reactive-reset clears all reactive dependencies."
  (tp-test-with-temp-buffer
    (defvar tp-test-reset-color nil "Test variable for reactive reset.")
    (setq tp-test-reset-color "red")
    (unwind-protect
        (progn
          (define-tp test-reactive-reset () '(face (:foreground $tp-test-reset-color)))
          (should tp-reactive-deps)
          (tp-reactive-reset)
          (should-not tp-reactive-deps))
      ;; Cleanup
      (makunbound 'tp-test-reset-color))))

(ert-deftest tp-test-layer-reset-clears-reactive ()
  "Test tp-layer-reset also clears reactive dependencies."
  (tp-test-with-temp-buffer
    (defvar tp-test-reset2-color nil "Test variable for layer reset.")
    (setq tp-test-reset2-color "red")
    (unwind-protect
        (progn
          (define-tp test-reactive-reset2 () '(face (:foreground $tp-test-reset2-color)))
          (should tp-reactive-deps)
          (tp-layer-reset)
          (should-not tp-reactive-deps))
      ;; Cleanup
      (makunbound 'tp-test-reset2-color))))

(ert-deftest tp-test-define-layer-group-with-reactive ()
  "Test define-tps with reactive variables."
  (tp-test-with-temp-buffer
    (defvar tp-test-group-color nil "Test variable for layer group.")
    (setq tp-test-group-color "red")
    (unwind-protect
        (progn
          (define-tps test-reactive-group ()
            '("first" :props (face (:foreground $tp-test-group-color)))
            '("second" :props (face (:foreground "blue"))))
          ;; Check the reactive layer is defined with resolved value
          (let ((props (cdr (assoc 'test-reactive-group-first tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "red")))
          ;; Check the non-reactive layer is defined
          (let ((props (cdr (assoc 'test-reactive-group-second tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "blue")))
          ;; Check the reactive layer is registered in tp-reactive-deps
          (should (assoc 'tp-test-group-color tp-reactive-deps))
          ;; The reactive layer should be in the dependencies
          (let* ((deps (cdr (assoc 'tp-test-group-color tp-reactive-deps)))
                 (layer-dep (assoc 'test-reactive-group-first deps)))
            (should layer-dep)))
      ;; Cleanup
      (makunbound 'tp-test-group-color))))

(ert-deftest tp-test-undefine-layer-clears-reactive ()
  "Test tp-undefine-layer clears reactive dependencies for that layer."
  (tp-test-with-temp-buffer
    (defvar tp-test-undef-color nil "Test variable for undefine.")
    (setq tp-test-undef-color "red")
    (unwind-protect
        (progn
          (define-tp test-undef-reactive () '(face (:foreground $tp-test-undef-color)))
          ;; Check the dependency is registered
          (should (assoc 'tp-test-undef-color tp-reactive-deps))
          (let* ((deps (cdr (assoc 'tp-test-undef-color tp-reactive-deps)))
                 (layer-dep (assoc 'test-undef-reactive deps)))
            (should layer-dep))
          (tp-undefine-layer 'test-undef-reactive)
          ;; Dependency should be cleaned up if no other layers use it
          (should-not (cdr (assoc 'tp-test-undef-color tp-reactive-deps))))
      ;; Cleanup
      (makunbound 'tp-test-undef-color))))

;;; ============================================================
;;; Layer Name in Property-Setting APIs Tests
;;; ============================================================

(ert-deftest tp-test-set-with-layer-name ()
  "Test tp-set accepts a layer name defined by define-tp.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (define-tp my-style () '(face bold help-echo "tip"))
    ;; Use layer name instead of plist
    (tp-set 1 6 'my-style)
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "tip"))
    ;; tp-name should NOT be set for direct property setting
    (should-not (tp-at 1 'tp-name))))

(ert-deftest tp-test-set-with-layer-name-on-string ()
  "Test tp-set accepts a layer name on string.
When using tp-set (direct property setting), tp-name is NOT added."
  (let ((str (copy-sequence "Hello World")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (define-tp my-style () '(face italic))
    (tp-set 0 5 'my-style str)
    (should (eq (get-text-property 0 'face str) 'italic))
    ;; tp-name should NOT be set for direct property setting
    (should-not (get-text-property 0 'tp-name str))))

(ert-deftest tp-test-set-entire-string-with-layer-name ()
  "Test tp-set with layer name on entire string (string form).
This tests the fix for the bug where (tp-set str 'layer-name) would
incorrectly generate an anonymous tp-name instead of using the layer name."
  (tp-test-with-temp-buffer
    ;; Define a layer with reactive variables
    (define-tp my-entire-string-layer () :props '(face (:background $my-entire-string-color))
      :data '((my-entire-string-color . "blue")))
    (let ((str (tp-set " " 'my-entire-string-layer)))
      ;; tp-name should be the defined layer name, not an anonymous tp-anon-X
      (should (eq (get-text-property 0 'tp-name str) 'my-entire-string-layer))
      ;; face should be correctly set
      (should (equal (plist-get (get-text-property 0 'face str) :background) "blue")))))

(ert-deftest tp-test-reset-with-layer-name ()
  "Test tp-reset accepts a layer name defined by define-tp.
When using tp-reset (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(mouse-face highlight))
    (define-tp my-style () '(face underline))
    ;; Use layer name - should completely replace
    (tp-reset 1 6 'my-style)
    (should (eq (tp-at 1 'face) 'underline))
    (should (null (tp-at 1 'mouse-face)))
    ;; tp-name should NOT be set for direct property setting
    (should-not (tp-at 1 'tp-name))))

(ert-deftest tp-test-add-with-layer-name ()
  "Test tp-add accepts a layer name defined by define-tp.
When using tp-add (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(help-echo "existing"))
    (define-tp my-style () '(face bold))
    ;; Use layer name - should preserve existing properties
    (tp-add 1 6 'my-style)
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "existing"))
    ;; tp-name should NOT be set for direct property setting
    (should-not (tp-at 1 'tp-name))))

(ert-deftest tp-test-match-set-with-layer-name ()
  "Test tp-match-set accepts a layer name.
When using tp-match-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (define-tp match-style () '(face bold help-echo "matched"))
    (tp-match-set "Hello" 'match-style)
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "matched"))
    (should (eq (tp-at 13 'face) 'bold))
    ;; tp-name should NOT be set for direct property setting
    (should-not (tp-at 1 'tp-name))))

(ert-deftest tp-test-match-set-with-layer-name-on-string ()
  "Test tp-match-set accepts a layer name on string.
When using tp-match-set (direct property setting), tp-name is NOT added.
For strings, returns a NEW string (original is not modified)."
  (let ((str (copy-sequence "Hello World Hello")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (define-tp match-style () '(face italic))
    (let ((result (tp-match-set "Hello" 'match-style str)))
      ;; Result should have the properties
      (should (eq (get-text-property 0 'face result) 'italic))
      (should (eq (get-text-property 12 'face result) 'italic))
      ;; tp-name should NOT be set for direct property setting
      (should-not (get-text-property 0 'tp-name result))
      ;; Original should NOT be modified
      (should (null (get-text-property 0 'face str))))))

(ert-deftest tp-test-match-reset-with-layer-name ()
  "Test tp-match-reset accepts a layer name."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(mouse-face highlight))
    (define-tp match-style () '(face bold))
    (tp-match-reset "Hello" 'match-style)
    (should (eq (tp-at 1 'face) 'bold))
    (should (null (tp-at 1 'mouse-face)))))

(ert-deftest tp-test-match-add-with-layer-name ()
  "Test tp-match-add accepts a layer name."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-set 1 6 '(help-echo "original"))
    (define-tp match-style () '(face bold))
    (tp-match-add "Hello" 'match-style)
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "original"))))

(ert-deftest tp-test-regexp-set-with-layer-name ()
  "Test tp-regexp-set accepts a layer name."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (define-tp number-style () '(face bold help-echo "number"))
    (tp-regexp-set "[0-9]+" 'number-style)
    (should (eq (tp-at 5 'face) 'bold))
    (should (equal (tp-at 5 'help-echo) "number"))
    (should (eq (tp-at 13 'face) 'bold))))

(ert-deftest tp-test-regexp-set-with-layer-name-on-string ()
  "Test tp-regexp-set accepts a layer name on string.
For strings, returns a NEW string (original is not modified)."
  (let ((str (copy-sequence "abc 123 def 456")))
    (setq tp-layer-alist nil)
    (setq tp-layer-groups nil)
    (define-tp number-style () '(face italic))
    (let ((result (tp-regexp-set "[0-9]+" 'number-style str)))
      ;; Result should have the properties
      (should (eq (get-text-property 4 'face result) 'italic))
      (should (eq (get-text-property 12 'face result) 'italic))
      ;; Original should NOT be modified
      (should (null (get-text-property 4 'face str))))))

(ert-deftest tp-test-regexp-reset-with-layer-name ()
  "Test tp-regexp-reset accepts a layer name."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (tp-set 5 8 '(mouse-face highlight))
    (define-tp number-style () '(face bold))
    (tp-regexp-reset "[0-9]+" 'number-style)
    (should (eq (tp-at 5 'face) 'bold))
    (should (null (tp-at 5 'mouse-face)))))

(ert-deftest tp-test-regexp-add-with-layer-name ()
  "Test tp-regexp-add accepts a layer name.
When using tp-regexp-add (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (tp-set 5 8 '(help-echo "original"))
    (define-tp number-style () '(face bold))
    (tp-regexp-add "[0-9]+" 'number-style)
    (should (eq (tp-at 5 'face) 'bold))
    (should (equal (tp-at 5 'help-echo) "original"))
    ;; tp-name should NOT be set for direct property setting
    (should-not (tp-at 5 'tp-name))))

(ert-deftest tp-test-set-with-group-name ()
  "Test tp-set accepts a group name defined by define-tps.
When using tp-set with a group, layers are set with tp-name and tp-layers."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (define-tps my-group ()
      '("style" . (face bold help-echo "grouped")))
    ;; Use group name
    (tp-set 1 6 'my-group)
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "grouped"))
    ;; tp-name should be set for layer groups
    (should (tp-at 1 'tp-name))))

(ert-deftest tp-test-set-with-group-name-multiple-layers ()
  "Test tp-set with group containing multiple layers.
When using tp-set with a group, all layers are set with tp-name and tp-layers."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (define-tps my-group ()
      '("first" . (face bold))
      '("second" . (face italic)))
    ;; Use group name - all layers are applied with tp-layers structure
    (tp-set 1 6 'my-group)
    ;; First layer's properties are applied at top
    (should (eq (tp-at 1 'face) 'bold))
    ;; tp-name should be set for the top layer
    (should (tp-at 1 'tp-name))
    ;; tp-layers should contain the rest of the layers
    (should (tp-at 1 'tp-layers))))

(ert-deftest tp-test-match-set-with-group-name ()
  "Test tp-match-set accepts a group name.
When using tp-match-set with a group, layers are set with tp-name."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (define-tps my-group ()
      '("style" . (face italic)))
    (tp-match-set "Hello" 'my-group)
    (should (eq (tp-at 1 'face) 'italic))
    (should (eq (tp-at 13 'face) 'italic))
    ;; tp-name should be set for layer groups
    (should (tp-at 1 'tp-name))))

(ert-deftest tp-test-resolve-props-returns-nil-for-unknown ()
  "Test tp--resolve-props returns nil for unknown layer name."
  (tp-test-with-temp-buffer
    (should (null (tp--resolve-props 'unknown-layer-name)))))

(ert-deftest tp-test-set-with-complex-layer ()
  "Test tp-set with layer containing complex nested properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (define-tp complex-layer ()
      '(face (:foreground "red" :underline (:style wave))
             help-echo "complex"))
    (tp-set 1 6 'complex-layer)
    (let ((face (tp-at 1 'face)))
      (should (equal (plist-get face :foreground) "red"))
      (should (equal (plist-get (plist-get face :underline) :style) 'wave)))
    (should (equal (tp-at 1 'help-echo) "complex"))))

;;; ============================================================
;;; Anonymous Layer and Reactive Text Property Tests
;;; ============================================================

(ert-deftest tp-test-set-anonymous-layer-no-tp-name-for-non-reactive ()
  "Test that tp-set with non-reactive plist does NOT get tp-name.
Per requirement 1: non-reactive properties should not have tp-name added,
preserving the native text property behavior."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold))
    ;; Non-reactive anonymous layer should NOT have tp-name
    (should-not (tp-at 1 'tp-name))
    ;; But the face property should still be set
    (should (eq (tp-at 1 'face) 'bold))))

(ert-deftest tp-test-set-anonymous-reactive-layer ()
  "Test that tp-set with anonymous reactive plist works."
  (tp-test-with-temp-buffer
    (defvar tp-test-anon-color nil "Test variable for anonymous reactive layer.")
    (setq tp-test-anon-color "red")
    (unwind-protect
        (progn
          (insert "Hello World")
          ;; Set with anonymous reactive plist
          (tp-set 1 6 '(face (:foreground $tp-test-anon-color)))
          ;; Should have resolved the reactive variable
          (let ((face (tp-at 1 'face)))
            (should (equal (plist-get face :foreground) "red")))
          ;; Should have a generated tp-name
          (should (tp-at 1 'tp-name))
          ;; The reactive variable should be registered in dependencies
          (should (assoc 'tp-test-anon-color tp-reactive-deps))
          ;; Change the variable - should update the text
          (setq tp-test-anon-color "blue")
          (let ((face (tp-at 1 'face)))
            (should (equal (plist-get face :foreground) "blue"))))
      ;; Cleanup
      (makunbound 'tp-test-anon-color))))

(ert-deftest tp-test-set-anonymous-layer-preserves-existing-tp-name ()
  "Test that tp-set with anonymous plist preserves existing tp-name property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; First set with a layer name - this does NOT set tp-name
    (define-tp my-existing-layer () '(face bold))
    (tp-set 1 6 'my-existing-layer)
    (should-not (tp-at 1 'tp-name))  ; no tp-name for direct setting
    ;; Now set with anonymous plist that has explicit tp-name
    (tp-set 1 6 '(face italic tp-name my-custom-name))
    ;; Explicit tp-name in plist should be preserved
    (should (eq (tp-at 1 'tp-name) 'my-custom-name))))

(ert-deftest tp-test-match-set-anonymous-reactive-layer ()
  "Test that tp-match-set with anonymous reactive plist works."
  (tp-test-with-temp-buffer
    (defvar tp-test-match-color nil "Test variable for match reactive layer.")
    (setq tp-test-match-color "green")
    (unwind-protect
        (progn
          (insert "Hello World Hello")
          ;; Set with anonymous reactive plist
          (tp-match-set "Hello" '(face (:foreground $tp-test-match-color)))
          ;; Should have resolved the reactive variable
          (let ((face (tp-at 1 'face)))
            (should (equal (plist-get face :foreground) "green")))
          ;; Should have a generated tp-name
          (should (tp-at 1 'tp-name))
          ;; Change the variable - should update the text
          (setq tp-test-match-color "yellow")
          (let ((face (tp-at 1 'face)))
            (should (equal (plist-get face :foreground) "yellow"))))
      ;; Cleanup
      (makunbound 'tp-test-match-color))))

(ert-deftest tp-test-regexp-set-anonymous-reactive-layer ()
  "Test that tp-regexp-set with anonymous reactive plist works."
  (tp-test-with-temp-buffer
    (defvar tp-test-regexp-color nil "Test variable for regexp reactive layer.")
    (setq tp-test-regexp-color "purple")
    (unwind-protect
        (progn
          (insert "abc 123 def 456")
          ;; Set with anonymous reactive plist
          (tp-regexp-set "[0-9]+" '(face (:foreground $tp-test-regexp-color)))
          ;; Should have resolved the reactive variable
          (let ((face (tp-at 5 'face)))
            (should (equal (plist-get face :foreground) "purple")))
          ;; Should have a generated tp-name
          (should (tp-at 5 'tp-name))
          ;; Change the variable - should update the text
          (setq tp-test-regexp-color "orange")
          (let ((face (tp-at 5 'face)))
            (should (equal (plist-get face :foreground) "orange"))))
      ;; Cleanup
      (makunbound 'tp-test-regexp-color))))

;;; ============================================================
;;; :watch, :data, and :compute Tests (Vue 3 style reactivity)
;;; ============================================================

(ert-deftest tp-test-define-layer-with-watch ()
  "Test reactive layers with :watch for side effects."
  (tp-test-with-temp-buffer
    (defvar tp-test-watch-var nil "Test variable for watch.")
    (defvar tp-test-watch-log nil "Log of watch callback invocations.")
    (setq tp-test-watch-var "initial")
    (setq tp-test-watch-log nil)
    (unwind-protect
        (progn
          (define-tp test-watch-layer () :props '(face (:foreground $tp-test-watch-var))
            :watch '((tp-test-watch-var
                     (lambda (new old layer)
                       (push (list new old layer) tp-test-watch-log)))))
          ;; Check the layer is defined with resolved value
          (let ((props (cdr (assoc 'test-watch-layer tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "initial")))
          ;; Check the watcher is registered
          (should (assoc 'test-watch-layer tp-layer-watchers))
          ;; Change the variable
          (setq tp-test-watch-var "changed")
          ;; Check the layer is updated
          (let ((props (cdr (assoc 'test-watch-layer tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "changed")))
          ;; Check the watcher was called
          (should (= (length tp-test-watch-log) 1))
          (let ((log-entry (car tp-test-watch-log)))
            (should (equal (nth 0 log-entry) "changed"))
            (should (equal (nth 1 log-entry) "initial"))
            (should (eq (nth 2 log-entry) 'test-watch-layer))))
      ;; Cleanup
      (makunbound 'tp-test-watch-var)
      (makunbound 'tp-test-watch-log))))

(ert-deftest tp-test-define-layer-with-data ()
  "Test reactive layers with :data for additional reactive variables."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          (define-tp test-data-layer () :props '(face (:foreground $tp-test-data-color))
            :data '(tp-test-data-extra))
          ;; Check that variables were auto-defined
          (should (boundp 'tp-test-data-color))
          (should (boundp 'tp-test-data-extra))
          ;; Check data is registered
          (should (assoc 'test-data-layer tp-layer-data))
          ;; Check the layer is defined
          (should (assoc 'test-data-layer tp-layer-alist)))
      ;; Cleanup
      (makunbound 'tp-test-data-color)
      (makunbound 'tp-test-data-extra))))

(ert-deftest tp-test-define-layer-with-compute ()
  "Test reactive layers with :compute for computed reactive variables."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Set up the source variables
          (setq tp-test-first-name "John")
          (setq tp-test-last-name "Doe")
          (define-tp test-compute-layer ()
            :props '(help-echo $tp-test-full-name)
            :data '(tp-test-first-name tp-test-last-name)
            :compute '((tp-test-full-name
                       (lambda ()
                         (concat tp-test-first-name " " tp-test-last-name)))))
          ;; Check the layer is defined
          (should (assoc 'test-compute-layer tp-layer-alist))
          ;; Check the computed is registered
          (should (assoc 'test-compute-layer tp-layer-computed))
          ;; Check the computed variable has initial value
          (should (equal tp-test-full-name "John Doe"))
          ;; Check the layer property uses the computed value
          (let ((props (cdr (assoc 'test-compute-layer tp-layer-alist))))
            (should (equal (plist-get props 'help-echo) "John Doe"))))
      ;; Cleanup
      (makunbound 'tp-test-first-name)
      (makunbound 'tp-test-last-name)
      (makunbound 'tp-test-full-name))))

(ert-deftest tp-test-define-layer-with-data-and-compute ()
  "Test reactive layers with :data and :compute together."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Set data values first
          (setq tp-test-dc-color "blue")
          (setq tp-test-dc-first "Jane")
          (setq tp-test-dc-last "Smith")
          ;; Define layer with :data and :compute
          (define-tp test-dc-layer ()
            :props '(face (:foreground $tp-test-dc-color) help-echo $tp-test-dc-full-name)
            :data '(tp-test-dc-first tp-test-dc-last)
            :compute '((tp-test-dc-full-name
                       (lambda ()
                         (concat tp-test-dc-first " " tp-test-dc-last)))))
          ;; Check data is registered
          (should (assoc 'test-dc-layer tp-layer-data))
          ;; Check computed is registered
          (should (assoc 'test-dc-layer tp-layer-computed))
          ;; Check the computed value
          (should (equal tp-test-dc-full-name "Jane Smith")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-dc-color))
      (ignore-errors (makunbound 'tp-test-dc-first))
      (ignore-errors (makunbound 'tp-test-dc-last))
      (ignore-errors (makunbound 'tp-test-dc-full-name)))))

(ert-deftest tp-test-define-layer-watch-requires-props ()
  "Test that :watch requires :props to be explicitly specified."
  (tp-test-with-temp-buffer
    (should-error
     (define-tp test-invalid ()
       :watch '((some-var (lambda (new old layer) nil)))))))

(ert-deftest tp-test-define-layer-compute-requires-props ()
  "Test that :compute requires :props to be explicitly specified."
  (tp-test-with-temp-buffer
    (should-error
     (define-tp test-invalid ()
       :compute '((some-var (lambda () "computed")))))))

(ert-deftest tp-test-define-layer-data-requires-props ()
  "Test that :data requires :props to be explicitly specified."
  (tp-test-with-temp-buffer
    (should-error
     (define-tp test-invalid ()
       :data '(some-var)))))

(ert-deftest tp-test-undefine-layer-clears-watch-compute-data ()
  "Test tp-undefine-layer clears watchers, computed, and data."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          (define-tp test-undef-wcd ()
            :props '(face (:foreground $tp-test-undef-color) help-echo $tp-test-undef-full)
            :data '(tp-test-undef-first tp-test-undef-last)
            :watch '((tp-test-undef-color (lambda (n o l) nil)))
            :compute '((tp-test-undef-full
                       (lambda ()
                         (concat tp-test-undef-first " " tp-test-undef-last)))))
          ;; Check registrations
          (should (assoc 'test-undef-wcd tp-layer-watchers))
          (should (assoc 'test-undef-wcd tp-layer-computed))
          (should (assoc 'test-undef-wcd tp-layer-data))
          ;; Undefine the layer
          (tp-undefine-layer 'test-undef-wcd)
          ;; Check all are cleaned up
          (should-not (assoc 'test-undef-wcd tp-layer-watchers))
          (should-not (assoc 'test-undef-wcd tp-layer-computed))
          (should-not (assoc 'test-undef-wcd tp-layer-data)))
      ;; Cleanup
      (makunbound 'tp-test-undef-color)
      (makunbound 'tp-test-undef-first)
      (makunbound 'tp-test-undef-last)
      (makunbound 'tp-test-undef-full))))

(ert-deftest tp-test-define-layer-group-with-watch ()
  "Test define-tps with :watch (format-4)."
  (tp-test-with-temp-buffer
    (defvar tp-test-group-watch-var nil "Test variable for group watch.")
    (defvar tp-test-group-watch-log nil "Log of watch callback invocations.")
    (setq tp-test-group-watch-var "red")
    (setq tp-test-group-watch-log nil)
    (unwind-protect
        (progn
          (define-tps test-watch-group ()
            '("reactive" :props (face (:foreground $tp-test-group-watch-var))
              :watch ((tp-test-group-watch-var
                       (lambda (new old layer)
                         (push (list new old layer) tp-test-group-watch-log)))))
            '("static" :props (face (:foreground "blue"))))
          ;; Check the group is defined
          (should (assoc 'test-watch-group tp-layer-groups))
          ;; Check the reactive layer has its watcher registered
          (should (assoc 'test-watch-group-reactive tp-layer-watchers))
          ;; Static layer should not have a watcher
          (should-not (assoc 'test-watch-group-static tp-layer-watchers))
          ;; Change the variable
          (setq tp-test-group-watch-var "green")
          ;; Check the watcher was called
          (should (= (length tp-test-group-watch-log) 1)))
      ;; Cleanup
      (makunbound 'tp-test-group-watch-var)
      (makunbound 'tp-test-group-watch-log))))

(ert-deftest tp-test-reactive-reset-clears-all ()
  "Test tp-reactive-reset clears watchers, computed, and data."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          (define-tp test-reset-all ()
            :props '(face (:foreground $tp-test-reset-color) help-echo $tp-test-reset-full)
            :data '(tp-test-reset-first tp-test-reset-last)
            :watch '((tp-test-reset-color (lambda (n o l) nil)))
            :compute '((tp-test-reset-full
                        (lambda ()
                          (concat tp-test-reset-first " " tp-test-reset-last)))))
          ;; Check registrations
          (should tp-layer-watchers)
          (should tp-layer-computed)
          (should tp-layer-data)
          ;; Reset reactive
          (tp-reactive-reset)
          ;; Check all are cleared
          (should-not tp-layer-watchers)
          (should-not tp-layer-computed)
          (should-not tp-layer-data))
      ;; Cleanup - variables may or may not be bound
      (ignore-errors (makunbound 'tp-test-reset-color))
      (ignore-errors (makunbound 'tp-test-reset-first))
      (ignore-errors (makunbound 'tp-test-reset-last))
      (ignore-errors (makunbound 'tp-test-reset-full)))))

(ert-deftest tp-test-auto-define-variables ()
  "Test that reactive variables are auto-defined when not bound."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Variables should not exist before
          (should-not (boundp 'tp-test-auto-var1))
          (should-not (boundp 'tp-test-auto-var2))
          (define-tp test-auto-layer () :props '(face (:foreground $tp-test-auto-var1))
            :data '(tp-test-auto-var2))
          ;; Variables should now exist
          (should (boundp 'tp-test-auto-var1))
          (should (boundp 'tp-test-auto-var2)))
      ;; Cleanup
      (makunbound 'tp-test-auto-var1)
      (makunbound 'tp-test-auto-var2))))

(ert-deftest tp-test-setq-local-triggers-update ()
  "Test that setq-local triggers reactive updates correctly."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define layer with auto-created variable (nil initial value)
          (define-tp test-local-layer () :props '(face (:foreground $tp-test-local-color)))
          ;; Apply layer to text
          (insert "Hello World")
          (tp-set 1 6 'test-local-layer)
          ;; Initial value should be nil
          (should (equal (plist-get (get-text-property 1 'face) :foreground) nil))
          ;; Use setq-local to set the value
          (setq-local tp-test-local-color "red")
          ;; Text property should be updated
          (should (equal (plist-get (get-text-property 1 'face) :foreground) "red")))
      ;; Cleanup
      (makunbound 'tp-test-local-color))))

(ert-deftest tp-test-data-setq-local-triggers-compute ()
  "Test that setq-local on :data variables triggers computed value updates."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define layer with :data and :compute
          (define-tp test-data-compute-layer ()
            :props '(help-echo $tp-test-dc-full)
            :data '(tp-test-dc-first tp-test-dc-last)
            :compute '((tp-test-dc-full
                       (lambda ()
                         (concat tp-test-dc-first " " tp-test-dc-last)))))
          ;; Apply layer to text
          (insert "Hello World")
          (tp-set 1 6 'test-data-compute-layer)
          ;; Initial computed value should be " " (concat nil nil = " ")
          (should (equal (get-text-property 1 'help-echo) " "))
          ;; Use setq-local to set first name
          (setq-local tp-test-dc-first "Kinney")
          ;; Computed should be "Kinney " now
          (should (equal (get-text-property 1 'help-echo) "Kinney "))
          ;; Use setq-local to set last name
          (setq-local tp-test-dc-last "Zhang")
          ;; Computed should be "Kinney Zhang" now
          (should (equal (get-text-property 1 'help-echo) "Kinney Zhang")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-dc-first))
      (ignore-errors (makunbound 'tp-test-dc-last))
      (ignore-errors (makunbound 'tp-test-dc-full)))))

(ert-deftest tp-test-data-with-initial-values ()
  "Test that :data supports initial values with cons cell format."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define layer with :data having initial values
          (define-tp test-data-init-layer ()
            :props '(face (:foreground $tp-test-init-color) help-echo $tp-test-init-name)
            :data '((tp-test-init-color . "blue")
                   (tp-test-init-name . "Initial Name")
                   tp-test-init-other))
          ;; Check initial values
          (should (equal tp-test-init-color "blue"))
          (should (equal tp-test-init-name "Initial Name"))
          (should (equal tp-test-init-other nil))
          ;; Apply layer to text
          (insert "Hello World")
          (tp-set 1 6 'test-data-init-layer)
          ;; Check text properties have initial values
          (should (equal (plist-get (get-text-property 1 'face) :foreground) "blue"))
          (should (equal (get-text-property 1 'help-echo) "Initial Name")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-init-color))
      (ignore-errors (makunbound 'tp-test-init-name))
      (ignore-errors (makunbound 'tp-test-init-other)))))

(ert-deftest tp-test-setq-local-only-updates-current-buffer ()
  "Test that setq-local only updates text properties in the current buffer."
  (let ((buf1 nil)
        (buf2 nil))
    (unwind-protect
        (progn
          ;; Define a reactive layer
          (define-tp test-multi-buf-layer () :props '(face (:foreground $tp-test-multi-color)))
          ;; Create first buffer with layer applied
          (setq buf1 (generate-new-buffer " *test-buf1*"))
          (with-current-buffer buf1
            (insert "Hello World")
            (tp-set 1 6 'test-multi-buf-layer))
          ;; Create second buffer with layer applied
          (setq buf2 (generate-new-buffer " *test-buf2*"))
          (with-current-buffer buf2
            (insert "Hello World")
            (tp-set 1 6 'test-multi-buf-layer))
          ;; Use setq-local in buf1
          (with-current-buffer buf1
            (setq-local tp-test-multi-color "red"))
          ;; buf1 should be updated
          (with-current-buffer buf1
            (should (equal (plist-get (get-text-property 1 'face) :foreground) "red")))
          ;; buf2 should NOT be updated (still nil)
          (with-current-buffer buf2
            (should (equal (plist-get (get-text-property 1 'face) :foreground) nil))))
      ;; Cleanup
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (ignore-errors (makunbound 'tp-test-multi-color)))))

(ert-deftest tp-test-setq-updates-all-buffers-with-property ()
  "Test that setq updates text properties in all buffers that have the property."
  (let ((buf1 nil)
        (buf2 nil))
    (unwind-protect
        (progn
          ;; Define a reactive layer
          (define-tp test-global-layer () :props '(face (:foreground $tp-test-global-color)))
          ;; Create first buffer with layer applied
          (setq buf1 (generate-new-buffer " *test-buf1*"))
          (with-current-buffer buf1
            (insert "Hello World")
            (tp-set 1 6 'test-global-layer))
          ;; Create second buffer with layer applied
          (setq buf2 (generate-new-buffer " *test-buf2*"))
          (with-current-buffer buf2
            (insert "Hello World")
            (tp-set 1 6 'test-global-layer))
          ;; Use global setq
          (setq tp-test-global-color "blue")
          ;; Both buffers should be updated
          (with-current-buffer buf1
            (should (equal (plist-get (get-text-property 1 'face) :foreground) "blue")))
          (with-current-buffer buf2
            (should (equal (plist-get (get-text-property 1 'face) :foreground) "blue"))))
      ;; Cleanup
      (when (buffer-live-p buf1) (kill-buffer buf1))
      (when (buffer-live-p buf2) (kill-buffer buf2))
      (ignore-errors (makunbound 'tp-test-global-color)))))

;;; ============================================================
;;; Re-definition Tests (Issue: define-tp should update all properties on re-execution)
;;; ============================================================

(ert-deftest tp-test-redefine-layer-updates-data-initial-values ()
  "Test that re-defining a layer with different :data initial values updates the variable."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition with gray color
          (define-tp test-redef-layer () :props '(face (:background $tp-test-redef-color))
            :data '((tp-test-redef-color . "gray")))
          ;; Check initial value
          (should (equal tp-test-redef-color "gray"))
          ;; Check layer props
          (let ((props (cdr (assoc 'test-redef-layer tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :background) "gray")))
          ;; Re-define with different color
          (define-tp test-redef-layer () :props '(face (:background $tp-test-redef-color))
            :data '((tp-test-redef-color . "blue")))
          ;; Check variable is updated
          (should (equal tp-test-redef-color "blue"))
          ;; Check layer props are updated
          (let ((props (cdr (assoc 'test-redef-layer tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :background) "blue"))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-redef-color)))))

(ert-deftest tp-test-redefine-layer-updates-props ()
  "Test that re-defining a layer updates :props correctly."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition
          (define-tp test-redef-props () :props '(face (:foreground $tp-test-redef-fg))
            :data '((tp-test-redef-fg . "red")))
          (let ((props (cdr (assoc 'test-redef-props tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :foreground) "red")))
          ;; Re-define with different props structure
          (define-tp test-redef-props ()
            :props '(face (:background $tp-test-redef-bg) help-echo "new")
            :data '((tp-test-redef-bg . "yellow")))
          ;; Check new props are applied
          (let ((props (cdr (assoc 'test-redef-props tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :background) "yellow"))
            (should (equal (plist-get props 'help-echo) "new"))
            ;; Old :foreground should NOT be present
            (should (null (plist-get (plist-get props 'face) :foreground)))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-redef-fg))
      (ignore-errors (makunbound 'tp-test-redef-bg)))))

(ert-deftest tp-test-redefine-layer-clears-old-reactive-deps ()
  "Test that re-defining a layer with different reactive vars clears old dependencies."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition with $old-var
          (define-tp test-redef-deps () :props '(face (:foreground $tp-test-old-var))
            :data '((tp-test-old-var . "red")))
          ;; Check old var is in dependencies
          (should (assoc 'tp-test-old-var tp-reactive-deps))
          (let ((deps (cdr (assoc 'tp-test-old-var tp-reactive-deps))))
            (should (assoc 'test-redef-deps deps)))
          ;; Re-define with $new-var
          (define-tp test-redef-deps () :props '(face (:foreground $tp-test-new-var))
            :data '((tp-test-new-var . "blue")))
          ;; Check old var is no longer in dependencies for this layer
          (when-let ((deps (cdr (assoc 'tp-test-old-var tp-reactive-deps))))
            (should-not (assoc 'test-redef-deps deps)))
          ;; Check new var is in dependencies
          (should (assoc 'tp-test-new-var tp-reactive-deps))
          (let ((deps (cdr (assoc 'tp-test-new-var tp-reactive-deps))))
            (should (assoc 'test-redef-deps deps))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-old-var))
      (ignore-errors (makunbound 'tp-test-new-var)))))

(ert-deftest tp-test-redefine-layer-updates-watchers ()
  "Test that re-defining a layer updates :watch correctly."
  (tp-test-with-temp-buffer
    ;; Use defvar to create dynamically-bound variables that watcher callbacks can access
    (defvar tp-test-watch-log-old nil "Log for old watcher.")
    (defvar tp-test-watch-log-new nil "Log for new watcher.")
    (setq tp-test-watch-log-old nil)
    (setq tp-test-watch-log-new nil)
    (unwind-protect
        (progn
          ;; First definition with old watcher
          (define-tp test-redef-watch () :props '(face (:foreground $tp-test-watch-var))
            :data '((tp-test-watch-var . "red"))
            :watch '((tp-test-watch-var
                     (lambda (new old layer)
                       (push (list 'old new) tp-test-watch-log-old)))))
          ;; Re-define with new watcher
          (define-tp test-redef-watch () :props '(face (:foreground $tp-test-watch-var))
            :data '((tp-test-watch-var . "red"))
            :watch '((tp-test-watch-var
                     (lambda (new old layer)
                       (push (list 'new new) tp-test-watch-log-new)))))
          ;; Change variable
          (setq tp-test-watch-var "blue")
          ;; Old watcher should NOT be called
          (should (null tp-test-watch-log-old))
          ;; New watcher should be called
          (should (= (length tp-test-watch-log-new) 1))
          (should (equal (car tp-test-watch-log-new) '(new "blue"))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-watch-var))
      (makunbound 'tp-test-watch-log-old)
      (makunbound 'tp-test-watch-log-new))))

(ert-deftest tp-test-redefine-layer-updates-compute ()
  "Test that re-defining a layer updates :compute correctly."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition with old compute
          (setq tp-test-compute-src "hello")
          (define-tp test-redef-compute ()
            :props '(help-echo $tp-test-compute-out)
            :data '(tp-test-compute-src)
            :compute '((tp-test-compute-out
                       (lambda () (upcase tp-test-compute-src)))))
          (should (equal tp-test-compute-out "HELLO"))
          ;; Re-define with different compute
          (define-tp test-redef-compute ()
            :props '(help-echo $tp-test-compute-out)
            :data '(tp-test-compute-src)
            :compute '((tp-test-compute-out
                       (lambda () (concat tp-test-compute-src "-suffix")))))
          ;; Check compute is updated
          (should (equal tp-test-compute-out "hello-suffix"))
          ;; Trigger re-compute by changing source
          (setq tp-test-compute-src "world")
          (should (equal tp-test-compute-out "world-suffix")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-compute-src))
      (ignore-errors (makunbound 'tp-test-compute-out)))))

(ert-deftest tp-test-redefine-layer-from-reactive-to-static ()
  "Test re-defining a layer from reactive to non-reactive clears dependencies."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition with reactive variable
          (define-tp test-reactive-to-static () :props '(face (:foreground $tp-test-r2s-color))
            :data '((tp-test-r2s-color . "red")))
          ;; Check reactive dependency is registered
          (should (assoc 'tp-test-r2s-color tp-reactive-deps))
          ;; Re-define as static (non-reactive)
          (define-tp test-reactive-to-static () '(face bold))
          ;; Check reactive dependency is cleared
          (when-let ((deps (cdr (assoc 'tp-test-r2s-color tp-reactive-deps))))
            (should-not (assoc 'test-reactive-to-static deps)))
          ;; Check layer has new static props
          (let ((props (tp-layer-props 'test-reactive-to-static)))
            (should (eq (plist-get props 'face) 'bold))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-r2s-color)))))

(ert-deftest tp-test-redefine-layer-group-updates-data ()
  "Test that re-defining a layer group with different :data initial values updates the variable."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition
          (define-tps test-redef-group ()
            '("layer1" :props (face (:background $tp-test-group-color))
                      :data ((tp-test-group-color . "gray"))))
          ;; Check initial value
          (should (equal tp-test-group-color "gray"))
          ;; Re-define with different color
          (define-tps test-redef-group ()
            '("layer1" :props (face (:background $tp-test-group-color))
                      :data ((tp-test-group-color . "blue"))))
          ;; Check variable is updated
          (should (equal tp-test-group-color "blue"))
          ;; Check layer props are updated
          (let ((props (cdr (assoc 'test-redef-group-layer1 tp-layer-alist))))
            (should (equal (plist-get (plist-get props 'face) :background) "blue"))))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-group-color)))))

(ert-deftest tp-test-redefine-applied-layer-updates-text ()
  "Test that re-defining a layer updates text regions that have it applied."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; First definition
          (define-tp test-redef-applied () :props '(face (:background $tp-test-applied-color))
            :data '((tp-test-applied-color . "gray")))
          ;; Apply to text
          (insert "Hello World")
          (tp-set 1 6 'test-redef-applied)
          ;; Check initial color
          (should (equal (plist-get (get-text-property 1 'face) :background) "gray"))
          ;; Re-define with different color
          (define-tp test-redef-applied () :props '(face (:background $tp-test-applied-color))
            :data '((tp-test-applied-color . "blue")))
          ;; The text should now have the new color
          ;; This happens because define-tp calls tp--update-layer-regions
          ;; at the end to update all text regions with the new properties
          (should (equal (plist-get (get-text-property 1 'face) :background) "blue")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-applied-color)))))

;;; ============================================================
;;; Reactive Text (tp-text) Tests
;;; ============================================================

(ert-deftest tp-test-tp-text-nil-initializes-to-current-text ()
  "Test that tp-text with nil value is initialized to current text."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold tp-text nil))
    ;; tp-text should be set to the current text
    (should (equal (tp-at 1 'tp-text) "Hello"))
    ;; face should still be bold
    (should (eq (tp-at 1 'face) 'bold))))

(ert-deftest tp-test-tp-text-string-object-replaces-content ()
  "Test that tp-text on string object replaces the string content."
  ;; When tp-text is set on a string, the returned string should have
  ;; the tp-text value as its content, not the original string
  (let ((result (tp-set "2" 'face '(:background "green") 'tp-text "6")))
    ;; The returned string should be "6", not "2"
    (should (equal result "6"))
    ;; Properties should be applied
    (should (equal (get-text-property 0 'face result) '(:background "green")))
    (should (equal (get-text-property 0 'tp-text result) "6"))))

(ert-deftest tp-test-tp-text-string-replaces-text ()
  "Test that tp-text with string value replaces the text in the region."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold tp-text "Hi"))
    ;; Text should be replaced
    (should (equal (buffer-substring-no-properties 1 3) "Hi"))
    ;; face should still be applied
    (should (eq (tp-at 1 'face) 'bold))
    ;; tp-text property should be set
    (should (equal (tp-at 1 'tp-text) "Hi"))))

(ert-deftest tp-test-tp-text-preserves-other-properties ()
  "Test that tp-text replacement preserves existing properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; First set some properties
    (tp-set 1 6 '(help-echo "greeting"))
    ;; Then set tp-text with face
    (tp-set 1 6 '(face bold tp-text "Hi"))
    ;; Text should be replaced
    (should (equal (buffer-substring-no-properties 1 3) "Hi"))
    ;; Both face and help-echo should be preserved
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "greeting"))))

(ert-deftest tp-test-tp-reset-with-tp-text ()
  "Test that tp-reset with tp-text works correctly."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-reset 1 6 '(face italic tp-text "Bye"))
    ;; Text should be replaced
    (should (equal (buffer-substring-no-properties 1 4) "Bye"))
    ;; Properties should be set
    (should (eq (tp-at 1 'face) 'italic))
    (should (equal (tp-at 1 'tp-text) "Bye"))))

(ert-deftest tp-test-tp-add-with-tp-text ()
  "Test that tp-add with tp-text works correctly."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(help-echo "existing"))
    (tp-add 1 6 '(face bold tp-text "Hi"))
    ;; Text should be replaced
    (should (equal (buffer-substring-no-properties 1 3) "Hi"))
    ;; Both properties should be present
    (should (eq (tp-at 1 'face) 'bold))
    (should (equal (tp-at 1 'help-echo) "existing"))))

(ert-deftest tp-test-tp-text-reactive-layer ()
  "Test tp-text with reactive variable."
  (tp-test-with-temp-buffer
    (defvar tp-test-reactive-text nil "Test variable for reactive text.")
    (setq tp-test-reactive-text "Initial")
    (unwind-protect
        (progn
          (define-tp test-reactive-text-layer ()
            :props '(face bold tp-text $tp-test-reactive-text))
          ;; Apply layer to text
          (insert "Hello World")
          (tp-set 1 6 'test-reactive-text-layer)
          ;; Initial text should be replaced
          (should (equal (buffer-substring-no-properties 1 8) "Initial"))
          ;; tp-text should be set
          (should (equal (tp-at 1 'tp-text) "Initial"))
          ;; Change the reactive variable
          (setq tp-test-reactive-text "Changed")
          ;; Text should be updated
          (should (equal (buffer-substring-no-properties 1 8) "Changed"))
          ;; face should still be applied
          (should (eq (tp-at 1 'face) 'bold)))
      ;; Cleanup
      (makunbound 'tp-test-reactive-text))))

(ert-deftest tp-test-tp-text-reactive-nil-initializes-variable ()
  "Test tp-text with nil reactive variable initializes the variable to source text.
When tp-text is bound to a reactive variable and that variable is nil,
the source text should be used and the reactive variable should be updated."
  (tp-test-with-temp-buffer
    (defvar tp-test-text-var nil "Test variable for tp-text initialization.")
    (setq tp-test-text-var nil)
    (unwind-protect
        (progn
          ;; Define layer with tp-text bound to a reactive variable
          (define-tp test-init-text-layer ()
            :props '(face bold tp-text $tp-test-text-var))
          ;; Apply layer to string - variable is nil, so source text should be used
          (let ((result (tp-set "2" 'test-init-text-layer)))
            ;; Result should be the source text "2"
            (should (equal result "2"))
            ;; tp-test-text-var should now be "2"
            (should (equal tp-test-text-var "2"))
            ;; tp-text property should be "2"
            (should (equal (tp-at 0 'tp-text result) "2")))
          ;; Now set the variable to a different value and test again
          (setq tp-test-text-var "18")
          ;; Redefine layer to reset resolved props to the new variable value.
          ;; This is necessary because the layer definition caches the resolved
          ;; tp-text value, and we want to test the behavior when the variable
          ;; already has a non-nil value at layer application time.
          (define-tp test-init-text-layer ()
            :props '(face bold tp-text $tp-test-text-var))
          (let ((result (tp-set "2" 'test-init-text-layer)))
            ;; Result should be the variable value "18", not source "2"
            (should (equal result "18"))
            ;; Variable should remain "18"
            (should (equal tp-test-text-var "18"))))
      ;; Cleanup
      (makunbound 'tp-test-text-var))))

(ert-deftest tp-test-tp-text-direct-string-uses-specified-text ()
  "Test tp-text with direct string value uses that string, not source text.
When tp-text is set directly to a string (not a reactive variable),
the inserted text should be that string, not the source text."
  (let ((result (tp-set "2" 'tp-text "23")))
    ;; Result should be "23", not "2"
    (should (equal result "23"))
    ;; tp-text property should be "23"
    (should (equal (tp-at 0 'tp-text result) "23"))))

(ert-deftest tp-test-tp-text-reactive-computed ()
  "Test tp-text with computed reactive variable."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          (setq tp-test-name-part1 "Hello")
          (setq tp-test-name-part2 "World")
          (define-tp test-computed-text-layer ()
            :props '(face bold tp-text $tp-test-full-text)
            :data '(tp-test-name-part1 tp-test-name-part2)
            :compute '((tp-test-full-text
                       (lambda ()
                         (concat tp-test-name-part1 " " tp-test-name-part2)))))
          ;; Apply layer to text
          (insert "placeholder")
          (tp-set 1 12 'test-computed-text-layer)
          ;; Text should be replaced with computed value
          (should (equal (buffer-substring-no-properties 1 12) "Hello World"))
          ;; Change a data variable
          (setq tp-test-name-part1 "Goodbye")
          ;; Text should be updated with new computed value
          (should (equal (buffer-substring-no-properties 1 14) "Goodbye World")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-name-part1))
      (ignore-errors (makunbound 'tp-test-name-part2))
      (ignore-errors (makunbound 'tp-test-full-text)))))

(ert-deftest tp-test-tp-text-same-text-different-properties ()
  "Test tp-text updates when text is same but properties differ.
When the reactive variable changes to a propertized string with the same
text content but different properties, the properties should be updated."
  (tp-test-with-temp-buffer
    (defvar tp-test-same-text nil "Test variable for same text different props.")
    (setq tp-test-same-text "emacs")
    (unwind-protect
        (progn
          (define-tp test-same-text-layer ()
            :props '(face (:foreground "green") tp-text $tp-test-same-text))
          ;; Apply layer to text - insert placeholder and apply layer to entire buffer
          (insert "placeholder")
          (tp-set (point-min) (point-max) 'test-same-text-layer)
          ;; Initial text should be "emacs" with foreground green
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) "emacs"))
          (should (equal (plist-get (tp-at (point-min) 'face) :foreground) "green"))
          ;; Change the reactive variable to same text but different properties
          (setq tp-test-same-text (propertize "emacs" 'face 'bold))
          ;; Text should still be "emacs"
          (should (equal (buffer-substring-no-properties (point-min) (point-max)) "emacs"))
          ;; Face should now include bold from the propertized string
          (let ((face-val (tp-at (point-min) 'face)))
            (should (or (eq face-val 'bold)
                        (and (listp face-val) (memq 'bold face-val))))))
      ;; Cleanup
      (makunbound 'tp-test-same-text))))

;;; ============================================================
;;; tp-text with Embedded Text Properties Tests
;;; ============================================================

(ert-deftest tp-test-tp-text-with-embedded-properties-string ()
  "Test that tp-set with tp-text preserves embedded properties on strings."
  ;; When tp-text is a propertized string, embedded properties should be preserved
  ;; (props still override embedded if there's a conflict)
  (let* ((propertized-text (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'custom-prop 'embedded-value propertized-text))
         (result (tp-set "X" 'tp-text propertized-text 'face 'bold)))
    ;; The text content should be from tp-text
    (should (equal result "Hello"))
    ;; The face property from props should be applied
    (should (equal (tp-at 0 'face result) 'bold))
    ;; tp-set now preserves embedded props
    (should (equal (tp-at 0 'custom-prop result) 'embedded-value))))

(ert-deftest tp-test-tp-add-with-embedded-properties-string ()
  "Test that tp-add with tp-text merges embedded properties on strings."
  ;; When tp-text is a propertized string and tp-add is used, props are merged
  (let* ((propertized-text (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'custom-prop 'embedded-value propertized-text))
         (result (tp-add "X" 'tp-text propertized-text 'face 'bold)))
    ;; The text content should be from tp-text
    (should (equal result "Hello"))
    ;; The face property from props should be applied
    (should (equal (tp-at 0 'face result) 'bold))
    ;; tp-add merges - embedded custom-prop should be present
    (should (equal (tp-at 0 'custom-prop result) 'embedded-value))))

(ert-deftest tp-test-tp-text-with-embedded-face-string ()
  "Test that tp-set with tp-text preserves embedded face on strings."
  (let* ((propertized-text (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'face 'italic propertized-text))
         ;; Set tp-text with its own face, and also specify help-echo
         (result (tp-set "X" 'tp-text propertized-text 'help-echo "tip")))
    ;; The text content should be from tp-text
    (should (equal result "Hello"))
    ;; tp-set now preserves embedded face
    (should (equal (tp-at 0 'face result) 'italic))
    ;; The help-echo from props should be applied
    (should (equal (tp-at 0 'help-echo result) "tip"))))

(ert-deftest tp-test-tp-add-with-embedded-face-string ()
  "Test that tp-add with tp-text merges embedded face on strings."
  (let* ((propertized-text (copy-sequence "Hello"))
         (_ (put-text-property 0 5 'face 'italic propertized-text))
         ;; Add tp-text with its own face, and also specify help-echo
         (result (tp-add "X" 'tp-text propertized-text 'help-echo "tip")))
    ;; The text content should be from tp-text
    (should (equal result "Hello"))
    ;; tp-add merges - embedded face should be present
    (should (equal (tp-at 0 'face result) 'italic))
    ;; The help-echo from props should be applied
    (should (equal (tp-at 0 'help-echo result) "tip"))))

(ert-deftest tp-test-tp-text-with-embedded-properties-buffer ()
  "Test that tp-set with tp-text preserves embedded properties in buffers."
  (tp-test-with-temp-buffer
    (insert "Original")
    (let* ((propertized-text (copy-sequence "New"))
           (_ (put-text-property 0 3 'custom-prop 'embedded-value propertized-text)))
      (tp-set 1 9 `(face bold tp-text ,propertized-text))
      ;; The text content should be replaced with tp-text value
      (should (equal (buffer-substring-no-properties 1 4) "New"))
      ;; The face from props should be applied
      (should (equal (tp-at 1 'face) 'bold))
      ;; tp-set now preserves embedded props
      (should (equal (tp-at 1 'custom-prop) 'embedded-value)))))

(ert-deftest tp-test-tp-text-with-mixed-properties ()
  "Test that tp-set with tp-text preserves embedded properties."
  ;; tp-set now preserves embedded props (props still take precedence for conflicts)
  (let* ((propertized-text (copy-sequence "ABCD"))
         ;; Set a property at position 0
         (_ (put-text-property 0 4 'region-type 'start propertized-text))
         (result (tp-set "X" 'tp-text propertized-text 'face 'bold)))
    ;; The text content should be from tp-text
    (should (equal result "ABCD"))
    ;; The face from props should be applied uniformly
    (should (equal (tp-at 0 'face result) 'bold))
    (should (equal (tp-at 3 'face result) 'bold))
    ;; tp-set now preserves embedded props
    (should (equal (tp-at 0 'region-type result) 'start))))

(ert-deftest tp-test-tp-reset-with-embedded-properties ()
  "Test that tp-reset preserves embedded text properties from tp-text."
  (let* ((propertized-text (copy-sequence "Test"))
         (_ (put-text-property 0 4 'custom-prop 'value propertized-text))
         (result (tp-reset "X" 'tp-text propertized-text 'face 'bold)))
    ;; The text content should be from tp-text
    (should (equal result "Test"))
    ;; The face from props should be applied
    (should (equal (tp-at 0 'face result) 'bold))
    ;; tp-reset now preserves embedded props from tp-text
    (should (equal (tp-at 0 'custom-prop result) 'value))))

(ert-deftest tp-test-tp-add-with-embedded-properties ()
  "Test that tp-add with embedded text properties preserves them."
  (let* ((propertized-text (copy-sequence "Test"))
         (_ (put-text-property 0 4 'custom-prop 'value propertized-text))
         (result (tp-add "X" 'tp-text propertized-text 'face 'bold)))
    ;; The text content should be from tp-text
    (should (equal result "Test"))
    ;; The face from props should be applied
    (should (equal (tp-at 0 'face result) 'bold))
    ;; The embedded custom-prop from tp-text should be preserved
    (should (equal (tp-at 0 'custom-prop result) 'value))))

(ert-deftest tp-test-tp-text-face-merging ()
  "Test that tp-add with tp-text merges embedded face property with props face."
  ;; This is the core use case for tp-add: merging face 'bold with face (:foreground \"red\")
  (let ((result (tp-add "emacs" 'face 'bold 'tp-text (propertize "vim" 'face '(:foreground "red")))))
    ;; Text should be replaced
    (should (equal result "vim"))
    ;; Face should be merged: (:foreground \"red\") + bold
    (let ((face-val (tp-at 0 'face result)))
      ;; Should contain both the plist and symbol
      (should (member 'bold (if (listp face-val) face-val (list face-val))))
      ;; Should have foreground red
      (should (or (eq face-val '(:foreground "red"))
                  (and (listp face-val)
                       (cl-some (lambda (f)
                                  (and (listp f)
                                       (equal (plist-get f :foreground) "red")))
                                face-val)))))))

(ert-deftest tp-test-tp-add-face-override-subprops ()
  "Test that tp-add with tp-text overrides same face sub-properties."
  ;; When new props have same sub-property as embedded, new value should override
  ;; Example: new (:foreground "green") should override embedded (:foreground "red")
  (let ((result (tp-add "emacs" 'face '(:foreground "green")
                        'tp-text (propertize "vim" 'face '(:foreground "red")))))
    (should (equal result "vim"))
    ;; Face should be (:foreground "green") - new overrides old
    (let ((face-val (tp-at 0 'face result)))
      (should (equal face-val '(:foreground "green")))))
  ;; More complex case: new (bold (:foreground "green")) with embedded (:foreground "red")
  (let ((result (tp-add "emacs" 'face '(bold (:foreground "green"))
                        'tp-text (propertize "vim" 'face '(:foreground "red")))))
    (should (equal result "vim"))
    ;; Face should be (bold (:foreground "green")) - new overrides old
    (let ((face-val (tp-at 0 'face result)))
      (should (member 'bold (if (listp face-val) face-val (list face-val))))
      ;; Should have green, not red
      (should (cl-some (lambda (f)
                         (and (listp f)
                              (keywordp (car-safe f))
                              (equal (plist-get f :foreground) "green")))
                       (if (and (listp face-val) (not (keywordp (car-safe face-val))))
                           face-val
                         (list face-val))))))
  ;; Mixed format case: new (bold :foreground "green") with embedded (:foreground "red")
  (let ((result (tp-add "emacs" 'face '(bold :foreground "green")
                        'tp-text (propertize "vim" 'face '(:foreground "red")))))
    (should (equal result "vim"))
    ;; Face should be (bold (:foreground "green")) - parsed correctly and new overrides old
    (let ((face-val (tp-at 0 'face result)))
      (should (member 'bold (if (listp face-val) face-val (list face-val))))
      ;; Should have green, not red
      (should (cl-some (lambda (f)
                         (and (listp f)
                              (keywordp (car-safe f))
                              (equal (plist-get f :foreground) "green")))
                       (if (and (listp face-val) (not (keywordp (car-safe face-val))))
                           face-val
                         (list face-val)))))))

;;; ============================================================
;;; New define-tp Format Tests (Parameterized and Non-Parameterized)
;;; ============================================================

(ert-deftest tp-test-define-tp-non-parameterized ()
  "Test define-tp with non-parameterized format (empty arglist)."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    (should (assoc 'tp-bold tp-layer-alist))
    ;; Unified structure: (LAYER-NAME nil BODY-FORM) where BODY-FORM is quoted
    (let ((entry (cdr (assoc 'tp-bold tp-layer-alist))))
      (should (= (length entry) 2))
      (should (null (car entry)))  ; arglist is nil
      (should (equal (eval (cadr entry)) '(face bold))))))

(ert-deftest tp-test-define-tp-non-parameterized-usage-string ()
  "Test non-parameterized layer usage with string: (tp-set string 'layer-name t).
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    (let ((result (tp-set "emacs" 'tp-bold t)))
      ;; Result should have the correct properties
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (eq (get-text-property 0 'face result) 'bold)))))

(ert-deftest tp-test-define-tp-non-parameterized-usage-region ()
  "Test non-parameterized layer usage with region: (tp-set start end '(layer-name t)).
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "emacs")
    (define-tp tp-bold ()
      '(face bold))
    (tp-set 1 6 '(tp-bold t))
    ;; Check properties in buffer
    (should-not (tp-at 1 'tp-name))  ; no tp-name for direct setting
    (should (eq (tp-at 1 'face) 'bold))))

(ert-deftest tp-test-define-tp-parameterized ()
  "Test define-tp with parameterized format."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      (list 'display (list 'space :width (list pixel))))
    ;; Check it's registered as a parameterized layer in tp-layer-alist
    (should (assoc 'tp-space tp-layer-alist))
    (should (tp-layer-parameterized-p 'tp-space))
    ;; Check the structure is correct (ARGLIST BODY-FORM)
    (let ((entry (cdr (assoc 'tp-space tp-layer-alist))))
      ;; entry is (ARGLIST BODY-FORM)
      (should (equal (car entry) '(pixel))))))

(ert-deftest tp-test-define-tp-parameterized-usage-string ()
  "Test parameterized layer usage with string: (tp-set string 'layer-name arg).
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      (list 'display (list 'space :width (list pixel))))
    (let ((result (tp-set "emacs" 'tp-space 2)))
      ;; Result should have the correct properties
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (2)))))))

(ert-deftest tp-test-define-tp-parameterized-usage-region ()
  "Test parameterized layer usage with region: (tp-set start end '(layer-name arg)).
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (insert "emacs")
    (define-tp tp-space (pixel)
      (list 'display (list 'space :width (list pixel))))
    (tp-set 1 6 '(tp-space 5))
    ;; Check properties in buffer
    (should-not (tp-at 1 'tp-name))  ; no tp-name for direct setting
    (should (equal (tp-at 1 'display) '(space :width (5))))))

(ert-deftest tp-test-define-tp-parameterized-backquote ()
  "Test parameterized layer with backquote syntax.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-test-space (pixel)
      `(display (space :width (,pixel))))
    (let ((result (tp-set "emacs" 'tp-test-space 10)))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (10)))))))

(ert-deftest tp-test-define-tp-parameterized-undefine ()
  "Test tp-undefine-layer clears parameterized layer info."
  (tp-test-with-temp-buffer
    (define-tp tp-test-param (arg)
      (list 'display arg))
    (should (assoc 'tp-test-param tp-layer-alist))
    (should (tp-layer-parameterized-p 'tp-test-param))
    (tp-undefine-layer 'tp-test-param)
    (should-not (assoc 'tp-test-param tp-layer-alist))))

(ert-deftest tp-test-layer-reset-clears-params ()
  "Test tp-layer-reset clears parameterized layers."
  (tp-test-with-temp-buffer
    (define-tp tp-test-param (arg)
      (list 'display arg))
    (should (assoc 'tp-test-param tp-layer-alist))
    (tp-layer-reset)
    (should-not tp-layer-alist)))

(ert-deftest tp-test-layer-with-extra-props-string ()
  "Test layer with extra native properties on string.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    ;; Non-parameterized layer with extra props
    (let ((result (tp-set "emacs" 'tp-bold t 'face '(:foreground "green"))))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      ;; Should have both face values in the plist
      (let ((props (text-properties-at 0 result)))
        (should (member 'face props))))))

(ert-deftest tp-test-parameterized-layer-with-extra-props-string ()
  "Test parameterized layer with extra native properties on string.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Parameterized layer with extra props
    (let ((result (tp-set "emacs" 'tp-space 6 'face '(:foreground "green"))))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      (should (equal (get-text-property 0 'face result) '(:foreground "green"))))))

(ert-deftest tp-test-layer-with-extra-props-region ()
  "Test layer with extra native properties on region.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    ;; Region form with extra props
    (let ((result (tp-set 0 5 '(tp-bold t face (:foreground "green")) "emacs")))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      ;; Should have both face values in the plist
      (let ((props (text-properties-at 0 result)))
        (should (member 'face props))))))

(ert-deftest tp-test-parameterized-layer-with-extra-props-region ()
  "Test parameterized layer with extra native properties on region.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Region form with extra props
    (let ((result (tp-set 0 5 '(tp-space 6 face (:foreground "green")) "emacs")))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      (should (equal (get-text-property 0 'face result) '(:foreground "green"))))))

(ert-deftest tp-test-layer-at-any-position-string ()
  "Test layer properties can be at any position in string form.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Layer in the middle of the plist
    (let ((result (tp-set "emacs"
                          'face '(:foreground "green")
                          'tp-space 6
                          'test "test")))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      (should (equal (get-text-property 0 'face result) '(:foreground "green")))
      (should (equal (get-text-property 0 'test result) "test")))))

(ert-deftest tp-test-layer-at-any-position-region ()
  "Test layer properties can be at any position in region form.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Layer in the middle of the plist
    (let ((result (tp-set 0 5 '(face (:foreground "green") tp-space 6 test "test") "emacs")))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      (should (equal (get-text-property 0 'face result) '(:foreground "green")))
      (should (equal (get-text-property 0 'test result) "test")))))

(ert-deftest tp-test-non-param-layer-at-any-position ()
  "Test non-parameterized layer at any position.
When using tp-set (direct property setting), tp-name is NOT added."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    ;; Layer in the middle of the plist
    (let ((result (tp-set "emacs"
                          'test1 "value1"
                          'tp-bold t
                          'test2 "value2")))
      (should-not (get-text-property 0 'tp-name result))  ; no tp-name for direct setting
      (should (equal (get-text-property 0 'test1 result) "value1"))
      (should (equal (get-text-property 0 'test2 result) "value2")))))

;; Tests for tp-push-layer and tp-put-layer with define-tp layers
(ert-deftest tp-test-push-layer-non-parameterized ()
  "Test tp-push-layer with non-parameterized define-tp layer."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    ;; String form
    (let ((result (tp-push-layer "emacs" 'tp-bold)))
      (should (eq (get-text-property 0 'tp-name result) 'tp-bold))
      (should (eq (get-text-property 0 'face result) 'bold)))))

(ert-deftest tp-test-push-layer-parameterized ()
  "Test tp-push-layer with parameterized define-tp layer."
  (tp-test-with-temp-buffer
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; String form with parameterized layer
    (let ((result (tp-push-layer "emacs" '(tp-space 6))))
      (should (eq (get-text-property 0 'tp-name result) 'tp-space))
      (should (equal (get-text-property 0 'display result) '(space :width (6)))))))

(ert-deftest tp-test-put-layer-non-parameterized ()
  "Test tp-put-layer with non-parameterized define-tp layer."
  (tp-test-with-temp-buffer
    (define-tp tp-italic ()
      '(face italic))
    ;; String form
    (let ((result (tp-put-layer "emacs" 'tp-italic 0)))
      (should (eq (get-text-property 0 'tp-name result) 'tp-italic))
      (should (eq (get-text-property 0 'face result) 'italic)))))

(ert-deftest tp-test-put-layer-parameterized ()
  "Test tp-put-layer with parameterized define-tp layer."
  (tp-test-with-temp-buffer
    (define-tp tp-width (pixels)
      `(display (space :width (,pixels))))
    ;; String form with parameterized layer
    (let ((result (tp-put-layer "emacs" '(tp-width 10) 0)))
      (should (eq (get-text-property 0 'tp-name result) 'tp-width))
      (should (equal (get-text-property 0 'display result) '(space :width (10)))))))

;; Tests for reactive variables with define-tp layers
(ert-deftest tp-test-define-tp-with-reactive-var-needs-tp-name ()
  "Test define-tp layers mixed with reactive variables get anonymous tp-name."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Define a reactive variable
    (defvar $tp-test-color "red")
    (defvar $tp-test-pixel 10)
    ;; Using reactive variables - should get anonymous tp-name
    ;; Note: When using backquote `, the $vars are expanded at read time
    ;; so this doesn't test the reactive detection. Instead we test that
    ;; the expansion works correctly.
    (let ((result (tp-set 0 5 `(face (:foreground ,$tp-test-color)
                                     tp-bold t
                                     tp-space ,$tp-test-pixel)
                          "emacs")))
      ;; Verify the expansion happened - display property should be set
      (should (equal (get-text-property 0 'display result) '(space :width (10)))))))

(ert-deftest tp-test-define-tp-without-reactive-var-no-tp-name ()
  "Test define-tp layers without reactive variables do NOT get tp-name."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; Not using reactive variables - should NOT have tp-name
    (let ((result (tp-set 0 5 '(face (:foreground "green")
                                     tp-bold t
                                     tp-space 6)
                          "emacs")))
      (should-not (get-text-property 0 'tp-name result))
      ;; Display property should be expanded from tp-space
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      ;; Face property exists (first one found is (:foreground "green"))
      (should (get-text-property 0 'face result)))))

(ert-deftest tp-test-define-tp-string-form-without-reactive-no-tp-name ()
  "Test define-tp layers in string form without reactive vars - no tp-name."
  (tp-test-with-temp-buffer
    (define-tp tp-bold ()
      '(face bold))
    (define-tp tp-space (pixel)
      `(display (space :width (,pixel))))
    ;; String form - not using reactive variables - should NOT have tp-name
    (let ((result (tp-set "emacs"
                          'face '(:foreground "green")
                          'tp-bold t
                          'tp-space 6)))
      (should-not (get-text-property 0 'tp-name result))
      ;; Display property should be expanded from tp-space
      (should (equal (get-text-property 0 'display result) '(space :width (6))))
      ;; Face property exists
      (should (get-text-property 0 'face result)))))

;;; ============================================================
;;; Batched Updates Tests
;;; ============================================================

(ert-deftest tp-test-batch-updates-basic ()
  "Test that tp-with-batch-updates defers reactive updates."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define a reactive layer
          (define-tp test-batch-layer ()
            :props '(face (:foreground $tp-test-batch-color))
            :data '((tp-test-batch-color . "red")))
          (insert "Hello World")
          (tp-set 1 6 'test-batch-layer)
          ;; Initial color should be red
          (should (equal (plist-get (tp-at 1 'face) :foreground) "red"))
          ;; Now use batch updates
          (tp-with-batch-updates
            (setq tp-test-batch-color "blue")
            ;; Inside batch, layer definition is updated but buffer may not be
            ;; (implementation note: the layer props are always updated immediately)
            )
          ;; After batch ends, buffer should be updated
          (should (equal (plist-get (tp-at 1 'face) :foreground) "blue")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-batch-color)))))

(ert-deftest tp-test-batch-updates-multiple-vars ()
  "Test that tp-with-batch-updates consolidates multiple variable changes."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define a reactive layer with multiple vars
          (define-tp test-multi-batch ()
            :props '(face (:foreground $tp-test-fg :background $tp-test-bg))
            :data '((tp-test-fg . "white") (tp-test-bg . "black")))
          (insert "Hello World")
          (tp-set 1 6 'test-multi-batch)
          ;; Use batch updates
          (tp-with-batch-updates
            (setq tp-test-fg "yellow")
            (setq tp-test-bg "navy"))
          ;; Both should be updated
          (should (equal (plist-get (tp-at 1 'face) :foreground) "yellow"))
          (should (equal (plist-get (tp-at 1 'face) :background) "navy")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-fg))
      (ignore-errors (makunbound 'tp-test-bg)))))

;;; ============================================================
;;; Debug Mode Tests
;;; ============================================================

(ert-deftest tp-test-debug-mode-logs ()
  "Test that debug mode logs to *tp-debug* buffer."
  (tp-test-with-temp-buffer
    (let ((tp-debug-mode t)
          (tp-debug-echo nil))
      ;; Clear any existing debug buffer
      (tp-debug-clear)
      ;; Log a message
      (tp-debug-log "Test message %d" 42)
      ;; Check the debug buffer
      (with-current-buffer (get-buffer "*tp-debug*")
        (should (string-match-p "Test message 42" (buffer-string)))))))

(ert-deftest tp-test-debug-mode-disabled ()
  "Test that debug mode does not log when disabled."
  (tp-test-with-temp-buffer
    (let ((tp-debug-mode nil))
      ;; Clear any existing debug buffer
      (tp-debug-clear)
      ;; Try to log a message
      (tp-debug-log "Should not appear")
      ;; Check that buffer is empty or doesn't exist
      (let ((buf (get-buffer "*tp-debug*")))
        (if buf
            (with-current-buffer buf
              (should (string= (buffer-string) ""))))))))

;;; ============================================================
;;; Value Transformation Tests
;;; ============================================================

(ert-deftest tp-test-transform-basic ()
  "Test that :transform transforms tp-text values."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define a layer with transform
          (define-tp test-transform-layer ()
            :props '(face bold tp-text $tp-test-value)
            :data '((tp-test-value . "hello"))
            :transform #'upcase)
          (insert "placeholder")
          (tp-set 1 12 'test-transform-layer)
          ;; Text should be transformed to uppercase
          (should (equal (buffer-substring-no-properties 1 6) "HELLO")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-value)))))

(ert-deftest tp-test-transform-with-reactive-update ()
  "Test that :transform works with reactive updates."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define a layer with transform (format as currency)
          (define-tp test-currency-layer ()
            :props '(face bold tp-text $tp-test-amount)
            :data '((tp-test-amount . "100"))
            :transform (lambda (text)
                         (format "$%s.00" text)))
          (insert "placeholder")
          (tp-set 1 12 'test-currency-layer)
          ;; Text should be formatted
          (should (equal (buffer-substring-no-properties 1 8) "$100.00"))
          ;; Update the variable
          (setq tp-test-amount "250")
          ;; Text should be updated with transform applied
          (should (equal (buffer-substring-no-properties 1 8) "$250.00")))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-amount)))))

(ert-deftest tp-test-transform-removed-on-redefine ()
  "Test that :transform is removed when layer is redefined without it."
  (tp-test-with-temp-buffer
    (unwind-protect
        (progn
          ;; Define with transform
          (define-tp test-redef-transform ()
            :props '(face bold tp-text $tp-test-text)
            :data '((tp-test-text . "hello"))
            :transform #'upcase)
          ;; Check transform is registered
          (should (assoc 'test-redef-transform tp-layer-transforms))
          ;; Redefine without transform
          (define-tp test-redef-transform ()
            :props '(face bold tp-text $tp-test-text)
            :data '((tp-test-text . "hello")))
          ;; Transform should be removed
          (should-not (assoc 'test-redef-transform tp-layer-transforms)))
      ;; Cleanup
      (ignore-errors (makunbound 'tp-test-text)))))

;;; ============================================================
;;; Built-in Text Property Name Validation Tests
;;; ============================================================

(ert-deftest tp-test-builtin-text-property-check ()
  "Test that tp--builtin-text-property-p correctly identifies built-in properties."
  ;; Check known built-in properties
  (should (tp--builtin-text-property-p 'face))
  (should (tp--builtin-text-property-p 'display))
  (should (tp--builtin-text-property-p 'invisible))
  (should (tp--builtin-text-property-p 'help-echo))
  (should (tp--builtin-text-property-p 'keymap))
  (should (tp--builtin-text-property-p 'mouse-face))
  (should (tp--builtin-text-property-p 'read-only))
  (should (tp--builtin-text-property-p 'front-sticky))
  (should (tp--builtin-text-property-p 'rear-nonsticky))
  ;; Check non-built-in properties
  (should-not (tp--builtin-text-property-p 'tp-my-custom-layer))
  (should-not (tp--builtin-text-property-p 'my-layer))
  (should-not (tp--builtin-text-property-p 'custom-property)))

(ert-deftest tp-test-define-tp-rejects-builtin-names ()
  "Test that define-tp rejects built-in text property names."
  (tp-test-with-temp-buffer
    ;; Test that using 'face as a layer name raises an error
    (should-error
     (eval '(define-tp face () '(help-echo "test"))))
    ;; Test that using 'display as a layer name raises an error
    (should-error
     (eval '(define-tp display () '(face bold))))
    ;; Test that using 'invisible as a layer name raises an error
    (should-error
     (eval '(define-tp invisible () '(face bold))))
    ;; Test that using 'keymap as a layer name raises an error
    (should-error
     (eval '(define-tp keymap () '(face bold))))
    ;; Test that valid names work fine
    (should (define-tp tp-test-valid-layer () '(face bold)))))

(ert-deftest tp-test-define-tp-parameterized-rejects-builtin ()
  "Test that parameterized define-tp also rejects built-in names."
  (tp-test-with-temp-buffer
    ;; Parameterized layer with built-in name should fail
    (should-error
     (eval '(define-tp display (value) `(face (:height ,value)))))
    ;; Valid parameterized layer should work
    (should (define-tp tp-test-param-valid (value) `(face (:height ,value))))))

;;; ============================================================
;;; Nested Layer Resolution Tests
;;; ============================================================

(ert-deftest tp-test-nested-layer-resolution ()
  "Test that nested custom layers are resolved to built-in properties.
When a layer's body returns a plist containing other custom layer names,
those should be recursively expanded to their built-in properties."
  (tp-test-with-temp-buffer
    ;; Define a base layer that returns built-in properties
    (define-tp tp-test-base-layer (color)
      `(face (:foreground ,color :background "white")))
    ;; Define a wrapper layer that uses the base layer
    (define-tp tp-test-wrapper-layer (plist)
      (let ((color (plist-get plist :color)))
        `(tp-test-base-layer ,color
                             help-echo "wrapper")))
    ;; Use the wrapper layer
    (let ((result (tp-set "test" 'tp-test-wrapper-layer '(:color "red"))))
      ;; The face property should be resolved from tp-test-base-layer
      (should (equal (plist-get (get-text-property 0 'face result) :foreground) "red"))
      (should (equal (plist-get (get-text-property 0 'face result) :background) "white"))
      ;; help-echo should also be present
      (should (equal (get-text-property 0 'help-echo result) "wrapper"))
      ;; tp-test-base-layer should NOT be present as a property
      (should (null (get-text-property 0 'tp-test-base-layer result))))))

(ert-deftest tp-test-deeply-nested-layer-resolution ()
  "Test that deeply nested layers (3 levels) are fully resolved."
  (tp-test-with-temp-buffer
    ;; Define 3 levels of nesting
    (define-tp tp-test-level1 (val)
      `(face (:foreground ,val)))
    (define-tp tp-test-level2 (val)
      `(tp-test-level1 ,val help-echo "level2"))
    (define-tp tp-test-level3 (val)
      `(tp-test-level2 ,val display "level3"))
    ;; Use the most deeply nested layer
    (let ((result (tp-set "test" 'tp-test-level3 "blue")))
      ;; All properties should be resolved
      (should (equal (plist-get (get-text-property 0 'face result) :foreground) "blue"))
      (should (equal (get-text-property 0 'help-echo result) "level2"))
      (should (equal (get-text-property 0 'display result) "level3"))
      ;; None of the custom layer names should be present
      (should (null (get-text-property 0 'tp-test-level1 result)))
      (should (null (get-text-property 0 'tp-test-level2 result))))))

;;; ============================================================
;;; Duplicate Property Merging Tests
;;; ============================================================

(ert-deftest tp-test-merge-duplicate-face-symbols ()
  "Test that multiple face symbols in one call are merged into a face list."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs"
                          'face 'bold
                          'face 'italic)))
      ;; Should be a list with italic first (later takes precedence)
      (let ((face-prop (get-text-property 0 'face result)))
        (should (listp face-prop))
        (should (memq 'bold face-prop))
        (should (memq 'italic face-prop))
        ;; italic should come before bold (later value takes precedence)
        (should (< (cl-position 'italic face-prop)
                   (cl-position 'bold face-prop)))))))

(ert-deftest tp-test-merge-duplicate-face-plists ()
  "Test that multiple face plists in one call are merged."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs"
                          'face '(:background "green")
                          'face '(:foreground "red"))))
      (let ((face-prop (get-text-property 0 'face result)))
        ;; Should be a merged plist
        (should (plist-get face-prop :background))
        (should (plist-get face-prop :foreground))
        (should (equal (plist-get face-prop :background) "green"))
        (should (equal (plist-get face-prop :foreground) "red"))))))

(ert-deftest tp-test-merge-duplicate-face-later-overrides ()
  "Test that later face plist values override earlier ones for same key."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs"
                          'face '(:foreground "red")
                          'face '(:foreground "yellow"))))
      (let ((face-prop (get-text-property 0 'face result)))
        ;; Later value should override
        (should (equal (plist-get face-prop :foreground) "yellow"))))))

(ert-deftest tp-test-merge-other-props-later-overrides ()
  "Test that non-face duplicate properties use later value."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs"
                          'help-echo "first"
                          'help-echo "second")))
      (should (equal (get-text-property 0 'help-echo result) "second")))))

(ert-deftest tp-test-merge-multiple-layers-with-face ()
  "Test merging multiple layers that each contribute face properties."
  (tp-test-with-temp-buffer
    (define-tp tp-test-layer1 ()
      '(face (:foreground "blue")))
    (define-tp tp-test-layer2 ()
      '(face (:background "yellow")))
    (let ((result (tp-set "emacs"
                          'tp-test-layer1 t
                          'tp-test-layer2 t
                          'face '(:weight bold))))
      (let ((face-prop (get-text-property 0 'face result)))
        ;; All face properties should be merged
        (should (equal (plist-get face-prop :foreground) "blue"))
        (should (equal (plist-get face-prop :background) "yellow"))
        (should (equal (plist-get face-prop :weight) 'bold))))))

(ert-deftest tp-test-merge-in-region-form ()
  "Test duplicate property merging in region form."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-set 1 6 '(face bold face (:foreground "green")))
    (let ((face-prop (tp-at 1 'face)))
      ;; Should be a list with plist and symbol
      (should (listp face-prop))
      ;; Check properties
      (should (or (memq 'bold face-prop)
                  (eq face-prop 'bold))))))

(ert-deftest tp-test-tp-add-merge-faces ()
  "Test that tp-add also merges duplicate face properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-add 1 6 '(face bold face (:foreground "red")))
    (let ((face-prop (tp-at 1 'face)))
      ;; Should have both face values merged
      (should (listp face-prop))
      (should (memq 'bold face-prop))
      ;; Check for the plist part with :foreground
      (should (cl-some (lambda (f)
                         (and (listp f)
                              (keywordp (car f))
                              (equal (plist-get f :foreground) "red")))
                       face-prop)))))

(ert-deftest tp-test-tp-reset-merge-faces ()
  "Test that tp-reset also merges duplicate face properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-reset 1 6 '(face bold face (:foreground "red")))
    (let ((face-prop (tp-at 1 'face)))
      ;; Should have both face values merged
      (should (listp face-prop))
      (should (memq 'bold face-prop)))))

(ert-deftest tp-test-merge-mouse-face ()
  "Test that mouse-face properties are also merged."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs"
                          'mouse-face 'highlight
                          'mouse-face '(:background "blue"))))
      (let ((mouse-face-prop (get-text-property 0 'mouse-face result)))
        ;; Should be a list with plist and symbol
        (should (listp mouse-face-prop))
        (should (memq 'highlight mouse-face-prop))))))

;;; ============================================================
;;; Nil Value Property Tests (Issue: "Odd length text property list")
;;; ============================================================

(ert-deftest tp-test-set-with-nil-value ()
  "Test tp-set with nil value produces valid property list.
Regression test for: (tp-set \"emacs\" 'face nil) erroring with
\"Odd length text property list\"."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs" 'face nil)))
      ;; Result should be #("emacs" 0 5 (face nil))
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) nil))
      ;; Verify the property list is valid (has even length)
      (let ((props (text-properties-at 0 result)))
        (should (= (% (length props) 2) 0))))))

(ert-deftest tp-test-set-with-nil-value-in-middle ()
  "Test tp-set with nil value in middle of property list."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs" 'face 'bold 'help-echo nil 'display "test")))
      ;; Result should have face=bold, help-echo=nil, display="test"
      (should (eq (get-text-property 0 'face result) 'bold))
      (should (eq (get-text-property 0 'help-echo result) nil))
      (should (equal (get-text-property 0 'display result) "test")))))

(ert-deftest tp-test-set-with-multiple-nil-values ()
  "Test tp-set with multiple nil values."
  (tp-test-with-temp-buffer
    (let ((result (tp-set "emacs" 'face nil 'help-echo nil)))
      (should (eq (get-text-property 0 'face result) nil))
      (should (eq (get-text-property 0 'help-echo result) nil)))))

(ert-deftest tp-test-reset-with-nil-value ()
  "Test tp-reset with nil value works correctly."
  (tp-test-with-temp-buffer
    (let ((result (tp-reset "emacs" 'face nil)))
      ;; Result should have face=nil
      (should (eq (get-text-property 0 'face result) nil)))))

(ert-deftest tp-test-add-with-nil-value ()
  "Test tp-add with nil value works correctly."
  (tp-test-with-temp-buffer
    (let ((result (tp-add "emacs" 'face nil)))
      ;; Result should have face=nil
      (should (eq (get-text-property 0 'face result) nil)))))

(ert-deftest tp-test-set-nil-value-in-buffer ()
  "Test tp-set with nil value in buffer region."
  (tp-test-with-temp-buffer
    (insert "emacs")
    (tp-set 1 6 '(face nil))
    (should (eq (tp-at 1 'face) nil))))

;;; ============================================================
;;; Non-Destructive String Modification Tests
;;; ============================================================

(ert-deftest tp-test-set-preserves-text-property-intervals ()
  "Test that tp-set preserves text property intervals when adding new properties.
When a string has different properties at different positions, adding a new
property should preserve the original interval structure.

Test string: \" button \" (8 characters, positions 0-7)
  - Position 0-1: display property (first space character)
  - Position 1-7: no display property (text \"button \")
  - Position 7-8: display property (last space character)"
  (let ((original #(" button " 0 1 (display (space :width (4)))
                               7 8 (display (space :width (4))))))
    (let ((result (tp-set original 'face '(:foreground "red"))))
      ;; Result should be a new string with properties
      (should (stringp result))
      ;; Result should NOT be the same object as original
      (should (not (eq original result)))
      ;; Original should NOT be modified
      (should (null (get-text-property 0 'face original)))
      ;; Result should have face property everywhere
      (should (equal (get-text-property 0 'face result) '(:foreground "red")))
      (should (equal (get-text-property 4 'face result) '(:foreground "red")))
      (should (equal (get-text-property 7 'face result) '(:foreground "red")))
      ;; Result should preserve display property at original positions
      (should (equal (get-text-property 0 'display result) '(space :width (4))))
      (should (null (get-text-property 2 'display result)))  ;; No display at position 2
      (should (equal (get-text-property 7 'display result) '(space :width (4)))))))

(ert-deftest tp-test-set-does-not-modify-original-string ()
  "Test that tp-set returns a new string and does not modify the original."
  (let ((original "Hello"))
    (let ((result (tp-set original 'face 'bold)))
      ;; Result should be a new string with properties
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) 'bold))
      ;; Original should NOT be modified (no properties)
      (should (null (get-text-property 0 'face original)))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-reset-does-not-modify-original-string ()
  "Test that tp-reset returns a new string and does not modify the original."
  (let ((original "Hello"))
    (let ((result (tp-reset original 'face 'bold)))
      ;; Result should be a new string with properties
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) 'bold))
      ;; Original should NOT be modified (no properties)
      (should (null (get-text-property 0 'face original)))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-add-does-not-modify-original-string ()
  "Test that tp-add returns a new string and does not modify the original."
  (let ((original "Hello"))
    (let ((result (tp-add original 'face 'bold)))
      ;; Result should be a new string with properties
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) 'bold))
      ;; Original should NOT be modified (no properties)
      (should (null (get-text-property 0 'face original)))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-remove-does-not-modify-original-string ()
  "Test that tp-remove returns a new string and does not modify the original."
  ;; First create a propertized string (using propertize to create the original)
  (let ((original (propertize "Hello" 'face 'bold 'help-echo "tip")))
    (let ((result (tp-remove original 'face)))
      ;; Result should be a new string without face property
      (should (stringp result))
      (should (null (get-text-property 0 'face result)))
      (should (equal (get-text-property 0 'help-echo result) "tip"))
      ;; Original should still have face property
      (should (eq (get-text-property 0 'face original) 'bold))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-set-region-modifies-original-string ()
  "Test that tp-set with region form DOES modify the original string.
The region form (tp-set START END PROPS STRING) modifies the string in-place."
  (let ((original (copy-sequence "Hello World")))
    (let ((result (tp-set 0 5 '(face bold) original)))
      ;; Result should be the same object as original (modified in-place)
      (should (eq result original))
      ;; Both should have the face property
      (should (eq (get-text-property 0 'face result) 'bold))
      (should (eq (get-text-property 0 'face original) 'bold)))))

(ert-deftest tp-test-match-set-does-not-modify-original-string ()
  "Test that tp-match-set returns a new string and does not modify the original."
  (let ((original "Hello World"))
    (let ((result (tp-match-set "Hello" '(face bold) original)))
      ;; Result should be a new string with properties
      (should (stringp result))
      (should (eq (get-text-property 0 'face result) 'bold))
      ;; Original should NOT be modified (no properties)
      (should (null (get-text-property 0 'face original)))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-regexp-set-does-not-modify-original-string ()
  "Test that tp-regexp-set returns a new string and does not modify the original."
  (let ((original "abc 123 def"))
    (let ((result (tp-regexp-set "[0-9]+" '(face bold) original)))
      ;; Result should be a new string with properties on the match
      (should (stringp result))
      (should (eq (get-text-property 4 'face result) 'bold))
      ;; Original should NOT be modified (no properties)
      (should (null (get-text-property 4 'face original)))
      ;; Strings should not be eq (different objects)
      (should (not (eq original result))))))

(ert-deftest tp-test-remove-custom-layer ()
  "Test that tp-remove correctly removes custom text property layers.
When a layer is removed, only its face contribution should be removed,
not the entire face property."
  ;; First define the custom layer
  (tp-layer-reset)
  (eval '(define-tp tp-delete (color)
           `(face (:strike-through ,color))))
  ;; Test with entire string form
  (let* ((str "emacs")
         (str-with-props (tp-set str 'face 'bold 'tp-delete t))
         (result (tp-remove str-with-props 'tp-delete)))
    ;; Original should still have the properties
    (should (get-text-property 0 'face str-with-props))
    ;; Result should have face 'bold (only the tp-delete contribution removed)
    (should (equal (get-text-property 0 'face result) 'bold))
    ;; Result should not have tp-delete property
    (should (null (get-text-property 0 'tp-delete result)))
    ;; Result should not have tp-name property
    (should (null (get-text-property 0 'tp-name result)))))

(ert-deftest tp-test-remove-custom-layer-preserves-other-props ()
  "Test that tp-remove with layer name preserves other properties.
When the layer property is set (via mixed syntax), its face contribution
can be tracked and removed."
  (tp-layer-reset)
  (eval '(define-tp tp-delete (color)
           `(face (:strike-through ,color))))
  ;; Test with mixed syntax - set layer alongside other properties
  ;; This allows tracking of the layer property
  (let* ((str "emacs")
         (str-with-props (tp-set str 'help-echo "test" 'tp-delete "red"))
         (result (tp-remove str-with-props 'tp-delete)))
    ;; help-echo should still be present
    (should (equal (get-text-property 0 'help-echo result) "test"))
    ;; face (from tp-delete) should be removed
    (should (null (get-text-property 0 'face result)))
    ;; tp-delete property should be removed
    (should (null (get-text-property 0 'tp-delete result)))))

(provide 'tp-ert-tests)
;;; tp-ert-tests.el ends here
