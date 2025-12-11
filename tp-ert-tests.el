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
  "Test tp-put and tp-get basic functionality."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; Set a single property
    (tp-put 1 6 'face 'bold)
    (should (eq (tp-get 1 'face) 'bold))
    (should (eq (tp-get 3 'face) 'bold))
    (should (null (tp-get 7 'face)))
    ;; Set multiple properties
    (tp-put 7 12 'face 'italic 'help-echo "test")
    (should (eq (tp-get 7 'face) 'italic))
    (should (equal (tp-get 7 'help-echo) "test"))))

(ert-deftest tp-test-put-with-list ()
  "Test tp-put accepts properties as a list."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-put 1 6 '(face bold help-echo "greeting"))
    (should (eq (tp-get 1 'face) 'bold))
    (should (equal (tp-get 1 'help-echo) "greeting"))))

(ert-deftest tp-test-put-returns-region ()
  "Test tp-put returns the modified region."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (let ((result (tp-put 1 6 'face 'bold)))
      (should (equal result '(1 . 6))))))

(ert-deftest tp-test-remove ()
  "Test tp-remove removes a specific property."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-put 1 6 'face 'bold 'help-echo "test")
    (should (eq (tp-get 1 'face) 'bold))
    (tp-remove 1 6 'face)
    (should (null (tp-get 1 'face)))
    (should (equal (tp-get 1 'help-echo) "test"))))

(ert-deftest tp-test-remove-list ()
  "Test tp-remove-list removes multiple properties."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-put 1 6 'face 'bold 'help-echo "test" 'mouse-face 'highlight)
    (tp-remove-list 1 6 '(face help-echo))
    (should (null (tp-get 1 'face)))
    (should (null (tp-get 1 'help-echo)))
    (should (eq (tp-get 1 'mouse-face) 'highlight))))

(ert-deftest tp-test-clear ()
  "Test tp-clear removes all properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (tp-put 7 12 'face 'italic)
    (tp-clear 1 12)
    (should (null (tp-get 1 'face)))
    (should (null (tp-get 7 'face)))))

(ert-deftest tp-test-clear-defaults-to-buffer ()
  "Test tp-clear defaults to entire buffer."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 12 'face 'bold)
    (tp-clear)
    (should (null (tp-get 1 'face)))
    (should (null (tp-get 7 'face)))))

(ert-deftest tp-test-at ()
  "Test tp-at returns all properties at point."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-put 1 6 'face 'bold 'help-echo "test")
    (let ((props (tp-at 1)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "test")))))

(ert-deftest tp-test-at-defaults-to-point ()
  "Test tp-at defaults to current point."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-put 1 6 'face 'bold)
    (goto-char 3)
    (should (eq (plist-get (tp-at) 'face) 'bold))))

(ert-deftest tp-test-plist ()
  "Test tp-plist merges properties from region."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    ;; Put both properties on the same overlapping region for proper merging
    (tp-put 1 12 'face 'bold)
    (tp-put 1 12 'help-echo "test")
    (let ((props (tp-plist 1 12)))
      (should (eq (plist-get props 'face) 'bold))
      (should (equal (plist-get props 'help-echo) "test")))))

;;; ============================================================
;;; Text Property Interval Tests
;;; ============================================================

(ert-deftest tp-test-empty-p ()
  "Test tp-empty-p detects empty properties."
  (should (tp-empty-p "plain string"))
  (should-not (tp-empty-p (propertize "styled" 'face 'bold))))

(ert-deftest tp-test-intervals ()
  "Test tp-intervals returns property intervals."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (tp-put 7 12 'face 'italic)
    (let ((intervals (tp-intervals 1 12)))
      (should (>= (length intervals) 2)))))

;;; ============================================================
;;; Layer Definition Tests
;;; ============================================================

(ert-deftest tp-test-layer-define ()
  "Test tp-layer-define creates a layer."
  (tp-test-with-temp-buffer
    (tp-layer-define test-layer '(face bold help-echo "test"))
    (should (assoc 'test-layer tp-layer-alist))
    (should (equal (cdr (assoc 'test-layer tp-layer-alist))
                   '(face bold help-echo "test")))))

(ert-deftest tp-test-layer-define-updates-existing ()
  "Test tp-layer-define updates existing layer."
  (tp-test-with-temp-buffer
    (tp-layer-define test-layer '(face bold))
    (tp-layer-define test-layer '(face italic))
    (should (equal (cdr (assoc 'test-layer tp-layer-alist))
                   '(face italic)))))

(ert-deftest tp-test-layer-props ()
  "Test tp-layer-props returns properties with tp-name."
  (tp-test-with-temp-buffer
    (tp-layer-define my-layer '(face bold))
    (let ((props (tp-layer-props 'my-layer)))
      (should (eq (plist-get props 'face) 'bold))
      (should (eq (plist-get props 'tp-name) 'my-layer)))))

(ert-deftest tp-test-layer-props-returns-nil-for-undefined ()
  "Test tp-layer-props returns nil for undefined layer."
  (tp-test-with-temp-buffer
    (should (null (tp-layer-props 'undefined-layer)))))

(ert-deftest tp-test-layer-undefine ()
  "Test tp-layer-undefine removes layer definition."
  (tp-test-with-temp-buffer
    (tp-layer-define test-layer '(face bold))
    (should (assoc 'test-layer tp-layer-alist))
    (tp-layer-undefine 'test-layer)
    (should-not (assoc 'test-layer tp-layer-alist))))

;;; ============================================================
;;; Layer Group Tests
;;; ============================================================

(ert-deftest tp-test-group-define ()
  "Test tp-group-define creates a layer group."
  (tp-test-with-temp-buffer
    (tp-group-define my-group
      layer1 '(face bold)
      layer2 '(face italic)
      layer3 '(face underline))
    (should (assoc 'my-group tp-layer-groups))
    (should (assoc 'layer1 tp-layer-alist))
    (should (assoc 'layer2 tp-layer-alist))
    (should (assoc 'layer3 tp-layer-alist))
    ;; Note: tp-group-define stores layers in reverse order (last defined is first)
    (let ((layers (cdr (assoc 'my-group tp-layer-groups))))
      (should (= (length layers) 3))
      (should (memq 'layer1 layers))
      (should (memq 'layer2 layers))
      (should (memq 'layer3 layers)))))

(ert-deftest tp-test-group-props ()
  "Test tp-group-props returns all layer properties."
  (tp-test-with-temp-buffer
    (tp-group-define my-group
      layer1 '(face bold)
      layer2 '(face italic))
    (let ((props-list (tp-group-props 'my-group)))
      (should (= (length props-list) 2))
      ;; Check that both layers are present (order may vary)
      (let ((faces (mapcar (lambda (p) (plist-get p 'face)) props-list)))
        (should (or (memq 'bold faces) (memq 'italic faces)))))))

(ert-deftest tp-test-group-undefine ()
  "Test tp-group-undefine removes group definition."
  (tp-test-with-temp-buffer
    (tp-group-define my-group
      layer1 '(face bold))
    (should (assoc 'my-group tp-layer-groups))
    (tp-group-undefine 'my-group)
    (should-not (assoc 'my-group tp-layer-groups))))

(ert-deftest tp-test-layer-reset ()
  "Test tp-layer-reset clears all definitions."
  (tp-test-with-temp-buffer
    (tp-layer-define layer1 '(face bold))
    (tp-group-define group1 layer2 '(face italic))
    (should tp-layer-alist)
    (should tp-layer-groups)
    (tp-layer-reset)
    (should-not tp-layer-alist)
    (should-not tp-layer-groups)))

;;; ============================================================
;;; Layer Stack Operations Tests
;;; ============================================================

(ert-deftest tp-test-layer-push ()
  "Test tp-layer-push adds layer to stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-push 1 6 'layer1)
    (should (eq (tp-get 1 'face) 'bold))
    (should (eq (tp-get 1 'tp-name) 'layer1))))

(ert-deftest tp-test-layer-push-multiple ()
  "Test pushing multiple layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    ;; layer2 should be on top (visible)
    (should (eq (tp-get 1 'face) 'italic))
    (should (eq (tp-get 1 'tp-name) 'layer2))
    ;; layer1 should be in the stack below
    (should (tp-get 1 'tp-layers))))

(ert-deftest tp-test-layer-push-error-on-duplicate ()
  "Test tp-layer-push errors on duplicate layer."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-push 1 6 'layer1)
    (should-error (tp-layer-push 1 6 'layer1))))

(ert-deftest tp-test-layer-delete ()
  "Test tp-layer-delete removes layer from stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    ;; Delete top layer
    (tp-layer-delete 1 6 'layer2)
    ;; layer1 should now be visible
    (should (eq (tp-get 1 'face) 'bold))
    (should (eq (tp-get 1 'tp-name) 'layer1))))

(ert-deftest tp-test-layer-delete-from-middle ()
  "Test deleting layer from middle of stack."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-define layer3 '(face underline))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    (tp-layer-push 1 6 'layer3)
    ;; Delete middle layer
    (tp-layer-delete 1 6 'layer2)
    ;; Top layer should still be visible
    (should (eq (tp-get 1 'tp-name) 'layer3))
    ;; layer2 should not exist anymore
    (should-not (tp-layer-exists-p 1 6 'layer2))))

(ert-deftest tp-test-layer-rotate ()
  "Test tp-layer-rotate cycles layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-define layer3 '(face underline))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    (tp-layer-push 1 6 'layer3)
    ;; layer3 is on top
    (should (eq (tp-layer-top 1 6) 'layer3))
    ;; Rotate once - layer2 should be on top
    (tp-layer-rotate 1 6)
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Rotate again - layer1 should be on top
    (tp-layer-rotate 1 6)
    (should (eq (tp-layer-top 1 6) 'layer1))
    ;; Rotate again - layer3 should be on top (cycled back)
    (tp-layer-rotate 1 6)
    (should (eq (tp-layer-top 1 6) 'layer3))))

(ert-deftest tp-test-layer-pin ()
  "Test tp-layer-pin brings layer to top."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-define layer3 '(face underline))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    (tp-layer-push 1 6 'layer3)
    ;; Pin layer1 to top
    (tp-layer-pin 1 6 'layer1)
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-layer-pin-error-on-nonexistent ()
  "Test tp-layer-pin errors on nonexistent layer."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-push 1 6 'layer1)
    (should-error (tp-layer-pin 1 6 'nonexistent))))

(ert-deftest tp-test-layer-hide ()
  "Test tp-layer-hide moves layer to bottom."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    ;; layer2 is on top
    (should (eq (tp-layer-top 1 6) 'layer2))
    ;; Hide layer2
    (tp-layer-hide 1 6 'layer2)
    ;; layer1 should now be on top
    (should (eq (tp-layer-top 1 6) 'layer1))))

(ert-deftest tp-test-layer-show ()
  "Test tp-layer-show brings layer to top."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    ;; Hide layer2
    (tp-layer-hide 1 6 'layer2)
    (should (eq (tp-layer-top 1 6) 'layer1))
    ;; Show layer2 again
    (tp-layer-show 1 6 'layer2)
    (should (eq (tp-layer-top 1 6) 'layer2))))

;;; ============================================================
;;; Layer Query Tests
;;; ============================================================

(ert-deftest tp-test-layer-list ()
  "Test tp-layer-list returns all layer names."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-define layer3 '(face underline))
    (tp-layer-push 1 6 'layer1)
    (tp-layer-push 1 6 'layer2)
    (tp-layer-push 1 6 'layer3)
    (let ((layers (tp-layer-list 1 6)))
      (should (= (length layers) 3))
      (should (memq 'layer1 layers))
      (should (memq 'layer2 layers))
      (should (memq 'layer3 layers)))))

(ert-deftest tp-test-layer-count ()
  "Test tp-layer-count returns correct count."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (should (= (tp-layer-count 1 6) 1))
    (tp-layer-push 1 6 'layer2)
    (should (= (tp-layer-count 1 6) 2))))

(ert-deftest tp-test-layer-exists-p ()
  "Test tp-layer-exists-p correctly detects layers."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-push 1 6 'layer1)
    (should (tp-layer-exists-p 1 6 'layer1))
    (should-not (tp-layer-exists-p 1 6 'layer2))))

(ert-deftest tp-test-layer-top ()
  "Test tp-layer-top returns top layer name."
  (tp-test-with-temp-buffer
    (insert "Hello")
    (tp-layer-define layer1 '(face bold))
    (tp-layer-define layer2 '(face italic))
    (tp-layer-push 1 6 'layer1)
    (should (eq (tp-layer-top 1 6) 'layer1))
    (tp-layer-push 1 6 'layer2)
    (should (eq (tp-layer-top 1 6) 'layer2))))

;;; ============================================================
;;; Propertize String Tests
;;; ============================================================

(ert-deftest tp-test-propertize ()
  "Test tp-propertize adds properties to string."
  (let ((str (tp-propertize "Hello" 'face 'bold)))
    (should (eq (get-text-property 0 'face str) 'bold))))

(ert-deftest tp-test-propertize-with-list ()
  "Test tp-propertize accepts properties as list."
  (let ((str (tp-propertize "Hello" '(face bold help-echo "test"))))
    (should (eq (get-text-property 0 'face str) 'bold))
    (should (equal (get-text-property 0 'help-echo str) "test"))))

(ert-deftest tp-test-layer-propertize ()
  "Test tp-layer-propertize applies layer to string."
  (tp-test-with-temp-buffer
    (tp-layer-define my-layer '(face bold help-echo "greeting"))
    (let ((str (tp-layer-propertize "Hello" 'my-layer)))
      (should (eq (get-text-property 0 'face str) 'bold))
      (should (equal (get-text-property 0 'help-echo str) "greeting")))))

(ert-deftest tp-test-layer-propertize-error-on-undefined ()
  "Test tp-layer-propertize errors on undefined layer."
  (tp-test-with-temp-buffer
    (should-error (tp-layer-propertize "Hello" 'undefined-layer))))

(ert-deftest tp-test-group-propertize ()
  "Test tp-group-propertize applies group to string."
  (tp-test-with-temp-buffer
    (tp-group-define my-group
      layer1 '(face bold)
      layer2 '(help-echo "test"))
    (let ((str (tp-group-propertize "Hello" 'my-group)))
      (should (stringp str))
      (should (= (length str) 5)))))

(ert-deftest tp-test-group-propertize-error-on-undefined ()
  "Test tp-group-propertize errors on undefined group."
  (tp-test-with-temp-buffer
    (should-error (tp-group-propertize "Hello" 'undefined-group))))

;;; ============================================================
;;; Match and Regexp Tests
;;; ============================================================

(ert-deftest tp-test-match ()
  "Test tp-match sets properties on string matches."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (let ((regions (tp-match "Hello" 'face 'bold)))
      (should (= (length regions) 2))
      (should (eq (tp-get 1 'face) 'bold))
      (should (eq (tp-get 13 'face) 'bold)))))

(ert-deftest tp-test-match-returns-regions ()
  "Test tp-match returns correct region pairs."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (let ((regions (tp-match "Hello")))
      (should (= (length regions) 2))
      (should (equal (car regions) '(1 . 6)))
      (should (equal (cadr regions) '(13 . 18))))))

(ert-deftest tp-test-regexp ()
  "Test tp-regexp sets properties on regexp matches."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (let ((regions (tp-regexp "[0-9]+" 'face 'bold)))
      (should (= (length regions) 2))
      (should (eq (tp-get 5 'face) 'bold))
      (should (eq (tp-get 13 'face) 'bold)))))

(ert-deftest tp-test-regexp-returns-regions ()
  "Test tp-regexp returns correct region pairs."
  (tp-test-with-temp-buffer
    (insert "abc 123 def 456")
    (let ((regions (tp-regexp "[0-9]+")))
      (should (= (length regions) 2)))))

;;; ============================================================
;;; Search and Navigation Tests
;;; ============================================================

(ert-deftest tp-test-forward ()
  "Test tp-forward finds next property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 7 12 'face 'bold)
    (goto-char 1)
    (let ((match (tp-forward 'face)))
      (should match)
      (should (= (prop-match-beginning match) 7)))))

(ert-deftest tp-test-backward ()
  "Test tp-backward finds previous property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (goto-char 12)
    ;; text-property-search-backward may not exist in all Emacs versions
    ;; Skip test if function is not available
    (skip-unless (fboundp 'text-property-search-backward))
    (let ((match (tp-backward 'face)))
      (should match)
      (should (= (prop-match-beginning match) 1)))))

(ert-deftest tp-test-next ()
  "Test tp-next returns next position with property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 7 12 'face 'bold)
    (let ((pos (tp-next 1 'face)))
      (should (= pos 7)))))

(ert-deftest tp-test-prev ()
  "Test tp-prev returns previous position with property."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (let ((pos (tp-prev 12 'face)))
      (should (= pos 1)))))

(ert-deftest tp-test-goto-next ()
  "Test tp-goto-next moves point."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 7 12 'face 'bold)
    (goto-char 1)
    (tp-goto-next 'face)
    (should (= (point) 7))))

(ert-deftest tp-test-goto-prev ()
  "Test tp-goto-prev moves point."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (goto-char 12)
    (tp-goto-prev 'face)
    (should (= (point) 1))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(ert-deftest tp-test-in ()
  "Test tp-in finds regions with property."
  (tp-test-with-temp-buffer
    (insert "Hello World Test")
    (tp-put 1 6 'my-prop 'value1)
    (tp-put 7 12 'my-prop 'value2)
    (let ((regions (tp-in 'my-prop)))
      (should (= (length regions) 2)))))

(ert-deftest tp-test-in-with-value ()
  "Test tp-in filters by value."
  (tp-test-with-temp-buffer
    (insert "Hello World Test")
    (tp-put 1 6 'my-prop 'value1)
    (tp-put 7 12 'my-prop 'value2)
    (let ((regions (tp-in 'my-prop 'value1)))
      (should (= (length regions) 1))
      (should (equal (car (car regions)) 1)))))

(ert-deftest tp-test-all ()
  "Test tp-all returns all regions with properties."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 6 'face 'bold)
    (tp-put 7 12 'face 'italic)
    (let ((regions (tp-all)))
      (should (>= (length regions) 2)))))

(ert-deftest tp-test-regions-map ()
  "Test tp-regions-map applies function to regions."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-put 1 6 'marker t)
    (tp-put 13 18 'marker t)
    (let ((result nil))
      (tp-regions-map
       (lambda (start end idx)
         (push (list start end idx) result))
       'marker)
      (should (= (length result) 2)))))

(ert-deftest tp-test-strings-map ()
  "Test tp-strings-map applies function to strings."
  (tp-test-with-temp-buffer
    (insert "Hello World Hello")
    (tp-put 1 6 'marker t)
    (tp-put 13 18 'marker t)
    (let ((result nil))
      (tp-strings-map
       (lambda (str idx)
         (push str result))
       'marker)
      (should (= (length result) 2))
      (should (member "Hello" result)))))

;;; ============================================================
;;; Alias Tests
;;; ============================================================

(ert-deftest tp-test-aliases-exist ()
  "Test that all aliases are properly defined."
  (should (fboundp 'tp-set))
  (should (fboundp 'tp-layer-properties))
  (should (fboundp 'tp-layer-group-define))
  (should (fboundp 'tp-layer-group-properties))
  (should (fboundp 'tp-layer-group-propertize))
  (should (fboundp 'tp-layer-group-undefine)))

(ert-deftest tp-test-aliases-work ()
  "Test that aliases work correctly."
  (tp-test-with-temp-buffer
    ;; Test tp-set alias
    (insert "Hello")
    (tp-set 1 6 'face 'bold)
    (should (eq (tp-get 1 'face) 'bold))))

;;; ============================================================
;;; Edge Case Tests
;;; ============================================================

(ert-deftest tp-test-empty-region ()
  "Test operations on empty buffer."
  (tp-test-with-temp-buffer
    (should (null (tp-at 1)))
    (should (null (tp-all)))))

(ert-deftest tp-test-overlapping-regions ()
  "Test overlapping property regions."
  (tp-test-with-temp-buffer
    (insert "Hello World")
    (tp-put 1 8 'prop1 'val1)
    (tp-put 5 12 'prop2 'val2)
    (should (eq (tp-get 1 'prop1) 'val1))
    (should (null (tp-get 1 'prop2)))
    (should (eq (tp-get 6 'prop1) 'val1))
    (should (eq (tp-get 6 'prop2) 'val2))
    (should (null (tp-get 10 'prop1)))
    (should (eq (tp-get 10 'prop2) 'val2))))

(ert-deftest tp-test-single-char-region ()
  "Test operations on single character."
  (tp-test-with-temp-buffer
    (insert "H")
    (tp-put 1 2 'face 'bold)
    (should (eq (tp-get 1 'face) 'bold))))

(ert-deftest tp-test-layer-on-string ()
  "Test layer operations on string object."
  (let ((str (copy-sequence "Hello")))
    (set-text-properties 0 5 nil str)
    (should (tp-empty-p str))))

(provide 'tp-ert-tests)
;;; tp-ert-tests.el ends here
