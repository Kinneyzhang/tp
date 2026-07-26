;;; tp-core-tests.el --- ERT tests for tp-core.el -*- lexical-binding: t -*-

;;; Commentary:

;; Unit tests for the tp-core foundation module.

;;; Code:

(require 'ert)
(require 'tp-core)

;;; tp--map-intervals

(ert-deftest tp-core-test-map-intervals-string-clips ()
  "Intervals extending beyond the range are clipped to it."
  (let ((str (copy-sequence "hello world")))
    (put-text-property 0 11 'face 'bold str)
    (should (equal (tp--map-intervals str 3 7 #'list)
                   '((3 7 (face bold)))))))

(ert-deftest tp-core-test-map-intervals-string-full ()
  "Full-range walk over a string returns each property run."
  (let ((str (copy-sequence "hello world")))
    (put-text-property 0 5 'face 'bold str)
    (should (equal (tp--map-intervals str nil nil #'list)
                   '((0 5 (face bold)) (5 11 nil))))))

(ert-deftest tp-core-test-map-intervals-single-property ()
  "PROPERTY narrows runs to that property and passes its value."
  (let ((str (copy-sequence "hello world")))
    (put-text-property 0 5 'face 'bold str)
    (put-text-property 2 8 'help-echo "tip" str)
    (should (equal (tp--map-intervals str nil nil #'list 'face)
                   '((0 5 bold) (5 11 nil))))))

(ert-deftest tp-core-test-map-intervals-buffer-clips ()
  "Buffer walk clips to the requested range with 1-based positions."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 12 'face 'bold)
    (should (equal (tp--map-intervals nil 4 8 #'list)
                   '((4 8 (face bold)))))))

(ert-deftest tp-core-test-map-intervals-buffer-multiple-runs ()
  "Multiple runs in a buffer are visited in order, gaps included."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'face 'bold)
    (put-text-property 7 12 'face 'italic)
    (should (equal (tp--map-intervals nil nil nil #'list 'face)
                   '((1 6 bold) (6 7 nil) (7 12 italic))))))

(ert-deftest tp-core-test-map-intervals-out-of-range-normalized ()
  "Out-of-bounds START/END are clamped, not signaled."
  (let ((str (copy-sequence "abc")))
    (put-text-property 0 3 'p 1 str)
    (should (equal (tp--map-intervals str -5 99 #'list 'p)
                   '((0 3 1))))))

(ert-deftest tp-core-test-map-intervals-empty-range ()
  "An empty range visits nothing."
  (let ((str (copy-sequence "abc")))
    (should (equal (tp--map-intervals str 1 1 #'list) nil))))

;;; tp-face-properties

(ert-deftest tp-core-test-face-properties ()
  "The face-family property list contains the three face properties."
  (should (equal tp-face-properties '(face font-lock-face mouse-face))))

(provide 'tp-core-tests)
;;; tp-core-tests.el ends here
