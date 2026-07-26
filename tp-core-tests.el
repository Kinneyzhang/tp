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

;;; API-COORD-01: ABSOLUTE coordinates in tp-intervals / tp-intervals-map

(ert-deftest tp-core-test-intervals-buffer-relative-default ()
  "Without ABSOLUTE, buffer intervals stay START-relative (legacy)."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 4 8 'face 'bold)
    (should (equal (tp-intervals 3 9)
                   '((0 1 nil) (1 5 (face bold)) (5 6 nil))))))

(ert-deftest tp-core-test-intervals-buffer-absolute ()
  "With ABSOLUTE, buffer intervals use native 1-based positions."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 4 8 'face 'bold)
    (should (equal (tp-intervals 3 9 nil t)
                   '((3 4 nil) (4 8 (face bold)) (8 9 nil))))
    ;; Clipping still applies in native coordinates.
    (should (equal (tp-intervals 5 7 nil t)
                   '((5 7 (face bold)))))))

(ert-deftest tp-core-test-intervals-string-ignores-absolute ()
  "String intervals are already absolute; ABSOLUTE changes nothing."
  (let ((s (copy-sequence "hello world")))
    (put-text-property 3 7 'face 'bold s)
    (should (equal (tp-intervals 2 9 s) (tp-intervals 2 9 s t)))
    (should (equal (tp-intervals 2 9 s t)
                   '((2 3 nil) (3 7 (face bold)) (7 9 nil))))))

(ert-deftest tp-core-test-intervals-map-absolute ()
  "tp-intervals-map passes ABSOLUTE through to native positions."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 4 8 'face 'bold)
    (should (equal (tp-intervals-map #'list 3 9)
                   '((0 1 nil nil) (1 5 (face bold) nil) (5 6 nil nil))))
    (should (equal (tp-intervals-map #'list 3 9 nil t)
                   '((3 4 nil nil) (4 8 (face bold) nil) (8 9 nil nil))))))

(ert-deftest tp-core-test-intervals-map-splits-layer-stack ()
  "tp-intervals-map hands the tp-layers stack to FUNCTION separately."
  (with-temp-buffer
    (insert "hello")
    (set-text-properties
     1 6 '(face bold tp-layers ((face italic tp-name below))))
    (let ((res (tp-intervals-map #'list 1 6 nil t)))
      (should (= (length res) 1))
      (pcase-let ((`(,beg ,end ,top ,below) (car res)))
        (should (= beg 1))
        (should (= end 6))
        (should (eq (plist-get top 'face) 'bold))
        (should-not (plist-member top 'tp-layers))
        (should (equal below '((face italic tp-name below))))))))

(ert-deftest tp-core-test-intervals-map-drops-nil-results ()
  "nil results from FUNCTION are removed from the returned list."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 4 8 'face 'bold)
    (should (equal (tp-intervals-map
                    (lambda (beg end top _below)
                      (when (plist-get top 'face) (cons beg end)))
                    1 12 nil t)
                   '((4 . 8))))))

(provide 'tp-core-tests)
;;; tp-core-tests.el ends here
