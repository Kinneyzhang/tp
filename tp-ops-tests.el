;;; tp-ops-tests.el --- ERT regression tests for tp-ops.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for confirmed bugs fixed in the ops-core module
;; (tp-ops.el, with supporting fixes in tp-core.el).  Each section is
;; tagged with the canonical bug id it guards against.

;;; Code:

(require 'ert)
(require 'tp)

;;; B1: tp-remove string form must not drop the 3rd+ properties

(ert-deftest tp-ops-test-remove-string-three-props ()
  "Removing three properties from a string removes all three."
  (let* ((str (propertize "hi" 'face 'bold 'help-echo "x" 'mouse-face 'highlight))
         (result (tp-remove str 'face 'help-echo 'mouse-face)))
    (should (null (get-text-property 0 'face result)))
    (should (null (get-text-property 0 'help-echo result)))
    (should (null (get-text-property 0 'mouse-face result)))))

(ert-deftest tp-ops-test-remove-string-four-props ()
  "Removing four properties removes all four; nothing rides along."
  (let* ((str (propertize "hi" 'face 'bold 'help-echo "x"
                          'mouse-face 'highlight 'keymap 'km))
         (result (tp-remove str 'face 'help-echo 'mouse-face 'keymap)))
    (should (null (text-properties-at 0 result)))))

(ert-deftest tp-ops-test-remove-string-third-prop-kept-elsewhere ()
  "Properties not listed stay when 3+ properties are removed."
  (let* ((str (propertize "hi" 'face 'bold 'help-echo "x"
                          'mouse-face 'highlight 'keymap 'km))
         (result (tp-remove str 'face 'help-echo 'mouse-face)))
    (should (eq (get-text-property 0 'keymap result) 'km))))

;;; B2: string-form removal operates per interval

(ert-deftest tp-ops-test-remove-string-prop-preserves-other-intervals ()
  "Removing a property keeps each interval's own remaining props."
  (let* ((s (concat (propertize "ab" 'face 'bold)
                    (propertize "cd" 'face 'italic 'help-echo "x")))
         (result (tp-remove s 'help-echo)))
    (should (eq (get-text-property 0 'face result) 'bold))
    (should (eq (get-text-property 2 'face result) 'italic))
    (should (null (get-text-property 2 'help-echo result)))))

(ert-deftest tp-ops-test-remove-string-sub-key-per-interval ()
  "Sub-key removal does not smear one interval's face over another."
  (let* ((s (concat (propertize "ab" 'face '(:weight bold :underline t))
                    (propertize "cd" 'face 'italic)))
         (result (tp-remove s 'face :underline)))
    (let ((face0 (get-text-property 0 'face result)))
      (should (eq (plist-get face0 :weight) 'bold))
      (should (null (plist-get face0 :underline))))
    (should (eq (get-text-property 2 'face result) 'italic))))

(ert-deftest tp-ops-test-remove-string-nested-sub-key-per-interval ()
  "Nested sub-key removal keeps other intervals' face values intact."
  (let* ((s (concat (propertize "ab" 'face '(:underline (:style wave :position t)))
                    (propertize "cd" 'face 'italic)))
         (result (tp-remove s 'face :underline '(:style))))
    (let* ((face0 (get-text-property 0 'face result))
           (underline (plist-get face0 :underline)))
      (should (plist-get underline :position))
      (should (null (plist-get underline :style))))
    (should (eq (get-text-property 2 'face result) 'italic))))

(ert-deftest tp-ops-test-remove-string-prop-interval-boundaries-kept ()
  "Interval boundaries survive removal of an unrelated property."
  (let* ((s (concat (propertize "ab" 'face 'bold)
                    "cd"
                    (propertize "ef" 'face 'underline 'help-echo "z")))
         (result (tp-remove s 'help-echo)))
    (should (eq (get-text-property 0 'face result) 'bold))
    (should (null (get-text-property 2 'face result)))
    (should (eq (get-text-property 4 'face result) 'underline))
    (should (null (get-text-property 4 'help-echo result)))))

;;; B3: tp-clear defaults bounds from OBJECT

(ert-deftest tp-ops-test-clear-string-defaults ()
  "tp-clear with a string OBJECT clears the whole string by default."
  (with-temp-buffer                     ; empty buffer: old code no-oped
    (let ((s (propertize "hey" 'face 'bold 'help-echo "x")))
      (tp-clear nil nil s)
      (should (null (text-properties-at 0 s)))
      (should (tp-empty-p s)))))

(ert-deftest tp-ops-test-clear-string-defaults-in-longer-buffer ()
  "tp-clear on a short string works even when current buffer is longer."
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (let ((s (propertize "ab" 'face 'bold)))
      (tp-clear nil nil s)              ; old code: args-out-of-range
      (should (tp-empty-p s)))))

(ert-deftest tp-ops-test-clear-buffer-defaults-still-work ()
  "tp-clear with no args still clears the whole current buffer."
  (with-temp-buffer
    (insert "Hello")
    (put-text-property 1 6 'face 'bold)
    (tp-clear)
    (should (null (text-properties-at 1)))))

(ert-deftest tp-ops-test-clear-buffer-object-defaults ()
  "tp-clear defaults bounds from a buffer OBJECT, not the current buffer."
  (let ((buf (generate-new-buffer " tp-ops-test-clear")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert "Hello")
            (put-text-property 1 6 'face 'bold))
          (with-temp-buffer             ; empty current buffer
            (tp-clear nil nil buf))
          (with-current-buffer buf
            (should (null (text-properties-at 1)))))
      (kill-buffer buf))))

;;; B4: (tp-get STRING START END ...) range form

(ert-deftest tp-ops-test-get-string-numeric-range ()
  "String object with a numeric range returns the intervals in range."
  (let ((str (propertize "hey" 'face 'bold)))
    (should (equal (tp-get str 0 2) '((0 2 (face bold)))))))

(ert-deftest tp-ops-test-get-string-numeric-range-with-property ()
  "String range form accepts a property like the buffer region form."
  (let ((str (concat (propertize "ab" 'face 'bold)
                     (propertize "cd" 'face 'italic))))
    (should (equal (tp-get str 0 4 'face)
                   '((0 2 bold) (2 4 italic))))
    (should (equal (tp-get str 2 4 'face) '((2 4 italic))))))

(ert-deftest tp-ops-test-get-string-numeric-range-with-sub-path ()
  "String range form supports nested sub-paths."
  (let ((str (propertize "hey" 'face '(:foreground "red" :weight bold))))
    (should (equal (tp-get str 0 3 'face :foreground)
                   '((0 3 "red"))))))

(ert-deftest tp-ops-test-get-string-numeric-start-without-end-errors ()
  "A numeric START without a numeric END signals a clear error."
  (should-error (tp-get (propertize "hey" 'face 'bold) 0)))

;;; B5: tp--parse-face-list trailing bare keyword

(ert-deftest tp-ops-test-parse-face-list-trailing-keyword ()
  "A trailing bare keyword does not produce a bogus (KEY nil) pair."
  (should (equal (tp--parse-face-list '(bold :foreground))
                 '((bold)))))

(ert-deftest tp-ops-test-parse-face-list-inline-keyword-still-works ()
  "Inline keyword-value pairs are still consumed normally."
  (should (equal (tp--parse-face-list '(bold :foreground "green"))
                 '((bold) :foreground "green"))))

;;; B6: region form with flat prop/val signals immediately

(ert-deftest tp-ops-test-set-region-flat-args-error ()
  "Region form with flat PROP/VAL args signals an immediate error."
  (with-temp-buffer
    (insert "hello")
    (should-error (tp-set 1 4 'face 'bold))
    ;; Nothing was applied
    (should (null (get-text-property 1 'face)))))

(ert-deftest tp-ops-test-add-and-reset-region-flat-args-error ()
  "tp-add and tp-reset region forms reject flat PROP/VAL args too."
  (with-temp-buffer
    (insert "hello")
    (should-error (tp-add 1 4 'face 'bold))
    (should-error (tp-reset 1 4 'face 'bold))))

(ert-deftest tp-ops-test-set-region-with-object-still-works ()
  "Region form with a plist and trailing OBJECT is unaffected."
  (let ((s (copy-sequence "hello")))
    (tp-set 0 3 '(face bold) s)
    (should (eq (get-text-property 0 'face s) 'bold)))
  (with-temp-buffer
    (insert "hello")
    (tp-set 1 4 '(face bold))
    (should (eq (get-text-property 1 'face) 'bold))))

;;; B7: tp-add face-family prepend semantics for all tp-face-properties

(ert-deftest tp-ops-test-add-font-lock-face-prepends-string ()
  "tp-add prepends font-lock-face like face (string form)."
  (let* ((s (propertize "hey" 'font-lock-face 'bold))
         (result (tp-add s 'font-lock-face 'italic)))
    (should (equal (get-text-property 0 'font-lock-face result)
                   '(italic bold)))))

(ert-deftest tp-ops-test-add-mouse-face-prepends-string ()
  "tp-add prepends mouse-face like face (string form)."
  (let* ((s (propertize "hey" 'mouse-face 'highlight))
         (result (tp-add s 'mouse-face 'region)))
    (should (equal (get-text-property 0 'mouse-face result)
                   '(region highlight)))))

(ert-deftest tp-ops-test-add-font-lock-face-prepends-buffer ()
  "tp-add prepends font-lock-face like face (buffer region form)."
  (with-temp-buffer
    (insert "hey")
    (put-text-property 1 4 'font-lock-face 'bold)
    (tp-add 1 4 '(font-lock-face italic))
    (should (equal (get-text-property 1 'font-lock-face) '(italic bold)))))

(ert-deftest tp-ops-test-add-font-lock-face-prepends-string-region ()
  "tp-add prepends font-lock-face in the string region form."
  (let ((s (propertize "hey" 'font-lock-face 'bold)))
    (tp-add 0 3 '(font-lock-face italic) s)
    (should (equal (get-text-property 0 'font-lock-face s) '(italic bold)))))

(ert-deftest tp-ops-test-add-font-lock-face-plist-merge ()
  "tp-add deep-merges font-lock-face plists like face plists."
  (let* ((s (propertize "hey" 'font-lock-face '(:foreground "red")))
         (result (tp-add s 'font-lock-face '(:background "blue"))))
    (let ((flf (get-text-property 0 'font-lock-face result)))
      (should (equal (plist-get flf :foreground) "red"))
      (should (equal (plist-get flf :background) "blue")))))

(ert-deftest tp-ops-test-add-non-face-property-still-replaces ()
  "Non-face properties keep plain replacement semantics in tp-add."
  (let* ((s (propertize "hey" 'help-echo "old"))
         (result (tp-add s 'help-echo "new")))
    (should (equal (get-text-property 0 'help-echo result) "new"))))

;;; B8: tp-intervals clips to the requested range

(ert-deftest tp-ops-test-intervals-clipped-buffer ()
  "Buffer intervals are clipped: offsets stay within [0, END-START)."
  (with-temp-buffer
    (insert "abcdef")
    (put-text-property 1 5 'face 'bold)
    (let ((intervals (tp-intervals 3 6)))
      (dolist (iv intervals)
        (should (>= (nth 0 iv) 0))
        (should (<= (nth 1 iv) 3))
        (should (< (nth 0 iv) (nth 1 iv))))
      (should (equal intervals '((0 2 (face bold)) (2 3 nil)))))))

(ert-deftest tp-ops-test-intervals-clipped-string ()
  "String intervals are clipped to [START, END)."
  (let ((s (copy-sequence "abcdef")))
    (put-text-property 0 6 'face 'bold s)
    (let ((intervals (tp-intervals 2 4 s)))
      (dolist (iv intervals)
        (should (>= (nth 0 iv) 2))
        (should (<= (nth 1 iv) 4)))
      (should (equal intervals '((2 4 (face bold))))))))

(provide 'tp-ops-tests)
;;; tp-ops-tests.el ends here
