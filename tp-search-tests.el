;;; tp-search-tests.el --- ERT regression tests for tp-search.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for confirmed bugs fixed in the search module
;; (tp-search.el).  Each section is tagged with the canonical bug id
;; it guards against.

;;; Code:

(require 'ert)
(require 'tp)

;;; B37: backward searches must use `equal' matching like tp-forward

(ert-deftest tp-search-test-backward-value-matches-forward ()
  "tp-backward with a non-nil VALUE finds the same region tp-forward finds.
The old code passed no predicate to `text-property-search-backward',
whose default matches values NOT `equal' to VALUE (inverted)."
  (with-temp-buffer
    (insert "aaa bbb aaa ")
    (put-text-property 1 4 'k 'x)
    (put-text-property 5 8 'k 'y)
    (put-text-property 9 12 'k 'x)
    (goto-char (point-min))
    (let ((fwd (tp-forward 'k 'y)))
      (should fwd)
      (should (equal (list (prop-match-beginning fwd)
                           (prop-match-end fwd)
                           (prop-match-value fwd))
                     '(5 8 y))))
    (goto-char (point-max))
    (let ((bwd (tp-backward 'k 'y)))
      (should bwd)
      (should (equal (list (prop-match-beginning bwd)
                           (prop-match-end bwd)
                           (prop-match-value bwd))
                     '(5 8 y))))))

(ert-deftest tp-search-test-backward-value-adjacent-regions ()
  "tp-backward finds a matching region among adjacent (gap-free) runs."
  (with-temp-buffer
    (insert "aaabbbccc")
    (put-text-property 1 4 'k 'x)
    (put-text-property 4 7 'k 'y)
    (put-text-property 7 10 'k 'x)
    (goto-char (point-max))
    (let ((m (tp-backward 'k 'y)))
      (should m)
      (should (= (prop-match-beginning m) 4))
      (should (= (prop-match-end m) 7))
      (should (eq (prop-match-value m) 'y)))))

(ert-deftest tp-search-test-backward-value-n-walks-regions ()
  "tp-backward with N=2 walks two matching regions backward."
  (with-temp-buffer
    (insert "aaa bbb aaa ")
    (put-text-property 1 4 'k 'x)
    (put-text-property 5 8 'k 'x)
    (put-text-property 9 12 'k 'x)
    (goto-char (point-max))
    (let ((m (tp-backward 'k 'x nil 2)))
      (should m)
      (should (= (prop-match-beginning m) 5)))))

(ert-deftest tp-search-test-backward-value-no-match-returns-nil ()
  "tp-backward returns nil when no region has an `equal' value."
  (with-temp-buffer
    (insert "aaa bbb")
    (put-text-property 1 4 'k 'x)
    (goto-char (point-max))
    (should (null (tp-backward 'k 'missing)))))

(ert-deftest tp-search-test-backward-do-value-buffer ()
  "tp-backward-do with a non-nil VALUE rewrites the matching region."
  (with-temp-buffer
    (insert "aaa bbb aaa ")
    (put-text-property 1 4 'k 'x)
    (put-text-property 5 8 'k 'y)
    (put-text-property 9 12 'k 'x)
    (let ((count (tp-backward-do #'upcase 'k 'y)))
      (should (= count 1))
      (should (equal (buffer-substring-no-properties 1 13)
                     "aaa BBB aaa ")))))

;;; B38: zero-width patterns must not loop forever in buffer branches

(ert-deftest tp-search-test-match-empty-pattern-buffer-terminates ()
  "tp-match-set with an empty literal pattern terminates on buffers."
  (with-temp-buffer
    (insert "abc")
    (let ((regions (tp-match-set "" '(face bold))))
      ;; Zero-width matches are recorded at each position, like the
      ;; string branch records them.
      (should (equal regions '((1 . 1) (2 . 2) (3 . 3) (4 . 4)))))))

(ert-deftest tp-search-test-regexp-zero-width-buffer-terminates ()
  "tp-regexp-set with a regexp matching empty terminates on buffers."
  (with-temp-buffer
    (insert "axbxc")
    (let ((regions (tp-regexp-set "x*" '(face bold))))
      (should regions)
      ;; The actual x's still got their property.
      (should (eq (get-text-property 2 'face) 'bold))
      (should (eq (get-text-property 4 'face) 'bold)))))

(ert-deftest tp-search-test-match-empty-pattern-string-clean ()
  "tp-match-set with an empty pattern on a string no-ops cleanly.
The old string branch signaled args-out-of-range after scanning past
the end of the string."
  (let ((result (tp-match-set "" '(face bold) "abc")))
    (should (equal result "abc"))))

(ert-deftest tp-search-test-regexp-zero-width-string-clean ()
  "tp-regexp-set with a zero-width-capable regexp works on strings."
  (let ((result (tp-regexp-set "x*" '(face bold) "axb")))
    (should (equal (substring-no-properties result) "axb"))
    (should (eq (get-text-property 1 'face result) 'bold))))

;;; B39: length-changing replacements error on strings, work in buffers

(ert-deftest tp-search-test-forward-do-longer-replacement-errors ()
  "A replacement longer than the match signals a clear error on strings.
Strings cannot change length in place; the old code silently truncated
(or signaled args-out-of-range past the string end).  The string is
left unchanged."
  (let ((str (copy-sequence "hello world")))
    (tp-set 6 11 '(marker t) str)
    (should-error (tp-forward-do (lambda (txt) (concat (upcase txt) "XYZ"))
                                 'marker nil str))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-forward-do-longer-in-bounds-errors ()
  "A longer in-bounds replacement errors instead of clobbering.
Old code silently wrote 10 chars, yielding \"hellohellod\"."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t) str)
    (should-error (tp-forward-do (lambda (txt) (concat txt txt))
                                 'marker nil str))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-backward-do-longer-replacement-errors ()
  "tp-backward-do rejects length-changing replacements on strings."
  (let ((str (copy-sequence "hello world")))
    (tp-set 6 11 '(marker t) str)
    (should-error (tp-backward-do (lambda (txt) (concat (upcase txt) "12345"))
                                  'marker nil str))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-search-map-longer-replacement-errors ()
  "tp-search-map rejects length-changing replacements on strings."
  (let ((str (copy-sequence "hello world")))
    (tp-set 6 11 '(marker t) str)
    (should-error (tp-search-map (lambda (txt) (concat (upcase txt) "!!!"))
                                 'marker nil str))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-forward-do-shorter-replacement-errors ()
  "A shorter replacement errors instead of leaving residue (\"ABllo\")."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t) str)
    (should-error (tp-forward-do (lambda (_txt) "AB") 'marker nil str))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-search-map-same-length-string-ok ()
  "Same-length replacements still mutate the string in place."
  (let ((str (copy-sequence "hello world hello")))
    (tp-set 0 5 '(marker t) str)
    (tp-set 12 17 '(marker t) str)
    (should (= (tp-search-map #'upcase 'marker nil str) 2))
    (should (equal (substring-no-properties str) "HELLO world HELLO"))))

;;; B43: -do shortfall is all-or-nothing on strings and buffers alike

(ert-deftest tp-search-test-forward-do-shortfall-string ()
  "Requesting the Nth match when fewer exist applies nothing (string).
The count of available matches is still returned."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t) str)
    (should (= (tp-forward-do #'upcase 'marker nil str 3) 1))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-forward-do-shortfall-buffer ()
  "Requesting the Nth match when fewer exist applies nothing (buffer)."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'marker t)
    (should (= (tp-forward-do #'upcase 'marker t nil 3) 1))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "hello world"))))

(ert-deftest tp-search-test-backward-do-shortfall-string ()
  "tp-backward-do shortfall applies nothing on strings."
  (let ((str (copy-sequence "hello world")))
    (tp-set 6 11 '(marker t) str)
    (should (= (tp-backward-do #'upcase 'marker nil str 2) 1))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-forward-do-exact-count-applies ()
  "With exactly TIMES matches, FUNCTION is applied to the TIMES-th."
  (let ((str (copy-sequence "aaa bbb aaa")))
    (tp-set 0 3 '(marker t) str)
    (tp-set 8 11 '(marker t) str)
    (should (= (tp-forward-do #'upcase 'marker nil str 2) 2))
    (should (equal (substring-no-properties str) "aaa bbb AAA"))))

(ert-deftest tp-search-test-forward-do-buffer-longer-replacement-grows ()
  "Buffers may grow on longer replacements (delete-region + insert).
Uses an explicit VALUE: the buffer paths of the -do functions match
with predicate t, where VALUE nil matches property-absent runs."
  (with-temp-buffer
    (insert "hello world")
    (put-text-property 1 6 'marker t)
    (tp-forward-do (lambda (txt) (concat txt txt)) 'marker t)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "hellohello world"))))

;;; B40: tp-search-map must operate on OBJECT, not the current buffer

(ert-deftest tp-search-test-search-map-non-current-buffer ()
  "tp-search-map with a buffer OBJECT mutates that buffer only."
  (let ((target (generate-new-buffer " tp-search-test-target")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "aaa bbb")
            (put-text-property 1 4 'marker t))
          (with-temp-buffer
            (insert "current buffer text")
            (let ((count (tp-search-map #'upcase 'marker nil target)))
              (should (= count 1)))
            ;; Current buffer untouched.
            (should (equal (buffer-string) "current buffer text")))
          ;; Target buffer modified.
          (should (equal (with-current-buffer target
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))
                         "AAA bbb")))
      (kill-buffer target))))

(ert-deftest tp-search-test-search-do-non-current-buffer-bounds ()
  "tp--search-do computes default bounds in OBJECT, not the current buffer."
  (let ((target (generate-new-buffer " tp-search-test-target2")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (insert "aaa bbb ccc")
            (put-text-property 9 12 'marker t))
          (with-temp-buffer
            ;; Current buffer is much shorter than the target.
            (insert "x")
            (let ((seen nil))
              (tp--search-do (lambda (match _obj) (push match seen))
                             'marker nil target)
              (should (equal seen '((9 12 t)))))))
      (kill-buffer target))))

;;; B41: length-changing replacements over multiple matches

(ert-deftest tp-search-test-search-map-growing-replacements ()
  "Growing replacements do not corrupt later match positions."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (put-text-property 1 4 'marker t)
    (put-text-property 5 8 'marker t)
    (put-text-property 9 12 'marker t)
    (let ((count (tp-search-map (lambda (_txt) "XXXXXX") 'marker nil nil)))
      (should (= count 3))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "XXXXXX XXXXXX XXXXXX")))))

(ert-deftest tp-search-test-search-map-shrinking-replacements ()
  "Shrinking replacements do not corrupt later match positions."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (put-text-property 1 4 'marker t)
    (put-text-property 5 8 'marker t)
    (put-text-property 9 12 'marker t)
    (tp-search-map (lambda (_txt) "-") 'marker nil nil)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "- - -"))))

(ert-deftest tp-search-test-search-map-mixed-length-positions ()
  "Callbacks see up-to-date positions after earlier length changes."
  (with-temp-buffer
    (insert "aaa bbb ccc")
    (put-text-property 1 4 'marker t)
    (put-text-property 5 8 'marker t)
    (put-text-property 9 12 'marker t)
    (let ((texts nil))
      (tp-search-map (lambda (txt _start _end idx)
                       (push txt texts)
                       (format "<%d>%s" idx txt))
                     'marker nil nil)
      ;; Each callback received the intact matched text, not garbage
      ;; from stale positions.
      (should (equal (nreverse texts) '("aaa" "bbb" "ccc")))
      (should (equal (buffer-substring-no-properties (point-min) (point-max))
                     "<0>aaa <1>bbb <2>ccc")))))

;;; B42: tp-match-add / tp-regexp-add preserve existing faces in buffers

(ert-deftest tp-search-test-match-add-buffer-preserves-face ()
  "tp-match-add on a buffer merges faces instead of replacing them."
  (with-temp-buffer
    (insert "hello")
    (put-text-property 1 6 'face 'italic)
    (tp-match-add "hello" '(face bold))
    (should (equal (get-text-property 1 'face) '(bold italic)))))

(ert-deftest tp-search-test-match-add-face-string-buffer-parity ()
  "tp-match-add merges faces identically for strings and buffers."
  (let* ((str (propertize "hello" 'face 'italic))
         (str-face (get-text-property 0 'face
                                      (tp-match-add "hello" '(face bold) str)))
         (buf-face (with-temp-buffer
                     (insert "hello")
                     (put-text-property 1 6 'face 'italic)
                     (tp-match-add "hello" '(face bold))
                     (get-text-property 1 'face))))
    (should (equal str-face buf-face))
    (should (equal buf-face '(bold italic)))))

(ert-deftest tp-search-test-regexp-add-buffer-preserves-face ()
  "tp-regexp-add on a buffer merges faces instead of replacing them."
  (with-temp-buffer
    (insert "abc 123")
    (put-text-property 5 8 'face 'underline)
    (tp-regexp-add "[0-9]+" '(face bold))
    (should (equal (get-text-property 5 'face) '(bold underline)))))

(ert-deftest tp-search-test-match-add-buffer-non-face-deep-merge ()
  "tp-match-add still deep-merges non-face plist properties in buffers."
  (with-temp-buffer
    (insert "hello")
    (put-text-property 1 6 'data '(:a 1))
    (tp-match-add "hello" '(data (:b 2)))
    (let ((val (get-text-property 1 'data)))
      (should (equal (plist-get val :a) 1))
      (should (equal (plist-get val :b) 2)))))

;;; B44: property removal through tp-search-map on strings

(ert-deftest tp-search-test-search-map-removes-props-on-string ()
  "A callback returning a stripped string removes properties."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t face bold) str)
    (tp-search-map (lambda (txt) (substring-no-properties txt))
                   'marker nil str)
    (should (null (text-properties-at 0 str)))
    (should (equal (substring-no-properties str) "hello world"))))

(ert-deftest tp-search-test-search-map-removes-single-prop-on-string ()
  "A callback removing one property keeps the others."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t face bold) str)
    (tp-search-map (lambda (txt)
                     (remove-text-properties 0 (length txt) '(face nil) txt)
                     txt)
                   'marker nil str)
    (should (null (get-text-property 0 'face str)))
    (should (eq (get-text-property 0 'marker str) t))))

;;; Guard: nil return still means "no replacement" (used by tp-render)

(ert-deftest tp-search-test-search-map-nil-return-no-replacement ()
  "A callback returning nil leaves text and properties untouched."
  (let ((str (copy-sequence "hello world")))
    (tp-set 0 5 '(marker t face bold) str)
    (let ((count (tp-search-map (lambda (_txt) nil) 'marker nil str)))
      (should (= count 1))
      (should (equal (substring-no-properties str) "hello world"))
      (should (eq (get-text-property 0 'face str) 'bold)))))

;;; 0.3.0 A1: capture-group targeting via SUBEXP in tp-regexp-*

(ert-deftest tp-search-test-regexp-subexp-string ()
  "SUBEXP applies properties to the capture group only (string path)."
  (let ((s (tp-regexp-set "\\(foo\\)-bar" '(face bold)
                          "foo-bar foo-bar" nil nil 1)))
    (should (eq (get-text-property 0 'face s) 'bold))
    (should (eq (get-text-property 2 'face s) 'bold))
    (should-not (get-text-property 3 'face s))
    (should-not (get-text-property 6 'face s))
    (should (eq (get-text-property 8 'face s) 'bold))
    (should-not (get-text-property 11 'face s))))

(ert-deftest tp-search-test-regexp-subexp-buffer ()
  "SUBEXP applies properties and reports regions for the group (buffer path)."
  (with-temp-buffer
    (insert "foo-bar")
    (let ((regions (tp-regexp-set "\\(foo\\)-\\(bar\\)" '(face bold)
                                  (current-buffer) nil nil 2)))
      (should (equal regions '((5 . 8))))
      (should (eq (get-text-property 5 'face) 'bold))
      (should-not (get-text-property 1 'face))
      (should-not (get-text-property 4 'face)))))

(ert-deftest tp-search-test-regexp-subexp-group-not-participating ()
  "A match where the SUBEXP group does not participate contributes nothing."
  (with-temp-buffer
    (insert "b a b")
    (let ((regions (tp-regexp-set "\\(a\\)\\|b" '(face bold)
                                  (current-buffer) nil nil 1)))
      ;; Only the "a" match has group 1; the "b" matches contribute
      ;; neither properties nor regions.
      (should (equal regions '((3 . 4))))
      (should (eq (get-text-property 3 'face) 'bold))
      (should-not (get-text-property 1 'face))
      (should-not (get-text-property 5 'face))))
  ;; String path mirror.
  (let ((s (tp-regexp-set "\\(a\\)\\|b" '(face bold) "b a b" nil nil 1)))
    (should (eq (get-text-property 2 'face s) 'bold))
    (should-not (get-text-property 0 'face s))
    (should-not (get-text-property 4 'face s))))

(ert-deftest tp-search-test-regexp-subexp-zero-width-guard ()
  "The zero-width guard still terminates when SUBEXP is given."
  ;; "\\(x\\)*" matches the empty string everywhere in "ab" with
  ;; group 1 never participating; both paths must terminate cleanly.
  (let ((s (tp-regexp-set "\\(x\\)*" '(face bold) "ab" nil nil 1)))
    (should (equal (substring-no-properties s) "ab"))
    (should-not (text-properties-at 0 s))
    (should-not (text-properties-at 1 s)))
  (with-temp-buffer
    (insert "ab")
    (should-not (tp-regexp-set "\\(x\\)*" '(face bold)
                               (current-buffer) nil nil 1))
    (should-not (get-text-property 1 'face))))

;;; 0.3.0 A2: START/END bounds in tp-match-* / tp-regexp-*

(ert-deftest tp-search-test-match-bounds-string ()
  "START/END restrict tp-match-set to [START, END) in a string (0-based)."
  (let ((s (tp-match-set "foo" '(face bold) "foo foo foo" 4 11)))
    (should-not (get-text-property 0 'face s))
    (should (eq (get-text-property 4 'face s) 'bold))
    (should (eq (get-text-property 8 'face s) 'bold))))

(ert-deftest tp-search-test-match-bounds-buffer ()
  "START/END restrict tp-match-set to [START, END) in a buffer (1-based)."
  (with-temp-buffer
    (insert "foo foo foo")
    (let ((regions (tp-match-set "foo" '(face bold) (current-buffer) 5 12)))
      (should (equal regions '((5 . 8) (9 . 12))))
      (should-not (get-text-property 1 'face))
      (should (eq (get-text-property 5 'face) 'bold))
      (should (eq (get-text-property 9 'face) 'bold)))))

(ert-deftest tp-search-test-regexp-bounds-do-not-cross-boundary ()
  "Bounded regexp matching behaves as if only [START, END) existed."
  ;; A greedy "a+" would match the whole object; with bounds it must
  ;; match exactly the bounded portion instead of being discarded.
  (let ((s (tp-regexp-set "a+" '(face bold) "aaaa" 1 3)))
    (should-not (get-text-property 0 'face s))
    (should (eq (get-text-property 1 'face s) 'bold))
    (should (eq (get-text-property 2 'face s) 'bold))
    (should-not (get-text-property 3 'face s)))
  (with-temp-buffer
    (insert "aaaa")
    (should (equal (tp-regexp-set "a+" '(face bold) (current-buffer) 2 4)
                   '((2 . 4))))
    (should-not (get-text-property 1 'face))
    (should (eq (get-text-property 2 'face) 'bold))
    (should-not (get-text-property 4 'face))))

(ert-deftest tp-search-test-match-reset-and-add-accept-bounds ()
  "tp-match-reset/add accept the same START/END bounds."
  (let* ((base (tp-set "foo foo" 'face 'italic))
         (s (tp-match-reset "foo" '(face bold) base 4 7)))
    (should (eq (get-text-property 0 'face s) 'italic))
    (should (eq (get-text-property 4 'face s) 'bold)))
  (let ((s (tp-match-add "foo" '(face bold) "foo foo" 4 7)))
    (should-not (get-text-property 0 'face s))
    (should (eq (get-text-property 4 'face s) 'bold))))

;;; 0.3.0 A3: PREDICATE / NOT-CURRENT exposure in tp-forward/tp-backward

(defmacro tp-search-tests--with-lvl-buffer (&rest body)
  "Run BODY in a temp buffer with `lvl' runs 1/2/3 over \"aaabbbccc\"."
  (declare (indent 0))
  `(with-temp-buffer
     (insert "aaabbbccc")
     (put-text-property 1 4 'lvl 1)
     (put-text-property 4 7 'lvl 2)
     (put-text-property 7 10 'lvl 3)
     ,@body))

(ert-deftest tp-search-test-forward-predicate-buffer ()
  "A function PREDICATE selects buffer matches by property value."
  (tp-search-tests--with-lvl-buffer
    (goto-char (point-min))
    (let ((m (tp-forward 'lvl nil nil 1
                         (lambda (_ v) (and (numberp v) (> v 1))))))
      (should m)
      (should (equal (list (prop-match-beginning m)
                           (prop-match-end m)
                           (prop-match-value m))
                     '(4 7 2))))))

(ert-deftest tp-search-test-forward-predicate-string ()
  "A function PREDICATE selects string matches by property value."
  (let ((s (copy-sequence "aaabbbccc")))
    (tp-set 0 3 '(lvl 1) s)
    (tp-set 3 6 '(lvl 2) s)
    (tp-set 6 9 '(lvl 3) s)
    (should (equal (tp-forward 'lvl nil s 2
                               (lambda (_ v) (and (numberp v) (> v 1))))
                   '((3 6 2) (6 9 3))))))

(ert-deftest tp-search-test-backward-predicate-buffer ()
  "tp-backward accepts the same function PREDICATE as tp-forward."
  (tp-search-tests--with-lvl-buffer
    (goto-char (point-max))
    (let ((m (tp-backward 'lvl nil nil 1
                          (lambda (_ v) (and (numberp v) (< v 3))))))
      (should m)
      (should (equal (list (prop-match-beginning m)
                           (prop-match-end m)
                           (prop-match-value m))
                     '(4 7 2))))))

(ert-deftest tp-search-test-backward-predicate-string ()
  "tp-backward with a PREDICATE returns string matches innermost first."
  (let ((s (copy-sequence "aaabbbccc")))
    (tp-set 0 3 '(lvl 1) s)
    (tp-set 3 6 '(lvl 2) s)
    (tp-set 6 9 '(lvl 3) s)
    (should (equal (tp-backward 'lvl nil s 2
                                (lambda (_ v) (and (numberp v) (> v 1))))
                   '((6 9 3) (3 6 2))))))

(ert-deftest tp-search-test-forward-not-current-skips-point-region ()
  "NOT-CURRENT makes tp-forward skip the matching region around point."
  (with-temp-buffer
    (insert "aabbaa")
    (put-text-property 1 3 'k 'x)
    (put-text-property 3 5 'k 'y)
    (put-text-property 5 7 'k 'x)
    (goto-char (point-min))
    (let ((m (tp-forward 'k 'x)))
      (should (= (prop-match-beginning m) 1)))
    (goto-char (point-min))
    (let ((m (tp-forward 'k 'x nil 1 nil t)))
      (should (= (prop-match-beginning m) 5))
      (should (= (prop-match-end m) 7)))))

(ert-deftest tp-search-test-backward-not-current-skips-point-region ()
  "NOT-CURRENT makes tp-backward skip the matching region at point."
  (with-temp-buffer
    (insert "aa bb")
    (put-text-property 1 3 'k 'x)
    (put-text-property 4 6 'k 'x)
    (goto-char (point-max))
    ;; Default keeps the 0.2.0 behavior: the run ending at point wins.
    (let ((m (save-excursion (tp-backward 'k 'x))))
      (should (equal (list (prop-match-beginning m) (prop-match-end m))
                     '(4 6))))
    ;; NOT-CURRENT skips it and finds the previous matching run.
    (let ((m (save-excursion (tp-backward 'k 'x nil 1 nil t))))
      (should (equal (list (prop-match-beginning m) (prop-match-end m))
                     '(1 3))))))

(ert-deftest tp-search-test-predicate-t-equals-default ()
  "An explicit PREDICATE of t keeps the default `equal' matching."
  (tp-search-tests--with-lvl-buffer
    (goto-char (point-min))
    (let ((default-m (save-excursion (tp-forward 'lvl 2)))
          (t-m (save-excursion (tp-forward 'lvl 2 nil 1 t))))
      (should (= (prop-match-beginning default-m) (prop-match-beginning t-m)))
      (should (= (prop-match-end default-m) (prop-match-end t-m))))))

(ert-deftest tp-search-test-predicate-adjacent-runs-stay-separate ()
  "Adjacent matching runs with different values are separate matches.
Mirrors `text-property-search-forward', which ends a match where the
value changes when a non-nil predicate is given."
  (let ((s (copy-sequence "abcdef")))
    (tp-set 0 3 '(lvl 1) s)
    (tp-set 3 6 '(lvl 2) s)
    (should (equal (tp-forward 'lvl nil s 5 (lambda (_ v) (numberp v)))
                   '((0 3 1) (3 6 2))))))

(ert-deftest tp-search-test-forward-do-predicate ()
  "tp-forward-do passes PREDICATE through to select the target match."
  (let ((s (copy-sequence "abc def")))
    (tp-set 0 3 '(lvl 1) s)
    (tp-set 4 7 '(lvl 2) s)
    (should (= (tp-forward-do #'upcase 'lvl nil s 1 nil nil
                              (lambda (_ v) (eq v 2)))
               1))
    (should (equal (substring-no-properties s) "abc DEF"))))

(ert-deftest tp-search-test-backward-do-predicate ()
  "tp-backward-do passes PREDICATE through to select the target match."
  (with-temp-buffer
    (insert "abc def")
    (put-text-property 1 4 'lvl 1)
    (put-text-property 5 8 'lvl 2)
    (should (= (tp-backward-do #'upcase 'lvl nil (current-buffer) 1 nil nil
                               (lambda (_ v) (eq v 1)))
               1))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "ABC def"))))

(ert-deftest tp-search-test-forward-do-defaults-unchanged ()
  "tp-forward-do without PREDICATE keeps the 0.2.0 `equal' matching."
  (let ((s (copy-sequence "abc def")))
    (tp-set 0 3 '(lvl 1) s)
    (tp-set 4 7 '(lvl 2) s)
    (should (= (tp-forward-do #'upcase 'lvl 2 s) 1))
    (should (equal (substring-no-properties s) "abc DEF"))))

(provide 'tp-search-tests)
;;; tp-search-tests.el ends here
