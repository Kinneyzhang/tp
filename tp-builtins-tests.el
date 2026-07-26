;;; tp-builtins-tests.el --- ERT tests for tp-builtins.el and tp-palette.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for the built-in layers, the display buffer
;; macros, and the palette module (tp-builtins.el / tp-palette.el).
;;
;; Run with:
;;   emacs --batch -L . -l tp.el -l tp-builtins-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'tp)

;;; Test helpers

(defmacro tp-builtins-test--with-builtins (&rest body)
  "Run BODY ensuring the shipped built-in layers are registered.
Other test files reset `tp-layer-alist' globally; reloading
tp-builtins restores the shipped layer definitions."
  (declare (indent defun))
  `(progn
     (unless (assoc 'tp-link tp-layer-alist)
       (load "tp-builtins" nil t))
     ,@body))

(defmacro tp-builtins-test--with-background-mode (mode &rest body)
  "Run BODY with the frame background-mode set to MODE, restoring it after."
  (declare (indent 1))
  (let ((old (gensym "old-mode-")))
    `(let ((,old (frame-parameter nil 'background-mode)))
       (unwind-protect
           (progn (set-frame-parameter nil 'background-mode ,mode)
                  ,@body)
         (set-frame-parameter nil 'background-mode ,old)))))

;;; B45: plistp / subr-x compatibility with Emacs 28.1

(ert-deftest tp-builtins-test-plistp-compat ()
  "The `tp-palette--plistp' compat helper mirrors `plistp' semantics."
  (should (tp-palette--plistp nil))
  (should (tp-palette--plistp '(:a 1)))
  (should (tp-palette--plistp '(:a 1 :b 2)))
  (should-not (tp-palette--plistp '(:a 1 :b)))
  (should-not (tp-palette--plistp "string"))
  (should-not (tp-palette--plistp '(:a . 1))))

(ert-deftest tp-builtins-test-palette-pure-suffix-stripping ()
  "`tp-palette-pure' strips variant suffixes (needs subr-x loaded)."
  (should (eq (tp-palette-pure 'info) 'info))
  (should (eq (tp-palette-pure 'info-fg) 'info))
  (should (eq (tp-palette-pure 'info-bg) 'info))
  (should (eq (tp-palette-pure 'info-fbg) 'info))
  (should (eq (tp-palette-pure 'heading-1-border) 'heading-1))
  (should-error (tp-palette-pure 'no-such-palette)))

;;; B46: display buffer macros must not mutate shared keymaps

(ert-deftest tp-builtins-test-display-buffer-no-shared-keymap-pollution ()
  "Using the display macros must leave the major-mode keymap untouched."
  (unwind-protect
      (progn
        (tp-switch-to-buffer "*tp-builtins-test-display*"
          (text-mode)
          (insert "hello"))
        ;; The shared text-mode keymap must NOT have gained a q binding.
        (should-not (lookup-key text-mode-map "q"))
        (with-current-buffer "*tp-builtins-test-display*"
          ;; q still quits, via the buffer-local minor mode.
          (should (eq (key-binding "q") #'quit-window))
          (should tp-display-buffer-mode)
          (should buffer-read-only)
          (should (equal (buffer-string) "hello"))))
    (when (get-buffer "*tp-builtins-test-display*")
      (kill-buffer "*tp-builtins-test-display*"))))

(ert-deftest tp-builtins-test-display-buffer-hygienic-binding ()
  "BODY must see the user's own `buffer' variable, not a macro capture."
  (unwind-protect
      (let ((buffer "user-value"))
        (tp-switch-to-buffer "*tp-builtins-test-hygiene*"
          (insert buffer))
        (with-current-buffer "*tp-builtins-test-hygiene*"
          (should (equal (buffer-string) "user-value"))))
    (when (get-buffer "*tp-builtins-test-hygiene*")
      (kill-buffer "*tp-builtins-test-hygiene*"))))

(ert-deftest tp-builtins-test-pop-to-buffer-expansion-hygiene ()
  "`tp-pop-to-buffer' expands to a gensym binding, never literal `buffer'."
  (let* ((expansion (macroexpand-1 '(tp-pop-to-buffer "b" (ignore))))
         (binding-var (caar (nth 1 expansion))))
    (should (eq (car expansion) 'let))
    (should (symbolp binding-var))
    (should-not (eq binding-var 'buffer))))

(ert-deftest tp-builtins-test-display-buffer-reusable ()
  "A second invocation erases and refills the (read-only) buffer."
  (unwind-protect
      (progn
        (tp-switch-to-buffer "*tp-builtins-test-reuse*"
          (insert "first"))
        (tp-switch-to-buffer "*tp-builtins-test-reuse*"
          (insert "second"))
        (with-current-buffer "*tp-builtins-test-reuse*"
          (should (equal (buffer-string) "second"))
          (should buffer-read-only)))
    (when (get-buffer "*tp-builtins-test-reuse*")
      (kill-buffer "*tp-builtins-test-reuse*"))))

(ert-deftest tp-builtins-test-palette-show-smoke ()
  "`tp-palette-show' renders the gallery without error."
  (tp-builtins-test--with-builtins
    (unwind-protect
        (progn
          (tp-palette-show)
          (with-current-buffer "*tp-palette-gallery*"
            (should (> (buffer-size) 0))
            (should buffer-read-only)))
      (when (get-buffer "*tp-palette-gallery*")
        (kill-buffer "*tp-palette-gallery*")))))

;;; B47: tp-link resolves its palette color lazily

(ert-deftest tp-builtins-test-link-no-frozen-color ()
  "The registered tp-link layer must not contain a baked-in hex color."
  (tp-builtins-test--with-builtins
    (let ((entry (assoc 'tp-link tp-layer-alist)))
      (should entry)
      (should-not (string-match-p "#[0-9a-fA-F]" (format "%S" entry))))))

(ert-deftest tp-builtins-test-link-lazy-theme-resolution ()
  "tp-link resolves the info color at application time per theme."
  (tp-builtins-test--with-builtins
    (tp-builtins-test--with-background-mode 'light
      (let ((face (get-text-property 0 'face (tp-set "x" 'tp-link t))))
        (should (equal (plist-get face :foreground) "#0969da"))
        (should (plist-get face :underline))))
    (tp-builtins-test--with-background-mode 'dark
      (let* ((s (tp-set "x" 'tp-link t))
             (face (get-text-property 0 'face s)))
        (should (equal (plist-get face :foreground) "#58a6ff"))
        (should (plist-get face :underline))
        (should (eq (get-text-property 0 'mouse-face s) 'highlight))
        (should (eq (get-text-property 0 'pointer s) 'hand))))))

;;; B48: palette redefinition must not go stale

(ert-deftest tp-builtins-test-palette-redefinition-updates-colors ()
  "Redefining a palette updates what the color lookups return."
  (unwind-protect
      (tp-builtins-test--with-background-mode 'light
        (define-tp-palette tp-builtins-test-pal
          :fg ("#111111" . "#aaaaaa") :bg ("#222222" . "#bbbbbb"))
        (should (tp-palette-p 'tp-builtins-test-pal))
        (should (equal (tp-palette-fg-color 'tp-builtins-test-pal) "#111111"))
        (define-tp-palette tp-builtins-test-pal
          :fg ("#333333" . "#cccccc") :bg ("#444444" . "#dddddd"))
        (should (equal (tp-palette-fg-color 'tp-builtins-test-pal) "#333333"))
        (should (equal (tp-palette-bg-color 'tp-builtins-test-pal) "#444444")))
    (setq tp-palette-alist
          (assq-delete-all 'tp-builtins-test-pal tp-palette-alist))))

;;; B49: tp-headline accepts integer heights, never emits :height nil

(ert-deftest tp-builtins-test-headline-integer-height ()
  "An integer height (absolute, 1/10 pt units) produces a valid face."
  (tp-builtins-test--with-builtins
    (let ((face (get-text-property 0 'face (tp-set "h" 'tp-headline 120))))
      (should (equal (plist-get face :height) 120))
      (should (eq (plist-get face :weight) 'bold)))))

(ert-deftest tp-builtins-test-headline-float-height ()
  "A float height (scaling factor) keeps its documented behavior."
  (tp-builtins-test--with-builtins
    (let ((face (get-text-property 0 'face (tp-set "h" 'tp-headline 1.5))))
      (should (equal (plist-get face :height) 1.5))
      (should (eq (plist-get face :weight) 'bold)))))

(ert-deftest tp-builtins-test-headline-plist-height ()
  "A (:height H :bold B) plist is honored."
  (tp-builtins-test--with-builtins
    (let ((face (get-text-property 0 'face
                                   (tp-set "h" 'tp-headline
                                           '(:height 1.2 :bold nil)))))
      (should (equal (plist-get face :height) 1.2))
      (should-not (plist-get face :weight)))))

(ert-deftest tp-builtins-test-headline-never-emits-nil-height ()
  "A plist without :height must not produce (:height nil)."
  (tp-builtins-test--with-builtins
    (let ((face (get-text-property 0 'face
                                   (tp-set "h" 'tp-headline '(:bold t)))))
      (should-not (plist-member face :height))
      (should (eq (plist-get face :weight) 'bold)))))

(ert-deftest tp-builtins-test-headline-invalid-spec-errors ()
  "Unsupported tp-headline specs signal an error instead of a no-op."
  (tp-builtins-test--with-builtins
    (should-error (tp-set "h" 'tp-headline "big"))))

;;; B50: tp-space uses the documented pixel spec

(ert-deftest tp-builtins-test-space-pixel-spec ()
  "The shipped tp-space emits (space :width (PIXEL)) as documented."
  (tp-builtins-test--with-builtins
    ;; Other test files redefine tp-space; make sure we exercise the
    ;; shipped definition.
    (load "tp-builtins" nil t)
    (should (equal (get-text-property 0 'display (tp-set "emacs" 'tp-space 2))
                   '(space :width (2))))))

;;; B51: tp-parse-color accepts one-sided cons colors

(ert-deftest tp-builtins-test-parse-color-one-sided-cons ()
  "A cons with a nil side means no color for that mode."
  (tp-builtins-test--with-background-mode 'light
    (should (equal (tp-parse-color '("red" . nil)) "red"))
    (should-not (tp-parse-color '(nil . "green"))))
  (tp-builtins-test--with-background-mode 'dark
    (should-not (tp-parse-color '("red" . nil)))
    (should (equal (tp-parse-color '(nil . "green")) "green"))))

(ert-deftest tp-builtins-test-parse-color-existing-forms ()
  "Strings, two-sided conses and plists keep their behavior."
  (tp-builtins-test--with-background-mode 'light
    (should (equal (tp-parse-color "red") "red"))
    (should (equal (tp-parse-color '("red" . "green")) "red"))
    (should (equal (tp-parse-color '(:light "red" :dark "green")) "red")))
  (tp-builtins-test--with-background-mode 'dark
    (should (equal (tp-parse-color '("red" . "green")) "green"))
    (should (equal (tp-parse-color '(:light "red" :dark "green")) "green")))
  (should-not (tp-parse-color nil))
  (should-error (tp-parse-color 42)))

;;; API-NAME-02: prefix-conforming tp-define-palette alias

(ert-deftest tp-builtins-test-define-palette-alias ()
  "tp-define-palette is a working macro alias of define-tp-palette."
  (unwind-protect
      (progn
        (tp-define-palette tp-test-alias-palette
          :fg ("#111111" . "#eeeeee"))
        (should (tp-palette-p 'tp-test-alias-palette))
        (tp-builtins-test--with-background-mode 'light
          (should (equal (tp-palette-fg-color 'tp-test-alias-palette)
                         "#111111")))
        (tp-builtins-test--with-background-mode 'dark
          (should (equal (tp-palette-fg-color 'tp-test-alias-palette)
                         "#eeeeee"))))
    (setq tp-palette-alist
          (assq-delete-all 'tp-test-alias-palette tp-palette-alist))))

(provide 'tp-builtins-tests)
;;; tp-builtins-tests.el ends here
