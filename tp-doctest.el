;;; tp-doctest.el --- executable README examples -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Executable documentation tests: each assertion reproduces an example
;; from README.md / README_CN.md (the code blocks are identical across
;; the two files) and compares the result against the exact output the
;; docs claim.  Run with `make doctest'; the batch process exits
;; non-zero if any assertion fails.  When changing a README example,
;; update the matching assertion here in the same commit.

;;; Code:

(require 'tp)
(tp-layer-reset)

(defvar tp-doctest--fails 0)
(defvar tp-doctest--total 0)
(defmacro chk (label expected &rest body)
  `(let* ((exp ,expected)
          (got (condition-case err (progn ,@body) (error (list :ERROR err)))))
     (setq tp-doctest--total (1+ tp-doctest--total))
     (if (equal got exp)
         (princ (format "PASS %s\n" ,label))
       (setq tp-doctest--fails (1+ tp-doctest--fails))
       (princ (format "FAIL %s\n  expected: %S\n  got:      %S\n"
                      ,label exp got)))))
(defmacro chk-str (label expected &rest body)
  "Compare prin1 form (covers propertized strings)."
  `(chk ,label ,expected (prin1-to-string (progn ,@body))))

;; ---- Quick Start ----
(chk-str "QS-set" "#(\"hello\" 0 5 (face bold))" (tp-set "hello" 'face 'bold))
(chk "QS-layer" 'spotlight
     (progn
       (define-tp spotlight () '(face (:background "yellow")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-push-layer 1 6 'spotlight)
         (tp-layer-top 1 6))))
(defvar accent-color "red")
(chk "QS-reactive" '(:foreground "blue")
     (progn
       (define-tp accent ()
         :props '(face (:foreground $accent-color)))
       (with-temp-buffer
         (insert "Hello")
         (tp-push-layer 1 6 'accent)
         (setq accent-color "blue")
         (tp-at 1 'face))))

;; ---- Features ----
(chk "F-getstyle" '((0 5 wave))
     (let ((str (copy-sequence "Hello World")))
       (tp-set 0 5 '(face (:underline (:color "green" :style wave))) str)
       (tp-get str 'face :underline :style)))
(chk "F-getmulti" '((0 5 (:color "green" :style wave)))
     (let ((str (copy-sequence "Hello World")))
       (tp-set 0 5 '(face (:underline (:color "green" :style wave))) str)
       (tp-get str 'face :underline '(:color :style))))
(chk "F-dupface" '((:foreground "red") (:background "green") bold)
     (tp-at 0 'face (tp-set "emacs"
                            'face 'bold
                            'face '(:background "green")
                            'face '(:foreground "red"))))
(chk "F-override" '(:foreground "yellow")
     (tp-at 0 'face (tp-set "emacs"
                            'face '(:foreground "red")
                            'face '(:foreground "yellow"))))
(chk "F-search" '((0 5 t) (12 17 t))
     (let ((my-string (copy-sequence "Hello World Hello")))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-search my-string 'marker)))
(chk "F-searchmap" "HELLO world HELLO"
     (let ((my-string (copy-sequence "hello world hello")))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-search-map #'upcase 'marker nil my-string)
       (substring-no-properties my-string)))
(chk "F-teaser-fullname" '(help-echo "John Doe" face (:foreground "purple") tp-name full-name-layer)
     (progn
       (define-tp full-name-layer ()
         :props '(help-echo $full-name face (:foreground $name-color))
         :data '((first-name . "John") (last-name . "Doe") (name-color . "purple"))
         :compute '((full-name (lambda () (concat first-name " " last-name))))
         :watch '((first-name (lambda (new old layer)
                                (message "Name changed from %s to %s" old new)))))
       (tp-layer-props 'full-name-layer)))

;; ---- tp-set my-style ----
(chk-str "S-mystyle" "#(\" \" 0 1 (face (:foreground \"blue\") tp-name my-style))"
         (progn
           (define-tp my-style ()
             :props '(face (:foreground $my-color))
             :data '((my-color . "blue")))
           (tp-set " " 'my-style)))

;; ---- tp-member ----
(chk "M-member-str" '((face nil) nil)
     (let ((str (copy-sequence "Hello")))
       (tp-set 0 5 '(face nil) str)
       (list (tp-member 0 'face str)
             (tp-member 0 'display str))))
(chk "M-member-buf" '(face bold)
     (with-temp-buffer
       (insert "Hello")
       (tp-set 1 6 '(face bold))
       (tp-member 1 'face)))

;; ---- tp-remove nested ----
(chk "R-remove-nested" '(:color "blue")
     (let ((original (propertize "Hello" 'face '(:underline (:style wave :color "blue")))))
       (let ((result (tp-remove original 'face :underline '(:style))))
         (tp-at 0 '(face :underline) result))))

;; ---- tp-forward / tp-backward ----
(chk "N-fwd-t" 7
     (with-temp-buffer
       (insert "Hello World Test")
       (tp-set 7 12 '(marker t))
       (goto-char 1)
       (let ((match (tp-forward 'marker t)))
         (when match (prop-match-beginning match)))))
(chk "N-fwd-nil" '(1 7)
     (with-temp-buffer
       (insert "Hello World Test")
       (tp-set 7 12 '(marker t))
       (goto-char 1)
       (let ((match (tp-forward 'marker)))
         (list (prop-match-beginning match) (prop-match-end match)))))
(chk "N-bwd-t" '(7 12)
     (with-temp-buffer
       (insert "Hello World Test")
       (tp-set 7 12 '(marker t))
       (goto-char (point-max))
       (let ((match (tp-backward 'marker t)))
         (list (prop-match-beginning match) (prop-match-end match)))))
(chk "N-fwd-heading" 'heading
     (with-temp-buffer
       (insert "Hello World")
       (tp-set 1 6 '(type heading))
       (goto-char 1)
       (let ((match (tp-forward 'type 'heading)))
         (when match (prop-match-value match)))))
(chk "N-fwd-string" '((0 5 t) (12 17 t))
     (let ((my-string (copy-sequence "Hello World Hello")))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-forward 'marker nil my-string 2)))

;; ---- tp-forward-do / tp-search-map examples ----
(chk "DO-fdo" "hello world HELLO"
     (let ((my-string (copy-sequence "hello world hello")))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-forward-do #'upcase 'marker nil my-string 2)
       (substring-no-properties my-string)))
(chk "DO-bdo" "HELLO world hello"
     (let ((my-string (copy-sequence "hello world hello")))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-backward-do #'upcase 'marker nil my-string 2)
       (substring-no-properties my-string)))
(chk "DO-fdo-pos" '("hello world HELLO" (12 17))
     (let ((my-string (copy-sequence "hello world hello"))
           (match-info nil))
       (tp-set 0 5 '(marker t) my-string)
       (tp-set 12 17 '(marker t) my-string)
       (tp-forward-do
        (lambda (text start end)
          (setq match-info (list start end))
          (upcase text))
        'marker nil my-string 2)
       (list (substring-no-properties my-string) match-info)))
(chk "SM-idx" '("AAA BBB CCC" ((0 0 3) (1 4 7) (2 8 11)))
     (let ((my-string (copy-sequence "aaa bbb ccc"))
           (positions nil))
       (tp-set 0 3 '(marker t) my-string)
       (tp-set 4 7 '(marker t) my-string)
       (tp-set 8 11 '(marker t) my-string)
       (tp-search-map
        (lambda (text start end idx)
          (push (list idx start end) positions)
          (upcase text))
        'marker nil my-string)
       (list (substring-no-properties my-string) (nreverse positions))))

;; ---- Layer definitions ----
(defvar my-color)
(chk "L-format3" '((:foreground "blue") "status: active")
     (progn
       (tp-layer-reset)
       (define-tp my-reactive-layer ()
         :props '(face (:foreground $my-color) help-echo $status-note)
         :data '((my-color . "red") (status . "active"))
         :compute '((status-note (lambda () (concat "status: " status))))
         :watch '((my-color (lambda (new old layer) (message "Color changed!"))))
         :transform (lambda (text) (upcase text)))
       (with-temp-buffer
         (insert "Hello World")
         (tp-push-layer 1 10 'my-reactive-layer)
         (setq my-color "blue")
         (list (tp-at 1 'face) (tp-at 1 'help-echo)))))
(chk "L-statuscolors" 3
     (progn
       (tp-layer-reset)
       (define-tp highlight ()
         '(face (:background "yellow" :foreground "black")))
       (define-tp error ()
         '(face (:background "red" :foreground "white")))
       (define-tp info ()
         '(face (:background "blue" :foreground "white")))
       (define-tps status-colors ()
         'highlight 'error 'info)
       (length (tp-group-props 'status-colors))))
(chk "L-moon" '(display "🌕")
     (progn
       (tp-layer-reset)
       (define-tps moon-phases ()
         '("new" . (display "🌑"))
         '("waxing-crescent" . (display "🌒"))
         '("first-quarter" . (display "🌓"))
         '("full" . (display "🌕")))
       (tp-layer-props 'moon-phases-full)))
(chk-str "L-paramgroup"
         "#(\"emacs\" 0 5 (face (:foreground \"orange\") tp-name tp-test-l1 tp-layers ((face (:foreground \"red\") tp-name tp-test-l2) (face (:background \"green\") tp-name tp-test-l3))))"
         (progn
           (tp-layer-reset)
           (define-tp tp-test-l1 (color)
             `(face (:foreground ,color)))
           (define-tp tp-test-l2 (color)
             `(face (:foreground ,color)))
           (define-tp tp-test-l3 ()
             '(face (:background "green")))
           (define-tps tp-test-group1 (color)
             `(tp-test-l1 ,color)
             '(tp-test-l2 "red")
             'tp-test-l3)
           (tp-set "emacs" 'tp-test-group1 "orange")))
(chk "L-props" '((face bold help-echo "tip")
                 (face bold help-echo "tip" tp-name my-layer))
     (progn
       (tp-layer-reset)
       (define-tp my-layer ()
         '(face bold help-echo "tip"))
       (list (tp-layer-props 'my-layer)
             (tp-layer-props 'my-layer t))))
(chk "L-groupprops" 2
     (progn
       (tp-layer-reset)
       (define-tp layer1 () '(face bold))
       (define-tp layer2 () '(face italic))
       (define-tps my-group ()
         'layer1 'layer2)
       (length (tp-group-props 'my-group))))
(chk "L-undeflayer" nil
     (progn
       (tp-layer-reset)
       (define-tp temp-layer () '(face bold))
       (tp-undefine-layer 'temp-layer)
       (tp-layer-props 'temp-layer)))
(chk "L-undefgroup" nil
     (progn
       (tp-layer-reset)
       (define-tp l1 () '(face bold))
       (define-tps my-group ()
         'l1)
       (tp-undefine-group 'my-group)
       (assoc 'my-group tp-layer-groups)))
(chk "L-reset" '(nil nil)
     (progn
       (define-tp test-layer () '(face bold))
       (tp-layer-reset)
       (list tp-layer-alist tp-layer-groups)))
(defvar my-reactive-color "red")
(chk "L-reactivereset" '(face (:foreground "red"))
     (progn
       (tp-layer-reset)
       (define-tp reactive-layer ()
         :props '(face (:foreground $my-reactive-color)))
       (tp-reactive-reset)
       (tp-layer-props 'reactive-layer)))

;; ---- tp-put-layer / tp-push-layer ----
(chk "P-base" 'base
     (progn
       (tp-layer-reset)
       (define-tp base () '(face default))
       (define-tp highlight () '(face (:background "yellow")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-put-layer 1 10 'base 0)
         (tp-at 1 'tp-name))))
(chk "P-idx1" 2
     (progn
       (tp-layer-reset)
       (define-tp base () '(face default))
       (define-tp highlight () '(face (:background "yellow")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-put-layer 1 10 'base 0)
         (tp-put-layer 1 10 'highlight 1)
         (tp-layer-count 1 10))))
(chk "P-bottom" 'base
     (progn
       (tp-layer-reset)
       (define-tp base () '(face default))
       (define-tp info () '(face (:foreground "blue")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-put-layer 1 10 'base 0)
         (tp-put-layer 1 10 'info -1)
         (tp-layer-top 1 10))))
(chk "P-inline" '(bold "tip")
     (with-temp-buffer
       (insert "Hello World")
       (tp-put-layer 1 10 '(face bold help-echo "tip") 0)
       (list (tp-at 1 'face) (tp-at 1 'help-echo))))
(chk "P-names" '(bold (layer-a layer-b))
     (progn
       (tp-layer-reset)
       (define-tp layer-a () '(face bold))
       (define-tp layer-b () '(face italic))
       (with-temp-buffer
         (insert "Hello World")
         (tp-put-layer 1 10 '(layer-a layer-b) 0)
         (list (tp-at 1 'face) (tp-layer-list 1 10)))))
(chk "P-param" '(:foreground "red")
     (progn
       (tp-layer-reset)
       (define-tp tp-color (color)
         `(face (:foreground ,color)))
       (with-temp-buffer
         (insert "Hello World")
         (tp-put-layer 1 10 '(tp-color "red") 0)
         (tp-at 1 'face))))
(chk "P-stack" '(:face (:background "yellow") :top highlight :layers (highlight base) :hidden 1)
     (progn
       (tp-layer-reset)
       (define-tp base () '(face default))
       (define-tp highlight () '(face (:background "yellow")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-push-layer 1 10 'base)
         (tp-push-layer 1 10 'highlight)
         (list :face (tp-at 1 'face)
               :top (tp-layer-top 1 10)
               :layers (tp-layer-list 1 10)
               :hidden (length (tp-at 1 'tp-layers))))))

;; ---- Utilities ----
(chk "U-intervals" '((0 5 (face bold)) (5 6 nil) (6 11 (face italic)))
     (with-temp-buffer
       (insert "Hello World")
       (tp-set 1 6 '(face bold))
       (tp-set 7 12 '(face italic))
       (tp-intervals 1 12)))
(chk "U-intervalsmap" '((0 5 bold) (5 6 nil) (6 11 italic))
     (with-temp-buffer
       (insert "Hello World")
       (tp-set 1 6 '(face bold))
       (tp-set 7 12 '(face italic))
       (tp-intervals-map
        (lambda (start end props belows)
          (ignore belows)
          (list start end (plist-get props 'face)))
        1 12)))
(chk "U-plist" '(help-echo "Tip" face italic)
     (with-temp-buffer
       (insert "Hello World")
       (tp-set 1 6 '(face bold help-echo "Tip"))
       (tp-set 7 12 '(face italic))
       (tp-plist 1 12)))
(chk "U-emptyp" '(t nil)
     (let* ((str "text")
            (new (tp-set str 'face 'bold)))
       (list (tp-empty-p str) (tp-empty-p new))))
(chk "U-emptyp2" t (tp-empty-p "plain text"))
(chk-str "U-popbuffer" "#(\"Important\" 0 9 (face (:foreground \"red\" :weight bold)))"
         (progn
           (tp-pop-to-buffer "*tp-demo*"
             (insert (tp-set "Important" 'face '(:foreground "red" :weight bold))
                     " message\n"))
           (with-current-buffer "*tp-demo*"
             (buffer-substring 1 10))))
(chk "U-parsecolor1" "red" (tp-parse-color "red"))
(chk "U-parsecolor2" t
     (and (member (tp-parse-color '("white" . "black")) '("white" "black")) t))

;; ---- Practical examples ----
(chk "X-taskstatus" 3
     (progn
       (tp-layer-reset)
       (define-tp status-todo () '(face (:foreground "gray")))
       (define-tp status-progress () '(face (:foreground "yellow")))
       (define-tp status-done () '(face (:foreground "green")))
       (define-tps task-status () 'status-todo 'status-progress 'status-done)
       (length (tp-group-props 'task-status))))
(chk "X-temphl" '(face (:background "yellow"))
     (progn
       (tp-layer-reset)
       (define-tp temp-highlight ()
         '(face (:background "yellow")))
       (tp-layer-props 'temp-highlight)))
(chk "X-synhl" 'code-error
     (progn
       (tp-layer-reset)
       (define-tp code-base ()
         '(face font-lock-keyword-face))
       (define-tp code-error ()
         '(face (:underline (:color "red" :style wave))
                help-echo "Syntax error"))
       (define-tp code-debug ()
         '(face (:background "dark blue")))
       (with-temp-buffer
         (insert (make-string 100 ?x))
         (tp-push-layer 1 100 'code-base)
         (tp-push-layer 50 60 'code-error)
         (tp-layer-top 50 60))))

;; ---- Reactive chapter ----
(defvar status-color nil)
(chk "RC-watch" '("Layer monitored-layer: color changed from nil to red"
                  "Layer monitored-layer: color changed from red to green")
     (let ((msgs nil))
       (tp-layer-reset)
       (cl-letf* (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) msgs))
                     nil)))
         (define-tp monitored-layer ()
           :props '(face (:foreground $status-color))
           :watch '((status-color
                     (lambda (new-val old-val layer-name)
                       (message "Layer %s: color changed from %s to %s"
                                layer-name old-val new-val)))))
         (setq status-color "red")
         (setq status-color "green"))
       (nreverse msgs)))
(chk "RC-groups" '(face (:foreground "green") tp-name status-indicators-success)
     (progn
       (tp-layer-reset)
       (define-tps status-indicators ()
         '("success" :props (face (:foreground $success-color))
           :data ((success-color . "green")))
         '("warning" :props (face (:foreground $warning-color))
           :data ((warning-color . "orange")))
         '("error"   :props (face (:foreground $error-color))
           :data ((error-color . "red"))))
       (tp-layer-props 'status-indicators-success)))
(defvar fg-color)
(defvar bg-color)
(chk "RC-batch" '(:foreground "red" :background "blue")
     (progn
       (tp-layer-reset)
       (define-tp themed-text ()
         :props '(face (:foreground $fg-color :background $bg-color))
         :data '((fg-color . "white") (bg-color . "black")))
       (with-temp-buffer
         (insert "Hello World")
         (tp-set 1 12 'themed-text)
         (setq fg-color "yellow")
         (setq bg-color "navy")
         (tp-with-batch-updates
           (setq fg-color "red")
           (setq bg-color "blue"))
         (tp-at 1 'face))))
(defvar my-face-color "blue")
(chk "RC-anon" '((:foreground "blue") (:foreground "red"))
     (progn
       (tp-layer-reset)
       (setq my-face-color "blue")
       (with-temp-buffer
         (insert "Hello World")
         (tp-set 1 10 '(face (:foreground $my-face-color)))
         (let ((before (tp-at 1 'face)))
           (setq my-face-color "red")
           (list before (tp-at 1 'face))))))

;; ---- Theme example (as in the docs) ----
(declare-function switch-to-light-theme "tp-doctest")
(defvar theme-fg "white")
(defvar theme-bg "black")
(defvar theme-accent "cyan")
(chk "RC-theme" '(:before ((:foreground "cyan" :weight bold)
                           (:foreground "white" :background "black"))
                  :after ((:foreground "blue" :weight bold)
                          (:foreground "black" :background "white")))
     (progn
       (tp-layer-reset)
       (setq theme-fg "white" theme-bg "black" theme-accent "cyan")
       (define-tp code-text ()
         :props '(face (:foreground $theme-fg :background $theme-bg)))
       (define-tp code-keyword ()
         :props '(face (:foreground $theme-accent :weight bold)))
       (defun switch-to-light-theme ()
         (interactive)
         (setq theme-fg "black")
         (setq theme-bg "white")
         (setq theme-accent "blue"))
       (defun switch-to-dark-theme ()
         (interactive)
         (setq theme-fg "white")
         (setq theme-bg "black")
         (setq theme-accent "cyan"))
       (with-temp-buffer
         (insert "(defun greet () (let (x) x))")
         (tp-set (point-min) (point-max) 'code-text)
         (tp-match-set '("defun" "defvar" "let" "if" "when") 'code-keyword)
         (let ((before (list (tp-at 2 'face) (tp-at 10 'face))))
           (switch-to-light-theme)
           (list :before before
                 :after (list (tp-at 2 'face) (tp-at 10 'face)))))))

;; ---- Regexp and string-form examples ----
(tp-layer-reset)
(chk "X-buffer-return" '(1 . 10)
     (let ((my-buffer (generate-new-buffer "*test*")))
       (with-current-buffer my-buffer (insert "Hello World"))
       (prog1 (tp-set 1 10 '(face italic) my-buffer)
         (kill-buffer my-buffer))))
(chk-str "X-regexp-case-fold" "#(\"Hello WORLD\" 0 5 (face bold) 6 11 (face bold))"
         (tp-regexp-set "[A-Z]+" '(face bold) "Hello WORLD"))
(chk-str "X-regexp-multi" "#(\"abc 123 XYZ\" 0 3 (face bold) 4 7 (face bold) 8 11 (face bold))"
         (tp-regexp-set '("[0-9]+" "[A-Z]+") '(face bold) "abc 123 XYZ"))
(chk "X-regexp-reset-new-string" '((face italic) (help-echo "original"))
     (let ((str (copy-sequence "abc 123 def")))
       (tp-set 4 7 '(help-echo "original") str)
       (let ((result (tp-regexp-reset "[0-9]+" '(face italic) str)))
         (list (tp-at 4 result) (tp-at 4 str)))))
(chk "X-regexp-add-new-string" '((face italic help-echo "number") (help-echo "number"))
     (let ((str (copy-sequence "abc 123 def")))
       (tp-set 4 7 '(help-echo "number") str)
       (let ((result (tp-regexp-add "[0-9]+" '(face italic) str)))
         (list (tp-at 4 result) (tp-at 4 str)))))
(chk "X-match-per-pattern-order" '((7 . 12) (1 . 6) (14 . 19))
     (with-temp-buffer
       (insert "Hello world, Hello again")
       (tp-match-set '("world" "Hello") '(face bold))))
(chk "X-do-shortfall-all-or-nothing" '(1 "hello world")
     (let ((str (copy-sequence "hello world")))
       (tp-set 0 5 '(marker t) str)
       (list (tp-forward-do #'upcase 'marker nil str 3)
             (substring-no-properties str))))

(princ (format "\nTOTAL: %d  FAILS: %d\n" tp-doctest--total tp-doctest--fails))
(when (> tp-doctest--fails 0) (kill-emacs 1))

;;; tp-doctest.el ends here
