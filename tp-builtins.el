;;; tp-builtins.el --- Built-in layers and display helpers for tp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Batteries: the built-in layers (tp-fg, tp-bg, tp-button, tp-link,
;; tp-space, tp-headline, tp-action, ...), the palette gallery command
;; `tp-palette-show', and the read-only display buffer macros.

;;; Code:

(require 'cl-lib)
(require 'tp-core)
(require 'tp-layer)
(require 'tp-ops)
(require 'tp-palette)

(defvar tp-display-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `tp-display-buffer-mode'.")

(define-minor-mode tp-display-buffer-mode
  "Minor mode enabled in tp read-only display buffers.
It binds \\`q' to `quit-window' in its own buffer-local minor-mode
keymap, leaving the major-mode keymap (which is shared by every
buffer of that major mode) untouched."
  :lighter nil
  :keymap tp-display-buffer-mode-map)

(eval-and-compile
  (defun tp--display-buffer-form (buffer-or-name body display-fn)
    "Build the shared expansion of the display-buffer macros.
BUFFER-OR-NAME and BODY are the macro arguments; DISPLAY-FN is
the symbol of the function used to display the populated buffer."
    (let ((buffer (gensym "tp-buffer-")))
      `(let ((,buffer (get-buffer-create ,buffer-or-name)))
         (tp-with-current-buffer ,buffer
           (erase-buffer)
           ,@body
           (tp-display-buffer-mode 1)
           (read-only-mode 1))
         (,display-fn ,buffer)))))

(defmacro tp-pop-to-buffer (buffer-or-name &rest body)
  "Show BUFFER-OR-NAME with `pop-to-buffer' after filling it by BODY.
The buffer is created if needed and erased, then BODY runs inside
it with `inhibit-read-only' non-nil.  The buffer is finally made
read-only with `tp-display-buffer-mode' enabled, so \\`q' quits
its window."
  (declare (indent defun))
  (tp--display-buffer-form buffer-or-name body 'pop-to-buffer))

(defmacro tp-switch-to-buffer (buffer-or-name &rest body)
  "Show BUFFER-OR-NAME with `switch-to-buffer' after filling it by BODY.
The buffer is created if needed and erased, then BODY runs inside
it with `inhibit-read-only' non-nil.  The buffer is finally made
read-only with `tp-display-buffer-mode' enabled, so \\`q' quits
its window."
  (declare (indent defun))
  (tp--display-buffer-form buffer-or-name body 'switch-to-buffer))

(define-tp tp-palette (palette)
  (let* ((pure-palette (tp-palette-pure palette))
         (fg-color (tp-palette-fg-color pure-palette))
         (bg-color (tp-palette-bg-color pure-palette))
         (border-color (tp-palette-border-color pure-palette)))
    (pcase palette
      ((pred tp-palette-p)
       `(face (,@(when fg-color (list :foreground fg-color))
               ,@(when bg-color (list :background bg-color))
               ,@(when border-color (list :box (list :color border-color))))))
      ((pred tp-palette-fg-p)
       `(face (,@(when fg-color (list :foreground fg-color)))))
      ((pred tp-palette-bg-p)
       `(face (,@(when bg-color (list :background bg-color)))))
      ((pred tp-palette-fbg-p)
       `(face (,@(when fg-color (list :foreground fg-color))
               ,@(when bg-color (list :background bg-color)))))
      ((pred tp-palette-border-p)
       `(face (,@(when border-color (list :box (list :color border-color))))))
      (_ (error "Invalid palette: %S" palette)))))

(defun tp--suffix-symbol (symbol string)
  "Intern the symbol named by SYMBOL's name with STRING appended.
For example (tp--suffix-symbol \\='info \"-fg\") returns `info-fg'.
A generic helper with no tp semantics of its own, used by
`tp-palette-show' to build the suffixed palette variant names."
  (intern (concat (symbol-name symbol) string)))

(define-obsolete-function-alias 'tp-suffix-symbol
  'tp--suffix-symbol "0.3.0")

;;;###autoload
(defun tp-palette-show ()
  "Display a gallery of every palette registered in `tp-palette-alist'.
Shows the read-only buffer *tp-palette-gallery* listing, for each
palette NAME, the symbols the `tp-palette' layer accepts: NAME itself
\(foreground, background and border together) plus the NAME-fg,
NAME-bg, NAME-fbg and NAME-border variants, each label rendered in
the colors it selects for the current theme.  Press \\`q' to quit
the gallery window."
  (interactive)
  (let ((alist (seq-reverse tp-palette-alist)))
    (tp-switch-to-buffer "*tp-palette-gallery*"
      (insert
       "Please set " (tp-set "'tp-palette" 'tp-palette 'code)
       " text property with following symbols:\n\n"
       (mapconcat
        (lambda (item)
          (let* ((symbol (car item))
                 (name (symbol-name symbol)))
            (concat (tp-set name 'tp-palette symbol)
                    " "
                    (tp-set (concat name "-fg")
                            'tp-palette
                            (tp--suffix-symbol symbol "-fg"))
                    " "
                    (tp-set (concat name "-bg")
                            'tp-palette
                            (tp--suffix-symbol symbol "-bg"))
                    " "
                    (tp-set (concat name "-fbg")
                            'tp-palette
                            (tp--suffix-symbol symbol "-fbg"))
                    " "
                    (tp-set (concat name "-border")
                            'tp-palette
                            (tp--suffix-symbol symbol "-border")))))
        alist "\n")))))

(define-tp tp-fg (color)
  `(face (:foreground ,color)))

(define-tp tp-bg (color)
  `(face (:background ,color)))

(define-tp tp-button (type)
  (let ((palette (intern
                  (format "%s%s%s" "button-" (symbol-name type) "-fbg"))))
    `( tp-palette ,palette pointer hand
       face (:box ( :line-width -1
                    :style released-button)))))

(define-tp tp-underline (color)
  `(face (:underline (:color ,color))))

(define-tp tp-delete (color)
  `(face (:strike-through ,color)))

(define-tp tp-link ()
  ;; No color is resolved here: the body of a zero-arg layer is
  ;; evaluated once, when this file is loaded, so any color computed
  ;; here would be frozen forever (wrong after a theme switch, or in a
  ;; daemon session started before any frame exists).  Instead the
  ;; nested parameterized layer `tp-palette' resolves the info
  ;; foreground lazily at application time, and `:underline t'
  ;; underlines with that same foreground color.
  '( face (:underline t)
     tp-palette info-fg
     mouse-face highlight
     pointer hand))

(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

(define-tp tp-headline (props)
  ;; PROPS is either a number - a float scaling factor or an integer
  ;; absolute height in units of 1/10 pt, both valid face :height
  ;; values - implying bold, or a (:height H :bold B) plist.
  (let (height boldp)
    (cond ((numberp props)
           (setq height props boldp t))
          ((tp-palette--plistp props)
           (setq height (plist-get props :height)
                 boldp (plist-get props :bold)))
          (t (error "Invalid tp-headline spec: %S" props)))
    `(face (,@(when height (list :height height))
            ,@(when boldp '(:weight bold))))))

(define-tp tp-action (sexp)
  ;; SEXP is a function or plist
  (let (action keys)
    (if (functionp sexp)
        (progn
          (setq action sexp)
          (setq keys `(,(kbd "RET") [mouse-1])))
      (setq action (plist-get sexp :action))
      (setq keys (or (plist-get sexp :keys)
                     `(,(kbd "RET") [mouse-1]))))
    `( keymap ,(let ((keymap (make-sparse-keymap)))
                 (dolist (key keys)
                   (define-key keymap key action))
                 keymap)
       rear-nonsticky (keymap))))

(provide 'tp-builtins)
;;; tp-builtins.el ends here
