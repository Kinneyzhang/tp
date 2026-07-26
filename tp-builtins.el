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

(defmacro tp-pop-to-buffer (buffer-or-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-or-name)))
     (tp-with-current-buffer buffer
       (erase-buffer)
       ,@body
       (local-set-key "q" (lambda ()
                            (interactive)
                            (local-unset-key "q")
                            (quit-window)))
       (read-only-mode 1))
     (pop-to-buffer buffer)))

(defmacro tp-switch-to-buffer (buffer-or-name &rest body)
  (declare (indent defun))
  `(let ((buffer (get-buffer-create ,buffer-or-name)))
     (tp-with-current-buffer buffer
       (erase-buffer)
       ,@body
       (local-set-key "q" (lambda ()
                            (interactive)
                            (local-unset-key "q")
                            (quit-window)))
       (read-only-mode 1))
     (switch-to-buffer buffer)))

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

(defun tp-suffix-symbol (symbol string)
  (intern (concat (symbol-name symbol) string)))

;;;###autoload
(defun tp-palette-show ()
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
                            (tp-suffix-symbol symbol "-fg"))
                    " "
                    (tp-set (concat name "-bg")
                            'tp-palette
                            (tp-suffix-symbol symbol "-bg"))
                    " "
                    (tp-set (concat name "-fbg")
                            'tp-palette
                            (tp-suffix-symbol symbol "-fbg"))
                    " "
                    (tp-set (concat name "-border")
                            'tp-palette
                            (tp-suffix-symbol symbol "-border")))))
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
  (let ((color (tp-palette-fg-color 'info)))
    `( tp-underline ,color
       tp-palette info-fg
       mouse-face highlight
       pointer hand)))

(define-tp tp-space (width)
  `(display (space :width ,width)))

(define-tp tp-headline (props)
  (let (height boldp)
    (cond ((floatp props)
           (setq height props boldp t))
          ((plistp props)
           (setq height (plist-get props :height)
                 boldp (plist-get props :bold))))
    `(face (:height ,height
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
