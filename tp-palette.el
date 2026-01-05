;;; tp-palette.el --- Color palette definitions for tp.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; This file provides color palette definitions for tp.el.
;; Colors are designed to work well in both light and dark themes.
;; Each palette entry has the format:
;;   (:fg (LIGHT-FG . DARK-FG) :bg (LIGHT-BG . DARK-BG) :border (LIGHT-BORDER . DARK-BORDER))
;;
;; Color scheme inspired by:
;; - GitHub Primer Design System
;; - Tailwind CSS Color Palette
;; - Material Design Color System
;; - One Dark / One Light themes

;;; Code:

;; ============================================
;; 基础文本样式 (Basic Text Styles)
;; ============================================

(defvar tp-palette-alist nil)

(defmacro define-tp-palette (name &rest plist)
  (declare (indent defun))
  (let ((var (intern (concat "tp-palette-" (symbol-name name)))))
    `(progn
       (setf (alist-get ',name tp-palette-alist)
             '(,@plist))
       (defvar ,var '(,@plist)))))

(define-tp-palette button-primary
  :fg ("#ffffff" . "#ffffff") :bg ("#007bff" . "#007bff"))

(define-tp-palette button-secondary
  :fg ("#ffffff" . "#ffffff") :bg ("#6c757d" . "#6c757d"))

(define-tp-palette button-info
  :fg ("#ffffff" . "#ffffff") :bg ("#17a2b8" . "#17a2b8"))

(define-tp-palette button-success
  :fg ("#ffffff" . "#ffffff") :bg ("#28a745" . "#28a745"))

(define-tp-palette button-warning
  :fg ("#000000" . "#000000") :bg ("#ffc107" . "#ffc107"))

(define-tp-palette button-danger
  :fg ("#ffffff" . "#ffffff") :bg ("#dc3545" . "#dc3545"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tp-palette heading-1
  :fg ("#0969da" . "#58a6ff") :bg ("#f0f7ff" . "#1a2634")
  :border ("#54aeff" . "#388bfd"))

(define-tp-palette heading-2
  :fg ("#8250df" . "#a371f7") :bg ("#fbefff" . "#271d36")
  :border ("#c297ff" . "#6e40c9"))

(define-tp-palette heading-3
  :fg ("#1a7f37" . "#3fb950") :bg ("#f0fff4" . "#1a2e1f")
  :border ("#4ac26b" . "#238636"))

(define-tp-palette heading-4
  :fg ("#953800" . "#ffa657") :bg ("#fff8f0" . "#2e2318")
  :border ("#ffb86c" . "#9e6a03"))

(define-tp-palette heading-5
  :fg ("#bf3989" . "#db61a2") :bg ("#fff0f7" . "#2e1f28")
  :border ("#f28cb1" . "#8b3d63"))

(define-tp-palette heading-6
  :fg ("#0598bc" . "#39c5cf") :bg ("#f0faff" . "#1a2e33")
  :border ("#56d4dd" . "#2d7d85"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tp-palette todo
  :fg ("#ffffff" . "#ffffff") :bg ("#cf222e" . "#da3633")
  :border ("#a40e26" . "#f85149"))

(define-tp-palette done
  :fg ("#ffffff" . "#ffffff") :bg ("#1a7f37" . "#238636")
  :border ("#116329" . "#2ea043"))

(define-tp-palette code
  :fg ("#0550ae" . "#79c0ff") :bg ("#f6f8fa" . "#161b22")
  :border ("#d0d7de" . "#30363d"))

(define-tp-palette block
  :fg ("#24292f" . "#c9d1d9") :bg ("#f6f8fa" .  "#161b22")
  :border ("#d0d7de" .  "#30363d"))

(define-tp-palette quote
  :fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#161b22")
  :border ("#d0d7de" . "#30363d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tp-palette success
  :fg ("#1a7f37" . "#3fb950") :bg ("#dafbe1" . "#1b4721")
  :border ("#4ac26b" . "#238636"))

(define-tp-palette warning
  :fg ("#9a6700" . "#d29922") :bg ("#fff8c5" . "#3d2e00")
  :border ("#d4a72c" . "#9e6a03"))

(define-tp-palette error
  :fg ("#cf222e" . "#f85149") :bg ("#ffebe9" . "#542426")
  :border ("#ff8182" . "#f85149"))

(define-tp-palette info
  :fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
  :border ("#54aeff" .  "#388bfd"))

(define-tp-palette neutral
  :fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#21262d")
  :border ("#d0d7de" .  "#30363d"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tp-palette rainbow-1
  :fg ("#e45649" . "#e06c75") :border ("#e45649" . "#e06c75"))

(define-tp-palette rainbow-2
  :fg ("#986801" . "#d19a66") :border ("#986801" . "#d19a66"))

(define-tp-palette rainbow-3
  :fg ("#c18401" . "#e5c07b") :border ("#c18401" . "#e5c07b"))

(define-tp-palette rainbow-4
  :fg ("#50a14f" . "#98c379") :border ("#50a14f" . "#98c379"))

(define-tp-palette rainbow-5
  :fg ("#0184bc" . "#56b6c2") :border ("#0184bc" . "#56b6c2"))

(define-tp-palette rainbow-6
  :fg ("#4078f2" . "#61afef") :border ("#4078f2" . "#61afef"))

(define-tp-palette rainbow-7
  :fg ("#a626a4" . "#c678dd") :border ("#a626a4" . "#c678dd"))

(define-tp-palette rainbow-8
  :fg ("#bf3989" . "#db61a2") :border ("#bf3989" . "#db61a2"))

;;; Utilities

(defun tp-palette--get-color (symbol key)
  "Get color value for KEY from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist.
KEY should be one of :fg, :bg, or :border.
Returns nil if SYMBOL is unbound or doesn't contain KEY."
  (setq symbol (intern (concat "tp-palette-"
                               (symbol-name symbol))))
  (when (and (symbolp symbol) (boundp symbol))
    (let ((plist (symbol-value symbol)))
      (when (plistp plist)
        (tp-parse-color (plist-get plist key))))))

(defun tp-palette-fg-color (symbol)
  "Get the foreground color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :fg key.
Returns nil if SYMBOL is unbound or doesn't contain :fg."
  (tp-palette--get-color symbol :fg))

(defun tp-palette-bg-color (symbol)
  "Get the background color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :bg key.
Returns nil if SYMBOL is unbound or doesn't contain :bg."
  (tp-palette--get-color symbol :bg))

(defun tp-palette-border-color (symbol)
  "Get the border color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :border key.
Returns nil if SYMBOL is unbound or doesn't contain :border."
  (tp-palette--get-color symbol :border))

(provide 'tp-palette)
;;; tp-palette.el ends here
