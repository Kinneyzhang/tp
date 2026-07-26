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

(require 'subr-x)                       ; string-trim-right

(defalias 'tp-palette--plistp
  (if (fboundp 'plistp)
      #'plistp
    (lambda (object)
      (let ((len (proper-list-p object)))
        (and len (zerop (% len 2)) t))))
  "Return non-nil if OBJECT is a property list.
Compatibility shim: `plistp' was only added in Emacs 29.1, while
the library supports Emacs 28.1.")

(defvar tp-palette-alist nil
  "Alist of (NAME . PLIST) palette definitions.
This is the single source of truth for palette lookups.")

(defmacro define-tp-palette (name &rest plist)
  "Register a color palette named NAME, defined by PLIST.
PLIST maps the keys :fg, :bg and :border to colors in any format
accepted by `tp-parse-color' (usually a (LIGHT . DARK) cons).
The palette is stored in `tp-palette-alist'; re-evaluating a
definition updates the stored palette in place."
  (declare (indent defun))
  `(setf (alist-get ',name tp-palette-alist) '(,@plist)))

(defalias 'tp-define-palette 'define-tp-palette
  "Register a color palette named NAME; alias of `define-tp-palette'.
This is the package-prefix-conforming name for the palette
definition macro, so it is discoverable via the tp- prefix;
`define-tp-palette' is the historical name and both are permanent -
neither will be removed.  See `define-tp-palette' for the full
documentation of NAME and PLIST.")
(function-put 'tp-define-palette 'lisp-indent-function 'defun)

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

(define-tp-palette gray-50
  :fg ("#24292f" . "#fafafa") :bg ("#fafafa" . "#171717")
  :border ("#e5e5e5" . "#262626"))

(define-tp-palette gray-100
  :fg ("#24292f" . "#f5f5f5") :bg ("#f5f5f5" . "#1c1c1c")
  :border ("#e5e5e5" . "#262626"))

(define-tp-palette gray-200
  :fg ("#24292f" . "#e5e5e5") :bg ("#e5e5e5" . "#262626")
  :border ("#d4d4d4" . "#404040"))

(define-tp-palette gray-300
  :fg ("#24292f" . "#d4d4d4") :bg ("#d4d4d4" . "#404040")
  :border ("#a3a3a3" . "#525252"))

(define-tp-palette gray-400
  :fg ("#24292f" . "#a3a3a3") :bg ("#a3a3a3" . "#525252")
  :border ("#737373" . "#737373"))

(define-tp-palette gray-500
  :fg ("#ffffff" . "#737373") :bg ("#737373" . "#737373")
  :border ("#525252" . "#a3a3a3"))

(define-tp-palette gray-600
  :fg ("#ffffff" . "#525252") :bg ("#525252" . "#a3a3a3")
  :border ("#404040" . "#d4d4d4"))

(define-tp-palette gray-700
  :fg ("#ffffff" . "#404040") :bg ("#404040" . "#d4d4d4")
  :border ("#262626" . "#e5e5e5"))

(define-tp-palette gray-800
  :fg ("#ffffff" . "#262626") :bg ("#262626" . "#e5e5e5")
  :border ("#171717" . "#f5f5f5"))

(define-tp-palette gray-900
  :fg ("#ffffff" . "#171717") :bg ("#171717" . "#f5f5f5")
  :border ("#0a0a0a" . "#fafafa"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-tp-palette mark
  :fg ("#9a6700" . "#f0c239") :bg ("#fff8c5" . "#533d00")
  :border ("#d4a72c" . "#9e6a03"))

(define-tp-palette tag
  :fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" .  "#21262d")
  :border ("#d0d7de" .  "#888"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; github 经典绿色主题
(define-tp-palette heatmap-g0
  :fg ("#ebedf0" . "#161b22"))

(define-tp-palette heatmap-g1
  :fg ("#9be9a8" . "#0e4429"))

(define-tp-palette heatmap-g2
  :fg ("#40c463" . "#006d32"))

(define-tp-palette heatmap-g3
  :fg ("#30a14e" . "#26a641"))

(define-tp-palette heatmap-g4
  :fg ("#216e39" . "#39d353"))

;; github 万圣节主题
(define-tp-palette heatmap-h0
  :fg ("#ebedf0" . "#161b22"))

(define-tp-palette heatmap-h1
  :fg ("#ffee4a" . "#631c03"))

(define-tp-palette heatmap-h2
  :fg ("#ffc501" . "#bd561d"))

(define-tp-palette heatmap-h3
  :fg ("#fe9600" . "#fa7a18"))

(define-tp-palette heatmap-h4
  :fg ("#03001c" . "#fddf68"))

;;; Utilities

(defun tp-theme-dark-p ()
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun tp-theme-light-p ()
  (eq (frame-parameter nil 'background-mode) 'light))

(defun tp-parse-color (color)
  "Resolve COLOR to a color string for the current theme.
COLOR may be:
- a color string, returned as is: \"red\"
- a (LIGHT . DARK) cons: (\"red\" . \"green\"); either side may be
  nil, meaning no color for that mode
- a (:light LIGHT :dark DARK) plist: (:light \"red\" :dark \"green\")
Return nil when COLOR is nil, or when the side selected by the
current theme is nil.  When the theme cannot be determined, fall
back to the light color."
  (cond ((stringp color) color)
        ((and (consp color)
              (or (stringp (car color)) (null (car color)))
              (or (stringp (cdr color)) (null (cdr color))))
         (cond
          ((tp-theme-light-p) (car color))
          ((tp-theme-dark-p) (cdr color))
          ;; Default to light color when background-mode is unknown
          (t (car color))))
        ((and (tp-palette--plistp color)
              (or (plist-member color :light)
                  (plist-member color :dark)))
         (cond
          ((tp-theme-light-p) (plist-get color :light))
          ((tp-theme-dark-p) (plist-get color :dark))
          ;; Default to light color when background-mode is unknown
          (t (plist-get color :light))))
        ((null color) nil)
        (t (error "Invalid format of color %S" color))))

(defun tp-palette--get-color (symbol key)
  "Get color value for KEY from the palette named SYMBOL.
SYMBOL is looked up in `tp-palette-alist'.  KEY should be one of
:fg, :bg, or :border.  Return nil if SYMBOL names no registered
palette or its definition doesn't contain KEY."
  (let ((plist (alist-get symbol tp-palette-alist)))
    (when (tp-palette--plistp plist)
      (tp-parse-color (plist-get plist key)))))

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

(defun tp-palette-p (symbol)
  (assoc symbol tp-palette-alist))

(defun tp-palette-fg-p (symbol)
  (save-match-data
    (let ((str (symbol-name symbol)))
      (and (string-match "\\(.+\\)-fg$" str)
           (tp-palette-p (intern (match-string 1 str)))))))

(defun tp-palette-bg-p (symbol)
  (save-match-data
    (let ((str (symbol-name symbol)))
      (and (string-match "\\(.+\\)-bg$" str)
           (tp-palette-p (intern (match-string 1 str)))))))

(defun tp-palette-fbg-p (symbol)
  (save-match-data
    (let ((str (symbol-name symbol)))
      (and (string-match "\\(.+\\)-fbg$" str)
           (tp-palette-p (intern (match-string 1 str)))))))

(defun tp-palette-border-p (symbol)
  (save-match-data
    (let ((str (symbol-name symbol)))
      (and (string-match "\\(.+\\)-border$" str)
           (tp-palette-p (intern (match-string 1 str)))))))

(defun tp-palette-pure (symbol)
  (pcase symbol
    ((pred tp-palette-p) symbol)
    ((pred tp-palette-fg-p)
     (intern (string-trim-right (symbol-name symbol) "-fg")))
    ((pred tp-palette-bg-p)
     (intern (string-trim-right (symbol-name symbol) "-bg")))
    ((pred tp-palette-fbg-p)
     (intern (string-trim-right (symbol-name symbol) "-fbg")))
    ((pred tp-palette-border-p)
     (intern (string-trim-right (symbol-name symbol) "-border")))
    (_ (error "Invalid format of tp-palette: %S" symbol))))

(provide 'tp-palette)
;;; tp-palette.el ends here
