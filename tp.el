;;; tp.el --- Text Properties manipulation library for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Version: 0.3.0
;; Keywords: convenience text-properties
;; Author: Geekinney (kinneyzhang666@gmail.com)
;; Package-Requires: ((emacs "28.1") (dash "2.19.1"))
;; URL: https://github.com/Kinneyzhang/tp

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; tp.el is a comprehensive text property manipulation library.
;;
;; It is organized as a stack of modules, each depending only on the
;; ones before it:
;;
;;   tp-core.el     Foundation: intervals, plist/face merge engine,
;;                  debug logging, pure $var utilities.
;;   tp-reactive.el Reactive state: dependency registry, variable
;;                  watchers, batching queue.
;;   tp-layer.el    Layer registry: `define-tp', `define-tps',
;;                  layer/group resolution and expansion.
;;   tp-ops.el      Core primitives: `tp-set', `tp-reset', `tp-add',
;;                  `tp-get', `tp-at', `tp-remove', `tp-clear'.
;;   tp-search.el   Pattern matching (`tp-match-*', `tp-regexp-*') and
;;                  property search/navigation (`tp-search', ...).
;;   tp-render.el   Reactive re-rendering engine (installs itself into
;;                  tp-reactive and tp-ops).
;;   tp-stack.el    Layer stack operations: push/pop/move/merge/...
;;   tp-palette.el  Color palette data (light/dark aware).
;;   tp-builtins.el Built-in layers (tp-fg, tp-link, tp-action, ...)
;;                  and display helpers.
;;
;; Requiring this file loads the whole library:
;;
;;   (require 'tp)
;;
;; See README.md for a guided tour and docs/ for details.
;;
;; Inspired by https://github.com/emacsorphanage/ov
;; Requires Emacs 28.1+ for `object-intervals'.

;;; Code:

(require 'tp-core)
(require 'tp-reactive)
(require 'tp-layer)
(require 'tp-ops)
(require 'tp-search)
(require 'tp-render)
(require 'tp-stack)
(require 'tp-palette)
(require 'tp-builtins)

(provide 'tp)
;;; tp.el ends here
