;;; melpa-recipe.el --- draft MELPA recipe for tp -*- lexical-binding: t -*-

;; Draft recipe for a future MELPA submission (not yet submitted).
;; The package ships the nine library modules plus the tp.el umbrella;
;; test suites, doctests, and dev scripts are excluded.
;;
;; Verified locally with: package-lint (0 findings, main file tp.el)
;; and a multi-Emacs CI matrix (28.1 / 29.4 / 30.1).

(tp :fetcher github
    :repo "Kinneyzhang/tp"
    :files ("tp.el" "tp-core.el" "tp-reactive.el" "tp-layer.el"
            "tp-ops.el" "tp-search.el" "tp-render.el" "tp-stack.el"
            "tp-palette.el" "tp-builtins.el"))

;;; melpa-recipe.el ends here
