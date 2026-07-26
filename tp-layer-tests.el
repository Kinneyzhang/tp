;;; tp-layer-tests.el --- ERT regression tests for tp-layer.el -*- lexical-binding: t -*-

;;; Commentary:

;; Regression tests for confirmed bugs fixed in the layer-definition
;; module (tp-layer.el).  Each section is tagged with the canonical
;; bug id it guards against.

;;; Code:

(require 'ert)
(require 'tp)

(defmacro tp-layer-tests--with-clean (&rest body)
  "Run BODY with a clean layer/reactive state, resetting afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn (tp-layer-reset) ,@body)
     (tp-layer-reset)))

;; Dynamic variables used by reactive tests ($foo refers to variable foo).
(defvar tp-layer-test-b15-color nil)
(defvar tp-layer-test-b23-color nil)
(defvar tp-layer-test-b26-color nil)

;;; B20: documented parameterized define-tps format must yield props

(ert-deftest tp-layer-test-param-group-docstring-format ()
  "The define-tps docstring Format 2 example returns real props."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-status (color)
      `((face (:foreground ,color)))
      '(face (:weight bold)))
    (should (tp-group-parameterized-p 'tp-layer-test-status))
    (should (equal (tp-group-props-with-arg 'tp-layer-test-status "red")
                   '((face (:foreground "red"))
                     (face (:weight bold)))))))

(ert-deftest tp-layer-test-param-group-resolves-in-tp-set-path ()
  "tp--resolve-props builds a layered structure from a parameterized group."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-status (color)
      `((face (:foreground ,color)))
      '(face (:weight bold)))
    (let ((props (tp--resolve-props '(tp-layer-test-status "red"))))
      (should (equal (plist-get props 'face) '(:foreground "red")))
      (should (equal (plist-get props 'tp-layers)
                     '((face (:weight bold))))))))

(ert-deftest tp-layer-test-param-group-layer-reference-specs ()
  "Parameterized groups still accept layer-name and (LAYER ARG) specs."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-bold () '(face bold))
    (define-tp tp-layer-test-fg (c) `(face (:foreground ,c)))
    (define-tps tp-layer-test-mixed (color)
      'tp-layer-test-bold
      `(tp-layer-test-fg ,color))
    (should (equal (tp-group-props-with-arg 'tp-layer-test-mixed "blue")
                   '((face bold)
                     (face (:foreground "blue")))))))

(ert-deftest tp-layer-test-param-group-named-element ()
  "Parameterized groups accept named (\"NAME\" :props PLIST) elements."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-named (color)
      `(("fg" :props (face (:foreground ,color)))))
    (should (equal (tp-group-props-with-arg 'tp-layer-test-named "red")
                   '((face (:foreground "red")))))))

;;; B21: cyclic layer references signal a clear error, not stack overflow

(ert-deftest tp-layer-test-cycle-self-reference ()
  "A layer referencing itself signals an error naming the cycle."
  (tp-layer-tests--with-clean
    (tp--set-layer-props 'tp-layer-test-cyc '(tp-layer-test-cyc t face bold))
    (let ((err (should-error (tp-layer-props 'tp-layer-test-cyc))))
      (should (string-match-p "cyclic layer reference"
                              (error-message-string err)))
      (should (string-match-p "tp-layer-test-cyc -> tp-layer-test-cyc"
                              (error-message-string err))))))

(ert-deftest tp-layer-test-cycle-mutual-reference ()
  "Two layers referencing each other signal an error naming both."
  (tp-layer-tests--with-clean
    (tp--set-layer-props 'tp-layer-test-ca '(tp-layer-test-cb t face bold))
    (tp--set-layer-props 'tp-layer-test-cb '(tp-layer-test-ca t face italic))
    (let ((err (should-error (tp-layer-props 'tp-layer-test-ca))))
      (should (string-match-p
               "tp-layer-test-ca -> tp-layer-test-cb -> tp-layer-test-ca"
               (error-message-string err))))))

(ert-deftest tp-layer-test-cycle-diamond-is-not-a-cycle ()
  "Re-using the same layer along different branches is not a cycle."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-base () '(face bold))
    (tp--set-layer-props 'tp-layer-test-left '(tp-layer-test-base t help-echo "l"))
    (tp--set-layer-props 'tp-layer-test-right '(tp-layer-test-base t mouse-face highlight))
    (tp--set-layer-props 'tp-layer-test-top
                         '(tp-layer-test-left t tp-layer-test-right t))
    (let ((props (tp-layer-props 'tp-layer-test-top)))
      (should (equal (plist-get props 'help-echo) "l"))
      (should (eq (plist-get props 'mouse-face) 'highlight)))))

;;; B22: extra body forms in define-tp simple format are an error

(ert-deftest tp-layer-test-extra-body-forms-error ()
  "define-tp with two simple body forms errors instead of dropping one."
  (should-error
   (eval '(define-tp tp-layer-test-extra ()
            '(face bold)
            '(display "x"))
         t)))

(ert-deftest tp-layer-test-single-body-form-still-works ()
  "define-tp with exactly one simple body form still defines the layer."
  (tp-layer-tests--with-clean
    (eval '(define-tp tp-layer-test-single () '(face bold)) t)
    (should (equal (tp-layer-props 'tp-layer-test-single) '(face bold)))))

(ert-deftest tp-layer-test-keyword-format-unaffected-by-arity-check ()
  "The reactive keyword format still accepts multiple keyword pairs."
  (tp-layer-tests--with-clean
    (eval '(define-tp tp-layer-test-kw ()
             :props '(face bold)
             :transform #'upcase)
          t)
    (should (equal (plist-get (tp-layer-props 'tp-layer-test-kw) 'face) 'bold))
    (should (eq (cdr (assoc 'tp-layer-test-kw tp-layer-transforms)) #'upcase))))

;;; B23: $-symbols in parameterized bodies resolve instead of leaking

(ert-deftest tp-layer-test-param-layer-resolves-reactive-symbols ()
  "$-syms in a parameterized body resolve to current variable values."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b23-color "green")
    (define-tp tp-layer-test-preact (x)
      `(face (:foreground $tp-layer-test-b23-color) help-echo ,x))
    (should (equal (tp-layer-props-with-arg 'tp-layer-test-preact "hi")
                   '(face (:foreground "green") help-echo "hi")))
    ;; And through the tp-set resolution pipeline as well.
    (should (equal (tp--resolve-props '(tp-layer-test-preact "hi"))
                   '(face (:foreground "green") help-echo "hi")))))

(ert-deftest tp-layer-test-param-layer-reactive-syms-not-registered ()
  "Resolved $-syms in parameterized bodies create no reactive deps."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b23-color "green")
    (define-tp tp-layer-test-preact (x)
      `(face (:foreground $tp-layer-test-b23-color) help-echo ,x))
    (tp-layer-props-with-arg 'tp-layer-test-preact "hi")
    (should-not (tp--layer-has-reactive-deps-p 'tp-layer-test-preact))))

;;; B24: accessors return copies, not internal storage

(ert-deftest tp-layer-test-props-mutation-does-not-corrupt-static-layer ()
  "Mutating the plist returned for a define-tp layer leaves it intact."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-st () '(face bold))
    (let ((props (tp-layer-props 'tp-layer-test-st)))
      (setcar (cdr props) 'MUTATED))
    (should (equal (tp-layer-props 'tp-layer-test-st) '(face bold)))))

(ert-deftest tp-layer-test-props-mutation-does-not-corrupt-old-format ()
  "Mutating the plist returned for an old-format layer leaves it intact."
  (tp-layer-tests--with-clean
    (tp--set-layer-props 'tp-layer-test-old '(face bold))
    (let ((props (tp-layer-props 'tp-layer-test-old)))
      (setcar (cdr props) 'MUTATED))
    (should (equal (tp-layer-props 'tp-layer-test-old) '(face bold)))))

(ert-deftest tp-layer-test-props-deep-mutation-does-not-corrupt ()
  "Mutating nested structure of the returned plist leaves storage intact."
  (tp-layer-tests--with-clean
    (tp--set-layer-props 'tp-layer-test-deep '(face (:weight bold)))
    (let ((props (tp-layer-props 'tp-layer-test-deep)))
      (setcar (plist-get props 'face) 'MUTATED))
    (should (equal (tp-layer-props 'tp-layer-test-deep)
                   '(face (:weight bold))))))

(ert-deftest tp-layer-test-group-props-mutation-does-not-corrupt ()
  "Mutating plists returned by tp-group-props leaves layers intact."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-gm () '(face bold))
    (define-tps tp-layer-test-gmg () 'tp-layer-test-gm)
    (let ((props-list (tp-group-props 'tp-layer-test-gmg)))
      (setcar (cdar props-list) 'MUTATED))
    (should (equal (tp-group-props 'tp-layer-test-gmg) '((face bold))))))

;;; B25: :transform in define-tps group elements is registered

(ert-deftest tp-layer-test-group-element-transform-registered ()
  "A format-4 group element's :transform lands in tp-layer-transforms."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-tg ()
      '("a" :props (face (:foreground $tp-layer-test-b26-color))
        :data ((tp-layer-test-b26-color . "red"))
        :transform upcase))
    (should (eq (cdr (assoc 'tp-layer-test-tg-a tp-layer-transforms))
                'upcase))))

(ert-deftest tp-layer-test-group-element-transform-removed-on-redefine ()
  "Redefining a group element without :transform unregisters the old one."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-tg ()
      '("a" :props (face bold) :transform upcase))
    (should (assoc 'tp-layer-test-tg-a tp-layer-transforms))
    (define-tps tp-layer-test-tg ()
      '("a" . (face bold)))
    (should-not (assoc 'tp-layer-test-tg-a tp-layer-transforms))))

;;; B26: group redefinition / undefinition cleans up generated layers

(ert-deftest tp-layer-test-group-redefine-removes-orphans ()
  "Shrinking a group on redefinition undefines the dropped layers."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-rg ()
      '(face bold) '(face italic) '(face underline))
    (should (assoc 'tp-layer-test-rg-1 tp-layer-alist))
    (should (assoc 'tp-layer-test-rg-2 tp-layer-alist))
    (define-tps tp-layer-test-rg ()
      '(face bold))
    (should (assoc 'tp-layer-test-rg-0 tp-layer-alist))
    (should-not (assoc 'tp-layer-test-rg-1 tp-layer-alist))
    (should-not (assoc 'tp-layer-test-rg-2 tp-layer-alist))
    (should (equal (tp-group-props 'tp-layer-test-rg) '((face bold))))))

(ert-deftest tp-layer-test-undefine-group-removes-generated-layers ()
  "tp-undefine-group also undefines layers generated by the group."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-ug ()
      '(face bold)
      '("named" . (face italic)))
    (tp-undefine-group 'tp-layer-test-ug)
    (should-not (assoc 'tp-layer-test-ug tp-layer-groups))
    (should-not (assoc 'tp-layer-test-ug-0 tp-layer-alist))
    (should-not (assoc 'tp-layer-test-ug-named tp-layer-alist))))

(ert-deftest tp-layer-test-undefine-group-keeps-referenced-layers ()
  "Layers merely referenced by a group survive its undefinition."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-keep () '(face bold))
    (define-tps tp-layer-test-ug2 ()
      'tp-layer-test-keep
      '(face italic))
    (tp-undefine-group 'tp-layer-test-ug2)
    (should (assoc 'tp-layer-test-keep tp-layer-alist))
    (should-not (assoc 'tp-layer-test-ug2-0 tp-layer-alist))))

(ert-deftest tp-layer-test-undefine-group-cleans-reactive-deps ()
  "Undefining a group unregisters reactive deps of generated layers."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-ug3 ()
      '("r" :props (face (:foreground $tp-layer-test-b26-color))
        :data ((tp-layer-test-b26-color . "red"))))
    (should (tp--layer-has-reactive-deps-p 'tp-layer-test-ug3-r))
    (tp-undefine-group 'tp-layer-test-ug3)
    (should-not (tp--layer-has-reactive-deps-p 'tp-layer-test-ug3-r))
    (should-not (assoc 'tp-layer-test-ug3-r tp-layer-alist))))

(ert-deftest tp-layer-test-group-redefine-to-parameterized-cleans-up ()
  "Redefining a plain group as parameterized undefines its old layers."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-pg ()
      '(face bold))
    (should (assoc 'tp-layer-test-pg-0 tp-layer-alist))
    (define-tps tp-layer-test-pg (color)
      `((face (:foreground ,color))))
    (should-not (assoc 'tp-layer-test-pg-0 tp-layer-alist))
    (should (tp-group-parameterized-p 'tp-layer-test-pg))))

;;; B27: unknown keywords in group elements are an error, not a misparse

(ert-deftest tp-layer-test-group-element-unknown-keyword-errors ()
  "An unknown keyword in a format-4 group element signals an error."
  (tp-layer-tests--with-clean
    (let ((err (should-error
                (eval '(define-tps tp-layer-test-bad ()
                         '("a" :props (face bold)
                           :bogus (:props (face italic))))
                      t))))
      (should (string-match-p "Unknown keyword"
                              (error-message-string err))))))

;;; B15: anonymous reactive layers are interned, not minted per call

(ert-deftest tp-layer-test-anonymous-layer-interned ()
  "Equal reactive plists reuse a single anonymous layer entry."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b15-color "red")
    (let* ((s1 (tp-set (copy-sequence "hi")
                       '(face (:foreground $tp-layer-test-b15-color))))
           (s2 (tp-set (copy-sequence "hi")
                       '(face (:foreground $tp-layer-test-b15-color))))
           (n1 (get-text-property 0 'tp-name s1))
           (n2 (get-text-property 0 'tp-name s2)))
      (should n1)
      (should (eq n1 n2))
      ;; Exactly one anonymous registry entry for the shared spec.
      (should (= (length tp-layer-alist) 1)))))

(ert-deftest tp-layer-test-anonymous-layer-distinct-specs-distinct ()
  "Different reactive plists still get different anonymous layers."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b15-color "red")
    (let* ((s1 (tp-set (copy-sequence "hi")
                       '(face (:foreground $tp-layer-test-b15-color))))
           (s2 (tp-set (copy-sequence "hi")
                       '(face (:background $tp-layer-test-b15-color))))
           (n1 (get-text-property 0 'tp-name s1))
           (n2 (get-text-property 0 'tp-name s2)))
      (should n1)
      (should n2)
      (should-not (eq n1 n2)))))

(ert-deftest tp-layer-test-anonymous-layer-reuse-keeps-reactivity ()
  "Reactive updates still reach buffer text using a reused anonymous layer."
  (tp-layer-tests--with-clean
    (with-temp-buffer
      (setq tp-layer-test-b15-color "red")
      (insert "Hello World")
      (tp-set 1 3 '(face (:foreground $tp-layer-test-b15-color)))
      (tp-set 7 9 '(face (:foreground $tp-layer-test-b15-color)))
      (should (eq (get-text-property 1 'tp-name)
                  (get-text-property 7 'tp-name)))
      (setq tp-layer-test-b15-color "blue")
      (should (equal (plist-get (get-text-property 1 'face) :foreground)
                     "blue"))
      (should (equal (plist-get (get-text-property 7 'face) :foreground)
                     "blue")))))

(ert-deftest tp-layer-test-anonymous-registry-cleared-on-reset ()
  "tp-layer-reset clears the anonymous-layer intern registry."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b15-color "red")
    (tp-set (copy-sequence "hi")
            '(face (:foreground $tp-layer-test-b15-color)))
    (should tp--anonymous-layer-registry)
    (tp-layer-reset)
    (should-not tp--anonymous-layer-registry)))

;;; 0.3.0 A4: multi-argument parameterized layers

(ert-deftest tp-layer-test-multi-arg-define-and-props-with-args ()
  "define-tp accepts multi-symbol arglists; props-with-args expands them."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (should (tp-layer-parameterized-p 'tp-layer-test-fgbg))
    (should (equal (tp-layer-arglist 'tp-layer-test-fgbg) '(fg bg)))
    (should (equal (tp-layer-props-with-args 'tp-layer-test-fgbg
                                             '("red" "blue"))
                   '(face (:foreground "red" :background "blue"))))
    (should (equal (tp-layer-props-with-args 'tp-layer-test-fgbg
                                             '("red" "blue") t)
                   '(face (:foreground "red" :background "blue")
                     tp-name tp-layer-test-fgbg)))))

(ert-deftest tp-layer-test-props-with-arg-is-thin-wrapper ()
  "tp-layer-props-with-arg keeps its single-argument contract."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fg1 (c) `(face (:foreground ,c)))
    (should (equal (tp-layer-props-with-arg 'tp-layer-test-fg1 "red")
                   '(face (:foreground "red"))))
    (should (equal (tp-layer-props-with-arg 'tp-layer-test-fg1 "red")
                   (tp-layer-props-with-args 'tp-layer-test-fg1 '("red"))))))

(ert-deftest tp-layer-test-props-with-args-non-parameterized-nil ()
  "props-with-args and tp-layer-arglist return nil for other layers."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-np () '(face bold))
    (should-not (tp-layer-props-with-args 'tp-layer-test-np '(1)))
    (should-not (tp-layer-arglist 'tp-layer-test-np))
    (should-not (tp-layer-props-with-args 'tp-layer-test-missing '(1)))))

(ert-deftest tp-layer-test-multi-arg-tp-set-flat-string-form ()
  "The flat (tp-set STRING \\='LAYER ARG1 ARG2) form binds all params."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (let ((s (tp-set "hello" 'tp-layer-test-fgbg "red" "blue")))
      (should (equal (get-text-property 0 'face s)
                     '(:foreground "red" :background "blue"))))))

(ert-deftest tp-layer-test-multi-arg-tp-set-flat-with-extra-props ()
  "Extra props after multi args survive, with no stray nil pair."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (let ((s (tp-set "hello" 'tp-layer-test-fgbg "red" "blue"
                     'help-echo "tip")))
      (should (equal (plist-get (get-text-property 0 'face s) :foreground)
                     "red"))
      (should (equal (get-text-property 0 'help-echo s) "tip"))
      ;; The odd-length flat spec is padded with nil by key merging;
      ;; resolution must strip it instead of setting a nil property.
      (should (equal (text-properties-at 0 s)
                     '(face (:foreground "red" :background "blue")
                       help-echo "tip"))))))

(ert-deftest tp-layer-test-multi-arg-tp-set-region-list-form ()
  "The region form (tp-set START END \\='(LAYER ARG1 ARG2)) works (1-based)."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (with-temp-buffer
      (insert "hello")
      (tp-set 1 4 '(tp-layer-test-fgbg "red" "blue"))
      (should (equal (get-text-property 1 'face)
                     '(:foreground "red" :background "blue")))
      (should-not (get-text-property 4 'face)))))

(ert-deftest tp-layer-test-multi-arg-tp-set-wrapped-args-plist-form ()
  "The plist spec (LAYER (ARG1 ARG2) EXTRA...) passes args as one list."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    ;; Layer at the head of the plist.
    (let ((s (copy-sequence "hello")))
      (tp-set 0 5 '(tp-layer-test-fgbg ("red" "blue") help-echo "tip") s)
      (should (equal (get-text-property 0 'face s)
                     '(:foreground "red" :background "blue")))
      (should (equal (get-text-property 0 'help-echo s) "tip")))
    ;; Layer at a non-head plist position.
    (let ((s (copy-sequence "hello")))
      (tp-set 0 5 '(help-echo "tip" tp-layer-test-fgbg ("red" "blue")) s)
      (should (equal (plist-get (get-text-property 0 'face s) :background)
                     "blue"))
      (should (equal (get-text-property 0 'help-echo s) "tip")))))

(ert-deftest tp-layer-test-multi-arg-normalize-layer-spec ()
  "tp--normalize-layer-spec accepts (LAYER ARG1 ARG2) specs."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (should (equal (tp--normalize-layer-spec
                    '(tp-layer-test-fgbg "red" "blue"))
                   '(face (:foreground "red" :background "blue")
                     tp-name tp-layer-test-fgbg)))))

(ert-deftest tp-layer-test-multi-arg-tp-put-layer ()
  "tp-put-layer accepts multi-argument parameterized layer specs."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-fgbg (fg bg)
      `(face (:foreground ,fg :background ,bg)))
    (let ((s (copy-sequence "hi")))
      (tp-put-layer s '(tp-layer-test-fgbg "red" "blue") 0)
      (should (equal (get-text-property 0 'face s)
                     '(:foreground "red" :background "blue")))
      (should (eq (get-text-property 0 'tp-name s) 'tp-layer-test-fgbg)))))

(ert-deftest tp-layer-test-multi-arg-cycle-detection ()
  "Cycle detection still fires through the multi-argument path."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-mcyc (a b)
      `(tp-layer-test-mcyc (,a ,b)))
    (let ((err (should-error
                (tp-layer-props-with-args 'tp-layer-test-mcyc '(1 2)))))
      (should (string-match-p "cyclic layer reference"
                              (error-message-string err))))))

(ert-deftest tp-layer-test-multi-arg-props-are-copies ()
  "props-with-args returns fresh copies; mutation cannot corrupt storage."
  (tp-layer-tests--with-clean
    ;; The (:weight bold) subform is a shared constant in the
    ;; backquoted body; without copy-on-return, mutating the returned
    ;; plist would corrupt every later expansion.
    (define-tp tp-layer-test-mcopy (a b)
      `(face (:weight bold) help-echo ,(format "%s-%s" a b)))
    (let ((props (tp-layer-props-with-args 'tp-layer-test-mcopy '("x" "y"))))
      (setcar (plist-get props 'face) 'MUTATED))
    (should (equal (tp-layer-props-with-args 'tp-layer-test-mcopy '("x" "y"))
                   '(face (:weight bold) help-echo "x-y")))))

(ert-deftest tp-layer-test-multi-arg-group ()
  "define-tps accepts multi-symbol arglists usable through tp-set specs."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-mgrp (fg w)
      `((face (:foreground ,fg)))
      `((face (:weight ,w))))
    (should (tp-group-parameterized-p 'tp-layer-test-mgrp))
    (should (equal (tp--group-arglist 'tp-layer-test-mgrp) '(fg w)))
    (should (equal (tp--group-props-with-args 'tp-layer-test-mgrp
                                              '("red" bold))
                   '((face (:foreground "red")) (face (:weight bold)))))
    ;; Flat (GROUP ARG1 ARG2) spec through the tp-set pipeline.
    (let ((props (tp--resolve-props '(tp-layer-test-mgrp "red" bold))))
      (should (equal (plist-get props 'face) '(:foreground "red")))
      (should (equal (plist-get props 'tp-layers)
                     '((face (:weight bold))))))
    ;; Single-argument groups keep working through the wrapper.
    (define-tps tp-layer-test-sgrp (color)
      `((face (:foreground ,color))))
    (should (equal (tp-group-props-with-arg 'tp-layer-test-sgrp "red")
                   '((face (:foreground "red")))))))

;;; 0.3.0 A5: tp-describe-layer and its data collector

(ert-deftest tp-layer-test-describe-data-unified ()
  "Describe data for a define-tp layer reports the unified format."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-du () '(face bold))
    (let ((data (tp--describe-layer-data 'tp-layer-test-du)))
      (should (eq (plist-get data :name) 'tp-layer-test-du))
      (should (eq (plist-get data :format) 'unified))
      (should (equal (plist-get data :body) '(quote (face bold))))
      (should (equal (plist-get data :props)
                     '(face bold tp-name tp-layer-test-du)))
      (should-not (plist-get data :arglist))
      (should-not (plist-get data :reactive-deps))
      (should-not (plist-get data :transform))
      (should-not (plist-get data :group)))))

(ert-deftest tp-layer-test-describe-data-flat ()
  "Describe data for an old-format layer reports the flat format."
  (tp-layer-tests--with-clean
    (tp--set-layer-props 'tp-layer-test-df '(face italic))
    (let ((data (tp--describe-layer-data 'tp-layer-test-df)))
      (should (eq (plist-get data :format) 'flat))
      (should (equal (plist-get data :body) '(face italic)))
      (should (equal (plist-get data :props)
                     '(face italic tp-name tp-layer-test-df))))))

(ert-deftest tp-layer-test-describe-data-parameterized ()
  "Describe data for a parameterized layer reports arglist and a note."
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-dp (a b)
      `(face (:foreground ,a :background ,b)))
    (let ((data (tp--describe-layer-data 'tp-layer-test-dp)))
      (should (eq (plist-get data :format) 'parameterized))
      (should (equal (plist-get data :arglist) '(a b)))
      ;; Expanded props need arguments, so a placeholder note is used.
      (should (stringp (plist-get data :props)))
      (should (string-match-p "tp-layer-props-with-args"
                              (plist-get data :props))))))

(ert-deftest tp-layer-test-describe-data-reactive ()
  "Describe data for a reactive layer reports format and dependencies."
  (tp-layer-tests--with-clean
    (setq tp-layer-test-b15-color "red")
    (define-tp tp-layer-test-dr ()
      '(face (:foreground $tp-layer-test-b15-color)))
    (let ((data (tp--describe-layer-data 'tp-layer-test-dr)))
      (should (eq (plist-get data :format) 'reactive))
      (should (equal (plist-get data :reactive-deps)
                     '(tp-layer-test-b15-color))))))

(ert-deftest tp-layer-test-describe-data-group-and-transform ()
  "Describe data reports the owning group and transform presence."
  (tp-layer-tests--with-clean
    (define-tps tp-layer-test-dg ()
      '("a" :props (face bold) :transform upcase))
    (let ((data (tp--describe-layer-data 'tp-layer-test-dg-a)))
      (should (eq (plist-get data :group) 'tp-layer-test-dg))
      (should (plist-get data :transform)))))

(ert-deftest tp-layer-test-describe-data-unknown-layer-nil ()
  "Describe data returns nil for names not in tp-layer-alist."
  (tp-layer-tests--with-clean
    (should-not (tp--describe-layer-data 'tp-layer-test-nonexistent))))

(ert-deftest tp-layer-test-describe-layer-command ()
  "tp-describe-layer is a command and renders a help buffer."
  (should (commandp 'tp-describe-layer))
  (tp-layer-tests--with-clean
    (define-tp tp-layer-test-dc () '(face bold))
    (save-window-excursion
      (tp-describe-layer 'tp-layer-test-dc)
      (with-current-buffer (help-buffer)
        (should (string-match-p "tp-layer-test-dc is a tp layer"
                                (buffer-string)))
        (should (string-match-p "Storage format: unified"
                                (buffer-string)))))
    (should-error (tp-describe-layer 'tp-layer-test-missing)
                  :type 'user-error)))

(provide 'tp-layer-tests)
;;; tp-layer-tests.el ends here
