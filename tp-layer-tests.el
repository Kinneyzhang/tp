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

(provide 'tp-layer-tests)
;;; tp-layer-tests.el ends here
