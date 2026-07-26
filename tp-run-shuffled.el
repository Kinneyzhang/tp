;;; tp-run-shuffled.el --- run the ERT suite in a shuffled order -*- lexical-binding: t -*-

;; Copyright (C) 2024-2026 Geekinney

;; Author: Geekinney (kinneyzhang666@gmail.com)

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; Development script (not part of the installed package): runs every
;; loaded ERT test individually in a shuffled order to catch
;; inter-test state leaks that the fixed definition order hides.
;;
;; ERT's `member' selector does NOT control execution order (tests
;; always run in definition order), so this script loops over the
;; shuffled names and runs each test on its own.
;;
;; Usage (after loading tp and all *-tests.el files):
;;   emacs -Q --batch -L . -l tp.el -l tp-tests.el ... -l tp-run-shuffled.el
;; or: make test-shuffled
;;
;; The shuffle seed is printed; reproduce a failing order with
;;   SHUFFLE_SEED=<seed> make test-shuffled

;;; Code:

(require 'ert)
(require 'cl-lib)

(defun tp-run-shuffled--permute (list state)
  "Return LIST deterministically permuted from integer seed STATE."
  (let* ((v (vconcat list))
         (n (length v)))
    (dotimes (i (1- n))
      ;; Simple LCG so a printed seed reproduces the exact order.
      (setq state (mod (+ (* state 1103515245) 12345) 2147483648))
      (let* ((j (+ i (mod state (- n i))))
             (tmp (aref v i)))
        (aset v i (aref v j))
        (aset v j tmp)))
    (append v nil)))

(let* ((names (mapcar #'ert-test-name (ert-select-tests t t)))
       (seed (let ((env (getenv "SHUFFLE_SEED")))
               (if (and env (not (string-empty-p env)))
                   (string-to-number env)
                 (progn (random t) (abs (random 1000000))))))
       (shuffled (tp-run-shuffled--permute names seed))
       (unexpected 0))
  (message "tp: running %d tests in shuffled order (SHUFFLE_SEED=%d)"
           (length shuffled) seed)
  (dolist (name shuffled)
    (let ((stats (ert-run-tests-batch name)))
      (cl-incf unexpected (ert-stats-completed-unexpected stats))))
  (message "tp: shuffled run complete: %d tests, %d unexpected (seed %d)"
           (length shuffled) unexpected seed)
  (kill-emacs (if (zerop unexpected) 0 1)))

;;; tp-run-shuffled.el ends here
