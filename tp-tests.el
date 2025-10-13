(require 'tp-tests-utils)

;; tp-layer-alist
;; tp-layer-groups

(tp-layer-define test1
  '(face link :foreground "orange"))

(tp-layer-define test2
  '(face link :foreground "cyan"))

(tp-layer-group-define test-group
  test1 '( display "this is top layer"
           face (:background "red" :foreground "#000"))
  test2 '( display "this is middle layer"
           face (:background "green" :foreground "#000"))
  test3 '( display "this is bottom layer"
           face (:background "cyan" :foreground "#000")))

(setq tp-layer-alist nil)
(setq tp-layer-groups nil)

(tp-layer-propertize "emacs" 'test1)
(tp-layer-group-propertize "emacs" 'test-group)

(defun tp-tests-layer-rotate (&optional btn)
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (tp-layer-rotate (line-beginning-position)
                       (line-end-position)))))

(pop-buffer-do nil
  (insert (tp-layer-group-propertize "emacs" 'test-group)
          "\n\n")
  (insert-text-button
   " eval (tp-tests-layer-rotate) "
   'action 'tp-tests-layer-rotate
   'face '(:box t)
   'follow-link t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pop-buffer-insert nil
  (propertize
   " "
   'display ""
   'face '(:underline (:position t :color "grey")))
  " "
  (propertize
   "hacking"
   'face '(:foreground "cyan"))
  " "
  (propertize
   "emacs"
   'face '(:slant italic :background "orange")))

(with-current-buffer pop-buffer-insert-buffer
  (erase-buffer)
  (tp-tests-run))

(with-current-buffer pop-buffer-insert-buffer
  (let ((inhibit-read-only 1))
    ;; (tp-all (point-min) (point-max))
    (tp-layer-set 'default (point-min) (point-max))
    (tp-layer-push 1
      (point-min) (point-max)
      '( face (:background "green" :foreground "grey")
         display (height 1.2)))
    
    (tp-layer-push 2
      (point-min) (point-max)
      '(face (:background "orange" :foreground "#000")))

    (tp-layer-push 3
      (point-min) (point-max)
      '(face (:weight bold :background "SlateBlue2")))))

(with-current-buffer pop-buffer-insert-buffer
  (let ((inhibit-read-only 1))
    ;; (tp-layer-demote (point-min) (point-max))
    ;; (tp-layer-rotate (point-min) (point-max))
    ;; (tp-layer-delete 2 (point-min) (point-max))
    ;; (tp-layer-pin 1 (point-min) (point-max))
    ;; (tp-layer-pin '3 1 16)
    ))
