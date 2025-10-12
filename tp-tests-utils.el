(defvar pop-buffer-insert-buffer
  "*pop-buffer-insert*")

(defvar pop-buffer-win-conf nil)

(defun my-pop-to-buffer (buffer-or-name &optional action norecord)
  (declare (indent defun))
  (let ((buffer (pop-to-buffer buffer-or-name action norecord)))
    (with-current-buffer buffer
      (local-set-key "q" 'pop-buffer-quit))
    buffer))

(defun remove-from-list (list-var element)
  (set list-var (delete element (symbol-value list-var))))

(defun pop-buffer-quit ()
  (interactive)
  (let ((win-conf pop-buffer-win-conf))
    (local-unset-key "q")
    (setq-local pop-buffer-win-conf nil)
    (remove-from-list 'margin-work-modes 'quick-buffer-mode)
    (set-window-configuration win-conf)))

(defmacro with-pop-buffer (buffer-or-name height &rest body)
  (declare (indent defun))
  `(let* ((win-conf (current-window-configuration))
          (height ,height)
          (buffer (my-pop-to-buffer ,buffer-or-name
                    `(,@(if height
                            `(display-buffer-at-bottom
                              (cons window-height height))
                          `(display-buffer-full-frame))))))
     (add-to-list 'margin-work-modes 'quick-buffer-mode)
     (with-current-buffer buffer
       (setq major-mode 'quick-buffer-mode)
       (setq-local pop-buffer-win-conf win-conf)
       (let ((inhibit-read-only t))
         (erase-buffer)
         ,@body)
       (read-only-mode 1))))

(defun pop-buffer-insert (height &rest body)
  (declare (indent defun))
  (if (member pop-buffer-insert-buffer
              (mapcar #'buffer-name (window-buffers)))
      (let ((inhibit-read-only t))
        (select-window (get-buffer-window
                        pop-buffer-insert-buffer))
        (erase-buffer)
        (apply #'insert body))
    (with-pop-buffer pop-buffer-insert-buffer height
      (apply #'insert body))))

(defmacro pop-buffer-do (height &rest body)
  (declare (indent defun))
  `(with-pop-buffer ,pop-buffer-insert-buffer ,height
     ,@body))

(provide 'tp-tests-utils)
