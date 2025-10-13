;; 对 text properties 的封装
;; elisp 原生的 text properties 函数的局限性
;; https://github.com/emacsorphanage/ov

(require 'dash)

;;; tp layer define

(defvar tp-layer-alist nil
  "Alist 的每个元素是单个 layer")

(defvar tp-layer-groups nil
  "group 的每个元素是 layer 组，组中存储的是多个 layer")

(defmacro tp-layer-define (name properties)
  "定义一个名称为 name 的文本属性层，数据存放在 tp-layer-alist 中"
  (declare (indent defun))
  `(progn
     (if (assoc ',name tp-layer-alist)
         (setf (cdr (assoc ',name tp-layer-alist)) ,properties)
       (push (cons ',name ,properties) tp-layer-alist))
     (assoc ',name tp-layer-alist)))

(defmacro tp-layer-group-define (name &rest layers)
  "每个属性层在 tp-layer-alist 中，属性组指在 tp-layer-groups 中存储属性层的名称
层级关系与layers定义顺序一致。最上面的定义表示顶层，渲染时会显示出来。"
  (declare (indent defun))
  `(let ((layer-names
          (nreverse
           (-map (lambda (lst)
                   (let ((layer-name (car lst)))
                     (eval `(tp-layer-define ,layer-name ,(cadr lst)))
                     layer-name))
                 (-partition 2 ',layers)))))
     (if (assoc ',name tp-layer-groups)
         (setf (cdr (assoc ',name tp-layer-groups)) layer-names)
       (push (cons ',name layer-names) tp-layer-groups))
     (assoc ',name tp-layer-groups)))


;;; tp layer

(defun tp-top-layer-props (properties)
  "获取最上面的属性层，也就是实际要渲染的层"
  (if-let ((idx (-elem-index 'tp-layers properties)))
      (-remove-at-indices (list idx (1+ idx)) properties)
    properties))

(defun tp-below-layers-props (properties)
  "获取最上层以下的属性层列表"
  (plist-get properties 'tp-layers))

(defun tp-all (start end &optional object)
  "获取 OBJECT 的 start 到 end 范围内文本的所有 text properties。
OBJECT 可以为 buffer 或 string，nil 默认为当前 buffer。
point 从 0 开始。"
  (let ((object (or object (current-buffer))))
    (cond
     ((stringp object)
      (object-intervals (substring object start end)))
     ((bufferp object)
      (with-current-buffer (get-buffer-create object)
        (object-intervals (buffer-substring start end))))
     (t (error "Invalid format of object: %S"
               (type-of object))))))

(defun tp-empty-p (object)
  (null (object-intervals object)))

(defun tp-intervals-map (function start end &optional object)
  "处理 start，end 之间的所有 intervals 的文本属性
function 的四个参数分别为:区间开始位置，区间结束位置，顶层属性和下层属性列表"
  (remove
   nil
   (mapcar
    (lambda (tp)
      (let* ((interval-start (nth 0 tp)) ;; start from 0
             (interval-end (nth 1 tp))
             (interval-props (nth 2 tp))
             (top-props (tp-top-layer-props interval-props))
             (below-props-lst (tp-below-layers-props interval-props)))
        (funcall function
                 interval-start interval-end
                 top-props below-props-lst)))
    (tp-all start end object))))

(defun tp-layer-props (name start end &optional object)
  "返回 object 的 start 和 end 之间文本属性层为 name 的属性列表"
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (when-let ((props (seq-find
                        (lambda (props)
                          (equal name (plist-get props 'tp-name)))
                        (append (list top) belows))))
       (list (+ start i-start) (+ start i-end) props)))
   start end object))

(defun tp-layer-set (name start end &optional object)
  "将 object 的 start 和 end 之间的文本当前的属性层命名为 name"
  (if (tp-empty-p object)
      (add-text-properties
       start end (list 'tp-name name) end object)
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (plist-put top 'tp-name name)
                (list 'tp-layers belows))
        object))
     start end object))
  object)

(defun tp-layer-push (start end name &optional properties object)
  "设置 properties 为最上面的 prop 层，name 是 layer 的名字"
  (declare (indent defun))
  (when (tp-layer-props name start end object)
    (error "Already exist layer named %S" name))
  (if (tp-empty-p object)
      (set-text-properties
       start end (append properties (list 'tp-name name))
       object)
    (tp-intervals-map
     (lambda (i-start i-end top belows)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (append (append properties (list 'tp-name name))
                (list 'tp-layers (append (list top) belows)))
        object))
     start end object))
  object)

(defun tp-layer-delete (name start end &optional object)
  "移除 object 的 start 和 end 之间文本的名称为 name 的层"
  ;; 当前顶层放到 tp-layers 开头，properties 设置为当前顶层。
  ;; FIXME: 需要检查 name 是否已经存在，存在则报错
  (declare (indent defun))
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (set-text-properties
      (+ start i-start) (+ start i-end)
      ;; name 是顶层，删除该层后，下一层上移
      (if (equal name (plist-get top 'tp-name))
          (append (nth 0 belows)
                  (list 'tp-layers (seq-drop belows 1)))
        ;; name 不是顶层，直接删除
        (append top
                (list 'tp-layers
                      (-remove (lambda (props)
                                 (equal name (plist-get
                                              props 'tp-name)))
                               belows))))
      object))
   start end object)
  nil)

(defun tp-layer-rotate (start end &optional object)
  "将 start 和 end 之间的顶层文本属性移到最后一层，相当于循环显示不同层。"
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     (set-text-properties
      (+ start i-start) (+ start i-end)
      (append (nth 0 belows)
              (list 'tp-layers
                    (append (seq-drop belows 1)
                            (list top))))
      object))
   start end object)
  nil)

(defun tp-layer-pin (name start end &optional object)
  "将 start 和 end 之间名称为 name 的层移动最上面。"
  (unless (tp-layer-props name start end object)
    (error "Doesn't exist a layer named %S" name))
  (tp-intervals-map
   (lambda (i-start i-end top belows)
     ;; name layer 本身就位于最上层时，无需操作
     (unless (equal (plist-get top 'tp-name) name)
       (set-text-properties
        (+ start i-start) (+ start i-end)
        (let ((new-top
               ;; 获取新的置顶层
               (seq-find (lambda (props)
                           (equal (plist-get props 'tp-name)
                                  name))
                         belows))
              ;; 移除掉被置顶的层
              (rest-belows
               (-remove (lambda (props)
                          (equal (plist-get props 'tp-name)
                                 name))
                        belows)))
          (append new-top
                  (list 'tp-layers
                        (append (list top) rest-belows))))
        object)))
   start end object)
  nil)

;;; propertize string

(defun tp-propertize (string properties &optional layer)
  (declare (indent defun))
  (let ((layer (or layer (org-id-uuid))))
    (tp-layer-push layer
      0 (length string) properties string)
    string))

(defun tp-layer-propertize (string layer)
  (if-let ((layer-info (assoc layer tp-layer-alist)))
      (tp-propertize string
        (cdr layer-info) (car layer-info))
    (error "layer %S doesn't exist!" layer)))

(defun tp-layer-group-propertize (string layer-group)
  (if-let* ((group-info (assoc layer-group tp-layer-groups))
            (layers (cdr group-info)))
      (progn
        (dolist (layer layers)
          (setq string (tp-layer-propertize string layer)))
        string)
    (error "layer group %S doesn't exist!" layer-group)))

;;; search

(defun tp-forward (property &optional value predicate not-current)
  (text-property-search-forward property value predicate not-current))

(defun tp-backward (property &optional value predicate not-current)
  (text-property-search-backward property value predicate not-current))

(defun tp-forward-do (function property &optional
                               value predicate not-current)
  "从当前位置向前搜索，并执行 function，function的参数是 start end value"
  (when-let* ((match (tp-forward property value predicate not-current))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (value (prop-match-value match)))
    (funcall function start end value)))

(defun tp-backward-do (function property &optional
                                value predicate not-current)
  "从当前位置向后搜索，并执行 function，function的参数是 start end value"
  (when-let* ((match (tp-backward property value predicate not-current))
              (start (prop-match-beginning match))
              (end (prop-match-end match))
              (value (prop-match-value match)))
    (funcall function start end value)))

(defun tp-regions-map (function property &optional
                                value predicate collect)
  "对属性匹配的开头和结尾 point 执行 function。collect 为 t 时返回结果列表"
  (save-excursion
    (goto-char (point-min))
    (let ((idx 0) lst)
      (while-let ((match (tp-forward property value predicate))
                  (start (prop-match-beginning match))
                  (end (prop-match-end match)))
        (let ((res (funcall function start end idx)))
          (when collect (push res lst)))
        (cl-incf idx 1))
      (nreverse lst))))

(defun tp-strings-map (function property &optional
                                value predicate collect)
  "对属性匹配的字符串执行 function"
  (tp-regions-map
   (lambda (start end idx)
     (funcall function (buffer-substring start end) idx))
   property value predicate))

(provide 'tp)
