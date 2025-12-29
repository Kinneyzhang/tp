# 组件优化提案文档（Widget Optimization Proposals）

本文档基于当前 `tp-define-widget` 的实现，参考 Vue3 组合式 API 的设计理念，提出一系列优化和扩展方案。

## 当前实现

### 现有特性
- `:props` - 支持属性定义，包含默认值 `(prop . default)`
- `:slot` - 布尔值，`t` 表示支持 slot，`nil` 表示不支持
- `:render` - 渲染函数 `(lambda (props slot) ...)`
- 支持多个 slot 值（字符串和嵌套组件）

---

## 优化提案

### 1. 生命周期钩子（Lifecycle Hooks）

**参考**: Vue3 的 `onMounted`, `onBeforeUpdate`, `onUpdated` 等

**功能描述**:
```elisp
(tp-define-widget my-widget
  :props '(value)
  :slot t
  :on-render (lambda (props slot) ...)  ; 渲染前
  :on-rendered (lambda (result) ...)    ; 渲染后
  :render (lambda (props slot) ...))
```

**作用**: 
- 在渲染前后执行特定逻辑（如日志记录、性能监控）
- 支持渲染结果的后处理

**是否必要**: ⭐⭐ 低优先级
- 目前可通过在 render 函数中处理
- 如果组件变得复杂且需要统一的渲染管道处理，则有价值

---

### 2. 命名插槽（Named Slots）

**参考**: Vue3 的 `<slot name="header">`, `v-slot:header`

**功能描述**:
```elisp
(tp-define-widget card
  :props '(title)
  :slots '(header content footer)  ; 定义多个命名插槽
  :render (lambda (props slots)
            (concat (plist-get slots :header)
                    "\n"
                    (plist-get slots :content)
                    "\n"
                    (plist-get slots :footer))))

;; 使用
(tp-widget-parse
 '(card :title "My Card"
        :header-slot "Header Content"
        :content-slot "Main Content"
        :footer-slot "Footer"))
```

**作用**:
- 支持更灵活的内容分发
- 组件可以有多个内容区域

**是否必要**: ⭐⭐⭐ 中等优先级
- 当组件需要在不同位置插入内容时非常有用
- 类似 Vue/React 的 slot 概念

---

### 3. 作用域插槽（Scoped Slots）

**参考**: Vue3 的作用域插槽，允许父组件访问子组件数据

**功能描述**:
```elisp
(tp-define-widget list-item
  :props '(items)
  :slot t  ; slot 可以是函数
  :render (lambda (props slot-fn)
            (mapconcat
             (lambda (item)
               ;; slot-fn 可以访问当前 item
               (funcall slot-fn item))
             (plist-get props :items)
             "\n")))

;; 使用
(tp-widget-parse
 '(list-item :items ("apple" "banana" "orange")
             (lambda (item)
               (tp-set item 'face 'bold))))
```

**作用**:
- 父组件可以访问子组件的内部数据
- 更灵活的渲染控制

**是否必要**: ⭐⭐ 低优先级
- 增加复杂度
- Emacs Lisp 的闭包可以部分实现此功能

---

### 4. 组件继承/组合（Component Inheritance/Composition）

**参考**: Vue3 的 `mixins`, `extends`

**功能描述**:
```elisp
(tp-define-widget base-button
  :props '((type . "default"))
  :slot t
  :render (lambda (props slot)
            (tp-set slot 'face 'button)))

(tp-define-widget primary-button
  :extends 'base-button
  :props '((type . "primary"))  ; 覆盖默认值
  :render (lambda (props slot parent-render)
            (let ((result (funcall parent-render props slot)))
              (tp-add result 'face '(:foreground "blue")))))
```

**作用**:
- 代码复用
- 创建组件变体

**是否必要**: ⭐⭐⭐ 中等优先级
- 对于创建组件库非常有用
- 避免重复代码

---

### 5. 响应式状态（Reactive State）

**参考**: Vue3 的 `ref`, `reactive`

**功能描述**:
```elisp
(tp-define-widget counter
  :state '((count . 0))  ; 组件内部状态
  :slot t
  :render (lambda (props state slot)
            (let ((count (plist-get state :count)))
              (format "Count: %d %s" count slot))))

;; 状态更新时自动重新渲染
(tp-widget-update 'counter :count 5)
```

**作用**:
- 组件拥有自己的内部状态
- 与现有的响应式系统（`$variable`）集成

**是否必要**: ⭐⭐⭐⭐ 高优先级
- 对于交互式组件非常重要
- 可以利用现有的 tp reactive 系统

---

### 6. 事件系统（Event System）

**参考**: Vue3 的 `$emit`, `v-on`

**功能描述**:
```elisp
(tp-define-widget button
  :props '(label)
  :emits '(click hover)  ; 声明可触发的事件
  :slot t
  :render (lambda (props slot emit)
            (tp-add slot
                    'mouse-1 (lambda () (funcall emit :click))
                    'pointer 'hand)))

;; 使用
(tp-widget-parse
 '(button :label "Click Me"
          :on-click (lambda () (message "Clicked!"))
          "Submit"))
```

**作用**:
- 组件间通信
- 事件驱动的交互

**是否必要**: ⭐⭐⭐⭐ 高优先级
- 对于交互式 UI 必要
- 支持按钮、链接等组件的回调

---

### 7. 依赖注入（Provide/Inject）

**参考**: Vue3 的 `provide`, `inject`

**功能描述**:
```elisp
(tp-define-widget theme-provider
  :provide '(theme)  ; 向下提供
  :props '((theme . "dark"))
  :slot t
  :render (lambda (props slot) slot))

(tp-define-widget themed-text
  :inject '(theme)  ; 从上层获取
  :slot t
  :render (lambda (props slot injected)
            (let ((theme (plist-get injected :theme)))
              (tp-set slot 'face
                      (if (equal theme "dark")
                          '(:foreground "white" :background "black")
                        '(:foreground "black" :background "white"))))))
```

**作用**:
- 跨层级的数据传递
- 主题、配置等全局状态的共享

**是否必要**: ⭐⭐ 低优先级
- Emacs 可以使用动态绑定实现
- 如果组件树很深，可能有价值

---

### 8. 条件渲染辅助（Conditional Rendering Helpers）

**参考**: Vue3 的 `v-if`, `v-show`, `v-for`

**功能描述**:
```elisp
;; 辅助函数
(defun tp-if (condition then &optional else)
  "条件渲染"
  (if condition then (or else "")))

(defun tp-for (items template)
  "列表渲染"
  (mapconcat template items ""))

;; 使用
(tp-define-widget user-list
  :props '(users show-email)
  :render (lambda (props _slot)
            (tp-for (plist-get props :users)
                    (lambda (user)
                      (concat (plist-get user :name)
                              (tp-if (plist-get props :show-email)
                                     (format " <%s>" (plist-get user :email))))))))
```

**作用**:
- 简化常见的渲染模式
- 提高代码可读性

**是否必要**: ⭐⭐⭐ 中等优先级
- 作为辅助函数很有用
- 可以独立于核心组件系统实现

---

### 9. 插槽类型验证（Slot Type Validation）

**功能描述**:
```elisp
(tp-define-widget container
  :slot 'string  ; 只接受字符串
  ;; 或
  :slot '(string widget)  ; 接受字符串和组件
  ;; 或
  :slot '(widget button text)  ; 只接受特定组件
  :render ...)
```

**作用**:
- 类型安全
- 更好的错误提示

**是否必要**: ⭐⭐ 低优先级
- 开发时有用
- 可能影响性能

---

### 10. 异步组件（Async Components）

**参考**: Vue3 的 `defineAsyncComponent`

**功能描述**:
```elisp
(tp-define-async-widget remote-content
  :props '(url)
  :loading "Loading..."
  :error "Failed to load"
  :render (lambda (props slot)
            (url-retrieve-synchronously (plist-get props :url))
            ...))
```

**作用**:
- 支持异步数据加载
- 加载和错误状态处理

**是否必要**: ⭐ 最低优先级
- Emacs 的异步模型与 Web 不同
- 可能需要使用 `url-retrieve` 和回调

---

## 优先级总结

| 优化项 | 优先级 | 复杂度 | 价值 |
|-------|-------|-------|-----|
| 响应式状态 | ⭐⭐⭐⭐ | 中 | 高 |
| 事件系统 | ⭐⭐⭐⭐ | 中 | 高 |
| 命名插槽 | ⭐⭐⭐ | 低 | 中 |
| 组件继承 | ⭐⭐⭐ | 中 | 中 |
| 条件渲染辅助 | ⭐⭐⭐ | 低 | 中 |
| 生命周期钩子 | ⭐⭐ | 低 | 低 |
| 作用域插槽 | ⭐⭐ | 高 | 中 |
| 依赖注入 | ⭐⭐ | 中 | 低 |
| 类型验证 | ⭐⭐ | 低 | 低 |
| 异步组件 | ⭐ | 高 | 低 |

---

## 建议实施顺序

1. **第一阶段**: 事件系统 + 响应式状态集成
   - 这两个特性对交互式组件最重要
   - 可以利用现有的 tp reactive 系统

2. **第二阶段**: 命名插槽 + 条件渲染辅助
   - 提升组件的灵活性和开发体验

3. **第三阶段**: 组件继承 + 生命周期钩子
   - 对于构建组件库有价值

4. **第四阶段**: 其他高级特性
   - 根据实际需求决定

---

## 示例：完整的组件定义（理想状态）

```elisp
(tp-define-widget button
  ;; 属性定义
  :props '(action
           (type . "default")
           (size . "medium")
           (disabled . nil))
  
  ;; 状态（响应式）
  :state '((loading . nil)
           (focused . nil))
  
  ;; 支持插槽
  :slot t
  
  ;; 可触发的事件
  :emits '(click focus blur)
  
  ;; 生命周期
  :on-render (lambda (props) 
               (unless (plist-get props :disabled)
                 (message "Button rendering...")))
  
  ;; 渲染函数
  :render (lambda (props state slot emit)
            (let* ((type (plist-get props :type))
                   (size (plist-get props :size))
                   (disabled (plist-get props :disabled))
                   (loading (plist-get state :loading))
                   (content (if loading "Loading..." slot))
                   (face (cond
                          (disabled '(:foreground "gray"))
                          ((equal type "primary") '(:foreground "white" :background "blue"))
                          ((equal type "danger") '(:foreground "white" :background "red"))
                          (t '(:foreground "black" :background "#eee")))))
              (tp-add content
                      'face face
                      'mouse-1 (unless disabled
                                 (lambda ()
                                   (funcall emit :click)
                                   (funcall (plist-get props :action))))
                      'pointer (unless disabled 'hand)))))
```

---

## 结论

当前的组件系统已经具备基本功能。上述优化提案可以根据实际使用场景和需求逐步实施。建议从**事件系统**和**响应式状态集成**开始，因为这两个特性对于构建交互式 UI 组件最为重要。
