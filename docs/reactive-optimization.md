# tp.el 响应式系统优化文档

本文档基于 [twidget](https://github.com/Kinneyzhang/twidget.git) 项目的实践经验，对 tp.el 的响应式系统进行了优化和增强。

## 优化建议评估

以下是针对 tp.el 响应式系统的六项优化建议的评估和实现情况：

### 1. 细粒度响应式更新（Granular Reactive Updates）

**建议**：支持区域内的部分更新，只更新响应式部分，保留周围文本属性。

**评估**：已经实现。tp.el 通过 `tp-intervals-map` 和基于区间的更新机制，已经支持细粒度的属性更新。更新只影响具有特定 `tp-name` 的区域。

### 2. 响应式符号清理（Reactive Symbol Cleanup）✅ 已实现

**建议**：当 widget 销毁时，添加注销响应式符号的机制。

**评估**：已经实现。`tp--unregister-reactive-deps` 函数负责清理：
- 当层被重新定义时自动调用
- 当层被取消定义（`tp-undefine-layer`）时自动调用
- 清理变量监听器、计算属性和数据变量

**关键函数**：
- `tp--unregister-reactive-deps`
- `tp--unregister-layer-watchers`
- `tp--unregister-layer-computed`
- `tp--unregister-layer-data`

### 3. 作用域响应式（Scoped Reactivity）✅ 已实现

**建议**：为响应式变量添加实例/上下文作用域。

**评估**：已经实现。`where` 参数在以下函数中支持缓冲区局部更新：
- `tp--update-layer-regions`
- `tp--update-reactive-text`

当使用 `setq-local` 时，更新只影响特定缓冲区。

### 4. 批量更新（Batched Updates）🆕 新增

**建议**：当多个响应式值同时变化时，批量处理更新以避免冗余的缓冲区修改。

**实现**：新增 `tp-with-batch-updates` 宏：

```elisp
;; 使用批量更新
(tp-with-batch-updates
  (setq my-color "red")
  (setq my-size 14)
  (setq my-text "Hello"))
;; 所有更新在批量结束后一次性应用到缓冲区
```

**关键函数和变量**：
- `tp-with-batch-updates` - 批量更新宏
- `tp--batch-update-active` - 标记是否在批量更新中
- `tp--batch-update-pending` - 待处理的更新列表
- `tp--flush-batch-updates` - 应用所有待处理更新

### 5. 值转换（Value Transformation）🆕 新增

**建议**：允许注册转换函数，在 tp-text 更新时运行。

**实现**：新增 `:transform` 选项：

```elisp
;; 定义带转换的层
(define-tp currency-display ()
  :props '(face bold tp-text $amount)
  :data '((amount . "100"))
  :transform (lambda (text)
               (format "$%s.00" text)))

;; 使用后，100 会显示为 $100.00
```

**关键函数和变量**：
- `tp-layer-transforms` - 存储层转换函数
- 转换在 `tp--handle-tp-text-property` 和 `tp--update-reactive-text` 中应用

### 6. 调试模式（Debug Mode）🆕 新增

**建议**：添加调试模式以追踪响应式更新。

**实现**：新增调试功能：

```elisp
;; 启用调试模式
(setq tp-debug-mode t)

;; 同时在 minibuffer 显示调试信息
(setq tp-debug-echo t)

;; 查看调试日志
(tp-debug-show)

;; 清除调试日志
(tp-debug-clear)
```

**关键函数和变量**：
- `tp-debug-mode` - 启用/禁用调试模式
- `tp-debug-echo` - 是否在 minibuffer 显示调试信息
- `tp-debug-log` - 记录调试信息
- `tp-debug-show` - 显示调试缓冲区
- `tp-debug-clear` - 清除调试日志

调试日志包含：
- 变量变化通知（旧值 → 新值）
- 层更新追踪
- 批量更新开始/结束
- 转换应用信息

## 新增功能详解

### 批量更新 (tp-with-batch-updates)

当需要同时修改多个响应式变量时，使用批量更新可以避免多次缓冲区更新：

```elisp
(define-tp themed-text ()
  :props '(face (:foreground $fg-color :background $bg-color))
  :data '((fg-color . "white") (bg-color . "black")))

(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 12 'themed-text)
  
  ;; 不使用批量更新：每个 setq 都会触发一次缓冲区更新
  (setq fg-color "yellow")  ; 第一次更新
  (setq bg-color "navy")    ; 第二次更新
  
  ;; 使用批量更新：所有变化在结束时一次性应用
  (tp-with-batch-updates
    (setq fg-color "red")
    (setq bg-color "blue")))  ; 只更新一次
```

### 值转换 (:transform)

转换函数允许在显示前处理 tp-text 的值：

```elisp
;; 数字格式化
(define-tp price-display ()
  :props '(tp-text $price)
  :data '((price . "99.9"))
  :transform (lambda (text)
               (format "$%.2f" (string-to-number text))))

;; 日期格式化
(define-tp date-display ()
  :props '(tp-text $timestamp)
  :data '((timestamp . "1703865600"))
  :transform (lambda (text)
               (format-time-string "%Y-%m-%d" 
                 (seconds-to-time (string-to-number text)))))

;; 大写转换
(define-tp uppercase-text ()
  :props '(tp-text $content)
  :data '((content . "hello"))
  :transform #'upcase)
```

### 调试模式

调试模式帮助开发者理解响应式更新流程：

```elisp
;; 启用完整调试
(setq tp-debug-mode t)
(setq tp-debug-echo t)

;; 定义和使用响应式层
(define-tp test-layer ()
  :props '(face (:foreground $my-color))
  :data '((my-color . "red")))

(with-temp-buffer
  (insert "Test")
  (tp-set 1 5 'test-layer)
  (setq my-color "blue"))

;; 调试输出示例：
;; [12:34:56.789] Variable my-color changed: "red" -> "blue" (where: global)
;; [12:34:56.790]   Updating layer test-layer (tp-text affected: no)
```

## 架构说明

这些优化遵循 tp.el 的分层架构原则：

1. **调试模式** - 作为基础工具层功能
2. **批量更新** - 在响应式系统层实现
3. **值转换** - 在层定义和响应式文本处理中实现

所有新功能都与现有的响应式系统无缝集成，不破坏现有 API。

## 相关函数一览

| 函数/变量 | 描述 |
|----------|------|
| `tp-debug-mode` | 启用调试模式 |
| `tp-debug-echo` | 启用 minibuffer 调试输出 |
| `tp-debug-log` | 记录调试信息 |
| `tp-debug-show` | 显示调试缓冲区 |
| `tp-debug-clear` | 清除调试日志 |
| `tp-with-batch-updates` | 批量更新宏 |
| `tp-layer-transforms` | 层转换函数存储 |
| `:transform` | 层定义中的转换选项 |
