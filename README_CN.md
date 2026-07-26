# tp.el - Emacs 文本属性操作库

<p align="center">
  <strong>一个功能强大的文本属性操作库，具有创新的属性层系统</strong>
</p>

<p align="center">
  <a href="#功能特性">功能特性</a> •
  <a href="#安装">安装</a> •
  <a href="#快速开始">快速开始</a> •
  <a href="#api-参考">API 参考</a> •
  <a href="#属性层系统">属性层系统</a> •
  <a href="#响应式文本属性">响应式文本属性</a>
</p>

---

## 目录

- [快速开始](#快速开始)
- [概述](#概述)
  - [核心创新](#核心创新)
- [功能特性](#功能特性)
  - [统一的 API 参数规范](#统一的-api-参数规范)
  - [三种属性操作语义](#三种属性操作语义)
  - [子属性的精细操作](#子属性的精细操作)
  - [创新的属性层系统](#创新的属性层系统)
  - [模式匹配与批量操作](#模式匹配与批量操作)
  - [响应式文本属性](#-响应式文本属性)
  - [增强的搜索与导航](#增强的搜索与导航)
- [系统要求](#系统要求)
- [安装](#安装)
- [API 参考](#api-参考)
  - [API 快速参考](#api-快速参考)
  - [核心属性函数](#核心属性函数)
    - [tp-set](#tp-set---设置文本属性)
    - [tp-reset](#tp-reset---替换所有属性)
    - [tp-add](#tp-add---添加合并属性)
    - [tp-get](#tp-get---获取属性值)
    - [tp-at](#tp-at---获取位置属性)
    - [tp-member](#tp-member---判断位置属性是否存在)
    - [tp-remove](#tp-remove---移除属性)
    - [tp-clear](#tp-clear---清除所有属性)
  - [模式匹配函数](#模式匹配函数)
    - [tp-match-set](#tp-match-set---匹配字符串)
    - [tp-match-reset](#tp-match-reset---匹配并重置)
    - [tp-match-add](#tp-match-add---匹配并添加)
    - [tp-regexp-set](#tp-regexp-set---匹配正则表达式)
    - [tp-regexp-reset](#tp-regexp-reset---正则匹配并重置)
    - [tp-regexp-add](#tp-regexp-add---正则匹配并添加)
  - [搜索和导航函数](#搜索和导航函数)
    - [tp-search-forward / tp-search-backward](#tp-search-forward--tp-search-backward)
    - [tp-forward / tp-backward](#tp-forward--tp-backward)
    - [tp-forward-do / tp-backward-do](#tp-forward-do--tp-backward-do)
    - [tp-search](#tp-search---搜索所有匹配)
    - [tp-search-map](#tp-search-map---对匹配文本应用函数)
- [属性层系统](#属性层系统)
  - [自定义文本属性](#自定义文本属性)
  - [文本属性层](#文本属性层)
  - [属性层概念](#属性层概念)
  - [属性层定义](#属性层定义)
    - [define-tp / define-tps](#define-tp--define-tps---定义自定义文本属性)
    - [tp-layer-props / tp-group-props](#tp-layer-props--tp-group-props)
    - [tp-layer-props-with-args / tp-group-props-with-args / tp-layer-arglist](#tp-layer-props-with-args--tp-group-props-with-args--tp-layer-arglist)
    - [tp-describe-layer](#tp-describe-layer---描述属性层)
    - [tp-undefine-layer / tp-undefine-group](#tp-undefine-layer--tp-undefine-group)
    - [tp-layer-reset](#tp-layer-reset)
    - [tp-reactive-reset](#tp-reactive-reset)
  - [属性层放置](#属性层放置)
    - [tp-put-layer](#tp-put-layer---在指定位置设置属性层)
    - [tp-push-layer](#tp-push-layer---推送属性层到顶部)
  - [属性层删除](#属性层删除)
    - [tp-delete-layer](#tp-delete-layer---按名称索引删除属性层)
    - [tp-pop-layer](#tp-pop-layer---弹出顶层)
  - [属性层移动](#属性层移动)
    - [tp-move-layer](#tp-move-layer---移动属性层到指定位置)
    - [tp-raise-layer](#tp-raise-layer---上移下移属性层)
    - [tp-lower-layer](#tp-lower-layer---tp-raise-layer-的镜像)
    - [tp-rotate-layer](#tp-rotate-layer---轮换属性层)
    - [tp-pin-layer](#tp-pin-layer---将属性层置顶)
    - [tp-switch-layer](#tp-switch-layer---交换两个属性层)
  - [属性层可见性](#属性层可见性)
    - [tp-hide-layer / tp-show-layer](#tp-hide-layer--tp-show-layer---隐藏与显示属性层)
  - [属性层合并](#属性层合并)
    - [tp-merge-layers](#tp-merge-layers---合并多个属性层)
    - [tp-flatten-layers](#tp-flatten-layers---扁平化所有属性层)
  - [属性层查询函数](#属性层查询函数)
    - [tp-layer-list](#tp-layer-list---列出所有属性层)
    - [tp-layer-count](#tp-layer-count)
    - [tp-layer-exists-p](#tp-layer-exists-p)
    - [tp-layer-top](#tp-layer-top)
    - [tp-layer-stack-at](#tp-layer-stack-at---获取某位置的完整层栈)
    - [tp-add-to-layers](#tp-add-to-layers---向特定属性层添加属性)
    - [tp-add-to-all-layers](#tp-add-to-all-layers---向所有属性层添加属性)
  - [实用工具函数](#实用工具函数)
    - [tp-intervals](#tp-intervals---获取文本属性区间)
    - [tp-intervals-map](#tp-intervals-map---对区间应用函数)
    - [tp-plist](#tp-plist---获取区域中的所有属性)
    - [tp-empty-p](#tp-empty-p---检查对象是否有属性)
    - [tp-region-layer-props](#tp-region-layer-props---获取区域中的层属性)
    - [tp-with-current-buffer / tp-pop-to-buffer / tp-switch-to-buffer](#tp-with-current-buffer--tp-pop-to-buffer--tp-switch-to-buffer)
  - [调色板系统](#调色板系统)
- [响应式文本属性](#响应式文本属性)
  - [核心概念](#核心概念)
  - [工作原理](#工作原理)
  - [定义响应式层](#定义响应式层)
  - [:data - 附加响应式状态](#data---附加响应式状态)
  - [:compute - 计算属性](#compute---计算属性)
  - [:watch - 副作用回调](#watch---副作用回调)
  - [:transform - 值转换](#transform---值转换)
  - [匿名响应式层](#匿名响应式层)
  - [API 中的层名解析](#api-中的层名解析)
  - [响应式层组](#响应式层组)
  - [批量更新](#批量更新)
  - [层-缓冲区注册表与生命周期](#层-缓冲区注册表与生命周期)
  - [调试模式](#调试模式)
  - [重置响应式状态](#重置响应式状态)
  - [完整示例：主题感知文本](#完整示例主题感知文本)
- [实用示例](#实用示例)
  - [多属性层语法高亮](#多属性层语法高亮)
  - [状态指示器](#状态指示器)
  - [临时高亮](#临时高亮)
- [许可证](#许可证)
- [贡献](#贡献)

---

## 快速开始

```elisp
;; 安装：克隆仓库，将其加入 load-path，然后 require
(add-to-list 'load-path "/path/to/tp")
(require 'tp)

;; 用统一的 API 设置属性（返回一个新的带属性字符串）
(tp-set "hello" 'face 'bold)
;; => #("hello" 0 5 (face bold))

;; 在缓冲区区域上堆叠属性层
(define-tp spotlight () '(face (:background "yellow")))
(with-temp-buffer
  (insert "Hello World")
  (tp-push-layer 1 6 'spotlight)
  (tp-layer-top 1 6))
;; => spotlight

;; 响应式：文本属性跟随变量变化
(defvar accent-color "red")
(define-tp accent ()
  :props '(face (:foreground $accent-color)))
(with-temp-buffer
  (insert "Hello")
  (tp-push-layer 1 6 'accent)
  (setq accent-color "blue")   ; 文本自动更新！
  (tp-at 1 'face))
;; => (:foreground "blue")
```

---

## 概述

**tp.el** 是一个全面增强 Emacs 文本属性操作的库。它不仅仅是对原生文本属性 API（如 `put-text-property`、`get-text-property`）的简单封装，更提供了许多**原生函数所不具备的功能拓展**。tp.el 在以下方面进行了创新：

自 0.2.0 起，本库被组织为一组分层模块（`tp-core`、`tp-reactive`、`tp-layer`、`tp-ops`、`tp-search`、`tp-render`、`tp-stack`、`tp-palette`、`tp-builtins`），由伞形文件 `tp.el` 统一加载 — `(require 'tp)` 仍会加载全部模块，对用户没有任何变化。模块一览见[安装](#安装)。

### 核心创新

1. **统一的 API 参数规范**：所有函数支持多种灵活的调用方式，同时适用于字符串和缓冲区
2. **子属性的精细操作**：支持嵌套属性的路径式访问、修改和深度合并
3. **创新的属性层系统**：在同一文本区域上堆叠、管理多组属性，实现属性的分层控制
4. **🆕 响应式文本属性**：当变量值改变时自动更新文本属性 - 受现代响应式 UI 框架启发的突破性功能
5. **模式匹配批量操作**：通过字符串或正则表达式批量应用属性
6. **增强的搜索导航**：丰富的属性搜索和遍历功能

## 功能特性

### 统一的 API 参数规范

原生 Emacs API 针对字符串和缓冲区有不同的函数和参数顺序，tp.el 统一了这一切：

- ✅ **三种调用约定**：所有核心函数（`tp-set`、`tp-get`、`tp-remove` 等）支持三种灵活的调用方式：
  ```elisp
  ;; 1. 当前缓冲区
  (tp-set START END '(face bold))
  ;; 2. 指定缓冲区或字符串
  (tp-set START END '(face bold) OBJECT)
  ;; 3. 整个字符串（平铺属性或层名称）
  (tp-set STRING 'face 'bold 'help-echo "tip")
  (tp-set STRING 'layer-name)
  ```
- ✅ **统一对象支持**：同一个函数同时支持字符串和缓冲区，无需记忆不同的 API

**只需记住一条规则**：当第一个参数是**字符串**时，调用作用于整个字符串；
当第一个参数是**数字**时，调用作用于 OBJECT 的 `[START, END)` 区域 —— 而
OBJECT 总是位于最后（nil 表示当前缓冲区）。所有核心函数和层栈函数都遵循
这条规则。

匹配/搜索家族（`tp-match-*`、`tp-regexp-*`、`tp-search-map`、
`tp-forward-do`/`tp-backward-do`）刻意采用了**第二种约定**：PATTERN
（或 FUNCTION）和 PLIST 在前，然后是 OBJECT，最后才是可选的 START/END
边界。对这些函数来说，作用于整个对象才是常见用法，因此 OBJECT 位于范围
参数之前而不是之后。

**返回值约定**（自 0.3.0 起）：

| 函数家族 | 返回值 |
|---|---|
| `tp-set` / `tp-reset` / `tp-add` | 缓冲区/区域形式返回 `(START . END)`；整字符串形式返回一个**新**字符串 |
| `tp-remove` | 缓冲区形式返回 nil；整字符串形式返回一个**新**字符串 |
| `tp-clear` | nil |
| `tp-match-*` / `tp-regexp-*` | 缓冲区返回 `(START . END)` 匹配列表；字符串返回一个**新**字符串 |
| 栈修改函数（delete/pop/move/raise/lower/rotate/pin/switch/hide/show/merge/flatten） | 被修改的属性区段数量（0 = 没有匹配任何层） |
| `tp-put-layer` / `tp-push-layer` | 给定 OBJECT 时返回 OBJECT（字符串形式返回该字符串本身），否则返回 `(START . END)` |
| `tp-add-to-layers` / `tp-add-to-all-layers` | 字符串形式返回该字符串本身（**就地**修改）；缓冲区返回 nil |

**命名空间地图**：接受*层名*参数的 `tp-layer-NAME` 函数
（`tp-layer-props`、`tp-layer-arglist` 等）查询的是层**注册表**（层定
义）；接受*位置*参数的函数 —— START END（`tp-layer-list`、
`tp-layer-count`、`tp-layer-top` 等）或单个 POS（`tp-layer-stack-at`）
—— 查询的是实际文本上的层**栈**。

**命名约定**：`tp-define-layer` / `tp-define-group` /
`tp-define-palette` 是今后符合前缀规范的规范名称（可通过
`C-h f tp-...` 发现）；`define-tp` / `define-tps` / `define-tp-group` /
`define-tp-palette` 是永久别名，永远不会被移除（本 README 的示例仍使用
历史名称）。`tp-search-forward` / `tp-search-backward` 自 0.3.0 起已废
弃 —— 参见[搜索和导航函数](#tp-search-forward--tp-search-backward)。

### 三种属性操作语义

原生 API 只有简单的设置和获取，tp.el 提供了三种清晰的操作语义：

- ✅ **`tp-reset`**：完全替换 - 清除所有现有属性，设置新属性
- ✅ **`tp-set`**：部分替换 - 只替换指定属性，保留其他属性
- ✅ **`tp-add`**：深度合并 - 智能合并嵌套属性，而非简单覆盖

```elisp
;; 深度合并示例
(tp-set 1 10 '(face (:foreground "red")))
(tp-add 1 10 '(face (:background "blue")))
;; 结果: face 是 (:foreground "red" :background "blue")
;; 原生 API 会完全覆盖，而 tp-add 会智能合并
```

### 子属性的精细操作

**这是原生 API 完全不具备的功能**。tp.el 支持对嵌套属性进行精细的读取、修改和删除：

- ✅ **路径式访问**：通过路径语法访问深层嵌套的属性值
  ```elisp
  ;; 获取嵌套属性（tp-get 返回 (START END VALUE) 区间列表）
  (tp-get str 'face :underline :style)  ; => ((0 5 wave))
  (tp-at 5 '(face :box :color))         ; => "blue"
  
  ;; 获取多个嵌套键
  (tp-get str 'face :underline '(:color :style))
  ;; => ((0 5 (:color "green" :style wave)))
  ```
- ✅ **子属性删除**：精确移除嵌套属性中的特定键
  ```elisp
  ;; 只删除 :underline 中的 :style，保留 :color
  (tp-remove 1 10 '(face :underline :style))
  ```
- ✅ **深度合并**：`tp-add` 递归合并嵌套的 plist 结构
- ✅ **Face 智能合并**：符号 face 自动前置到 face 列表，plist face 深度合并
- ✅ **单次设置中的重复属性自动合并**：在一次 `tp-set`/`tp-add`/`tp-reset` 调用中，如果同一属性（如 `face`）被指定多次，它们会自动合并

```elisp
;; 单次调用中合并多个 face
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; 结果: face 是 ((:foreground "red") (:background "green") bold)
;; （各条目堆叠为一个 face 列表，最新的在前）

;; 同一子属性后面的覆盖前面的
(tp-set "emacs"
        'face '(:foreground "red")
        'face '(:foreground "yellow"))
;; 结果: foreground 是 "yellow"

;; 与 tp-palette 层配合使用
(tp-set "emacs"
        'tp-palette 'info
        'face '(:foreground "red"))
;; 结果: tp-palette 的 face 与 (:foreground "red") 合并
```

### 创新的属性层系统

**这是 tp.el 最具创新性的功能**，原生 Emacs 完全不支持。属性层系统允许在同一文本区域上堆叠多组属性：

- ✅ **属性层栈概念**：多个属性层像栈一样堆叠，只有顶层可见，下层被保留
- ✅ **属性层定义与复用**：通过 `define-tp` 定义可复用的自定义文本属性和属性层
- ✅ **丰富的属性层操作**：
  - 放置：`tp-put-layer`（指定位置）、`tp-push-layer`（顶部）
  - 删除：`tp-delete-layer`（按名称/索引）、`tp-pop-layer`（顶层）
  - 移动：`tp-raise-layer` / `tp-lower-layer`（上下移动）、`tp-rotate-layer`（轮换）、`tp-pin-layer`（一次性置顶）、`tp-switch-layer`（交换）
  - 可见性：`tp-hide-layer` / `tp-show-layer`（隐藏属性层而不移除它）
  - 合并：`tp-merge-layers`（合并指定层）、`tp-flatten-layers`（扁平化所有层）
- ✅ **属性层查询**：`tp-layer-list`、`tp-layer-count`、`tp-layer-exists-p`、`tp-layer-top`、`tp-layer-stack-at`

```elisp
;; 属性层使用示例
(define-tp highlight () '(face (:background "yellow")))
(define-tp error () '(face (:foreground "red")))

;; 堆叠多个属性层
(tp-push-layer 1 10 'highlight)
(tp-push-layer 1 10 'error)  ; error 现在可见

;; 轮换显示
(tp-rotate-layer 1 10)  ; highlight 现在可见
```

### 模式匹配与批量操作

原生 API 需要手动搜索和循环，tp.el 提供了便捷的模式匹配功能：

- ✅ **字符串匹配**：`tp-match-set`、`tp-match-reset`、`tp-match-add`
- ✅ **正则匹配**：`tp-regexp-set`、`tp-regexp-reset`、`tp-regexp-add`
- ✅ **三种语义变体**：每种匹配都支持 set/reset/add 三种操作语义

```elisp
;; 高亮所有 TODO
(tp-match-set "TODO" '(face warning))

;; 正则匹配所有数字
(tp-regexp-set "[0-9]+" '(face font-lock-number-face))

;; 深度合并方式添加属性
(tp-match-add "TODO" '(face (:underline t)))
```

### 🆕 响应式文本属性

**这是 tp.el 最具创新性的新功能** - 响应式文本属性会在变量值改变时自动更新。受现代响应式 UI 框架（如 Vue.js）启发，这个功能为 Emacs 文本属性带来了响应式编程范式：

- ✅ **响应式变量**：在属性定义中使用 `$` 前缀的符号（如 `$my-color`），它们会自动解析为变量值
- ✅ **自动更新**：当响应式变量改变时，所有使用该变量的文本区域会自动更新
- ✅ **:data 附加状态**：定义不直接用于属性但可以触发更新的额外响应式变量
- ✅ **:compute 计算属性**：创建从其他响应式变量派生值的计算属性（类似 Vue 的 computed）
- ✅ **:watch 副作用监听**：当响应式变量改变时执行回调函数（类似 Vue 的 watch）
- ✅ **定向更新（0.3.0）**：层→缓冲区注册表使更新只访问展示受影响层的缓冲区；`tp-text` 重渲染只编辑差异区段（point 和标记保持原位）；`tp-reactive-track-buffer` / `tp-gc-anonymous-layers` 管理层的生命周期 —— 参见[层-缓冲区注册表与生命周期](#层-缓冲区注册表与生命周期)

```elisp
;; 定义一个带响应式属性的层
(defvar my-color "red")  ;; 响应式变量

;; 使用 define-tp 定义自定义文本属性（推荐方式）
(define-tp my-highlight ()
  '(face (:foreground $my-color)))

;; 应用该层
(tp-push-layer 1 10 'my-highlight)

;; 之后只需改变变量 - 文本自动更新！
(setq my-color "blue")  ;; 所有 my-highlight 层的文本自动变成蓝色！

;; 使用 :data、:compute、:watch 的高级示例
;; （注意：参数列表 () 是必需的，且各关键字的值必须加引号）
(define-tp full-name-layer ()
  :props '(help-echo $full-name face (:foreground $name-color))
  :data '((first-name . "John") (last-name . "Doe") (name-color . "purple"))
  :compute '((full-name (lambda () (concat first-name " " last-name))))
  :watch '((first-name (lambda (new old layer)
                         (message "名字从 %s 改为 %s" old new)))))
```

### 增强的搜索与导航

- ✅ **范围搜索**：`tp-search` 返回所有匹配区间的列表
- ✅ **N次搜索**：`tp-forward`/`tp-backward` 支持向前/向后搜索N次，并支持可选的 PREDICATE 匹配和 NOT-CURRENT
- ✅ **搜索并执行**：`tp-forward-do`/`tp-backward-do` 搜索 N 次并在第 N 个匹配处应用函数
- ✅ **批量转换**：`tp-search-map` 对所有匹配应用转换函数

```elisp
;; 搜索所有标记
(tp-search my-string 'marker)  ; => ((0 5 t) (12 17 t))

;; 将所有标记文本转为大写
(tp-search-map #'upcase 'marker nil my-string)
```

## 系统要求

- **Emacs 28.1+**（使用 `object-intervals` 函数）
- **dash.el 2.19.1+**（列表操作工具库）

## 安装

本库由 `tp-*.el` 模块家族加上伞形文件 `tp.el` 组成。安装即把目录加入
`load-path` 并 require 伞形文件，它会加载全部模块：

```elisp
;; 添加到 load-path
(add-to-list 'load-path "/path/to/tp")
(require 'tp)
```

或使用 `use-package`：

```elisp
(use-package tp
  :load-path "/path/to/tp")
```

各模块及其职责：

| 模块 | 职责 |
|---|---|
| `tp-core.el` | 区间、plist/face 合并引擎、调试日志、`$var` 工具 |
| `tp-reactive.el` | 响应式依赖注册表、变量监视器、批量更新队列 |
| `tp-layer.el` | `define-tp` / `define-tps`、属性层注册表与解析 |
| `tp-ops.el` | `tp-set` / `tp-reset` / `tp-add` / `tp-get` / `tp-at` / `tp-remove` / `tp-clear` |
| `tp-search.el` | `tp-match-*`、`tp-regexp-*`、`tp-search`、导航 |
| `tp-render.el` | 响应式重渲染引擎 |
| `tp-stack.el` | 属性层栈操作（push/pop/移动/合并/扁平化/...） |
| `tp-palette.el` | 亮色/暗色调色板数据 |
| `tp-builtins.el` | 内置属性层、调色板画廊、display-buffer 辅助工具 |

项目附带 `Makefile`：`make test` 运行所有 ERT 测试套件，`make doctest`
将 README 示例作为可执行测试运行（`tp-doctest.el`），`make compile`
字节编译各模块，`make clean` 清除编译产物。

---

## API 参考

### API 快速参考

tp.el 所有函数按类别组织的完整概览：

#### 核心属性函数
| 函数 | 描述 |
|------|------|
| [`tp-set`](#tp-set---设置文本属性) | 设置文本属性（仅替换指定属性） |
| [`tp-reset`](#tp-reset---替换所有属性) | 替换所有文本属性 |
| [`tp-add`](#tp-add---添加合并属性) | 添加/合并属性，支持深度合并 |
| [`tp-get`](#tp-get---获取属性值) | 从范围或字符串获取属性值 |
| [`tp-at`](#tp-at---获取位置属性) | 获取单个位置的属性值 |
| [`tp-member`](#tp-member---判断位置属性是否存在) | 类似 `tp-at`，但能区分"存在且值为 nil"与"不存在" |
| [`tp-remove`](#tp-remove---移除属性) | 移除属性或子属性 |
| [`tp-clear`](#tp-clear---清除所有属性) | 清除区域中的所有文本属性 |

#### 模式匹配函数
| 函数 | 描述 |
|------|------|
| [`tp-match-set`](#tp-match-set---匹配字符串) | 在字符串匹配处设置属性（可选边界） |
| [`tp-match-reset`](#tp-match-reset---匹配并重置) | 在字符串匹配处重置所有属性（可选边界） |
| [`tp-match-add`](#tp-match-add---匹配并添加) | 在字符串匹配处添加/合并属性（可选边界） |
| [`tp-regexp-set`](#tp-regexp-set---匹配正则表达式) | 在正则匹配处设置属性（可选边界和捕获组） |
| [`tp-regexp-reset`](#tp-regexp-reset---正则匹配并重置) | 在正则匹配处重置所有属性（可选边界和捕获组） |
| [`tp-regexp-add`](#tp-regexp-add---正则匹配并添加) | 在正则匹配处添加/合并属性（可选边界和捕获组） |

#### 搜索和导航函数
| 函数 | 描述 |
|------|------|
| [`tp-search-forward`](#tp-search-forward--tp-search-backward) | **已废弃（0.3.0）** —— 请使用 [`tp-forward`](#tp-forward--tp-backward) 或 Emacs 原语 |
| [`tp-search-backward`](#tp-search-forward--tp-search-backward) | **已废弃（0.3.0）** —— 请使用 [`tp-backward`](#tp-forward--tp-backward) 或 Emacs 原语 |
| [`tp-forward`](#tp-forward--tp-backward) | 向前搜索 N 次具有属性的文本（可选谓词匹配） |
| [`tp-backward`](#tp-forward--tp-backward) | 向后搜索 N 次具有属性的文本（可选谓词匹配） |
| [`tp-forward-do`](#tp-forward-do--tp-backward-do) | 向前搜索 N 次，在第 N 个匹配处应用函数 |
| [`tp-backward-do`](#tp-forward-do--tp-backward-do) | 向后搜索 N 次，在第 N 个匹配处应用函数 |
| [`tp-search`](#tp-search---搜索所有匹配) | 在范围或字符串中搜索所有匹配的属性 |
| [`tp-search-map`](#tp-search-map---对匹配文本应用函数) | 对所有匹配的文本应用函数（支持起始和结束范围） |

#### 属性层定义函数
| 函数 | 描述 |
|------|------|
| [`define-tp`](#define-tp--define-tps---定义自定义文本属性) | 定义自定义文本属性（层），支持可选参数 |
| [`define-tps`](#define-tp--define-tps---定义自定义文本属性) | 定义自定义文本属性组（层组），支持可选参数 |
| [`tp-define-layer` / `tp-define-group`](#define-tp--define-tps---定义自定义文本属性) | `define-tp` / `define-tps` 的前缀规范别名 |
| [`tp-layer-props`](#tp-layer-props--tp-group-props) | 获取属性层的属性 |
| [`tp-group-props`](#tp-layer-props--tp-group-props) | 获取属性层组中所有属性层的属性 |
| [`tp-layer-props-with-args`](#tp-layer-props-with-args--tp-group-props-with-args--tp-layer-arglist) | 用参数列表展开参数化属性层 |
| [`tp-group-props-with-args`](#tp-layer-props-with-args--tp-group-props-with-args--tp-layer-arglist) | 用参数列表展开参数化属性层组 |
| [`tp-layer-arglist`](#tp-layer-props-with-args--tp-group-props-with-args--tp-layer-arglist) | 获取参数化属性层的参数列表 |
| [`tp-describe-layer`](#tp-describe-layer---描述属性层) | 在帮助缓冲区中描述属性层的定义 |
| [`tp-undefine-layer`](#tp-undefine-layer--tp-undefine-group) | 移除属性层定义 |
| [`tp-undefine-group`](#tp-undefine-layer--tp-undefine-group) | 移除属性层组定义 |
| [`tp-layer-reset`](#tp-layer-reset) | 清除所有属性层/属性层组定义 |
| [`tp-reactive-reset`](#tp-reactive-reset) | 清除所有响应式依赖和监听器 |

#### 属性层放置函数
| 函数 | 描述 |
|------|------|
| [`tp-put-layer`](#tp-put-layer---在指定位置设置属性层) | 在指定索引位置设置属性层（可选 NOERROR） |
| [`tp-push-layer`](#tp-push-layer---推送属性层到顶部) | 将属性层推到堆栈顶部（可选 NOERROR） |

#### 属性层删除函数
| 函数 | 描述 |
|------|------|
| [`tp-delete-layer`](#tp-delete-layer---按名称索引删除属性层) | 按名称或索引删除属性层 |
| [`tp-pop-layer`](#tp-pop-layer---弹出顶层) | 移除顶层属性层 |

#### 属性层移动函数
| 函数 | 描述 |
|------|------|
| [`tp-move-layer`](#tp-move-layer---移动属性层到指定位置) | 将属性层从一个位置移动到另一个位置 |
| [`tp-raise-layer`](#tp-raise-layer---上移下移属性层) | 将属性层上移/下移 N 个位置 |
| [`tp-lower-layer`](#tp-lower-layer---tp-raise-layer-的镜像) | `tp-raise-layer` 的镜像：将属性层下移/上移 N 个位置 |
| [`tp-rotate-layer`](#tp-rotate-layer---轮换属性层) | 向上或向下轮换属性层 N 步 |
| [`tp-pin-layer`](#tp-pin-layer---将属性层置顶) | 将属性层移到顶部（一次性；之后的 push 仍可能覆盖它） |
| [`tp-switch-layer`](#tp-switch-layer---交换两个属性层) | 交换两个属性层的位置 |

#### 属性层可见性函数
| 函数 | 描述 |
|------|------|
| [`tp-hide-layer`](#tp-hide-layer--tp-show-layer---隐藏与显示属性层) | 隐藏属性层而不将其从栈中移除 |
| [`tp-show-layer`](#tp-hide-layer--tp-show-layer---隐藏与显示属性层) | 让隐藏的属性层重新渲染 |

#### 属性层合并函数
| 函数 | 描述 |
|------|------|
| [`tp-merge-layers`](#tp-merge-layers---合并多个属性层) | 将指定属性层合并为新属性层（隐藏层不贡献属性） |
| [`tp-flatten-layers`](#tp-flatten-layers---扁平化所有属性层) | 将所有属性层扁平化为单一属性层（隐藏层被丢弃） |

#### 属性层查询函数
| 函数 | 描述 |
|------|------|
| [`tp-layer-list`](#tp-layer-list---列出所有属性层) | 列出区域中的所有属性层名称 |
| [`tp-layer-count`](#tp-layer-count) | 计算区域中的属性层数量 |
| [`tp-layer-exists-p`](#tp-layer-exists-p) | 检查区域中是否存在某属性层 |
| [`tp-layer-top`](#tp-layer-top) | 获取顶层属性层的名称（按栈序，即使它被隐藏） |
| [`tp-layer-stack-at`](#tp-layer-stack-at---获取某位置的完整层栈) | 以 `(NAME . PROPS)` cons 形式返回某位置的完整有序层栈 |
| [`tp-region-layer-props`](#tp-region-layer-props---获取区域中的层属性) | 获取区域中特定层的属性 |

#### 属性层操作函数
| 函数 | 描述 |
|------|------|
| [`tp-add-to-layers`](#tp-add-to-layers---向特定属性层添加属性) | 通过索引或名称向特定层添加/合并属性 |
| [`tp-add-to-all-layers`](#tp-add-to-all-layers---向所有属性层添加属性) | 向所有现有层添加/合并属性 |

#### 实用工具函数
| 函数 | 描述 |
|------|------|
| [`tp-intervals`](#tp-intervals---获取文本属性区间) | 获取区域中的所有文本属性区间（可选 ABSOLUTE 坐标） |
| [`tp-intervals-map`](#tp-intervals-map---对区间应用函数) | 对区域中的所有区间应用函数（可选 ABSOLUTE 坐标） |
| [`tp-plist`](#tp-plist---获取区域中的所有属性) | 获取区域中存在的所有属性 |
| [`tp-empty-p`](#tp-empty-p---检查对象是否有属性) | 检查对象是否没有文本属性 |
| [`tp-with-current-buffer`](#tp-with-current-buffer--tp-pop-to-buffer--tp-switch-to-buffer) | 在绑定 `inhibit-read-only` 的情况下在缓冲区中执行 body |
| [`tp-pop-to-buffer`](#tp-with-current-buffer--tp-pop-to-buffer--tp-switch-to-buffer) | 填充缓冲区、设为只读并通过 `pop-to-buffer` 显示 |
| [`tp-switch-to-buffer`](#tp-with-current-buffer--tp-pop-to-buffer--tp-switch-to-buffer) | 填充缓冲区、设为只读并通过 `switch-to-buffer` 显示 |

#### 调色板函数
| 函数 | 描述 |
|------|------|
| [`tp-palette-alist`](#调色板系统) | 具名调色板注册表（变量） |
| [`define-tp-palette`](#调色板系统) | 注册或更新一个具名调色板（别名：`tp-define-palette`） |
| [`tp-palette-color`](#调色板系统) | 获取调色板的 `:fg` / `:bg` / `:border` 颜色，按主题解析 |
| [`tp-palette-has-p`](#调色板系统) | 测试调色板（或其某个键）是否已定义 |
| [`tp-palette-show`](#调色板系统) | 展示所有已注册调色板的画廊 |
| [`tp-parse-color`](#调色板系统) | 按当前亮色/暗色主题解析颜色规格 |

#### 响应式生命周期函数
| 函数 | 描述 |
|------|------|
| [`tp-with-batch-updates`](#批量更新) | 将多个响应式变量更改合并为一次更新 |
| [`tp-reactive-layer-buffers`](#层-缓冲区注册表与生命周期) | 注册为展示某层的缓冲区（或 `unknown`） |
| [`tp-reactive-track-buffer`](#层-缓冲区注册表与生命周期) | 插入已带属性的字符串后注册缓冲区 |
| [`tp-gc-anonymous-layers`](#层-缓冲区注册表与生命周期) | 回收已无注册的存活缓冲区展示的匿名层 |

---

### 核心属性函数

> **重要：字符串修改行为**
>
> 核心属性函数（`tp-set`、`tp-reset`、`tp-add`、`tp-remove`）根据调用方式有不同的行为：
>
> | 调用方式 | 底层实现 | 是否修改原始对象？ |
> |---------|---------|------------------|
> | `(tp-set STRING PROP VAL ...)` | 内部使用 `propertize` | **否** - 返回新字符串 |
> | `(tp-set START END PROPS)` | 对缓冲区使用 `put-text-property` | 是 - 修改当前缓冲区 |
> | `(tp-set START END PROPS STRING)` | 对字符串使用 `put-text-property` | **是** - 修改原始字符串 |
> | `(tp-set START END PROPS BUFFER)` | 对缓冲区使用 `put-text-property` | 是 - 修改缓冲区 |
>
> **总结：**
> - **整个字符串形式** `(tp-set "string" ...)`：创建一个**新的**带属性字符串。原始字符串不会被修改。内部使用 `propertize` 实现。
> - **区域形式（字符串对象）** `(tp-set 0 5 '(...) string)`：使用 `put-text-property` 或 `set-text-properties` **直接修改**原始字符串对象。
> - **缓冲区形式**：始终就地修改缓冲区。
>
> 这一区别适用于所有核心属性函数：`tp-set`、`tp-reset`、`tp-add` 和 `tp-remove`。

#### `tp-set` - 设置文本属性

在字符串或缓冲区区域上设置文本属性。只替换指定的属性，保留其他属性。

```elisp
;; 当前缓冲区（属性作为列表）- 就地修改缓冲区
(tp-set START END '(PROPERTY VALUE ...))
(tp-set START END LAYER-NAME)

;; 特定缓冲区或字符串 - 就地修改 OBJECT
(tp-set START END '(PROPERTY VALUE ...) OBJECT)
(tp-set START END LAYER-NAME OBJECT)

;; 整个字符串（平铺属性或层名称）- 返回新字符串
(tp-set STRING PROPERTY VALUE ...)
(tp-set STRING LAYER-NAME)
```

LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。

**返回值：**
- 缓冲区形式：返回 `(START . END)` 点对
- 字符串区域形式 `(tp-set 0 5 '(...) string)`：返回修改后的字符串（同一对象）
- 整个字符串形式 `(tp-set "string" ...)`：返回一个**新的**带属性字符串

**示例：**

```elisp
;; 在缓冲区区域设置 face
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold)))
;; => (1 . 10)

;; 设置多个属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "Click me")))
;; => (1 . 10)

;; 在特定缓冲区设置
(let ((my-buffer (generate-new-buffer "*test*")))
  (with-current-buffer my-buffer
    (insert "Hello World"))
  (prog1 (tp-set 1 10 '(face italic) my-buffer)
    (kill-buffer my-buffer)))
;; => (1 . 10)

;; 在字符串区域设置属性（0 索引）- 修改原始字符串
(let ((my-string (copy-sequence "Hello World")))
  (tp-set 0 5 '(face italic) my-string)
  my-string)
;; => #("Hello World" 0 5 (face italic))

;; 在整个字符串上设置属性 - 返回新字符串，原始字符串不变
(let ((original "Hello"))
  (let ((result (tp-set original 'face 'bold)))
    (list :original original
          :result result
          :original-has-props (get-text-property 0 'face original)
          :result-has-props (get-text-property 0 'face result))))
;; => (:original "Hello" :result #("Hello" 0 5 (face bold)) 
;;     :original-has-props nil :result-has-props bold)

;; 在整个字符串上使用已定义的层名称
(define-tp my-style ()
  :props '(face (:foreground $my-color))
  :data '((my-color . "blue")))
(tp-set " " 'my-style)
;; => #(" " 0 1 (face (:foreground "blue") tp-name my-style))
;;    （属性的打印顺序在不同 Emacs 版本间可能不同，值本身一致）

;; 单次调用中合并多个 face（重复属性自动合并）
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; => face 是 ((:foreground "red") (:background "green") bold)
;;    （各条目堆叠为一个 face 列表，最新的在前）

;; 同一子属性后面的值覆盖前面的
(tp-set "emacs"
        'face '(:foreground "red")
        'face '(:foreground "yellow"))
;; => face 的 :foreground 是 "yellow"（后面的覆盖前面的）

;; 与 tp-palette 层配合使用，合并额外的 face 属性
(tp-set "emacs"
        'tp-palette 'info
        'face '(:foreground "red"))
;; => tp-palette 的 face 与 (:foreground "red") 合并，:foreground 被覆盖
```

---

#### `tp-reset` - 替换所有属性

用指定的属性完全替换所有文本属性。

```elisp
;; 缓冲区/区域形式 - 就地修改
(tp-reset START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-reset START END LAYER-NAME &optional OBJECT)

;; 整个字符串形式 - 返回新字符串
(tp-reset STRING PROPERTY VALUE ...)
```

LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。

**返回值：**
- 缓冲区形式：返回 `(START . END)` 点对
- 字符串区域形式：返回修改后的字符串（同一对象）
- 整个字符串形式：返回一个**新的**带属性字符串

**示例：**

```elisp
;; 替换区域中的所有属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(help-echo "old"))  ; 设置已有属性
  (tp-reset 1 10 '(face bold))      ; 任何现有属性都会被移除
  (tp-at 1))
;; => (face bold)  ; help-echo 被移除了

;; 在整个字符串上 - 返回新字符串，原始字符串不变
(let ((original "Hello"))
  (let ((result (tp-reset original 'face 'italic)))
    (list :original-modified (get-text-property 0 'face original)
          :result-face (get-text-property 0 'face result))))
;; => (:original-modified nil :result-face italic)

;; 使用已定义的层名称
(define-tp error-style ()
  '(face (:foreground "red" :weight bold)))
(with-temp-buffer
  (insert "Hello World")
  (tp-reset 1 10 'error-style))
;; => (1 . 10)  ; 所有属性被 error-style 替换
```

---

#### `tp-add` - 添加/合并属性

添加或更新属性，支持嵌套属性列表的深度合并。

```elisp
;; 缓冲区/区域形式 - 就地修改
(tp-add START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-add START END LAYER-NAME &optional OBJECT)

;; 整个字符串形式 - 返回新字符串
(tp-add STRING PROPERTY VALUE ...)
```

LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。

**返回值：**
- 缓冲区形式：返回 `(START . END)` 点对
- 字符串区域形式：返回修改后的字符串（同一对象）
- 整个字符串形式：返回一个**新的**带属性字符串

**示例：**

```elisp
;; 添加属性（保留现有，合并嵌套）
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-add 1 10 '(help-echo "tooltip"))
  (tp-at 1))
;; => (face bold help-echo "tooltip")

;; 深度合并 face 属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red")))
  (tp-add 1 10 '(face (:background "blue")))
  (tp-at 1 'face))
;; => (:foreground "red" :background "blue")

;; 整个字符串形式 - 返回新字符串，原始字符串不变
(let ((original "Hello"))
  (let ((result (tp-add original 'face 'bold)))
    (list :original-modified (get-text-property 0 'face original)
          :result-face (get-text-property 0 'face result))))
;; => (:original-modified nil :result-face bold)

;; 使用已定义的层名称
(define-tp highlight-style ()
  '(face (:background "yellow")))
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-add 1 10 'highlight-style)
  (tp-at 1))
;; => 属性与 highlight-style 合并
```

---

#### `tp-get` - 获取属性值

从范围或字符串获取属性值，支持嵌套子属性访问。

返回 `(START END VALUE)` 区间列表，让你可以查看范围内所有的属性值。

对于单个位置的查询，请使用 `tp-at`。

```elisp
;; 范围 - 特定属性（返回区间列表）
(tp-get START END PROPERTY)
(tp-get START END PROPERTY OBJECT)

;; 范围 - 属性路径作为列表
(tp-get START END '(PROPERTY) OBJECT)
(tp-get START END '(PROPERTY SUB-KEY ...) OBJECT)

;; 范围 - 深层嵌套属性路径
(tp-get START END '(PROPERTY SUB-KEY SUB-SUB-KEY ...) OBJECT)

;; 范围 - 从嵌套属性中提取多个键
(tp-get START END '(PROPERTY SUB-KEY (KEY1 KEY2 ...)) OBJECT)

;; 范围 - 所有属性（返回区间列表）
(tp-get START END)
(tp-get START END OBJECT)

;; 整个字符串（返回区间列表）
(tp-get STRING)
(tp-get STRING PROPERTY)
(tp-get STRING PROPERTY SUB-KEY ...)
(tp-get STRING PROPERTY SUB-KEY '(KEY1 KEY2 ...))
(tp-get STRING '(PROPERTY SUB-KEY ...))
```

**示例：**

```elisp
;; 从范围获取 - 返回 (START END VALUE) 区间列表
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-get 1 10 'face))
;; => ((1 6 bold))

;; 获取多个区间
(let ((str (copy-sequence "Hello World Hello")))
  (tp-set 0 5 '(face bold) str)
  (tp-set 12 17 '(face italic) str)
  (tp-get 0 17 'face str))
;; => ((0 5 bold) (12 17 italic))

;; 使用列表形式的属性路径
(let ((my-string (copy-sequence "Hello World Hello World")))
  (tp-set 5 20 '(face (:underline (:style wave))) my-string)
  (tp-get 5 20 '(face :underline :style) my-string))
;; => ((5 20 wave))

;; 从整个字符串获取深层嵌套属性
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 5 '(face (:underline (:color "green"))) str)
  (tp-set 6 11 '(face (:underline (:color "yellow"))) str)
  (tp-get str 'face :underline :color))
;; => ((0 5 "green") (6 11 "yellow"))

;; 从嵌套属性中获取多个键
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 5 '(face (:underline (:color "green" :style wave))) str)
  (tp-set 6 11 '(face (:underline (:color "yellow" :style line))) str)
  (tp-get str 'face :underline '(:color :style)))
;; => ((0 5 (:color "green" :style wave)) (6 11 (:color "yellow" :style line)))

;; 获取范围内的所有属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold help-echo "test"))
  (tp-get 1 10))
;; => ((1 6 (face bold help-echo "test")))

;; 从整个字符串获取 - 返回区间列表
(let ((str (copy-sequence "Hello World Hello")))
  (tp-set 0 5 '(face bold) str)
  (tp-set 12 17 '(face italic) str)
  (list (tp-get str)              ; => ((0 5 (face bold)) (12 17 (face italic)))
        (tp-get str 'face)))      ; => ((0 5 bold) (12 17 italic))
;; => (((0 5 (face bold)) (12 17 (face italic))) ((0 5 bold) (12 17 italic)))
```

---

#### `tp-at` - 获取位置属性

```elisp
;; 获取位置的所有属性
(tp-at POS)
(tp-at POS OBJECT)

;; 获取位置的特定属性
(tp-at POS PROPERTY)
(tp-at POS PROPERTY OBJECT)

;; 获取位置的嵌套子属性
(tp-at POS '(PROPERTY SUB-KEY ...))
(tp-at POS '(PROPERTY SUB-KEY ...) OBJECT)
```

获取 POS 位置的文本属性，可选择按 PROPERTY 过滤。

对于单位置属性查询（以前使用 `tp-get`），现在使用 `tp-at`。

**示例：**

```elisp
;; 获取当前缓冲区位置 5 的所有属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "test"))
  (tp-at 5))
;; => (face bold help-echo "test")

;; 获取字符串位置 0 的所有属性
(let ((my-string (tp-set "Hello" 'face 'italic 'help-echo "greeting")))
  (tp-at 0 my-string))
;; => (face italic help-echo "greeting")

;; 获取位置的特定属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-at 5 'face))
;; => bold

;; 获取字符串位置的特定属性
(let ((my-string (tp-set "Hello" 'face 'italic)))
  (tp-at 0 'face my-string))
;; => italic

;; 获取位置的嵌套子属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red" :box (:color "blue"))))
  (list (tp-at 5 '(face :foreground))
        (tp-at 5 '(face :box :color))))
;; => ("red" "blue")

;; 从字符串获取嵌套子属性
(let ((str (copy-sequence "Hello")))
  (tp-set 0 5 '(face (:foreground "red" :underline t)) str)
  (tp-at 0 '(face :foreground) str))
;; => "red"
```

---

#### `tp-member` - 判断位置属性是否存在

```elisp
(tp-member POS PROPERTY &optional OBJECT)
```

类似 `tp-at`，但当 PROPERTY 在 POS 处存在时返回 `(PROPERTY VALUE)` 列表，不存在时返回 nil。由此可以区分"属性存在且值为 nil"与"属性完全不存在"（类似 `plist-member`）。

**示例：**

```elisp
;; 存在且值为 nil vs. 不存在
(let ((str (copy-sequence "Hello")))
  (tp-set 0 5 '(face nil) str)
  (list (tp-member 0 'face str)       ; 存在，值为 nil
        (tp-member 0 'display str)))  ; 不存在
;; => ((face nil) nil)

;; 在缓冲区中
(with-temp-buffer
  (insert "Hello")
  (tp-set 1 6 '(face bold))
  (tp-member 1 'face))
;; => (face bold)
```

---

#### `tp-remove` - 移除属性

从区域或整个字符串中移除属性或嵌套子属性。

```elisp
;; 移除整个属性（缓冲区）- 就地修改
(tp-remove START END PROPERTY &optional OBJECT)

;; 移除子属性（缓冲区）- 就地修改
(tp-remove START END '(PROPERTY SUB-KEY) &optional OBJECT)

;; 移除嵌套子属性（缓冲区）- 就地修改
(tp-remove START END '(PROPERTY SUB-KEY (NESTED-KEYS...)) &optional OBJECT)

;; 从整个字符串移除 - 返回新字符串
(tp-remove STRING PROP1 PROP2 ...)
(tp-remove STRING PROPERTY SUB-KEY)
(tp-remove STRING PROPERTY SUB-KEY '(NESTED-KEYS...))
```

**返回值：**
- 缓冲区形式：返回 `nil`
- 整个字符串形式：返回一个**新的**移除了属性的字符串

**示例：**

```elisp
;; 移除整个属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "test"))
  (tp-remove 1 10 'face)
  (tp-at 1))
;; => (help-echo "test")

;; 从 face 移除子属性
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red" :underline t)))
  (tp-remove 1 10 '(face :underline))
  (tp-at 1 'face))
;; => (:foreground "red")

;; 移除特定嵌套键，保留其他
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:underline (:style wave :position t :color "blue"))))
  (tp-remove 1 10 '(face :underline (:style :position)))
  (tp-at 1 '(face :underline)))
;; => (:color "blue")  ; :style 和 :position 被移除, :color 保留

;; 从整个字符串移除 - 返回新字符串，原始字符串不变
(let ((original (propertize "Hello" 'face 'bold 'help-echo "tip")))
  (let ((result (tp-remove original 'face)))
    (list :original-face (get-text-property 0 'face original)
          :result-face (get-text-property 0 'face result))))
;; => (:original-face bold :result-face nil)

;; 从字符串移除子属性 - 返回新字符串
(let ((original (propertize "Hello" 'face '(:foreground "red" :underline t))))
  (let ((result (tp-remove original 'face :underline)))
    (list :original (get-text-property 0 'face original)
          :result (get-text-property 0 'face result))))
;; => (:original (:foreground "red" :underline t) :result (:foreground "red"))

;; 从字符串移除嵌套键
(let ((original (propertize "Hello" 'face '(:underline (:style wave :color "blue")))))
  (let ((result (tp-remove original 'face :underline '(:style))))
    (tp-at 0 '(face :underline) result)))
;; => (:color "blue")
```

---

#### `tp-clear` - 清除所有属性

```elisp
(tp-clear &optional START END OBJECT)
```

清除区域中的所有文本属性。返回 nil。

**示例：**

```elisp
;; 清除区域
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-clear 1 10)
  (tp-at 1))
;; => nil

;; 清除整个缓冲区
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 12 '(face bold))
  (tp-clear)
  (tp-at 5))
;; => nil
```

---

### 模式匹配函数

#### `tp-match-set` - 匹配字符串

```elisp
(tp-match-set PATTERN PLIST &optional OBJECT START END)
(tp-match-set PATTERN LAYER-NAME &optional OBJECT START END)
```

在所有字符串模式匹配处设置属性。
PATTERN 可以是字符串（单个模式）或字符串列表（多个模式）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。
START 和 END（0.3.0 新增）将匹配限制在 OBJECT 的 `[START, END)` 部分，
使用原生坐标（字符串从 0 开始，缓冲区从 1 开始）。匹配的行为**如同
OBJECT 只由这一部分组成**，因此匹配不会跨越边界；颠倒的边界会被交换。
全部六个 `tp-match-*` / `tp-regexp-*` 函数都接受同样的边界参数。

**示例：**

```elisp
;; 在缓冲区中 - 返回 (START . END) 对的列表
(with-temp-buffer
  (insert "TODO: fix this. TODO: also this.")
  (tp-match-set "TODO" '(face warning)))
;; => ((1 . 5) (17 . 21))

;; 在字符串上 - 返回新的带属性字符串（原始字符串不变）
(tp-match-set "o" '(face bold) "Hello World")
;; => #("Hello World" 4 5 (face bold) 7 8 (face bold))

;; 多个模式 - 同时匹配 "world" 和 "Hello"
(with-temp-buffer
  (insert "Hello world, Hello again")
  (tp-match-set '("world" "Hello") '(face bold)))
;; => ((7 . 12) (1 . 6) (14 . 19))  ; 结果按模式分组：
;;    先是 "world" 的区域，再是每个 "Hello"，顺序与模式列表一致

;; 在字符串上使用多个模式
(tp-match-set '("Hello" "world") '(face bold) "Hello world")
;; => #("Hello world" 0 5 (face bold) 6 11 (face bold))

;; 使用已定义的层名称
(define-tp todo-style ()
  '(face (:foreground "orange" :weight bold)))
(with-temp-buffer
  (insert "TODO: fix this. TODO: also this.")
  (tp-match-set "TODO" 'todo-style))
;; => ((1 . 5) (17 . 21))

;; 用 START/END 边界限制匹配 - 只有第二个 TODO 在范围内
(with-temp-buffer
  (insert "TODO one TODO two")
  (tp-match-set "TODO" '(face warning) nil 5 18))
;; => ((10 . 14))
```

---

#### `tp-match-reset` - 匹配并重置

重置（完全替换）匹配处的所有属性。
PATTERN 可以是字符串或字符串列表（多个模式）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-match-reset PATTERN PLIST &optional OBJECT START END)
(tp-match-reset PATTERN LAYER-NAME &optional OBJECT START END)
```

START 和 END 将匹配限制在 OBJECT 的 `[START, END)` 部分
（参见 [`tp-match-set`](#tp-match-set---匹配字符串)）。

**示例：**

```elisp
;; 替换匹配文本上的所有属性
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-set 1 5 '(help-echo "original"))  ; 设置已有属性
  (tp-match-reset "TODO" '(face warning))
  (tp-at 1))
;; => (face warning)  ; help-echo 被移除

;; 多个模式
(with-temp-buffer
  (insert "TODO: fix. FIXME: also fix.")
  (tp-match-reset '("TODO" "FIXME") '(face warning)))
;; => ((1 . 5) (12 . 17))

;; 使用已定义的层名称
(define-tp alert-style ()
  '(face (:background "red" :foreground "white")))
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-match-reset "TODO" 'alert-style))
;; => ((1 . 5))
```

---

#### `tp-match-add` - 匹配并添加

在匹配处添加/合并属性，支持深度合并。
PATTERN 可以是字符串或字符串列表（多个模式）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-match-add PATTERN PLIST &optional OBJECT START END)
(tp-match-add PATTERN LAYER-NAME &optional OBJECT START END)
```

START 和 END 将匹配限制在 OBJECT 的 `[START, END)` 部分
（参见 [`tp-match-set`](#tp-match-set---匹配字符串)）。

**示例：**

```elisp
;; 与现有属性合并
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-set 1 5 '(help-echo "important"))
  (tp-match-add "TODO" '(face (:underline t)))
  (tp-at 1))
;; => (face (:underline t) help-echo "important")

;; 多个模式
(with-temp-buffer
  (insert "TODO: fix. FIXME: also fix.")
  (tp-match-add '("TODO" "FIXME") '(face (:underline t))))
;; => ((1 . 5) (12 . 17))

;; 使用已定义的层名称
(define-tp underline-style ()
  '(face (:underline (:color "blue" :style wave))))
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-match-add "TODO" 'underline-style))
;; => ((1 . 5))
```

---

#### `tp-regexp-set` - 匹配正则表达式

```elisp
(tp-regexp-set PATTERN PLIST &optional OBJECT START END SUBEXP)
(tp-regexp-set PATTERN LAYER-NAME &optional OBJECT START END SUBEXP)
```

在所有正则表达式匹配处设置属性。
PATTERN 可以是字符串（单个正则）或字符串列表（多个正则）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。
START 和 END（0.3.0 新增）将匹配限制在 OBJECT 的 `[START, END)` 部分，
使用原生坐标；匹配的行为如同 OBJECT 只由这一部分组成，颠倒的边界会被
交换（参见 [`tp-match-set`](#tp-match-set---匹配字符串)）。
SUBEXP（0.3.0 新增）指定 PATTERN 的一个捕获组（1 = 第一个组，与
font-lock 高亮的约定一致）：属性应用于每个匹配中的该捕获组，而不是整个
匹配。捕获组未参与的匹配不贡献任何内容；SUBEXP 超出模式的捕获组数量时
会发出明确的错误信号。全部三个 `tp-regexp-*` 函数都接受 SUBEXP。

**示例：**

```elisp
;; 高亮缓冲区中的所有数字
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" '(face font-lock-number-face))
  (list (tp-at 5 'face) (tp-at 13 'face)))
;; => (font-lock-number-face font-lock-number-face)

;; 在字符串上（默认受 `case-fold-search' 影响，"Hello" 也会匹配；
;; 需要区分大小写时请将其 let 绑定为 nil）
(tp-regexp-set "[A-Z]+" '(face bold) "Hello WORLD")
;; => #("Hello WORLD" 0 5 (face bold) 6 11 (face bold))

;; 多个正则 - 同时匹配数字和大写字母
;; （忽略大小写时 "abc" 也匹配 "[A-Z]+"）
(tp-regexp-set '("[0-9]+" "[A-Z]+") '(face bold) "abc 123 XYZ")
;; => #("abc 123 XYZ" 0 3 (face bold) 4 7 (face bold) 8 11 (face bold))

;; 使用已定义的层名称
(define-tp number-style ()
  '(face (:foreground "green")))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" 'number-style))
;; => ((5 . 8) (13 . 16))

;; SUBEXP - 只对每个匹配的捕获组 1 设置属性
(tp-regexp-set "\\([0-9]+\\)px" '(face bold) "margin: 10px 4px" nil nil 1)
;; => #("margin: 10px 4px" 8 10 (face bold) 13 14 (face bold))

;; 捕获组未参与的匹配不贡献任何内容：
;; "bar" 匹配该模式，但捕获组 1 只在 "foo" 中参与
(tp-regexp-set "\\(foo\\)\\|bar" '(face bold) "foo bar" nil nil 1)
;; => #("foo bar" 0 3 (face bold))

;; SUBEXP 超出模式的捕获组数量时发出明确的错误信号
(tp-regexp-set "[0-9]+" '(face bold) "abc 123" nil nil 2)
;; error: Regexp "[0-9]+" has no group 2

;; START/END 边界：如同只有这一部分存在 - 贪婪的 a+
;; 恰好匹配 [1, 3) 而不是整段字符
(tp-regexp-set "a+" '(face bold) "aaaa" 1 3)
;; => #("aaaa" 1 3 (face bold))

;; 颠倒的边界会被交换
(tp-regexp-set "a+" '(face bold) "aaaa" 3 1)
;; => #("aaaa" 1 3 (face bold))
```

---

#### `tp-regexp-reset` - 正则匹配并重置

重置（完全替换）正则匹配处的所有属性。
PATTERN 可以是字符串或字符串列表（多个正则）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-regexp-reset PATTERN PLIST &optional OBJECT START END SUBEXP)
(tp-regexp-reset PATTERN LAYER-NAME &optional OBJECT START END SUBEXP)
```

START/END 边界和 SUBEXP 捕获组的用法与
[`tp-regexp-set`](#tp-regexp-set---匹配正则表达式) 完全相同。

**示例：**

```elisp
;; 重置正则匹配处的所有属性
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "original"))
  (tp-regexp-reset "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold)  ; help-echo 被移除

;; 在字符串上 - 返回新字符串，原字符串保持不变
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "original") str)
  (let ((result (tp-regexp-reset "[0-9]+" '(face italic) str)))
    (list (tp-at 4 result) (tp-at 4 str))))
;; => ((face italic) (help-echo "original"))

;; 使用已定义的层名称
(define-tp code-number ()
  '(face (:foreground "cyan")))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-reset "[0-9]+" 'code-number))
;; => ((5 . 8) (13 . 16))
```

---

#### `tp-regexp-add` - 正则匹配并添加

在正则匹配处添加/合并属性，支持深度合并。
PATTERN 可以是字符串或字符串列表（多个正则）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-regexp-add PATTERN PLIST &optional OBJECT START END SUBEXP)
(tp-regexp-add PATTERN LAYER-NAME &optional OBJECT START END SUBEXP)
```

START/END 边界和 SUBEXP 捕获组的用法与
[`tp-regexp-set`](#tp-regexp-set---匹配正则表达式) 完全相同。

**示例：**

```elisp
;; 添加属性到正则匹配处（保留现有）
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "number"))
  (tp-regexp-add "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold help-echo "number")

;; 在字符串上 - 返回新字符串，原字符串保持不变
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "number") str)
  (let ((result (tp-regexp-add "[0-9]+" '(face italic) str)))
    (list (tp-at 4 result) (tp-at 4 str))))
;; => ((face italic help-echo "number") (help-echo "number"))

;; 使用已定义的层名称
(define-tp bold-underline ()
  '(face (:weight bold :underline t)))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-add "[0-9]+" 'bold-underline))
;; => ((5 . 8) (13 . 16))
```

---

### 搜索和导航函数

#### `tp-search-forward` / `tp-search-backward`

> ⚠️ **自 0.3.0 起已废弃。**这两个函数是 Emacs 的
> `text-property-search-forward` / `text-property-search-backward` 的原
> 始包装，其 nil-PREDICATE 默认行为（匹配非 nil 且与 VALUE **不**
> `equal` 的值）与本库其余部分使用的 `equal` 匹配相矛盾。请使用
> [`tp-forward` / `tp-backward`](#tp-forward--tp-backward) 获得 tp 对称
> 的 `equal` 匹配搜索 —— 它们现在也暴露了 PREDICATE 和 NOT-CURRENT ——
> 或者直接调用 Emacs 原语进行底层访问。这两个包装仍然可用，但已被标记
> 为过时（字节编译器会对新的调用发出警告）。

```elisp
(tp-search-forward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)   ; deprecated
(tp-search-backward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)  ; deprecated
```

---

#### `tp-forward` / `tp-backward`

```elisp
(tp-forward PROPERTY &optional VALUE OBJECT N PREDICATE NOT-CURRENT)
(tp-backward PROPERTY &optional VALUE OBJECT N PREDICATE NOT-CURRENT)
```

向前/向后搜索 N 次具有 PROPERTY 的文本。

- **N** 是搜索次数，默认为 1。
- **VALUE** 在缓冲区中与属性值做 `equal` 匹配。
  因此传入 nil 会匹配下一段 PROPERTY *不存在*（值为 nil）的区段；
  要查找带属性的区域，请显式传入属性值。
- **`tp-backward` 与 `tp-forward` 对称**：相同的 equal 匹配语义，
  方向相反。
- **OBJECT** 可以是缓冲区或字符串；nil 默认为当前缓冲区。
- **PREDICATE**（0.3.0 新增）自定义匹配方式：nil（默认值）和 t 都
  **完全**保持 0.2.0 的 `equal` 匹配契约；传入函数时以 `(VALUE PROP-VALUE)`
  调用，返回非 nil 即视为匹配。
- **NOT-CURRENT**（0.3.0 新增）非 nil 时跳过包含 point 的匹配区段，与
  `text-property-search-*` 原语的行为一致。仅缓冲区路径有效；字符串没
  有 point，因此在字符串上会被忽略。
- 对于缓冲区，返回最后一次成功搜索的 prop-match 对象。
- 对于字符串，返回**前 N 个** PROPERTY 匹配区段的 (START END VALUE)
  列表，从位置 0 开始计数（与 point 无关）；VALUE 为 nil 表示匹配任意
  值。`tp-backward` 按从末尾到开头的顺序返回。

**示例：**

```elisp
;; 查找下一个 'marker 等于 t 的文本
(with-temp-buffer
  (insert "Hello World Test")
  (tp-set 7 12 '(marker t))
  (goto-char 1)
  (let ((match (tp-forward 'marker t)))
    (when match
      (prop-match-beginning match))))
;; => 7

;; VALUE 为 nil 时 equal 匹配 nil - 即匹配没有该属性的区段
(with-temp-buffer
  (insert "Hello World Test")
  (tp-set 7 12 '(marker t))
  (goto-char 1)
  (let ((match (tp-forward 'marker)))
    (list (prop-match-beginning match) (prop-match-end match))))
;; => (1 7)  ; marker 不存在的区段

;; backward 与 forward 对称：相同的值匹配，方向相反
(with-temp-buffer
  (insert "Hello World Test")
  (tp-set 7 12 '(marker t))
  (goto-char (point-max))
  (let ((match (tp-backward 'marker t)))
    (list (prop-match-beginning match) (prop-match-end match))))
;; => (7 12)

;; 查找下一个 'type 等于 'heading 的文本
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(type heading))
  (goto-char 1)
  (let ((match (tp-forward 'type 'heading)))
    (when match
      (prop-match-value match))))
;; => heading

;; 在字符串中搜索
(let ((my-string (copy-sequence "Hello World Hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward 'marker nil my-string 2))
;; => ((0 5 t) (12 17 t))

;; PREDICATE - 用自定义函数代替 `equal' 进行匹配
;; （调用参数为 VALUE 和该区段的属性值）
(with-temp-buffer
  (insert "abcdef")
  (tp-set 1 3 '(size 10))
  (tp-set 3 6 '(size 20))
  (goto-char 1)
  (let ((match (tp-forward 'size 15 nil 1
                           (lambda (target v) (and v (> v target))))))
    (list (prop-match-beginning match) (prop-match-end match))))
;; => (3 6)  ; 第一个 size 超过 15 的区段

;; PREDICATE 也适用于字符串（返回前 N 个匹配区段）
(let ((str (copy-sequence "hello world")))
  (tp-set 0 5 '(size 10) str)
  (tp-set 6 11 '(size 20) str)
  (tp-forward 'size 15 str 2 (lambda (target v) (and v (> v target)))))
;; => ((6 11 20))

;; NOT-CURRENT - 跳过包含 point 的匹配区段
(with-temp-buffer
  (insert "one two")
  (tp-set 1 4 '(mark t))
  (tp-set 5 8 '(mark t))
  (let (a b)
    (goto-char 2)                    ; 位于第一个 mark 区段内
    (setq a (prop-match-beginning (tp-forward 'mark t)))
    (goto-char 2)
    (setq b (prop-match-beginning (tp-forward 'mark t nil 1 nil t)))
    (list a b)))
;; => (2 5)  ; 不带 NOT-CURRENT 时当前区段在 point 处即匹配
```

---

#### `tp-forward-do` / `tp-backward-do`

```elisp
(tp-forward-do FUNCTION PROPERTY &optional VALUE OBJECT TIMES START END PREDICATE NOT-CURRENT)
(tp-backward-do FUNCTION PROPERTY &optional VALUE OBJECT TIMES START END PREDICATE NOT-CURRENT)
```

向前/向后搜索 TIMES 次具有 PROPERTY 的文本，**仅在第 TIMES 个匹配处应用 FUNCTION**。

尽管带有 `-do` 后缀，它并**不是** for-each —— 要对*每个*匹配应用函数，
请使用 [`tp-search-map`](#tp-search-map---对匹配文本应用函数)。

- **FUNCTION** 的参数是 `(TEXT &optional START END IDX)`，其中 TEXT 是此次匹配到的文本，START 和 END 为开始结束的位置，IDX 是从 0 开始的匹配索引。FUNCTION 会按其实际接受的参数个数被调用。当 FUNCTION 返回字符串时，它将替换字符串或缓冲区中的匹配文本。
- **在缓冲区中替换文本可以改变长度**（先删除匹配文本，再插入替换文本）。**字符串无法就地改变长度**：长度不同的替换会发出错误信号；长度相同的替换会就地应用。
- **PROPERTY** 是要搜索的文本属性。
- **VALUE** 为 nil 时，表示搜索 PROPERTY 属性，不用匹配值。
- **OBJECT** 默认是当前 buffer 或指定的字符串或指定的 buffer。
- **TIMES** 表示向前/向后搜索几次，默认搜索一次。该函数会搜索 TIMES 次，但仅对第 TIMES 个匹配应用 FUNCTION。要么全有要么全无：当匹配数量不足 TIMES 时，完全不应用 FUNCTION，仅返回实际找到的匹配数量。
- **START** 和 **END** 默认为 OBJECT 的起始和结束位置。
- **PREDICATE** 和 **NOT-CURRENT**（0.3.0 新增）的用法与
  [`tp-forward` / `tp-backward`](#tp-forward--tp-backward) 相同，并被应
  用到每一次底层搜索；默认值完全保持 0.2.0 的行为。
- 返回成功匹配的数量。

**示例：**

```elisp
;; 仅将最后一次（第 2 次）匹配的文本转为大写
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward-do #'upcase 'marker nil my-string 2)
  my-string)
;; => "hello world HELLO"  ; 仅第 2 次匹配被转为大写

;; 在指定范围内搜索（仅搜索范围 6-17 内的匹配）
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward-do #'upcase 'marker nil my-string 2 6 17)
  my-string)
;; => "hello world hello"  ; 范围 6-17 内仅有 1 个匹配，请求的
;;    第 2 个匹配不存在：不做任何应用（要么全有要么全无；
;;    调用仍返回实际匹配数 1）

;; 使用带有 start 和 end 参数的函数
;; 函数接收位置信息；使用 upcase 保持相同长度
(let ((my-string (copy-sequence "hello world hello"))
      (match-info nil))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward-do
   (lambda (text start end)
     (setq match-info (list start end))
     (upcase text))
   'marker nil my-string 2)
  (list my-string match-info))
;; => ("hello world HELLO" (12 17))  ; 仅最后一次匹配被转换

;; 向后搜索 - 仅将最后一次（第 2 次）匹配的文本转为大写
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-backward-do #'upcase 'marker nil my-string 2)
  my-string)
;; => "HELLO world hello"  ; 向后搜索时第一个匹配（即最后找到的）被转为大写
```

---

#### `tp-search` - 搜索所有匹配

```elisp
;; 缓冲区/字符串区域
(tp-search START END PROPERTY &optional VALUE OBJECT)

;; 整个字符串
(tp-search STRING PROPERTY &optional VALUE)
```

在缓冲区/字符串范围或整个字符串中搜索所有具有 PROPERTY 的文本。

返回所有匹配区域的 (START END VALUE) 列表。

**示例：**

```elisp
;; 在缓冲区范围内查找所有 'marker 属性
(with-temp-buffer
  (insert "Hello World Test Again")
  (tp-set 1 6 '(marker t))
  (tp-set 13 17 '(marker t))
  (tp-search 1 22 'marker))
;; => ((1 6 t) (13 17 t))

;; 在字符串中查找所有值为 'heading 的 'type 属性
(let ((my-string (copy-sequence "Title Here Body Text")))
  (tp-set 0 10 '(type heading) my-string)
  (tp-search my-string 'type 'heading))
;; => ((0 10 heading))

;; 按值过滤
(with-temp-buffer
  (insert "Heading1 Body Heading2")
  (tp-set 1 9 '(type heading))
  (tp-set 10 14 '(type body))
  (tp-set 15 23 '(type heading))
  (tp-search 1 23 'type 'heading))
;; => ((1 9 heading) (15 23 heading))
```

---

#### `tp-search-map` - 对匹配文本应用函数

```elisp
(tp-search-map FUNCTION PROPERTY &optional VALUE OBJECT START END)
```

在 OBJECT 的 START 到 END 范围内，匹配到 PROPERTY 属性（值是 VALUE）的部分执行 FUNCTION 函数。

- **FUNCTION** 的参数是 `(TEXT &optional START END IDX)`，其中：
  - TEXT 是此次匹配到的文本
  - START 和 END 为开始结束的位置
  - IDX 是遍历中的当前从 0 开始的索引
  FUNCTION 会按其实际接受的参数个数被调用。当 FUNCTION 返回字符串时，
  它将替换字符串或缓冲区中的匹配文本。
- **在缓冲区中替换文本可以改变长度**（先删除匹配文本，再插入替换文本）。
  **字符串无法就地改变长度**：长度不同的替换会发出错误信号；
  长度相同的替换会就地应用。
- **PROPERTY** 是要搜索的文本属性。
- **VALUE** 为 nil 时，表示搜索 PROPERTY 属性，不用匹配值。
- **OBJECT** 默认是当前 buffer 或指定的字符串或指定的 buffer。
- **START** 和 **END** 默认为 OBJECT 的起始和结束位置。
- 返回处理的匹配数量。

**示例：**

```elisp
;; 将字符串中所有 marker 文本转为大写
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string)
  my-string)
;; => "HELLO world HELLO"

;; 仅在指定范围内搜索
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string 0 10)
  my-string)
;; => "HELLO world hello"  ; 仅范围 0-10 内的第一个匹配被处理

;; 使用 start、end 和 idx 参数的自定义转换
;; 函数接收位置信息；使用 upcase 保持相同长度
(let ((my-string (copy-sequence "aaa bbb ccc"))
      (positions nil))
  (tp-set 0 3 '(marker t) my-string)
  (tp-set 4 7 '(marker t) my-string)
  (tp-set 8 11 '(marker t) my-string)
  (tp-search-map
   (lambda (text start end idx)
     (push (list idx start end) positions)
     (upcase text))
   'marker nil my-string)
  (list my-string (nreverse positions)))
;; => ("AAA BBB CCC" ((0 0 3) (1 4 7) (2 8 11)))

;; 不使用可选参数的自定义转换
(let ((my-string (copy-sequence "hello world")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string)
  my-string)
;; => "HELLO world"
```

---

## 属性层系统

属性层系统是 tp.el 的创新功能，允许在同一文本区域堆叠多组属性。只有顶层属性可见，但下层属性会被保留，并可通过轮转或固定操作使其显现。

### 自定义文本属性

自定义文本属性是 tp.el 提供的一个**通用功能**。使用 `define-tp` 定义后，可以通过 `tp-set`/`tp-reset`/`tp-add` 等核心函数设置。

#### 核心特性

1. **与内置属性混合使用**：自定义文本属性可以和 Emacs 内置的文本属性（如 `face`、`display`、`help-echo` 等）无缝混合使用。

2. **重复属性自动合并**：在一次设置操作中，如果同一个属性（如 `face`）被指定多次，它们会自动合并，而非简单覆盖。

```elisp
;; 定义自定义文本属性
(define-tp tp-highlight ()
  '(face (:background "yellow")))

;; 与内置属性混合使用
(tp-set 1 10 '(tp-highlight t face bold help-echo "提示"))
;; 结果: 同时具有 tp-highlight 的背景色、bold 样式和 help-echo 属性

;; 重复属性自动合并示例
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; 结果: face 是 ((:foreground "red") (:background "green") bold)
;; 三个 face 属性堆叠为一个 face 列表，最新的在前

;; 同一子属性后面的覆盖前面的
(tp-set "emacs"
        'face '(:foreground "red")
        'face '(:foreground "yellow"))
;; 结果: foreground 是 "yellow"

;; 与 tp-palette 层配合使用
(tp-set "emacs"
        'tp-palette 'info
        'face '(:foreground "red"))
;; 结果: tp-palette 的 face 与 (:foreground "red") 合并
```

#### 自定义文本属性组

使用 `define-tps` 可以定义多个相关的文本属性组，它们可以单独使用，也可以作为一组使用。

---

### 文本属性层

文本属性层是 tp.el 的**独特功能**，需要使用特定的函数（`tp-put-layer`/`tp-push-layer`）才能设置和使用。

#### 核心特性

1. **引入层相关属性**：当使用 `tp-push-layer`/`tp-put-layer` 设置时，会自动引入 `tp-name`、`tp-layers` 等层相关属性，用于支持层的堆叠和操作。

2. **层堆叠机制**：可以在同一文本区域堆叠多组属性，只有顶层可见，下层被保留。

3. **丰富的层操作**：支持轮换、删除、合并等多种层操作。

```elisp
;; 定义文本属性（可同时用作自定义属性或层）
(define-tp tp-highlight ()
  '(face (:background "yellow")))

;; 作为普通自定义文本属性使用（不引入层属性）
(tp-set 1 10 '(tp-highlight t))
;; 结果: 只有 face 属性，没有 tp-name

;; 作为文本属性层使用（引入层相关属性）
(tp-push-layer 1 10 'tp-highlight)
;; 结果: 同时有 face 和 tp-name 属性，支持层操作
```

#### 何时使用哪种方式

| 场景 | 推荐方式 | 说明 |
|------|----------|------|
| 简单属性设置 | `tp-set`/`tp-reset`/`tp-add` | 当你只需要设置文本属性，不需要层堆叠功能时 |
| 与内置属性混合 | `tp-set`/`tp-reset`/`tp-add` | 自定义属性可以和内置属性无缝混合 |
| 需要层堆叠 | `tp-push-layer`/`tp-put-layer` | 当你需要在同一文本区域堆叠多组属性时 |
| 需要层操作 | `tp-push-layer`/`tp-put-layer` | 当你需要进行轮换、删除等层操作时 |

### 属性层概念

```
┌─────────────────────────────┐
│   顶层（可见）              │  ← idx=0，你看到的
├─────────────────────────────┤
│   中间层（隐藏）            │  ← idx=1，被保留
├─────────────────────────────┤
│   底层（隐藏）              │  ← idx=-1，被保留
└─────────────────────────────┘
```

### 属性层定义

#### `define-tp` / `define-tps` - 定义自定义文本属性

> 自 0.3.0 起，符合前缀规范的别名 `tp-define-layer`（对应
> `define-tp`）、`tp-define-group`（对应 `define-tps`）和
> `tp-define-palette`（对应 `define-tp-palette`）是今后的规范名称 ——
> 它们让这些宏可以通过 `C-h f tp-...` 被发现。历史名称是永久别名，永远
> 不会被移除；本 README 的示例仍继续使用它们。

##### `define-tp` - 定义单个自定义文本属性（层）

定义自定义文本属性，名称无需单引号引用。**所有格式中参数列表都是必需的**：无参数层（包括响应式关键字格式）用 `()`，参数化层用 `(ARG1 ARG2 ...)`，可包含任意数量的参数符号。支持三种格式：

**格式一 - 无参数（空参数列表，简单属性）：**

```elisp
(define-tp tp-bold ()
  '(face bold))

;; 用法:
(tp-set "emacs" 'tp-bold t)
(tp-set 0 5 '(tp-bold t) "emacs")
```

**格式二 - 有参数（带一个或多个参数）：**

```elisp
(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

;; 用法:
(tp-set "emacs" 'tp-space 2)
(tp-set 0 5 '(tp-space 2) "emacs")
```

自 0.3.0 起，参数列表可以声明**任意数量的参数**。调用规格既接受平铺的
参数 —— `(LAYER ARG1 ... ARGN)` —— 也接受包在一个列表中的参数 ——
`(LAYER (ARG1 ... ARGN))` —— 两种写法在 `tp-set` 和 `tp-put-layer` 中
都有效：

```elisp
(define-tp tp-colors (fg bg)
  `(face (:foreground ,fg :background ,bg)))

;; 整字符串形式：参数跟在层名后面
(tp-set "hello" 'tp-colors "red" "blue")
;; => #("hello" 0 5 (face (:foreground "red" :background "blue")))

;; 区域形式，包装的参数列表外加额外属性
(let ((str (copy-sequence "hello")))
  (tp-set 0 5 '(tp-colors ("red" "blue") help-echo "tip") str)
  (list (tp-at 0 'face str) (tp-at 0 'help-echo str)))
;; => ((:foreground "red" :background "blue") "tip")

;; tp-put-layer 规格
(with-temp-buffer
  (insert "Hello World")
  (tp-put-layer 1 10 '(tp-colors "white" "black") 0)
  (tp-at 1 'face))
;; => (:foreground "white" :background "black")

;; 参数个数不符的调用会发出明确的错误信号，指出层名和两个数量
(tp-set "hello" 'tp-colors "red")
;; error: tp layer tp-colors takes 2 argument(s), got 1
```

参数化层组（`define-tps`）以同样的方式接受多个参数；
`(GROUP ARG1 ... ARGN)` 和 `(GROUP (ARG1 ... ARGN))` 规格在 `tp-set`
家族中都有效。注意：参数化 body 中的 `$` 符号在展开时解析为其变量的当
前值 —— 参数化层**不是**响应式的。

**格式三 - 响应式特性（支持 :props、:data、:compute、:watch、:transform）：**

```elisp
(define-tp my-reactive-layer ()
  :props '(face (:foreground $my-color) help-echo $status-note)
  :data '((my-color . "red") (status . "active"))
  :compute '((status-note (lambda () (concat "status: " status))))
  :watch '((my-color (lambda (new old layer) (message "Color changed!"))))
  :transform (lambda (text) (upcase text)))

;; 用法:
(tp-push-layer 1 10 'my-reactive-layer)
;; 改变变量会自动更新文本
(setq my-color "blue")
```

**响应式关键字说明：**

- **:props** - 属性列表，`$` 前缀的符号是响应式变量
- **:data** - 额外的响应式变量列表（可以包含初始值）
- **:compute** - 计算属性列表，从其他变量派生值
- **:watch** - 监听器列表，变量改变时执行回调
- **:transform** - 转换函数，在显示 `tp-text` 值之前对其进行处理

注意：`:props`、`:data`、`:compute` 和 `:watch` 的值必须**加引号**
（它们在层定义时会被求值）；`:transform` 接受一个函数。

##### `define-tps` - 定义自定义文本属性组（层组）

定义多个相关的自定义文本属性，名称无需单引号引用。与 `define-tp` 一样，**参数列表是必需的**：无参数层组用 `()`，参数化层组用 `(ARG1 ARG2 ...)`（自 0.3.0 起支持任意数量的参数）。属性组中定义的文本属性可以单独使用，也可以使用组名称来设置多层。

**格式一 - 无参数（空参数列表）：**

```elisp
(define-tps tp-moon-phases ()
  '(display "🌑")
  '(display "🌕"))

;; 用法:
(tp-set 1 6 'tp-moon-phases)
```

**格式二 - 有参数（带单个参数）：**

```elisp
;; 先定义参数化的单独层
(define-tp tp-color1 (color)
  `(face (:foreground ,color)))
(define-tp tp-color2 (color)
  `(face (:foreground ,color)))
(define-tp tp-bg ()
  '(face (:background "green")))

;; 定义参数化的层组，引用上面定义的层
(define-tps tp-themed-status (color)
  `(tp-color1 ,color)      ;; 使用层组参数
  '(tp-color2 "red")       ;; 使用固定参数
  'tp-bg)                  ;; 引用无参数层

;; 用法 - 设置多层属性:
(tp-set "emacs" 'tp-themed-status "orange")
;; 结果: 三个层堆叠，tp-color1 为顶层，使用 "orange" 颜色
```

**支持的层定义格式：**

每个元素可以是以下格式之一：

1. **匿名层**（命名为 NAME-0, NAME-1 等）:
   ```elisp
   '(face (:background "yellow"))
   ```

2. **使用 cons-cell 命名层**（命名为 NAME-suffix）:
   ```elisp
   '("highlight" . (face (:background "yellow")))
   ```

3. **使用 :props 关键字命名层**:
   ```elisp
   '("highlight" :props (face (:background "yellow")))
   ```

4. **带响应式特性的命名层**（:props、:data、:watch、:compute）:
   ```elisp
   '("reactive" :props (face (:foreground $my-color))
                :data ((my-color . "red"))
                :watch ((my-color (lambda (new old layer) (message "Changed!")))))
   ```

**示例：**

```elisp
;; 定义无参数的自定义文本属性
(define-tp tp-highlight ()
  '(face (:background "yellow")))

;; 定义有参数的自定义文本属性
(define-tp tp-color (color)
  `(face (:foreground ,color)))

;; 定义属性组
(define-tps tp-status ()
  '("success" . (face (:foreground "green")))
  '("warning" . (face (:foreground "orange")))
  '("error" . (face (:foreground "red"))))

;; 使用自定义文本属性
(tp-set "Hello" 'tp-highlight t)        ; 无参数
(tp-set "Hello" 'tp-color "blue")       ; 有参数
(tp-set 1 6 'tp-status)                 ; 使用层组

;; 作为层使用（支持堆叠操作）
(tp-push-layer 1 10 'tp-highlight)
```

---
定义中的第一个属性层是顶层（默认可见）。

**示例：**

```elisp
;; 先定义状态层，然后将它们组合成层组
(progn
  (tp-layer-reset)
  (define-tp highlight ()
    '(face (:background "yellow" :foreground "black")))
  (define-tp error ()
    '(face (:background "red" :foreground "white")))
  (define-tp info ()
    '(face (:background "blue" :foreground "white")))
  (define-tps status-colors ()
    'highlight 'error 'info)
  (length (tp-group-props 'status-colors)))
;; => 3

;; 使用命名层定义层组
(progn
  (tp-layer-reset)
  (define-tps moon-phases ()
    '("new" . (display "🌑"))
    '("waxing-crescent" . (display "🌒"))
    '("first-quarter" . (display "🌓"))
    '("full" . (display "🌕")))
  (tp-layer-props 'moon-phases-full))
;; => (display "🌕")

;; 参数化层组，引用其他已定义的层
(progn
  (tp-layer-reset)
  (define-tp tp-test-l1 (color)
    `(face (:foreground ,color)))
  (define-tp tp-test-l2 (color)
    `(face (:foreground ,color)))
  (define-tp tp-test-l3 ()
    '(face (:background "green")))
  (define-tps tp-test-group1 (color)
    `(tp-test-l1 ,color)      ;; 使用层组参数
    '(tp-test-l2 "red")       ;; 使用固定参数
    'tp-test-l3)              ;; 引用无参数层
  (tp-set "emacs" 'tp-test-group1 "orange"))
;; => #("emacs" 0 5 (face (:foreground "orange") tp-name tp-test-l1
;;                        tp-layers ((face (:foreground "red") tp-name tp-test-l2)
;;                                   (face (:background "green") tp-name tp-test-l3))))
;;    （顶层属性的打印顺序在不同 Emacs 版本间可能不同，
;;      tp-layers 栈内顺序本身是稳定的）
```

---

#### `tp-layer-props` / `tp-group-props`

```elisp
(tp-layer-props LAYER-NAME &optional INCLUDE-TP-NAME)
(tp-group-props GROUP-NAME &optional INCLUDE-TP-NAME)
```

获取属性层或属性层组中所有属性层的属性。

默认情况下，结果只包含属性层自身的属性。当 INCLUDE-TP-NAME 非 nil 时，
会在结果末尾追加一个 `tp-name LAYER-NAME` 条目（属性层栈内部使用的形式）。
例外：注册了响应式依赖的属性层总是包含 `tp-name` —— 响应式引擎依靠
它定位并重新渲染这些区域。

**示例：**

```elisp
;; 获取属性层属性（默认不含 tp-name）
(progn
  (tp-layer-reset)
  (define-tp my-layer ()
    '(face bold help-echo "tip"))
  (list (tp-layer-props 'my-layer)
        (tp-layer-props 'my-layer t)))
;; => ((face bold help-echo "tip")
;;     (face bold help-echo "tip" tp-name my-layer))

;; 获取属性层组属性
(progn
  (tp-layer-reset)
  (define-tp layer1 ()
    '(face bold))
  (define-tp layer2 ()
    '(face italic))
  (define-tps my-group ()
    'layer1 'layer2)
  (length (tp-group-props 'my-group)))
;; => 2
```

---

#### `tp-layer-props-with-args` / `tp-group-props-with-args` / `tp-layer-arglist`

```elisp
(tp-layer-props-with-args LAYER-NAME ARGS &optional INCLUDE-TP-NAME)
(tp-group-props-with-args GROUP-NAME ARGS &optional INCLUDE-TP-NAME)
(tp-layer-arglist LAYER-NAME)
```

针对**参数化**属性层和属性层组的自省函数（0.3.0 新增）：

- **`tp-layer-props-with-args`** 用 ARGS 展开参数化属性层，ARGS 是按位
  置绑定到层参数的值列表。多余的值会被忽略；值的数量少于参数数量时会发
  出参数个数不符的错误信号。返回一个全新的副本（修改它不会破坏注册
  表）；对无参数或未定义的层返回 nil。原有的单参数
  `tp-layer-props-with-arg`（注意名称只差一个字符）保留为一个薄薄的
  `(list ARG)` 包装。
- **`tp-group-props-with-args`** 是层组版本，返回展开后的逐层 plist 列
  表；`tp-group-props-with-arg` 保留为单参数便捷形式。
- **`tp-layer-arglist`** 返回层参数列表的副本；当 LAYER-NAME 不是参数
  化层时返回 nil。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp tp-colors (fg bg)
    `(face (:foreground ,fg :background ,bg)))
  (tp-layer-props-with-args 'tp-colors '("red" "blue")))
;; => (face (:foreground "red" :background "blue"))

;; 参数列表本身
(tp-layer-arglist 'tp-colors)
;; => (fg bg)

;; 层组展开为每层一个 plist
(progn
  (define-tps tp-badge (fg bg)
    `(tp-colors ,fg ,bg)
    '(face bold))
  (tp-group-props-with-args 'tp-badge '("white" "black")))
;; => ((face (:foreground "white" :background "black")) (face bold))

;; 参数太少时发出与 tp-set 相同的明确错误信号
(tp-layer-props-with-args 'tp-colors '("red"))
;; error: tp layer tp-colors takes 2 argument(s), got 1
```

---

#### `tp-describe-layer` - 描述属性层

```elisp
(tp-describe-layer NAME)   ; interactive
```

弹出一个帮助缓冲区，描述属性层 NAME（交互式调用时可在所有已注册层中补
全）。该缓冲区会展示存储格式（flat / unified / parameterized /
reactive）、原始存储的 body、展开后的属性（参数化层需要参数，因此显示
占位说明）、参数列表、该层依赖的响应式变量、是否注册了 transform，以及
生成该层的层组（如果有）。

```elisp
(progn
  (tp-layer-reset)
  (define-tp tp-colors (fg bg)
    `(face (:foreground ,fg :background ,bg)))
  (tp-describe-layer 'tp-colors))
;; 弹出一个 *Help* 缓冲区:
;;   tp-colors is a tp layer.
;;
;;   Storage format: parameterized
;;   Arguments:      (fg bg)
;;   Stored body:    `(face (:foreground ,fg :background ,bg))
;;   Expanded props: parameterized layer: expand with `tp-layer-props-with-args'
;;   Reactive deps:  none
;;   Transform:      no
```

---

#### `tp-undefine-layer` / `tp-undefine-group`

```elisp
(tp-undefine-layer NAME)
(tp-undefine-group NAME)
```

移除属性层或属性层组定义。

**示例：**

```elisp
;; 取消定义属性层
(progn
  (tp-layer-reset)
  (define-tp temp-layer ()
    '(face bold))
  (tp-undefine-layer 'temp-layer)
  (tp-layer-props 'temp-layer))
;; => nil

;; 取消定义属性层组
(progn
  (tp-layer-reset)
  (define-tp l1 () '(face bold))
  (define-tps my-group ()
    'l1)
  (tp-undefine-group 'my-group)
  (assoc 'my-group tp-layer-groups))
;; => nil
```

---

#### `tp-layer-reset`

```elisp
(tp-layer-reset)
```

清除所有属性层和属性层组定义，包括所有响应式依赖和监听器。

**示例：**

```elisp
(progn
  (define-tp test-layer () '(face bold))
  (tp-layer-reset)
  (list tp-layer-alist tp-layer-groups))
;; => (nil nil)
```

---

#### `tp-reactive-reset`

```elisp
(tp-reactive-reset)
```

清除所有响应式文本属性的监听器和依赖关系，但不影响层定义。

当你想要移除所有响应式绑定但保留层定义时，这个函数很有用。

**示例：**

```elisp
;; 定义一个响应式层
(progn
  (defvar my-reactive-color "red")
  (define-tp reactive-layer ()
  :props '(face (:foreground $my-reactive-color)))
  ;; 仅清除响应式绑定
  (tp-reactive-reset)
  ;; 层仍然存在，但改变 my-reactive-color 不再更新它
  (tp-layer-props 'reactive-layer))
;; => (face (:foreground "red"))
```

---

### 属性层放置

> ⚠️ **栈操作的字符串形式会就地修改字符串。**与返回**新**属性字符串的
> `tp-set` 不同，每一个栈修改函数（`tp-put-layer`、`tp-push-layer`、
> `tp-pop-layer`、`tp-delete-layer`、`tp-move-layer`、`tp-raise-layer`、
> `tp-lower-layer`、`tp-rotate-layer`、`tp-pin-layer`、
> `tp-switch-layer`、`tp-hide-layer`、`tp-show-layer`、
> `tp-merge-layers`、`tp-flatten-layers`、`tp-add-to-layers`、
> `tp-add-to-all-layers`）的字符串形式都会**破坏性地**修改 STRING。绝
> 不要传入字符串字面量或不属于你的共享字符串 —— 请先用
> `copy-sequence`。将这一行为与 `tp-set` 的复制语义统一已列入 0.4 计划。

**返回值（0.3.0）：**`tp-put-layer` / `tp-push-layer` 在给定 OBJECT 时
返回 OBJECT（字符串形式返回该字符串本身），否则返回 `(START . END)`。
其余每个栈修改函数都返回**被修改的属性区段数量**；层名或索引不存在时
从不发出错误信号 —— 未匹配的区段被静默跳过，返回 0 表示没有任何匹配。

#### `tp-put-layer` - 在指定位置设置属性层

```elisp
;; 缓冲区/字符串区域
(tp-put-layer START END LAYER IDX OBJECT NOERROR)

;; 整个字符串
(tp-put-layer STRING LAYER IDX NOERROR)
```

在属性层堆栈的指定索引位置设置属性层。

- `IDX = 0`：顶部（可见属性层）
- `IDX = -1`：底部
- 其他值在该位置插入

LAYER 接受以下几种形式：

- 用 `define-tp` 定义的层名：`'highlight`
- 内联属性 plist（无需 `define-tp`）：`'(face bold help-echo "tip")`
- 层名列表（第一个层名位于顶部）：`'(layer-a layer-b)`
- 参数化层调用：`'(tp-color "red")` —— 多参数层同样可用：
  `'(tp-colors "white" "black")`

**栈模型：**只有顶层的属性是可见的文本属性；下层被保存在
`tp-layers` 文本属性中，直到被上移、轮换或扁平化。

**NOERROR（0.3.0 新增）：**LAYER 指向未定义的层或层组时，通常会发出错
误信号。NOERROR 非 nil 时，调用改为返回 nil 且不做任何修改 —— 在应用
可能尚未定义的层时非常方便。`tp-push-layer` 接受同样的末尾 NOERROR 参
数。

**示例：**

```elisp
;; 将 base 属性层放在顶部
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 'base 0)
    (tp-at 1 'tp-name)))
;; => base

;; 将 highlight 放在索引 1（顶部下面）
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 'base 0)
    (tp-put-layer 1 10 'highlight 1)
    (tp-layer-count 1 10)))
;; => 2

;; 将属性层放在底部
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp info () '(face (:foreground "blue")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 'base 0)
    (tp-put-layer 1 10 'info -1)
    (tp-layer-top 1 10)))
;; => base  ; info 在底部，base 可见

;; 内联 plist - 无需 define-tp
(with-temp-buffer
  (insert "Hello World")
  (tp-put-layer 1 10 '(face bold help-echo "tip") 0)
  (list (tp-at 1 'face) (tp-at 1 'help-echo)))
;; => (bold "tip")

;; 层名列表 - layer-a 位于顶部
(progn
  (tp-layer-reset)
  (define-tp layer-a () '(face bold))
  (define-tp layer-b () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 '(layer-a layer-b) 0)
    (list (tp-at 1 'face) (tp-layer-list 1 10))))
;; => (bold (layer-a layer-b))

;; 参数化层调用
(progn
  (tp-layer-reset)
  (define-tp tp-color (color)
    `(face (:foreground ,color)))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 '(tp-color "red") 0)
    (tp-at 1 'face)))
;; => (:foreground "red")

;; NOERROR - 未定义的层名返回 nil 而不发出错误信号
(with-temp-buffer
  (insert "Hello World")
  (tp-put-layer 1 10 'no-such-layer 0 nil t))
;; => nil  ; 没有任何修改
```

---

#### `tp-push-layer` - 推送属性层到顶部

```elisp
;; 缓冲区/字符串区域
(tp-push-layer START END LAYER OBJECT NOERROR)

;; 整个字符串
(tp-push-layer STRING LAYER NOERROR)
```

将属性层推到堆栈顶部（相当于 `tp-put-layer ... 0`）。
NOERROR（0.3.0 新增）的用法与
[`tp-put-layer`](#tp-put-layer---在指定位置设置属性层) 相同：未定义的
LAYER 返回 nil 而不发出错误信号。

**示例：**

```elisp
;; 首先推入 base 属性层
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-at 1 'tp-name)))
;; => base

;; 将 highlight 推到顶部（现在可见）
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-at 1 'tp-name)))
;; => highlight

;; 顶层的属性是可见的；下层保存在 `tp-layers' 中等待
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (list :face (tp-at 1 'face)
          :top (tp-layer-top 1 10)
          :layers (tp-layer-list 1 10)
          :hidden (length (tp-at 1 'tp-layers)))))
;; => (:face (:background "yellow") :top highlight :layers (highlight base) :hidden 1)
```

---

### 属性层删除

#### `tp-delete-layer` - 按名称/索引删除属性层

```elisp
;; 缓冲区/字符串区域
(tp-delete-layer START END LAYER-NAME/IDX OBJECT)

;; 整个字符串
(tp-delete-layer STRING LAYER-NAME/IDX)
```

通过名称或索引从堆栈任意位置删除属性层。

**示例：**

```elisp
;; 按名称删除
(progn
  (tp-layer-reset)
  (define-tp highlight () '(face (:background "yellow")))
  (define-tp base () '(face default))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-delete-layer 1 10 'highlight)
    (tp-at 1 'tp-name)))
;; => base

;; 删除顶层（idx=0）
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-delete-layer 1 10 0)
    (tp-at 1 'tp-name)))
;; => layer1

;; 删除底层
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-delete-layer 1 10 -1)
    (tp-layer-count 1 10)))
;; => 1
```

---

#### `tp-pop-layer` - 弹出顶层

```elisp
;; 缓冲区/字符串区域
(tp-pop-layer START END OBJECT)

;; 整个字符串
(tp-pop-layer STRING)
```

删除顶层（相当于 `tp-delete-layer ... 0`）。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-pop-layer 1 10)
    (tp-at 1 'tp-name)))
;; => layer1
```

---

### 属性层移动

#### `tp-move-layer` - 移动属性层到指定位置

```elisp
;; 缓冲区/字符串区域
(tp-move-layer START END FROM-ID TO-IDX OBJECT)

;; 整个字符串
(tp-move-layer STRING FROM-ID TO-IDX)
```

将属性层从一个位置移动到另一个位置。

- `FROM-ID` 标识要移动的层：可以是整数索引或层名称符号
- `TO-IDX` 是目标位置（整数索引）
- 索引 0 表示顶层（可见），-1 表示底层
- 两个索引都是指移动之前的位置

这是通用的属性层移动函数，`tp-raise-layer`、`tp-rotate-layer`、`tp-pin-layer` 和 `tp-switch-layer` 内部都使用它来实现。

**示例：**

```elisp
;; 将索引 2 的层移动到索引 0（顶部）
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (define-tp layer3 () '(face underline))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-push-layer 1 10 'layer3)
    ;; 堆栈: layer3 (0), layer2 (1), layer1 (2)
    (tp-move-layer 1 10 2 0)
    (tp-layer-top 1 10)))
;; => layer1

;; 按名称移动层到底部
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; 堆栈: layer2 (顶), layer1 (底)
    (tp-move-layer 1 10 'layer2 -1)
    (tp-layer-top 1 10)))
;; => layer1

;; 在字符串上移动
(let ((str (copy-sequence "Hello")))
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (tp-push-layer str 'layer1)
  (tp-push-layer str 'layer2)
  ;; layer2 在顶部
  (tp-move-layer str 'layer1 0)
  (tp-at 0 'tp-name str))
;; => layer1
```

---

#### `tp-raise-layer` - 上移/下移属性层

```elisp
;; 缓冲区/字符串区域
(tp-raise-layer START END IDX/LAYER-NAME N OBJECT)

;; 整个字符串
(tp-raise-layer STRING IDX/LAYER-NAME N)
```

将属性层上移 N 个位置。正数 N 向顶部移动，负数向底部移动。

**示例：**

```elisp
;; 将 layer1 上移 2 个位置（到顶部）
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (define-tp layer3 () '(face underline))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-push-layer 1 10 'layer3)
    ;; 堆栈: layer3 (顶), layer2, layer1 (底)
    (tp-raise-layer 1 10 'layer1 2)
    (tp-layer-top 1 10)))
;; => layer1

;; 将索引 0 的属性层下移 1 个位置
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; 堆栈: layer2 (idx 0), layer1 (idx 1)
    (tp-raise-layer 1 10 0 -1)
    (tp-layer-top 1 10)))
;; => layer1
```

---

#### `tp-lower-layer` - `tp-raise-layer` 的镜像

```elisp
;; 缓冲区/字符串区域
(tp-lower-layer START END IDX/LAYER-NAME N OBJECT)

;; 整个字符串
(tp-lower-layer STRING IDX/LAYER-NAME N)
```

将属性层下移 N 个位置（0.3.0 新增）。它是 `tp-raise-layer` 的镜像：正
数 N 向底部移动，负数 N 向顶部移动。N 默认为 1，最终位置会被钳制在栈的
范围内。返回被修改的属性区段数量。

**示例：**

```elisp
;; 将顶层下移一个位置
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (define-tp layer3 () '(face underline))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-push-layer 1 10 'layer3)
    ;; 堆栈: layer3 (顶), layer2, layer1 (底)
    (tp-lower-layer 1 10 'layer3 1)
    ;; 堆栈: layer2 (顶), layer3, layer1 (底)
    (list (tp-layer-top 1 10) (tp-layer-list 1 10))))
;; => (layer2 (layer2 layer3 layer1))
```

---

#### `tp-rotate-layer` - 轮换属性层

```elisp
;; 缓冲区/字符串区域（规范顺序，OBJECT 在最后 - 0.3.0 新增）
(tp-rotate-layer START END DIRECTION &optional COUNT OBJECT)

;; 整个字符串
(tp-rotate-layer STRING DIRECTION COUNT)

;; 缓冲区/字符串区域（历史顺序，永远保持可用）
(tp-rotate-layer START END OBJECT)
```

将属性层轮换 COUNT 步，保持它们的相对顺序。

- **DIRECTION** 为 `down` 或 nil 时将顶层移到底部（历史行为），为
  `up` 时将底层带到顶部；其他值会发出错误信号。
- **COUNT** 是轮换的步数，默认为 1；COUNT 小于 1 时不做任何轮换。隐藏
  层随栈中其他层一起轮换。
- 返回被修改的属性区段数量。

两种区域顺序通过第三个参数区分：符号 `up` / `down` 永远不是合法的
OBJECT，因此 `(tp-rotate-layer 1 5 'up)` 会无歧义地选中规范的
`(START END DIRECTION [COUNT] [OBJECT])` 顺序 —— 无需 nil OBJECT 占位。
第三个参数为其他值（缓冲区、字符串，或表示当前缓冲区的 nil）时选中历
史的 `(START END OBJECT [DIRECTION] [COUNT])` 顺序，后者继续可用。

**示例：**

```elisp
;; 堆栈: highlight (顶) -> base (底)
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    ;; 堆栈: highlight (顶) -> base (底)
    (tp-rotate-layer 1 10)
    ;; 堆栈: base (顶) -> highlight (底)
    (tp-layer-top 1 10)))
;; => base

;; 规范顺序：`up' 将底层带到顶部
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (define-tp layer3 () '(face underline))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-push-layer 1 10 'layer3)
    ;; 堆栈: layer3 (顶), layer2, layer1 (底)
    (tp-rotate-layer 1 10 'up)
    (tp-layer-list 1 10)))
;; => (layer1 layer3 layer2)

;; COUNT 一次轮换多步
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (define-tp layer3 () '(face underline))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-push-layer 1 10 'layer3)
    (tp-rotate-layer 1 10 'down 2)
    (tp-layer-list 1 10)))
;; => (layer1 layer3 layer2)
```

---

#### `tp-pin-layer` - 将属性层置顶

```elisp
;; 缓冲区/字符串区域
(tp-pin-layer START END IDX/LAYER-NAME OBJECT)

;; 整个字符串
(tp-pin-layer STRING IDX/LAYER-NAME)
```

将属性层移到栈顶。**一次性操作**：尽管名字里有 pin，但没有任何东西会
保持"钉住"状态 —— 这只是一次移动到索引 0 的操作，之后的
`tp-push-layer` 或 `tp-put-layer` 仍然可以覆盖被移动的层。

**示例：**

```elisp
;; 将 'base 设为顶层
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    ;; highlight 在顶部
    (tp-pin-layer 1 10 'base)
    (tp-layer-top 1 10)))
;; => base
```

---

#### `tp-switch-layer` - 交换两个属性层

```elisp
;; 缓冲区/字符串区域
(tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)

;; 整个字符串
(tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)
```

交换两个属性层的位置。

**示例：**

```elisp
;; 交换 layer1 和 layer2
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; layer2 在顶部
    (tp-switch-layer 1 10 'layer1 'layer2)
    ;; 现在 layer1 在顶部
    (tp-layer-top 1 10)))
;; => layer1
```

---

### 属性层可见性

#### `tp-hide-layer` / `tp-show-layer` - 隐藏与显示属性层

```elisp
;; 缓冲区/字符串区域
(tp-hide-layer START END NAME OBJECT)
(tp-show-layer START END NAME OBJECT)

;; 整个字符串
(tp-hide-layer STRING NAME)
(tp-show-layer STRING NAME)
```

隐藏属性层而不移除它，以及让它重新渲染（0.3.0 新增）。NAME 标识属性
层：层名符号，或指向完整栈（包含隐藏层）的整数索引（0 = 顶层，-1 =
底层）。

**可见性模型：**

- 隐藏层**仍留在栈中**：它仍计入 `tp-layer-count`，出现在
  `tp-layer-list` 和 `tp-layer-stack-at` 中，也可以被移动、上移或下移
  —— 但它不渲染。文本改为展示最顶部**未隐藏**层的属性。
- 因此隐藏当前可见的顶层会显露它下面的下一个可见层。
- 当**所有**层都被隐藏时，文本以裸文本渲染（只剩 `tp-layers` 这个簿记
  属性 —— 连 `tp-name` 也不渲染），同时所有层仍然可查询。
- 隐藏层在隐藏期间**持续接收响应式更新**，因此 `tp-show-layer` 总是显
  露最新的值（参见[层-缓冲区注册表与生命周期](#层-缓冲区注册表与生命周期)）。
- `tp-flatten-layers` 只合并可见层，`tp-merge-layers` 排除隐藏的匹配层
  的属性 —— 隐藏的内容绝不会泄漏（参见[属性层合并](#属性层合并)）。
- 隐藏状态以 `tp-hidden` 标志的形式存储在 `tp-layers` 栈存储内该层的
  plist 中，因此 `tp-hidden` 与 `tp-name` 一样是层内部的保留属性名。

两个函数都返回被修改的属性区段数量。NAME 不匹配任何层时从不发出错误信
号，隐藏一个已隐藏的层（或显示一个可见的层）是静默的空操作 —— 返回 0
表示没有任何变化。

**示例：**

```elisp
;; 隐藏顶层会显露下面的层；栈保持完整
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'highlight)
    (list :visible (tp-at 1 'tp-name)
          :face (tp-at 1 'face)
          :count (tp-layer-count 1 10)
          :layers (tp-layer-list 1 10))))
;; => (:visible base :face default :count 2 :layers (highlight base))

;; 所有层都隐藏时文本以裸文本渲染
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'base)
    (list :face (tp-at 1 'face) :count (tp-layer-count 1 10))))
;; => (:face nil :count 2)

;; tp-show-layer 恢复该层的渲染
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'highlight)
    (tp-show-layer 1 10 'highlight)
    (tp-at 1 'face)))
;; => (:background "yellow")

;; 返回值：被修改的区段数量；名称不存在时静默返回 0
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (list (tp-hide-layer 1 10 'base)
          (tp-hide-layer 1 10 'base)          ; 已经隐藏
          (tp-hide-layer 1 10 'nonexistent)))) ; 没有这个层
;; => (1 0 0)
```

---

### 属性层合并

#### `tp-merge-layers` - 合并多个属性层

```elisp
;; 缓冲区/字符串区域
(tp-merge-layers START END NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)

;; 整个字符串
(tp-merge-layers STRING NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...))
```

将指定的属性层合并为一个新属性层。列表中靠前的属性层优先级更高。

**隐藏层（0.3.0）：**隐藏的匹配层会和其他层一起被合并掉，但**不**向合
并层贡献任何属性，因此合并绝不会渲染出被隐藏的内容。当*所有*匹配层都
被隐藏时，合并层保留它们合并后的属性，但自身携带 `tp-hidden` 标志 ——
数据被保留而没有取消任何隐藏，对合并层执行 `tp-show-layer` 即可渲染
它。返回被修改的属性区段数量（0 = 列出的层都没有匹配）。

**示例：**

```elisp
;; 将 layer1 和 layer2 合并为 merged-layer
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(help-echo "tip"))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-merge-layers 1 10 'merged-layer '(layer1 layer2))
    (tp-at 1 'tp-name)))
;; => merged-layer

;; 按索引合并
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(help-echo "tip"))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-merge-layers 1 10 'merged '(0 1))
    (tp-layer-count 1 10)))
;; => 1

;; 隐藏层的属性绝不会泄漏进合并结果
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(help-echo "tip"))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-hide-layer 1 10 'layer2)
    (tp-merge-layers 1 10 'merged '(layer1 layer2))
    (list :face (tp-at 1 'face)
          :help (tp-at 1 'help-echo)
          :name (tp-at 1 'tp-name))))
;; => (:face bold :help nil :name merged)  ; layer2 被隐藏了
```

---

#### `tp-flatten-layers` - 扁平化所有属性层

```elisp
;; 缓冲区/字符串区域
(tp-flatten-layers START END NAME OBJECT)

;; 整个字符串
(tp-flatten-layers STRING NAME)
```

将所有属性层扁平化为一个具有给定名称的单一属性层。

**隐藏层（0.3.0）：**隐藏层会被**丢弃**，与图像编辑器的扁平化语义一致
—— 只有可见层的属性会合并进结果，因此扁平化绝不会渲染出被隐藏的内容。
当某个区段的*所有*层都被隐藏时，该区段的属性会被完全清除（裸文本），
与 `tp-hide-layer` 的全隐藏渲染行为一致。返回被修改的属性区段数量
（0 = 没有区段带有属性层）。

**示例：**

```elisp
;; 将所有属性层扁平化为 'flat-layer
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(help-echo "tip"))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-flatten-layers 1 10 'flat-layer)
    (tp-at 1 'tp-name)))
;; => flat-layer

;; 使用 nil 名称扁平化（无名属性层）
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-flatten-layers 1 10 nil)
    (tp-at 1 'tp-name)))
;; => nil

;; 扁平化会丢弃隐藏层
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'highlight)
    (tp-flatten-layers 1 10 'flat)
    (list (tp-at 1 'face) (tp-at 1 'tp-name))))
;; => (default flat)  ; highlight 的背景色消失了
```

---

### 属性层查询函数

#### `tp-layer-list` - 列出所有属性层

```elisp
(tp-layer-list START END &optional OBJECT)
```

获取区域中所有属性层名称的列表。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp highlight () '(face (:background "yellow")))
  (define-tp base () '(face default))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-layer-list 1 10)))
;; => (highlight base)
```

---

#### `tp-layer-count`

```elisp
(tp-layer-count START END &optional OBJECT)
```

计算区域中的属性层数量。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-layer-count 1 10)))
;; => 2
```

---

#### `tp-layer-exists-p`

```elisp
(tp-layer-exists-p START END NAME &optional OBJECT)
```

检查区域中是否存在某属性层。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (list (tp-layer-exists-p 1 10 'layer1)
          (tp-layer-exists-p 1 10 'layer2))))
;; => (t nil)
```

---

#### `tp-layer-top`

```elisp
(tp-layer-top START END &optional OBJECT)
```

获取顶层属性层的名称。最顶部的层按**栈序**报告，即使它被隐藏（参见
[`tp-hide-layer`](#tp-hide-layer--tp-show-layer---隐藏与显示属性层)）；
要区分隐藏层和可见层，请使用 `tp-layer-stack-at`。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    (tp-layer-top 1 10)))
;; => layer2
```

---

#### `tp-layer-stack-at` - 获取某位置的完整层栈

```elisp
(tp-layer-stack-at POS &optional OBJECT)
```

返回某一位置上完整的有序层栈（0.3.0 新增），列表中每层一个元素，最顶
层在前，每个元素是一个 cons `(NAME . PROPS)`：

- **NAME** 是层的 `tp-name` 符号，无名层为 nil。
- **PROPS** 是层的属性 plist，其中不含 `tp-name` 条目。隐藏层可通过
  PROPS 中值为 t 的 `tp-hidden` 条目辨认；可见层永远不携带该条目。

隐藏层按其栈位置包含在内。裸文本返回 nil。POS 使用 OBJECT 的原生坐标
（字符串从 0 开始，缓冲区从 1 开始）；OBJECT 是字符串、缓冲区，或表示
当前缓冲区的 nil。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-layer-stack-at 1)))
;; => ((highlight . (face (:background "yellow")))
;;     (base . (face default)))

;; 隐藏层在 PROPS 中携带 `tp-hidden' 条目
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    (tp-hide-layer 1 10 'highlight)
    (tp-layer-stack-at 1)))
;; => ((highlight . (tp-hidden t face (:background "yellow")))
;;     (base . (face default)))

;; 裸文本没有层栈
(with-temp-buffer
  (insert "Hello")
  (tp-layer-stack-at 1))
;; => nil
```

---

#### `tp-add-to-layers` - 向特定属性层添加属性

```elisp
;; 缓冲区/字符串区域
(tp-add-to-layers IDX-OR-LAYER-NAME-LIST START END PLIST &optional OBJECT)

;; 整个字符串
(tp-add-to-layers IDX-OR-LAYER-NAME-LIST STRING PROP VAL ...)
```

向区域或字符串中的特定属性层添加或合并属性。

- **IDX-OR-LAYER-NAME-LIST** 是层索引（整数）或层名称（符号）的列表。对于索引：0 表示顶层，-1 表示底层。
- 属性被深度合并到指定的层中（嵌套的 plist 被合并，而非替换）。
- OBJECT 在区域形式中默认为当前缓冲区。
- 与其他栈修改函数一样（且与 `tp-set` 不同），字符串形式会**就地**修
  改 STRING 并返回这个被修改的字符串本身。对于缓冲区，返回 nil。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face (:foreground "red")))
  (define-tp layer2 () '(face (:foreground "blue")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; 向两个层添加下划线
    (tp-add-to-layers '(0 1) 1 10 '(face (:underline t)))
    (tp-at 5)))
;; 两个层现在都有下划线与其颜色合并
```

---

#### `tp-add-to-all-layers` - 向所有属性层添加属性

```elisp
;; 缓冲区/字符串区域
(tp-add-to-all-layers START END PLIST &optional OBJECT)

;; 整个字符串
(tp-add-to-all-layers STRING PROP VAL ...)
```

向区域或字符串中的所有属性层添加或合并属性。

- 属性被深度合并到所有现有层中。
- OBJECT 在区域形式中默认为当前缓冲区。
- 与其他栈修改函数一样（且与 `tp-set` 不同），字符串形式会**就地**修
  改 STRING 并返回这个被修改的字符串本身。对于缓冲区，返回 nil。

**示例：**

```elisp
(let ((str (copy-sequence "Hello World")))
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (tp-push-layer 0 5 'layer1 str)
  (tp-push-layer 0 5 'layer2 str)
  ;; 向所有层添加下划线
  (tp-add-to-all-layers 0 5 '(face (:underline t)) str)
  str)
```

---

#### `tp-intervals` - 获取文本属性区间

```elisp
(tp-intervals START END &optional OBJECT ABSOLUTE)
```

从 OBJECT 中获取 START 到 END 之间的所有文本属性区间。

- 返回每个区间的 (START END PROPERTIES) 列表，包括没有属性的
  间隙区间，其 PROPERTIES 为 nil。
- 对于缓冲区输入，START 和 END 是从 1 开始的缓冲区位置，但返回的位置
  默认是**相对于 START 的 0 基偏移量**（历史约定）。ABSOLUTE 非 nil
  时（0.3.0 新增），返回的位置改为原生的 1 基缓冲区位置，可以不做偏移
  运算直接用于其他 tp 调用（`tp-set`、`tp-remove` 等）。对于字符串，
  位置始终是绝对的 0 基索引；ABSOLUTE 不改变任何行为。
- 使用 `object-intervals`（需要 Emacs 28.1+）。
- OBJECT 可以是缓冲区或字符串；nil 默认为当前缓冲区。

**示例：**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals 1 12))
;; => ((0 5 (face bold)) (5 6 nil) (6 11 (face italic)))
;;    位置是相对 START 的偏移量；(5 6 nil) 是无属性的间隙

;; ABSOLUTE - 原生缓冲区坐标
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals 1 12 nil t))
;; => ((1 6 (face bold)) (6 7 nil) (7 12 (face italic)))

;; ABSOLUTE 位置可直接回馈给其他 tp 调用
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (dolist (iv (tp-intervals 1 12 nil t))
    (when (eq (plist-get (nth 2 iv) 'face) 'bold)
      (tp-add (nth 0 iv) (nth 1 iv) '(help-echo "bold text"))))
  (tp-at 1 'help-echo))
;; => "bold text"
```

---

#### `tp-intervals-map` - 对区间应用函数

```elisp
(tp-intervals-map FUNCTION START END &optional OBJECT ABSOLUTE)
```

对 OBJECT 中 START 到 END 之间的所有区间应用 FUNCTION。

- FUNCTION 接收四个参数：interval-start、interval-end、top-props（直
  接渲染的属性，其中的 `tp-layers` 条目已被移除）和 below-props-lst
  （`tp-layers` 的值：埋在被渲染顶层之下的层 plist 存储 —— 当任何层被
  隐藏时它保存整个有序层栈；解码后的视图参见
  [`tp-layer-stack-at`](#tp-layer-stack-at---获取某位置的完整层栈)）。
- 没有属性的区间也会被访问，此时 top-props 为 nil（位置遵循与
  `tp-intervals` 相同的坐标约定，包括 0.3.0 新增的 ABSOLUTE 参数）。
- OBJECT 可以是缓冲区或字符串；nil 默认为当前缓冲区。
- 返回函数结果列表（nil 结果被移除）。

**示例：**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals-map
   (lambda (start end props belows)
     (list start end (plist-get props 'face)))
   1 12))
;; => ((0 5 bold) (5 6 nil) (6 11 italic))

;; ABSOLUTE - FUNCTION 接收原生缓冲区位置
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals-map
   (lambda (start end props belows)
     (list start end (plist-get props 'face)))
   1 12 nil t))
;; => ((1 6 bold) (6 7 nil) (7 12 italic))
```

---

#### `tp-region-layer-props` - 获取区域中的层属性

```elisp
(tp-region-layer-props START END LAYER-NAME &optional OBJECT)
```

返回区域 START 到 END 中 LAYER-NAME 的层属性。

- 返回匹配区间的 (START END PROPERTIES) 列表。
- OBJECT 默认为当前缓冲区。

**示例：**

```elisp
(progn
  (tp-layer-reset)
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World Test")
    (tp-push-layer 1 6 'highlight)
    (tp-push-layer 12 16 'highlight)
    (tp-region-layer-props 1 16 'highlight)))
;; => ((1 6 (face (:background "yellow") tp-name highlight))
;;     (12 16 (face (:background "yellow") tp-name highlight)))
```

---

#### `tp-plist` - 获取区域中的所有属性

```elisp
;; 缓冲区/字符串区域
(tp-plist START END &optional OBJECT)

;; 整个字符串
(tp-plist STRING)
```

获取区域或字符串中存在的所有属性的属性列表。

- 返回将范围内找到的属性合并成的单个 plist；当同一属性出现在多个
  区间中时，靠后区间的值胜出。
- OBJECT 在区域形式中默认为当前缓冲区。

**示例：**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold help-echo "Tip"))
  (tp-set 7 12 '(face italic))
  (tp-plist 1 12))
;; => (help-echo "Tip" face italic)  ; 靠后区间的 face 胜出
```

---

#### `tp-empty-p` - 检查对象是否有属性

```elisp
(tp-empty-p &optional OBJECT)
```

如果 OBJECT 没有文本属性，返回 t。

- OBJECT 可以是字符串或缓冲区；nil 默认为当前缓冲区。
- 使用 `object-intervals`（需要 Emacs 28.1+）。

**示例：**

```elisp
(tp-empty-p "plain text")  ; => t

;; 整个字符串形式的 tp-set 是非破坏性的：原始字符串保持无属性
(let* ((str "text")
       (new (tp-set str 'face 'bold)))
  (list (tp-empty-p str) (tp-empty-p new)))
;; => (t nil)
```

---

#### `tp-with-current-buffer` / `tp-pop-to-buffer` / `tp-switch-to-buffer`

```elisp
(tp-with-current-buffer BUFFER-OR-NAME BODY...)
(tp-pop-to-buffer BUFFER-OR-NAME BODY...)
(tp-switch-to-buffer BUFFER-OR-NAME BODY...)
```

用于操作和展示带属性内容的便捷宏：

- **`tp-with-current-buffer`** 在 BUFFER-OR-NAME 中求值 BODY，并将
  `inhibit-read-only` 绑定为 t。适合修改只读的展示缓冲区。
- **`tp-pop-to-buffer`** 创建（或复用）BUFFER-OR-NAME，清空它，在其中
  求值 BODY，然后将其设为只读并通过 `pop-to-buffer` 显示。在显示的
  缓冲区中按 `q` 可退出其窗口。
- **`tp-switch-to-buffer`** 与上者相同，但通过 `switch-to-buffer`
  显示缓冲区。

**示例：**

```elisp
(tp-pop-to-buffer "*tp-demo*"
  (insert (tp-set "Important" 'face '(:foreground "red" :weight bold))
          " message\n"))
;; 显示 *tp-demo* 及其中的带属性文本；按 `q' 退出窗口
```

---

### 调色板系统

`tp-palette.el` 内置了一组具名调色板，每个调色板包含独立的亮色模式和
暗色模式颜色；`tp-builtins.el` 通过内置的参数化 `tp-palette` 层将它们
暴露出来（如 `(tp-set "emacs" 'tp-palette 'info)`）。

- **`tp-palette-alist`**（变量）— `(NAME . PLIST)` 形式的调色板定义
  alist；调色板查询的唯一数据源。每个 PLIST 将 `:fg`、`:bg` 和
  `:border` 映射到颜色。
- **`define-tp-palette`** — 注册（或更新）一个调色板（自 0.3.0 起也可
  使用符合前缀规范的别名 `tp-define-palette`）：

  ```elisp
  (define-tp-palette my-brand
    :fg ("#0969da" . "#58a6ff")     ; ("亮色" . "暗色")
    :bg ("#ddf4ff" . "#1f3d5c"))
  ```

- **`tp-palette-color`**（0.3.0 新增）— **首选的**调色板访问器：获取调
  色板的 `:fg` / `:bg` / `:border` 颜色，按当前亮色/暗色主题解析。调色
  板或键不存在时返回 nil：

  ```elisp
  (tp-palette-color 'info :fg)
  ;; => 亮色主题下为 "#0969da"，暗色主题下为 "#58a6ff"
  (tp-palette-color 'no-such-palette :fg)
  ;; => nil
  ```

- **`tp-palette-has-p`**（0.3.0 新增）— **首选的**调色板谓词：只传
  SYMBOL 时测试它是否命名了一个已注册的调色板；KIND 为 `:fg` / `:bg` /
  `:border` 之一时，还要求其定义中含有该键（已定义的键在当前主题下仍
  可能解析不出颜色 —— 在意解析后颜色时请使用 `tp-palette-color`）：

  ```elisp
  (list (tp-palette-has-p 'info)
        (tp-palette-has-p 'info :border)
        (tp-palette-has-p 'no-such-palette))
  ;; => (t t nil)
  ```

  旧的按键便捷函数保留为兼容包装：`tp-palette-fg-color` /
  `tp-palette-bg-color` / `tp-palette-border-color`（`tp-palette-color`
  的固定 KEY 变体）、`tp-palette-p`（KIND 为 nil 的
  `tp-palette-has-p`），以及带后缀名的谓词 `tp-palette-fg-p` /
  `tp-palette-bg-p` / `tp-palette-fbg-p` / `tp-palette-border-p` —— 它
  们回答的是另一个问题：像 `info-fg` 这样的*变体名*是否表示一个已注册
  的调色板（`tp-palette` 层使用的约定）。

- **`tp-palette-show`** — 交互式命令，显示一个画廊缓冲区，展示每个已
  注册调色板及其 `-fg` / `-bg` / `-fbg` / `-border` 变体（按 `q` 退出）。
- **`tp-parse-color`** — 按当前主题解析颜色规格。接受普通颜色字符串、
  `("亮色" . "暗色")` cons（任意一侧可以为 nil），或
  `(:light L :dark D)` plist：

  ```elisp
  (tp-parse-color "red")                 ; => "red"
  (tp-parse-color '("white" . "black"))  ; => 亮色主题下为 "white"，
                                         ;    暗色主题下为 "black"
  ```

注意：`tp-layer-reset` 会清除所有属性层定义，包括 `tp-palette` 这样的
内置属性层。

---

## 实用示例

### 多属性层语法高亮

```elisp
;; 可以在缓冲区中运行的完整示例
(progn
  (tp-layer-reset)
  ;; 为不同高亮目的定义属性层
  (define-tp code-base ()
  '(face font-lock-keyword-face))
  (define-tp code-error ()
  '(face (:underline (:color "red" :style wave))
     help-echo "语法错误"))
  (define-tp code-debug ()
  '(face (:background "dark blue")))
  (with-temp-buffer
    (insert (make-string 100 ?x))  ; 创建 100 字符缓冲区
    ;; 应用基础高亮
    (tp-push-layer 1 100 'code-base)
    ;; 在有问题的代码上添加错误高亮
    (tp-push-layer 50 60 'code-error)
    ;; 检查位置 55 的顶层
    (tp-layer-top 50 60)))
;; => code-error

;; 切换函数（用于实际缓冲区）
(defun toggle-error-view (start end)
  "在错误和正常视图之间切换。"
  (interactive "r")
  (tp-rotate-layer start end))
```

### 状态指示器

```elisp
;; 包含属性层组的完整示例
(progn
  (tp-layer-reset)
  ;; 将状态属性层定义为一个组
  (define-tp status-todo () '(face (:foreground "gray")))
  (define-tp status-progress () '(face (:foreground "yellow")))
  (define-tp status-done () '(face (:foreground "green")))
  (define-tps task-status () 'status-todo 'status-progress 'status-done)
  ;; 检查组是否已定义
  (length (tp-group-props 'task-status)))
;; => 3

;; 循环切换状态（用于实际缓冲区）
(defun cycle-task-status ()
  "循环切换当前行的任务状态属性层。"
  (interactive)
  (tp-rotate-layer (line-beginning-position) (line-end-position)))
```

### 临时高亮

```elisp
;; 定义临时高亮属性层
(progn
  (tp-layer-reset)
  (define-tp temp-highlight ()
  '(face (:background "yellow")))
  (tp-layer-props 'temp-highlight))
;; => (face (:background "yellow"))

;; 闪烁函数（用于实际缓冲区）
(defun flash-region (start end)
  "临时闪烁一个区域。"
  (tp-push-layer start end 'temp-highlight)
  (run-with-timer 0.5 nil
                  (lambda (s e)
                    (tp-delete-layer s e 'temp-highlight))
                  start end))
```

---

## 响应式文本属性

> 📖 **完整的详细指南和示例，请参阅 [响应式文本属性完全指南](docs/reactive-text-properties.md)**
>
> 📖 **高级优化功能，请参阅 [响应式系统优化文档](docs/reactive-optimization.md)**

**响应式文本属性**是 tp.el 的突破性创新，它将响应式编程范式带入了 Emacs 文本属性。受 Vue.js 等现代前端框架启发，这个功能使文本属性能够在底层变量值改变时自动更新。

### 核心概念

传统的文本属性操作需要在每次想要改变属性值时手动更新所有受影响的文本区域。使用响应式文本属性，你只需定义一次变量关系，tp.el 会自动处理所有更新：

```elisp
;; 传统方式（需要手动更新）
(defvar my-color "red")
(tp-set 1 10 '(face (:foreground "red")))
;; 要改变颜色，你必须手动更新每个区域：
(setq my-color "blue")
(tp-set 1 10 '(face (:foreground "blue")))  ; 手动更新！

;; 响应式方式（自动更新）
(defvar my-color "red")
(define-tp my-layer ()
  :props '(face (:foreground $my-color)))
(tp-push-layer 1 10 'my-layer)
;; 只需改变变量 - 所有文本自动更新！
(setq my-color "blue")  ;; 所有使用 my-layer 的区域立即更新！
```

### 工作原理

1. **响应式变量**：`:props` 中任何以 `$` 为前缀的符号都被视为响应式变量。`$` 会被去掉以获取实际的变量名。

2. **变量监听器**：tp.el 使用 Emacs 的 `add-variable-watcher` 来监控响应式变量的变化。

3. **自动更新**：当响应式变量通过 `setq` 改变时，所有使用依赖该变量的层的文本区域会自动使用新的属性值更新。

### 定义响应式层

#### 基本响应式层

```elisp
(defvar highlight-color "yellow")

(define-tp my-highlight ()
  :props '(face (:background $highlight-color)))

(with-temp-buffer
  (insert "Hello World")
  (tp-push-layer 1 10 'my-highlight)
  ;; 文本以黄色高亮
  
  (setq highlight-color "cyan")
  ;; 文本现在以青色高亮 - 自动更新！
)
```

#### 多个响应式变量

```elisp
(defvar fg-color "white")
(defvar bg-color "black")

(define-tp themed-text ()
  :props '(face (:foreground $fg-color :background $bg-color)))

;; 改变任一变量都会更新文本
(setq fg-color "yellow")  ;; 更新前景色
(setq bg-color "navy")    ;; 更新背景色
```

### :data - 附加响应式状态

`:data` 关键字定义不直接用于 `:props` 但可以触发计算值更新或被监听的额外响应式变量：

```elisp
(define-tp user-info ()
  :props '(help-echo $full-name)
  :data '(first-name last-name)  ;; 不直接用于 props
  :compute '((full-name (lambda () (concat first-name " " last-name)))))
```

**带初始值：**

你可以使用 cons cell 指定初始值：

```elisp
(define-tp user-info ()
  :props '(help-echo $full-name)
  :data '((first-name . "张") (last-name . "三"))
  :compute '((full-name (lambda () (concat first-name last-name)))))

;; first-name 现在是 "张"，last-name 现在是 "三"
```

### :compute - 计算属性

`:compute` 关键字创建派生值，当它们的依赖项改变时会自动重新计算：

```elisp
(define-tp progress-display ()
  :props '(display $progress-text face (:foreground $progress-color))
  :data '((current . 0) (total . 100))
  :compute '((progress-text (lambda () (format "%d%%" (/ (* current 100) total))))
            (progress-color (lambda ()
                              (cond ((< current 30) "red")
                                    ((< current 70) "yellow")
                                    (t "green"))))))

;; 更新进度
(setq current 50)
;; progress-text 变成 "50%"，progress-color 变成 "yellow"，自动更新！
```

### :watch - 副作用回调

`:watch` 关键字让你在响应式变量改变时执行回调：

```elisp
(define-tp monitored-layer ()
  :props '(face (:foreground $status-color))
  :watch '((status-color 
            (lambda (new-val old-val layer-name)
              (message "层 %s: 颜色从 %s 改为 %s" 
                       layer-name old-val new-val)))))

(setq status-color "red")
;; 消息: "层 monitored-layer: 颜色从 nil 改为 red"

(setq status-color "green")
;; 消息: "层 monitored-layer: 颜色从 red 改为 green"
```

### :transform - 值转换

`:transform` 关键字允许你注册一个转换函数，在 `tp-text` 值显示之前对其进行处理。这对于格式化数字、日期或其他值非常有用：

```elisp
;; 数字格式化
(define-tp price-display ()
  :props '(tp-text $price)
  :data '((price . "99.9"))
  :transform (lambda (text)
               (format "$%.2f" (string-to-number text))))
;; 99.9 显示为 $99.00

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
;; "hello" 显示为 "HELLO"
```

转换函数的特点：
- 接收原始的 `tp-text` 字符串值
- 返回用于显示的转换后字符串
- 在初始显示和响应式更新时都会应用
- 转换函数中的错误会被捕获并记录

### 匿名响应式层

即使不使用 `define-tp`，你也可以使用响应式变量。当你在匿名 plist 中使用 `$` 前缀的符号时，tp.el 会自动生成唯一的层名：

```elisp
(defvar my-face-color "blue")

;; 匿名响应式层 - tp-name 自动生成
(tp-set 1 10 '(face (:foreground $my-face-color)))

;; 该层现在是响应式的 - 改变变量会更新文本
(setq my-face-color "red")
```

### API 中的层名解析

所有文本属性 API（`tp-set`、`tp-match-set`、`tp-regexp-set` 等）现在可以直接接受层名：

```elisp
(define-tp warning-style ()
  :props '(face (:foreground "orange" :weight bold)))

;; 使用层名代替 plist
(tp-set 1 10 'warning-style)

;; 适用于所有匹配函数
(tp-match-set "TODO" 'warning-style)
(tp-regexp-set "[0-9]+" 'warning-style)
```

### 响应式层组

层组也可以使用响应式特性：

```elisp
(define-tps status-indicators ()
  '("success" :props (face (:foreground $success-color))
              :data ((success-color . "green")))
  '("warning" :props (face (:foreground $warning-color))
              :data ((warning-color . "orange")))
  '("error"   :props (face (:foreground $error-color))
              :data ((error-color . "red"))))
```

### 批量更新

同时修改多个响应式变量时，每个 `setq` 都会触发一次缓冲区更新。使用 `tp-with-batch-updates` 可以合并所有更改，在结束时一次性应用：

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

批量更新的好处：
- 减少冗余的缓冲区修改
- 提高同时更改多个变量时的性能
- 当多个变量相互依赖时确保状态一致

### 层-缓冲区注册表与生命周期

自 0.3.0 起，响应式引擎维护一个**层→缓冲区注册表**：每条修改缓冲区并
盖上层标记的写入路径（`tp-set` 家族、栈修改函数、match/regexp 应用函
数）都会把目标缓冲区注册为展示该层，而一次响应式更新只访问**已注册的
缓冲区**，不再扫描整个 `(buffer-list)`。被杀死的缓冲区会被自动清理。
当某个层完全没有注册表条目时，会退回到旧行为做一次**学习性全量扫描**，
并把实际发现该层的每个缓冲区都注册上。

即使层被**隐藏**或**埋**在栈中其他层之下，更新也能到达它的区域：存储
在 `tp-layers` 中的条目会被就地更新，因此 `tp-show-layer`（或上移该
层）总是显露最新的值。

#### `tp-reactive-layer-buffers` - 查看注册表

```elisp
(tp-reactive-layer-buffers LAYER-NAME)
```

返回注册为展示 LAYER-NAME 的存活缓冲区 —— 一个列表（可能为空，表示
"已知：没有缓冲区展示该层"）—— 或者符号 `unknown`，表示该层完全没有注
册表条目：

```elisp
(progn
  (tp-layer-reset)
  (defvar reg-color "red")
  (define-tp reg-layer ()
    :props '(face (:foreground $reg-color)))
  (tp-reactive-layer-buffers 'reg-layer))
;; => unknown  ; 还从未被应用到任何缓冲区

(with-temp-buffer
  (rename-buffer "demo-buffer" t)
  (insert "Hello")
  (tp-push-layer 1 6 'reg-layer)
  (mapcar #'buffer-name (tp-reactive-layer-buffers 'reg-layer)))
;; => ("demo-buffer")
```

#### `tp-reactive-track-buffer` - 补齐字符串插入的缺口

```elisp
(tp-reactive-track-buffer &optional BUFFER)   ; interactive
```

**已知缺口：**向缓冲区插入一个*已经带属性的字符串*会绕过注册缓冲区的
缓冲区操作，因此在一次学习性全量扫描找到它之前，该缓冲区不在注册表中。
在这类插入之后调用 `tp-reactive-track-buffer`：它会扫描 BUFFER（默认
为当前缓冲区）中的层区域 —— 既包括被渲染的顶层，也包括埋在或隐藏在
`tp-layers` 栈存储中的层 —— 为每个层注册该缓冲区，并按缓冲区顺序返回
找到的层名：

```elisp
(let ((s (tp-set "hello" 'reg-layer)))   ; 带属性的字符串，游离状态
  (with-temp-buffer
    (insert s)                           ; 绕过了注册
    (tp-reactive-track-buffer)))
;; => (reg-layer)  ; 缓冲区现已为 reg-layer 注册
```

#### `tp-gc-anonymous-layers` - 回收未使用的匿名层

```elisp
(tp-gc-anonymous-layers)   ; interactive
```

[匿名响应式层](#匿名响应式层)是被驻留（intern）的：`equal` 相同的属性
规格会复用其注册表条目，而不是在每次 `tp-set` 时铸造一个新层。
`tp-gc-anonymous-layers` 取消定义所有已无注册的存活缓冲区仍在展示的驻
留匿名层（被埋住和被隐藏的层都算作存活），并返回被回收的层名：

```elisp
(defvar tmp-color "green")
(let ((buf (generate-new-buffer "*gc-demo*")))
  (with-current-buffer buf
    (insert "Hello")
    (tp-set 1 6 '(face (:foreground $tmp-color))))  ; 匿名层
  (kill-buffer buf)
  (tp-gc-anonymous-layers))
;; => (tp-anon-1)  ; 被回收的层名（计数器数字会变化）
```

**保守的 `unknown` 语义：**注册表状态为 `unknown` 的层 —— 从未通过任
何注册路径出现在任何缓冲区中，例如只被游离字符串引用 —— 会被刻意
**保留**。一个层只有在至少为一个缓冲区注册过、且所有已注册的缓冲区都不再
展示它（例如全部被杀死）之后才可回收。在插入带属性的字符串后请调用
`tp-reactive-track-buffer`，让它们所在的缓冲区也被注册。

#### 最小差异的 `tp-text` 重渲染

响应式 `tp-text` 替换只编辑新旧文本的**差异区段**（先插入后删除），因
此位于未变化文本中的 point 和标记保持原位；位于被编辑区段内的 point
落在编辑起点。值完全相同的更新是真正的空操作：不编辑文本、不搅动属
性，也不触碰缓冲区的修改标志。

```elisp
(progn
  (tp-layer-reset)
  (defvar counter-val "0")
  (define-tp counter-label ()
    :props '(tp-text $counter-val))
  (with-temp-buffer
    (insert "count: 0 items")
    (tp-set 8 9 'counter-label)
    (let ((m (copy-marker 10)))          ; 标记在 "items" 的 "i" 上
      (setq counter-val "9")             ; 只有数字被编辑
      (list (buffer-substring-no-properties 1 (point-max))
            (char-after m)))))
;; => ("count: 9 items" ?i)  ; 标记仍指向它原来的字符

;; 值相同的更新完全不触碰缓冲区
(with-temp-buffer
  (insert "count: 9 items")
  (tp-set 8 9 'counter-label)
  (set-buffer-modified-p nil)
  (setq counter-val "9")                 ; 与显示的文本相同
  (buffer-modified-p))
;; => nil
```

### 调试模式

tp.el 提供调试模式来帮助理解响应式更新流程：

```elisp
;; 启用调试模式
(setq tp-debug-mode t)

;; 同时在 minibuffer 显示调试信息（可选）
(setq tp-debug-echo t)

;; 查看调试日志
(tp-debug-show)

;; 清除调试日志
(tp-debug-clear)
```

调试日志包含：
- 变量变化通知（旧值 → 新值）
- 层更新追踪
- 批量更新开始/结束
- 转换应用信息

调试输出示例：
```
[12:34:56.789] Variable my-color changed: "red" -> "blue" (where: global)
[12:34:56.790]   Updating layer test-layer (tp-text affected: no)
```

### 重置响应式状态

清除所有响应式依赖和监听器：

```elisp
(tp-reactive-reset)  ;; 仅清除响应式状态

(tp-layer-reset)     ;; 清除层、组以及响应式状态
```

### 完整示例：主题感知文本

```elisp
;; 定义主题变量
(defvar theme-fg "white")
(defvar theme-bg "black")
(defvar theme-accent "cyan")

;; 定义主题感知层 - 每个层都引用主题变量
(define-tp code-text ()
  :props '(face (:foreground $theme-fg :background $theme-bg)))

(define-tp code-keyword ()
  :props '(face (:foreground $theme-accent :weight bold)))

;; 将层应用到当前缓冲区中的代码
(tp-set (point-min) (point-max) 'code-text)
(tp-match-set '("defun" "defvar" "let" "if" "when") 'code-keyword)

;; 切换到浅色主题 - 只需改变变量！
(defun switch-to-light-theme ()
  (interactive)
  (setq theme-fg "black")
  (setq theme-bg "white")
  (setq theme-accent "blue"))

;; 切换到深色主题
(defun switch-to-dark-theme ()
  (interactive)
  (setq theme-fg "white")
  (setq theme-bg "black")
  (setq theme-accent "cyan"))

;; 调用 `switch-to-light-theme' 后，关键字变为蓝色，其余代码变为
;; 白底黑字 - 每个区域都会自动重新渲染
```

---

## 许可证

GNU 通用公共许可证 v3 或更高版本。参见 [LICENSE](LICENSE) 文件。

---

## 贡献

欢迎贡献！请随时提交 issues 或 pull requests。

---

<p align="center">
  <em>tp.el - 让文本属性变得强大且易用</em>
</p>
