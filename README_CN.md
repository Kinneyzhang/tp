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
    - [tp-rotate-layer](#tp-rotate-layer---轮换属性层)
    - [tp-pin-layer](#tp-pin-layer---将属性层置顶)
    - [tp-switch-layer](#tp-switch-layer---交换两个属性层)
  - [属性层合并](#属性层合并)
    - [tp-merge-layers](#tp-merge-layers---合并多个属性层)
    - [tp-flatten-layers](#tp-flatten-layers---扁平化所有属性层)
  - [属性层查询函数](#属性层查询函数)
    - [tp-layer-list](#tp-layer-list---列出所有属性层)
    - [tp-layer-count](#tp-layer-count)
    - [tp-layer-exists-p](#tp-layer-exists-p)
    - [tp-layer-top](#tp-layer-top)
    - [tp-add-to-layers](#tp-add-to-layers---向特定属性层添加属性)
    - [tp-add-to-all-layers](#tp-add-to-all-layers---向所有属性层添加属性)
  - [实用工具函数](#实用工具函数)
    - [tp-intervals](#tp-intervals---获取文本属性区间)
    - [tp-intervals-map](#tp-intervals-map---对区间应用函数)
    - [tp-plist](#tp-plist---获取区域中的所有属性)
    - [tp-empty-p](#tp-empty-p---检查对象是否有属性)
    - [tp-region-layer-props](#tp-region-layer-props---获取区域中的层属性)
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

## 概述

**tp.el** 是一个全面增强 Emacs 文本属性操作的库。它不仅仅是对原生文本属性 API（如 `put-text-property`、`get-text-property`）的简单封装，更提供了许多**原生函数所不具备的功能拓展**。tp.el 在以下方面进行了创新：

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
  ;; 获取嵌套属性
  (tp-get str 'face :underline :style)  ; => wave
  (tp-at 5 '(face :box :color))         ; => "blue"
  
  ;; 获取多个嵌套键
  (tp-get str 'face :underline '(:color :style))
  ;; => ((:color "green" :style wave))
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
;; 结果: face 是 ((:background "green" :foreground "red") bold)

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
  - 移动：`tp-raise-layer`（上下移动）、`tp-rotate-layer`（轮换）、`tp-pin-layer`（置顶）、`tp-switch-layer`（交换）
  - 合并：`tp-merge-layers`（合并指定层）、`tp-flatten-layers`（扁平化所有层）
- ✅ **属性层查询**：`tp-layer-list`、`tp-layer-count`、`tp-layer-exists-p`、`tp-layer-top`

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

;; 高级响应式示例（使用 define-tp）：
;; 对于需要 :data、:compute、:watch 等高级特性的场景，
;; 可以使用 define-tp
(define-tp full-name-layer ()
  :props '(help-echo $full-name face (:foreground $name-color))
  :data '((first-name . "John") (last-name . "Doe"))  ;; 带初始值
  :compute '((full-name (lambda () (concat first-name " " last-name))))
  :watch '((first-name (lambda (new old layer)
                        (message "名字从 %s 改为 %s" old new)))))
```

### 增强的搜索与导航

- ✅ **范围搜索**：`tp-search` 返回所有匹配区间的列表
- ✅ **N次搜索**：`tp-forward`/`tp-backward` 支持向前/向后搜索N次
- ✅ **搜索并执行**：`tp-forward-do`/`tp-backward-do` 搜索并对匹配文本执行函数
- ✅ **批量转换**：`tp-search-map` 对所有匹配应用转换函数

```elisp
;; 搜索所有标记
(tp-search my-string 'marker)  ; => ((0 5 t) (12 17 t))

;; 将所有标记文本转为大写
(tp-search-map #'upcase my-string 'marker)
```

## 系统要求

- **Emacs 28.1+**（使用 `object-intervals` 函数）
- **dash.el**（列表操作工具库）

## 安装

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
| [`tp-remove`](#tp-remove---移除属性) | 移除属性或子属性 |
| [`tp-clear`](#tp-clear---清除所有属性) | 清除区域中的所有文本属性 |

#### 模式匹配函数
| 函数 | 描述 |
|------|------|
| [`tp-match-set`](#tp-match-set---匹配字符串) | 在字符串匹配处设置属性 |
| [`tp-match-reset`](#tp-match-reset---匹配并重置) | 在字符串匹配处重置所有属性 |
| [`tp-match-add`](#tp-match-add---匹配并添加) | 在字符串匹配处添加/合并属性 |
| [`tp-regexp-set`](#tp-regexp-set---匹配正则表达式) | 在正则匹配处设置属性 |
| [`tp-regexp-reset`](#tp-regexp-reset---正则匹配并重置) | 在正则匹配处重置所有属性 |
| [`tp-regexp-add`](#tp-regexp-add---正则匹配并添加) | 在正则匹配处添加/合并属性 |

#### 搜索和导航函数
| 函数 | 描述 |
|------|------|
| [`tp-search-forward`](#tp-search-forward--tp-search-backward) | text-property-search-forward 的原始包装 |
| [`tp-search-backward`](#tp-search-forward--tp-search-backward) | text-property-search-backward 的原始包装 |
| [`tp-forward`](#tp-forward--tp-backward) | 向前搜索 N 次具有属性的文本（支持缓冲区和字符串） |
| [`tp-backward`](#tp-forward--tp-backward) | 向后搜索 N 次具有属性的文本（支持缓冲区和字符串） |
| [`tp-forward-do`](#tp-forward-do--tp-backward-do) | 向前搜索并对最后一个匹配应用函数（支持起始和结束范围） |
| [`tp-backward-do`](#tp-forward-do--tp-backward-do) | 向后搜索并对最后一个匹配应用函数（支持起始和结束范围） |
| [`tp-search`](#tp-search---搜索所有匹配) | 在范围或字符串中搜索所有匹配的属性 |
| [`tp-search-map`](#tp-search-map---对匹配文本应用函数) | 对所有匹配的文本应用函数（支持起始和结束范围） |

#### 属性层定义函数
| 函数 | 描述 |
|------|------|
| [`define-tp`](#define-tp--define-tps---定义自定义文本属性) | 定义自定义文本属性（层），支持参数化 |
| [`define-tps`](#define-tp--define-tps---定义自定义文本属性) | 定义自定义文本属性组（层组），支持参数化 |
| [`tp-layer-props`](#tp-layer-props--tp-group-props) | 获取属性层的属性 |
| [`tp-group-props`](#tp-layer-props--tp-group-props) | 获取属性层组中所有属性层的属性 |
| [`tp-undefine-layer`](#tp-undefine-layer--tp-undefine-group) | 移除属性层定义 |
| [`tp-undefine-group`](#tp-undefine-layer--tp-undefine-group) | 移除属性层组定义 |
| [`tp-layer-reset`](#tp-layer-reset) | 清除所有属性层/属性层组定义 |
| [`tp-reactive-reset`](#tp-reactive-reset) | 清除所有响应式依赖和监听器 |

#### 属性层放置函数
| 函数 | 描述 |
|------|------|
| [`tp-put-layer`](#tp-put-layer---在指定位置设置属性层) | 在指定索引位置设置属性层 |
| [`tp-push-layer`](#tp-push-layer---推送属性层到顶部) | 将属性层推到堆栈顶部 |

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
| [`tp-rotate-layer`](#tp-rotate-layer---轮换属性层) | 轮换属性层（顶层移到底部） |
| [`tp-pin-layer`](#tp-pin-layer---将属性层置顶) | 将属性层置顶（使其可见） |
| [`tp-switch-layer`](#tp-switch-layer---交换两个属性层) | 交换两个属性层的位置 |

#### 属性层合并函数
| 函数 | 描述 |
|------|------|
| [`tp-merge-layers`](#tp-merge-layers---合并多个属性层) | 将指定属性层合并为新属性层 |
| [`tp-flatten-layers`](#tp-flatten-layers---扁平化所有属性层) | 将所有属性层扁平化为单一属性层 |

#### 属性层查询函数
| 函数 | 描述 |
|------|------|
| [`tp-layer-list`](#tp-layer-list---列出所有属性层) | 列出区域中的所有属性层名称 |
| [`tp-layer-count`](#tp-layer-count) | 计算区域中的属性层数量 |
| [`tp-layer-exists-p`](#tp-layer-exists-p) | 检查区域中是否存在某属性层 |
| [`tp-layer-top`](#tp-layer-top) | 获取顶层（可见）属性层的名称 |
| [`tp-region-layer-props`](#tp-region-layer-props---获取区域中的层属性) | 获取区域中特定层的属性 |

#### 属性层操作函数
| 函数 | 描述 |
|------|------|
| [`tp-add-to-layers`](#tp-add-to-layers---向特定属性层添加属性) | 通过索引或名称向特定层添加/合并属性 |
| [`tp-add-to-all-layers`](#tp-add-to-all-layers---向所有属性层添加属性) | 向所有现有层添加/合并属性 |

#### 实用工具函数
| 函数 | 描述 |
|------|------|
| [`tp-intervals`](#tp-intervals---获取文本属性区间) | 获取区域中的所有文本属性区间 |
| [`tp-intervals-map`](#tp-intervals-map---对区间应用函数) | 对区域中的所有区间应用函数 |
| [`tp-plist`](#tp-plist---获取区域中的所有属性) | 获取区域中存在的所有属性 |
| [`tp-empty-p`](#tp-empty-p---检查对象是否有属性) | 检查对象是否没有文本属性 |

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
  (tp-set 1 10 '(face italic) my-buffer)
  (kill-buffer my-buffer))
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
;; => #(" " 0 1 (tp-name my-style face (:foreground "blue") ...))

;; 单次调用中合并多个 face（重复属性自动合并）
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; => 三个 face 合并为一个: ((:background "green" :foreground "red") bold)

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
    (get-text-property 0 '(face :underline) result)))
;; => (:color "blue")
```

---

#### `tp-clear` - 清除所有属性

```elisp
(tp-clear &optional START END OBJECT)
```

清除区域中的所有文本属性。

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
(tp-match-set PATTERN PLIST &optional OBJECT)
(tp-match-set PATTERN LAYER-NAME &optional OBJECT)
```

在所有字符串模式匹配处设置属性。
PATTERN 可以是字符串（单个模式）或字符串列表（多个模式）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

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
;; => ((1 . 6) (7 . 12) (14 . 19))  ; 匹配 "Hello", "world", "Hello"

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
```

---

#### `tp-match-reset` - 匹配并重置

重置（完全替换）匹配处的所有属性。
PATTERN 可以是字符串或字符串列表（多个模式）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-match-reset PATTERN PLIST &optional OBJECT)
(tp-match-reset PATTERN LAYER-NAME &optional OBJECT)
```

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
(tp-match-add PATTERN PLIST &optional OBJECT)
(tp-match-add PATTERN LAYER-NAME &optional OBJECT)
```

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
(tp-regexp-set PATTERN PLIST &optional OBJECT)
(tp-regexp-set PATTERN LAYER-NAME &optional OBJECT)
```

在所有正则表达式匹配处设置属性。
PATTERN 可以是字符串（单个正则）或字符串列表（多个正则）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

**示例：**

```elisp
;; 高亮缓冲区中的所有数字
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" '(face font-lock-number-face))
  (list (tp-at 5 'face) (tp-at 13 'face)))
;; => (font-lock-number-face font-lock-number-face)

;; 在字符串上
(tp-regexp-set "[A-Z]+" '(face bold) "Hello WORLD")
;; => #("Hello WORLD" 6 11 (face bold))

;; 多个正则 - 同时匹配数字和大写字母
(tp-regexp-set '("[0-9]+" "[A-Z]+") '(face bold) "abc 123 XYZ")
;; => #("abc 123 XYZ" 4 7 (face bold) 8 11 (face bold))

;; 使用已定义的层名称
(define-tp number-style ()
  '(face (:foreground "green")))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" 'number-style))
;; => ((5 . 8) (13 . 16))
```

---

#### `tp-regexp-reset` - 正则匹配并重置

重置（完全替换）正则匹配处的所有属性。
PATTERN 可以是字符串或字符串列表（多个正则）。
PLIST 是属性列表，如 `'(face bold help-echo "tip")`。
LAYER-NAME 可以是通过 `define-tp` 定义的自定义文本属性名称或通过 `define-tps` 定义的属性组名称。
OBJECT 是缓冲区或字符串；nil 表示当前缓冲区。

```elisp
(tp-regexp-reset PATTERN PLIST &optional OBJECT)
(tp-regexp-reset PATTERN LAYER-NAME &optional OBJECT)
```

**示例：**

```elisp
;; 重置正则匹配处的所有属性
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "original"))
  (tp-regexp-reset "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold)  ; help-echo 被移除

;; 在字符串上
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "original") str)
  (tp-regexp-reset "[0-9]+" '(face italic) str)
  (tp-at 4 str))
;; => (face italic)

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
(tp-regexp-add PATTERN PLIST &optional OBJECT)
(tp-regexp-add PATTERN LAYER-NAME &optional OBJECT)
```

**示例：**

```elisp
;; 添加属性到正则匹配处（保留现有）
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "number"))
  (tp-regexp-add "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold help-echo "number")

;; 在字符串上
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "number") str)
  (tp-regexp-add "[0-9]+" '(face italic) str)
  (tp-at 4 str))
;; => (face italic help-echo "number")

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

```elisp
(tp-search-forward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
(tp-search-backward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
```

Emacs 的 `text-property-search-forward` 和 `text-property-search-backward` 的原始包装。
这些是直接使用 prop-match 对象的底层搜索函数。

---

#### `tp-forward` / `tp-backward`

```elisp
(tp-forward PROPERTY &optional VALUE OBJECT N)
(tp-backward PROPERTY &optional VALUE OBJECT N)
```

向前/向后搜索 N 次具有 PROPERTY 的文本。

- **N** 是搜索次数，默认为 1。
- **VALUE** 是可选的匹配值。
- **OBJECT** 可以是缓冲区或字符串；nil 默认为当前缓冲区。
- 对于缓冲区，返回最后一次成功搜索的 prop-match 对象。
- 对于字符串，返回所有匹配的 (START END VALUE) 列表。

**示例：**

```elisp
;; 查找下一个具有 'marker 属性的文本
(with-temp-buffer
  (insert "Hello World Test")
  (tp-set 7 12 '(marker t))
  (goto-char 1)
  (let ((match (tp-forward 'marker)))
    (when match
      (prop-match-beginning match))))
;; => 7

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
```

---

#### `tp-forward-do` / `tp-backward-do`

```elisp
(tp-forward-do FUNCTION PROPERTY &optional VALUE OBJECT TIMES START END)
(tp-backward-do FUNCTION PROPERTY &optional VALUE OBJECT TIMES START END)
```

在 OBJECT 的 START 到 END 范围内，向前/向后搜索匹配 PROPERTY 属性（值为 VALUE）的部分，**仅对最后一次匹配执行 FUNCTION 函数**。

- **FUNCTION** 的参数是 `(TEXT &optional START END)`，其中 TEXT 是此次匹配到的文本，START 和 END 为开始结束的位置。FUNCTION 的返回值将替换字符串或缓冲区中的匹配文本。
- **PROPERTY** 是要搜索的文本属性。
- **VALUE** 为 nil 时，表示搜索 PROPERTY 属性，不用匹配值。
- **OBJECT** 默认是当前 buffer 或指定的字符串或指定的 buffer。
- **TIMES** 表示向前/向后搜索几次，默认搜索一次。该函数会搜索 TIMES 次，但仅对找到的最后（第 N 次）匹配应用 FUNCTION。
- **START** 和 **END** 默认为 OBJECT 的起始和结束位置。
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
;; => "hello world HELLO"  ; 范围 6-17 内仅有 1 个匹配

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
  FUNCTION 的返回值将替换字符串或缓冲区中的匹配文本。
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
;; 结果: face 是 ((:background "green" :foreground "red") bold)
;; 三个 face 属性被智能合并

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

┌─────────────────────────────┐
│   顶层（可见）              │  ← idx=0，你看到的
├─────────────────────────────┤
│   中间层（隐藏）            │  ← idx=1，被保留
├─────────────────────────────┤
│   底层（隐藏）              │  ← idx=-1，被保留
└─────────────────────────────┘

### 属性层定义

#### `define-tp` / `define-tps` - 定义自定义文本属性

##### `define-tp` - 定义单个自定义文本属性（层）

定义自定义文本属性，名称无需单引号引用。支持三种格式：

**格式一 - 无参数（空参数列表，简单属性）：**

```elisp
(define-tp tp-bold ()
  '(face bold))

;; 用法:
(tp-set "emacs" 'tp-bold t)
(tp-set 0 5 '(tp-bold t) "emacs")
```

**格式二 - 有参数（带单个参数）：**

```elisp
(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

;; 用法:
(tp-set "emacs" 'tp-space 2)
(tp-set 0 5 '(tp-space 2) "emacs")
```

**格式三 - 响应式特性（支持 :props、:data、:compute、:watch、:transform）：**

```elisp
(define-tp my-reactive-layer ()
  :props '(face (:foreground $my-color))
  :data '((my-color . "red"))
  :compute '((full-name (lambda () (concat first-name " " last-name))))
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

##### `define-tps` - 定义自定义文本属性组（层组）

定义多个相关的自定义文本属性，名称无需单引号引用。属性组中定义的文本属性可以单独使用，也可以使用组名称来设置多层。

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
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
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
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
  (define-tps moon-phases ()
    '("new" . (display "🌑"))
    '("waxing-crescent" . (display "🌒"))
    '("first-quarter" . (display "🌓"))
    '("full" . (display "🌕")))
  (tp-layer-props 'moon-phases-full))
;; => (display "🌕" tp-name moon-phases-full)

;; 参数化层组，引用其他已定义的层
(progn
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
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
```

---

#### `tp-layer-props` / `tp-group-props`

```elisp
(tp-layer-props LAYER-NAME)
(tp-group-props GROUP-NAME)
```

获取属性层或属性层组中所有属性层的属性。

**示例：**

```elisp
;; 获取属性层属性
(progn
  (setq tp-layer-alist nil)
  (define-tp my-layer ()
    '(face bold help-echo "tip"))
  (tp-layer-props 'my-layer))
;; => (face bold help-echo "tip" tp-name my-layer)

;; 获取属性层组属性
(progn
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
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
  (setq tp-layer-alist nil)
  (define-tp temp-layer ()
    '(face bold))
  (tp-undefine-layer 'temp-layer)
  (tp-layer-props 'temp-layer))
;; => nil

;; 取消定义属性层组
(progn
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
  (define-tp l1 () '(face bold))
  (define-tps my-group 'l1)
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
;; => (face (:foreground "red") tp-name reactive-layer)
```

---

### 属性层放置

#### `tp-put-layer` - 在指定位置设置属性层

```elisp
;; 缓冲区/字符串区域
(tp-put-layer START END LAYER IDX OBJECT)

;; 整个字符串
(tp-put-layer STRING LAYER IDX)
```

在属性层堆栈的指定索引位置设置属性层。

- `IDX = 0`：顶部（可见属性层）
- `IDX = -1`：底部
- 其他值在该位置插入

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
```

---

#### `tp-push-layer` - 推送属性层到顶部

```elisp
;; 缓冲区/字符串区域
(tp-push-layer START END LAYER OBJECT)

;; 整个字符串
(tp-push-layer STRING LAYER)
```

将属性层推到堆栈顶部（相当于 `tp-put-layer ... 0`）。

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

#### `tp-rotate-layer` - 轮换属性层

```elisp
;; 缓冲区/字符串区域
(tp-rotate-layer START END OBJECT)

;; 整个字符串
(tp-rotate-layer STRING)
```

轮换属性层 - 顶层移到底部，下一层变为可见。

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
```

---

#### `tp-pin-layer` - 将属性层置顶

```elisp
;; 缓冲区/字符串区域
(tp-pin-layer START END IDX/LAYER-NAME OBJECT)

;; 整个字符串
(tp-pin-layer STRING IDX/LAYER-NAME)
```

将特定属性层移到顶部（使其可见）。

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

### 属性层合并

#### `tp-merge-layers` - 合并多个属性层

```elisp
;; 缓冲区/字符串区域
(tp-merge-layers START END NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)

;; 整个字符串
(tp-merge-layers STRING NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...))
```

将指定的属性层合并为一个新属性层。列表中靠前的属性层优先级更高。

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

获取顶层（可见）属性层的名称。

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
- 对于字符串，返回一个新的字符串（原始字符串不变）。对于缓冲区，返回 nil。

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
- 对于字符串，返回一个新的字符串（原始字符串不变）。对于缓冲区，返回 nil。

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
(tp-intervals START END &optional OBJECT)
```

从 OBJECT 中获取 START 到 END 之间的所有文本属性区间。

- 返回每个区间的 (START END PROPERTIES) 列表。
- 使用 `object-intervals`（需要 Emacs 28.1+）。
- OBJECT 可以是缓冲区或字符串；nil 默认为当前缓冲区。

**示例：**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals 1 12))
;; => ((0 5 (face bold)) (6 11 (face italic)))
```

---

#### `tp-intervals-map` - 对区间应用函数

```elisp
(tp-intervals-map FUNCTION START END &optional OBJECT)
```

对 OBJECT 中 START 到 END 之间的所有区间应用 FUNCTION。

- FUNCTION 接收四个参数：interval-start、interval-end、top-props（可见层属性）和 below-props-lst（隐藏层列表）。
- OBJECT 可以是缓冲区或字符串；nil 默认为当前缓冲区。
- 返回函数结果列表（nil 值被移除）。

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
;; => ((0 5 bold) (6 11 italic))
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

- 返回包含范围内找到的所有属性的 plist。
- OBJECT 在区域形式中默认为当前缓冲区。

**示例：**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold help-echo "Tip"))
  (tp-set 7 12 '(face italic))
  (tp-plist 1 12))
;; => (face bold help-echo "Tip" face italic)
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
(let ((str (copy-sequence "text")))
  (tp-set str 'face 'bold)
  (tp-empty-p str))  ; => nil
```

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
  (define-tps task-status 'status-todo 'status-progress 'status-done)
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
;; => (face (:background "yellow") tp-name temp-highlight)

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
(define-tps status-indicators
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

;; 定义主题感知层
(define-tp code-keyword ()
  :props '(face (:foreground $theme-accent :weight bold)))

(define-tp code-comment ()
  :props '(face (:foreground "gray" :slant italic)))

(define-tp code-string ()
  :props '(face (:foreground "green")))

;; 将层应用到代码
(tp-match-set '("defun" "defvar" "let" "if" "when") 'code-keyword)
(tp-regexp-set ";.*$" 'code-comment)
(tp-regexp-set "\"[^\"]*\"" 'code-string)

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
```

---

## 许可证

GNU 通用公共许可证 v2 或更高版本。

---

## 贡献

欢迎贡献！请随时提交 issues 或 pull requests。

---

<p align="center">
  <em>tp.el - 让文本属性变得强大且易用</em>
</p>
