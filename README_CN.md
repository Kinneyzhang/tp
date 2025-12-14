# tp.el - Emacs 文本属性操作库

<p align="center">
  <strong>一个功能强大的文本属性操作库，具有创新的图层系统</strong>
</p>

<p align="center">
  <a href="#功能特性">功能特性</a> •
  <a href="#安装">安装</a> •
  <a href="#快速开始">快速开始</a> •
  <a href="#api-参考">API 参考</a> •
  <a href="#图层系统">图层系统</a>
</p>

---

**tp.el** 提供了一个便捷统一的 API 来操作 Emacs 文本属性。灵感来自用于叠加层的 [ov.el](https://github.com/emacsorphanage/ov)，tp.el 提供：

- **统一 API**：所有属性设置函数同时支持 **字符串** 和 **缓冲区**
- **图层系统**：在同一文本区域上堆叠多组属性
- **模式匹配**：将属性应用到匹配字符串或正则表达式的文本

## 功能特性

- ✅ **统一对象支持**：`tp-set`、`tp-match`、`tp-regexp` 等函数同时支持字符串和缓冲区
- ✅ **清晰语义**：`tp-reset`（替换全部）、`tp-set`（替换指定）、`tp-add`（深度合并）
- ✅ **嵌套属性访问**：使用路径语法获取/设置/移除嵌套子属性
- ✅ **创新图层系统**：堆叠、轮换和管理多层属性
- ✅ **图层组**：定义可复用的相关图层集合
- ✅ **搜索和导航**：查找并导航带属性的文本
- ✅ **模式匹配**：将属性应用到字符串/正则匹配，支持 reset/add 变体
- ✅ **简洁 API**：一致的命名和调用约定

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

### 核心属性函数

#### `tp-set` - 设置文本属性

在字符串或缓冲区区域上设置文本属性。只替换指定的属性，保留其他属性。

```elisp
;; 当前缓冲区（属性作为列表）
(tp-set START END '(PROPERTY VALUE ...))

;; 特定缓冲区或字符串
(tp-set START END '(PROPERTY VALUE ...) OBJECT)

;; 整个字符串（平铺属性）
(tp-set STRING PROPERTY VALUE ...)
```

**示例：**

```elisp
;; 在缓冲区区域设置 face
(tp-set 1 10 '(face bold))  ; => (1 . 10)

;; 设置多个属性
(tp-set 1 10 '(face bold help-echo "Click me"))

;; 在特定缓冲区设置
(tp-set 1 10 '(face italic) my-buffer)

;; 在字符串上设置属性（0 索引）
(setq my-string (tp-set 0 5 '(face italic) "Hello World"))
;; => #("Hello World" 0 5 (face italic))

;; 在整个字符串上设置属性
(tp-set "Hello" 'face 'bold 'mouse-face 'highlight)
;; => #("Hello" 0 5 (face bold mouse-face highlight))
```

---

#### `tp-reset` - 替换所有属性

用指定的属性完全替换所有文本属性。

```elisp
(tp-reset START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-reset STRING PROPERTY VALUE ...)
```

**示例：**

```elisp
;; 替换区域中的所有属性
(tp-reset 1 10 '(face bold))  ; 任何现有属性都会被移除

;; 在字符串上
(tp-reset "Hello" 'face 'italic)
```

---

#### `tp-add` - 添加/合并属性

添加或更新属性，支持嵌套属性列表的深度合并。

```elisp
(tp-add START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-add STRING PROPERTY VALUE ...)
```

**示例：**

```elisp
;; 添加属性（保留现有，合并嵌套）
(tp-add 1 10 '(help-echo "tooltip"))

;; 深度合并 face 属性
(tp-set 1 10 '(face (:foreground "red")))
(tp-add 1 10 '(face (:background "blue")))
;; 结果: face 是 (:foreground "red" :background "blue")

;; Face 前置 - 符号 face 会被添加到 face 列表的开头
(tp-set "Hello" 'face 'bold)
(tp-add "Hello" 'face 'shadow)
;; 结果: face 是 (shadow bold)
```

---

#### `tp-set-face` - 设置 Face 属性

只设置 face 属性，保留其他属性。

```elisp
(tp-set-face START END FACE &optional OBJECT)
(tp-set-face STRING FACE)
```

**示例：**

```elisp
(tp-set-face 1 10 'bold)
(tp-set-face 1 10 '(:foreground "red" :weight bold))
(tp-set-face "Hello" 'italic)
```

---

#### `tp-set-display` - 设置 Display 属性

只设置 display 属性，保留其他属性。

```elisp
(tp-set-display START END DISPLAY &optional OBJECT)
(tp-set-display STRING DISPLAY)
```

**示例：**

```elisp
(tp-set-display 1 10 '(space :width 10))
(tp-set-display "  " '(space :width 20))
```

---

#### `tp-get` - 获取属性值

从位置或范围获取属性值，支持嵌套子属性访问。

对于范围和整个字符串查询，返回 `(START END VALUE)` 区间列表，让你可以查看范围内所有的属性值。

```elisp
;; 单个位置
(tp-get POSITION PROPERTY)
(tp-get POSITION PROPERTY OBJECT)

;; 嵌套子属性访问
(tp-get POSITION PROPERTY SUB-KEY ...)

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
;; 从当前缓冲区获取
(tp-get 5 'face)           ; => bold

;; 获取嵌套子属性
(tp-get 5 'face :foreground)      ; => "red"
(tp-get 5 'face :box :color)      ; => "blue"
(tp-get 5 'display :width)        ; => 10

;; 从字符串获取（0 索引）
(tp-get 0 'face my-string) ; => italic

;; 从范围获取 - 返回 (START END VALUE) 区间列表
(tp-get 1 10 'face)        ; => ((1 6 bold))

;; 获取多个区间
(tp-set 0 5 '(face bold) str)
(tp-set 12 17 '(face italic) str)
(tp-get 0 17 'face str)    ; => ((0 5 bold) (12 17 italic))

;; 使用列表形式的属性路径
(tp-get 5 20 '(face :underline :style) my-string)  ; => ((5 20 wave))

;; 从整个字符串获取深层嵌套属性
(tp-get str 'face :underline :color)  ; => ((0 5 "green") (6 11 "yellow"))

;; 从嵌套属性中获取多个键
(tp-get str 'face :underline '(:color :style))
;; => ((0 5 (:color "green" :style wave)) (6 11 (:color "yellow" :style line)))

;; 获取范围内的所有属性
(tp-get 1 10)              ; => ((1 6 (face bold help-echo "test")))

;; 从整个字符串获取 - 返回区间列表
(tp-get str)               ; => ((0 5 (face bold)) (12 17 (face italic)))
(tp-get str 'face)         ; => ((0 5 bold) (12 17 italic))
(tp-get str 'face :foreground)    ; => ((0 5 "red") (12 17 "blue"))
(tp-get str '(face :foreground))  ; => ((0 5 "red") (12 17 "blue"))
```

---

#### 细粒度属性函数

用于操作复杂属性（如 `face` 或 `display`）内的子属性：

```elisp
;; 获取子属性
(tp-get-sub POSITION PROPERTY SUB-PROPERTY &optional OBJECT)

;; 设置子属性
(tp-put-sub START END PROPERTY SUB-PROPERTY VALUE &optional OBJECT)

;; 移除子属性
(tp-remove-sub START END PROPERTY SUB-PROPERTY &optional OBJECT)
```

**示例：**

```elisp
;; 获取 face 的 :foreground
(tp-get-sub 1 'face :foreground)  ; => "red"

;; 设置 face 的 :weight
(tp-put-sub 1 6 'face :weight 'bold)

;; 移除 face 的 :background
(tp-remove-sub 1 6 'face :background)
```

---

#### `tp-at` - 获取所有属性

```elisp
(tp-at &optional POINT OBJECT)
```

获取 POINT 位置的所有文本属性，返回属性列表。

**示例：**

```elisp
(tp-at 5)  ; => (face bold help-echo "test")
(tp-at 0 my-string)  ; 从字符串获取
```

---

#### `tp-remove` - 移除属性

从区域或整个字符串中移除属性或嵌套子属性。

```elisp
;; 移除整个属性（缓冲区）
(tp-remove START END PROPERTY &optional OBJECT)

;; 移除子属性（缓冲区）
(tp-remove START END '(PROPERTY SUB-KEY) &optional OBJECT)

;; 移除嵌套子属性（缓冲区）
(tp-remove START END '(PROPERTY SUB-KEY (NESTED-KEYS...)) &optional OBJECT)

;; 从整个字符串移除
(tp-remove STRING PROP1 PROP2 ...)
(tp-remove STRING PROPERTY SUB-KEY)
(tp-remove STRING PROPERTY SUB-KEY '(NESTED-KEYS...))
```

**示例：**

```elisp
;; 移除整个属性
(tp-remove 1 10 'face)

;; 从 face 移除子属性
(tp-remove 1 10 '(face :underline))

;; 移除特定嵌套键，保留其他
(tp-remove 1 10 '(face :underline (:style :position)))
;; 从 :underline 移除 :style 和 :position
;; 如果 :underline 中存在 :color，则保留

;; 从整个字符串移除
(tp-remove "Hello World" 'face 'help-echo)  ; 移除多个属性
(tp-remove "Hello World" 'face :underline)  ; 移除子属性
(tp-remove "Hello World" 'face :underline '(:style :position))  ; 移除嵌套
```

---

#### `tp-remove-list` - 移除多个属性

```elisp
(tp-remove-list START END PROPERTIES &optional OBJECT)
```

一次移除多个属性。

**示例：**

```elisp
(tp-remove-list 1 10 '(face help-echo mouse-face))
```

---

#### `tp-clear` - 清除所有属性

```elisp
(tp-clear &optional START END OBJECT)
```

清除区域中的所有文本属性。

**示例：**

```elisp
(tp-clear 1 10)     ; 清除区域
(tp-clear)          ; 清除整个缓冲区
```

---

### 模式匹配函数

#### `tp-match` - 匹配字符串

```elisp
;; 缓冲区
(tp-match PATTERN '(PROPERTY VALUE ...))

;; 字符串或缓冲区对象
(tp-match PATTERN OBJECT '(PROPERTY VALUE ...))

;; 使用 (PATTERN STRING) 格式
(tp-match '(PATTERN STRING) '(PROPERTY VALUE ...))
```

在所有字符串模式匹配处设置属性。

**示例：**

```elisp
;; 在缓冲区中 - 返回 (START . END) 对的列表
(tp-match "TODO" '(face warning))
;; => ((10 . 14) (50 . 54) ...)

;; 在字符串上 - 返回修改后的字符串
(tp-match "o" "Hello World" '(face bold))
;; => #("Hello World" 4 5 (face bold) 7 8 (face bold))

;; 使用 (PATTERN STRING) 格式
(tp-match '("world" "Hello world") '(face bold))
;; => #("Hello world" 6 11 (face bold))
```

---

#### `tp-match-reset` - 匹配并重置

重置（完全替换）匹配处的所有属性。

```elisp
(tp-match-reset PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

**示例：**

```elisp
(tp-match-reset "TODO" '(face warning))
;; 替换匹配文本上的所有属性
```

---

#### `tp-match-add` - 匹配并添加

在匹配处添加/合并属性，支持深度合并。

```elisp
(tp-match-add PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

**示例：**

```elisp
(tp-match-add "TODO" '(face (:underline t)))
;; 与现有属性合并
```

---

#### `tp-regexp` - 匹配正则表达式

```elisp
;; 缓冲区
(tp-regexp PATTERN '(PROPERTY VALUE ...))

;; 字符串或缓冲区对象
(tp-regexp PATTERN OBJECT '(PROPERTY VALUE ...))
```

在所有正则表达式匹配处设置属性。

**示例：**

```elisp
;; 高亮缓冲区中的所有数字
(tp-regexp "[0-9]+" '(face font-lock-number-face))

;; 在字符串上
(tp-regexp "[A-Z]+" "Hello WORLD" '(face bold))
;; => #("Hello WORLD" 6 11 (face bold))
```

---

#### `tp-regexp-reset` - 正则匹配并重置

重置（完全替换）正则匹配处的所有属性。

```elisp
(tp-regexp-reset PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

---

#### `tp-regexp-add` - 正则匹配并添加

在正则匹配处添加/合并属性，支持深度合并。

```elisp
(tp-regexp-add PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

---

### 搜索和导航函数

#### `tp-forward` / `tp-backward`

```elisp
(tp-forward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
(tp-backward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
```

向前/向后搜索具有 PROPERTY 的文本。

**示例：**

```elisp
;; 查找下一个具有 'marker 属性的文本
(tp-forward 'marker)

;; 查找下一个 'type 等于 'heading 的文本
(tp-forward 'type 'heading)
```

---

#### `tp-next` / `tp-prev`

```elisp
(tp-next &optional POINT PROPERTY VALUE)
(tp-prev &optional POINT PROPERTY VALUE)
```

获取下一个/上一个具有文本属性的位置。

---

#### `tp-goto-next` / `tp-goto-prev`

```elisp
(tp-goto-next &optional PROPERTY VALUE)
(tp-goto-prev &optional PROPERTY VALUE)
```

将光标移动到下一个/上一个具有 PROPERTY 的文本。

---

#### `tp-regions-map` / `tp-strings-map`

```elisp
(tp-regions-map FUNCTION PROPERTY &optional VALUE PREDICATE COLLECT)
(tp-strings-map FUNCTION PROPERTY &optional VALUE PREDICATE COLLECT)
```

对所有具有 PROPERTY 的区域/字符串应用函数。

**示例：**

```elisp
;; 处理所有标记的文本
(tp-strings-map
 (lambda (str idx)
   (message "找到: %s，索引 %d" str idx))
 'marker)
```

---

### 查询函数

#### `tp-in` - 查找具有属性的区域

```elisp
(tp-in PROPERTY &optional VALUE START END)
```

获取当前缓冲区中所有具有 PROPERTY 的区域。

**示例：**

```elisp
;; 获取所有具有 'marker 属性的区域
(tp-in 'marker)
;; => ((1 5 (marker t ...)) (10 15 (marker t ...)))

;; 按值过滤
(tp-in 'type 'heading)
```

---

#### `tp-all` - 获取所有带属性的区域

```elisp
(tp-all &optional START END)
```

获取所有具有任何文本属性的区域。

---

#### `tp-intervals` - 获取属性区间

```elisp
(tp-intervals START END &optional OBJECT)
```

获取区域中所有文本属性区间。

---

#### `tp-empty-p` - 检查属性

```elisp
(tp-empty-p OBJECT)
```

如果 OBJECT 没有文本属性则返回 t。

---

#### `tp-plist` - 获取合并的属性

```elisp
(tp-plist START END &optional OBJECT)
```

获取区域中所有属性的合并属性列表。

---

## 图层系统

**图层系统**是 tp.el 的创新功能，允许在同一文本区域上堆叠多组属性。只有**顶层**可见，但下层会被保留，可以通过轮换或置顶来显示。

### 图层概念

```
┌─────────────────────────────┐
│   顶层（可见）              │  ← idx=0，你看到的
├─────────────────────────────┤
│   中间层（隐藏）            │  ← idx=1，被保留
├─────────────────────────────┤
│   底层（隐藏）              │  ← idx=-1，被保留
└─────────────────────────────┘
```

### 图层定义

#### `tp-define-layer` - 定义图层

定义单个图层或多个图层组。

**单个图层：**

```elisp
(tp-define-layer layer-name
  (face (:background "cyan") line-prefix ">>"))
```

**多个图层（图层组）：**

```elisp
(tp-define-layer my-group
  layer-1                                    ; 引用已存在的图层
  (face (:background "red") line-prefix ">>")    ; 匿名图层
  (face (:background "green" :weight bold)))     ; 另一个匿名图层
```

定义中的第一个图层是顶层（默认可见）。

**示例：**

```elisp
;; 定义单个图层
(tp-define-layer highlight
  (face (:background "yellow" :foreground "black")))

(tp-define-layer error
  (face (:background "red" :foreground "white")
   help-echo "错误!"))

(tp-define-layer info
  (face (:background "blue" :foreground "white")))

;; 定义图层组
(tp-define-layer status-colors
  highlight
  error
  info)
```

---

#### `tp-layer-props` / `tp-group-props`

```elisp
(tp-layer-props LAYER-NAME)
(tp-group-props GROUP-NAME)
```

获取图层或图层组中所有图层的属性。

---

#### `tp-layer-undefine` / `tp-group-undefine`

```elisp
(tp-layer-undefine NAME)
(tp-group-undefine NAME)
```

移除图层或图层组定义。

---

#### `tp-layer-reset`

```elisp
(tp-layer-reset)
```

清除所有图层和图层组定义。

---

### 图层放置

#### `tp-put-layer` - 在指定位置设置图层

```elisp
;; 缓冲区/字符串区域
(tp-put-layer START END LAYER IDX OBJECT)

;; 整个字符串
(tp-put-layer STRING LAYER IDX)
```

在图层堆栈的指定索引位置设置图层。

- `IDX = 0`：顶部（可见图层）
- `IDX = -1`：底部
- 其他值在该位置插入

**示例：**

```elisp
(tp-define-layer base (face default))
(tp-define-layer highlight (face (:background "yellow")))

;; 将 base 图层放在顶部
(tp-put-layer 1 10 'base 0)

;; 将 highlight 放在索引 1（顶部下面）
(tp-put-layer 1 10 'highlight 1)

;; 将图层放在底部
(tp-put-layer 1 10 'info -1)
```

---

#### `tp-push-layer` - 推送图层到顶部

```elisp
;; 缓冲区/字符串区域
(tp-push-layer START END LAYER OBJECT)

;; 整个字符串
(tp-push-layer STRING LAYER)
```

将图层推到堆栈顶部（相当于 `tp-put-layer ... 0`）。

**示例：**

```elisp
(tp-define-layer base (face default))
(tp-define-layer highlight (face (:background "yellow")))

;; 首先推入 base 图层
(tp-push-layer 1 10 'base)

;; 将 highlight 推到顶部（现在可见）
(tp-push-layer 1 10 'highlight)
```

---

### 图层删除

#### `tp-delete-layer` - 按名称/索引删除图层

```elisp
;; 缓冲区/字符串区域
(tp-delete-layer START END LAYER-NAME/IDX OBJECT)

;; 整个字符串
(tp-delete-layer STRING LAYER-NAME/IDX)
```

通过名称或索引从堆栈任意位置删除图层。

**示例：**

```elisp
;; 按名称删除
(tp-delete-layer 1 10 'highlight)

;; 删除顶层（idx=0）
(tp-delete-layer 1 10 0)

;; 删除底层
(tp-delete-layer 1 10 -1)
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

---

### 图层移动

#### `tp-raise-layer` - 上移/下移图层

```elisp
;; 缓冲区/字符串区域
(tp-raise-layer START END IDX/LAYER-NAME N OBJECT)

;; 整个字符串
(tp-raise-layer STRING IDX/LAYER-NAME N)
```

将图层上移 N 个位置。正数 N 向顶部移动，负数向底部移动。

**示例：**

```elisp
;; 将 layer1 上移 2 个位置
(tp-raise-layer 1 10 'layer1 2)

;; 将索引 2 的图层下移 1 个位置
(tp-raise-layer 1 10 2 -1)
```

---

#### `tp-rotate-layer` - 轮换图层

```elisp
;; 缓冲区/字符串区域
(tp-rotate-layer START END OBJECT)

;; 整个字符串
(tp-rotate-layer STRING)
```

轮换图层 - 顶层移到底部，下一层变为可见。

**示例：**

```elisp
;; 堆栈: highlight (顶) -> base (底)
(tp-rotate-layer 1 10)
;; 堆栈: base (顶) -> highlight (底)
```

---

#### `tp-pin-layer` - 将图层置顶

```elisp
;; 缓冲区/字符串区域
(tp-pin-layer START END IDX/LAYER-NAME OBJECT)

;; 整个字符串
(tp-pin-layer STRING IDX/LAYER-NAME)
```

将特定图层移到顶部（使其可见）。

**示例：**

```elisp
;; 将 'base 设为顶层
(tp-pin-layer 1 10 'base)
```

---

#### `tp-switch-layer` - 交换两个图层

```elisp
;; 缓冲区/字符串区域
(tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)

;; 整个字符串
(tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)
```

交换两个图层的位置。

**示例：**

```elisp
;; 交换 layer1 和 layer2
(tp-switch-layer 1 10 'layer1 'layer2)
```

---

### 图层合并

#### `tp-merge-layers` - 合并多个图层

```elisp
;; 缓冲区/字符串区域
(tp-merge-layers START END NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)

;; 整个字符串
(tp-merge-layers STRING NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...))
```

将指定的图层合并为一个新图层。列表中靠前的图层优先级更高。

**示例：**

```elisp
;; 将 layer1 和 layer2 合并为 merged-layer
(tp-merge-layers 1 10 'merged-layer '(layer1 layer2))

;; 按索引合并
(tp-merge-layers 1 10 'merged '(0 1 2))
```

---

#### `tp-flatten-layers` - 扁平化所有图层

```elisp
;; 缓冲区/字符串区域
(tp-flatten-layers START END NAME OBJECT)

;; 整个字符串
(tp-flatten-layers STRING NAME)
```

将所有图层扁平化为一个具有给定名称的单一图层。

**示例：**

```elisp
;; 将所有图层扁平化为 'flat-layer
(tp-flatten-layers 1 10 'flat-layer)

;; 使用 nil 名称扁平化（无名图层）
(tp-flatten-layers 1 10 nil)
```

---

### 图层查询函数

#### `tp-layer-list` - 列出所有图层

```elisp
(tp-layer-list START END &optional OBJECT)
```

获取区域中所有图层名称的列表。

**示例：**

```elisp
(tp-layer-list 1 10)  ; => (highlight base)
```

---

#### `tp-layer-count`

```elisp
(tp-layer-count START END &optional OBJECT)
```

计算区域中的图层数量。

---

#### `tp-layer-exists-p`

```elisp
(tp-layer-exists-p START END NAME &optional OBJECT)
```

检查区域中是否存在某图层。

---

#### `tp-layer-top`

```elisp
(tp-layer-top START END &optional OBJECT)
```

获取顶层（可见）图层的名称。

---

## 实用示例

### 多图层语法高亮

```elisp
;; 为不同高亮目的定义图层
(tp-define-layer code-base
  (face font-lock-keyword-face))

(tp-define-layer code-error
  (face (:underline (:color "red" :style wave))
   help-echo "语法错误"))

(tp-define-layer code-debug
  (face (:background "dark blue")))

;; 应用基础高亮
(tp-push-layer 1 100 'code-base)

;; 在有问题的代码上添加错误高亮
(tp-push-layer 50 60 'code-error)

;; 在错误和正常视图之间切换
(defun toggle-error-view ()
  (interactive)
  (tp-rotate-layer 50 60))
```

### 状态指示器

```elisp
;; 将状态图层定义为一个组
(tp-define-layer status-todo (face (:foreground "gray")))
(tp-define-layer status-progress (face (:foreground "yellow")))
(tp-define-layer status-done (face (:foreground "green")))
(tp-define-layer task-status status-todo status-progress status-done)

;; 循环切换状态
(defun cycle-task-status ()
  (interactive)
  (tp-rotate-layer (line-beginning-position) (line-end-position)))
```

### 临时高亮

```elisp
(tp-define-layer temp-highlight
  (face (:background "yellow")))

(defun flash-region (start end)
  "临时闪烁一个区域。"
  (tp-push-layer start end 'temp-highlight)
  (run-with-timer 0.5 nil
                  (lambda ()
                    (tp-delete-layer start end 'temp-highlight))))
```

---

## 别名

为方便使用，tp.el 提供以下别名：

| 别名 | 原函数 |
|------|--------|
| `tp-layer-properties` | `tp-layer-props` |
| `tp-layer-group-properties` | `tp-group-props` |
| `tp-layer-group-undefine` | `tp-group-undefine` |

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
