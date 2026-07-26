# tp 代码架构文档

本文档描述 tp 库的模块分层结构与函数调用层次，从底层基础模块到上层功能模块的分层组织。

自 0.2.0 起，原来的单文件 tp.el 已拆分为九个分层模块，`tp.el` 只作为总入口（`(require 'tp)` 依次加载全部模块，用户接口不变）。各模块的变更缘由见 [CHANGELOG.md](../CHANGELOG.md)。

## 目录

- [架构概述](#架构概述)
- [模块分层](#模块分层)
  - [tp-core.el：基础工具](#tp-coreel基础工具)
  - [tp-reactive.el：响应式基础设施](#tp-reactiveel响应式基础设施)
  - [tp-layer.el：层定义与解析](#tp-layerel层定义与解析)
  - [tp-ops.el：核心属性操作](#tp-opsel核心属性操作)
  - [tp-search.el：模式匹配与搜索](#tp-searchel模式匹配与搜索)
  - [tp-render.el：响应式渲染引擎](#tp-renderel响应式渲染引擎)
  - [tp-stack.el：属性层栈操作](#tp-stackel属性层栈操作)
  - [tp-palette.el：调色板数据](#tp-paletteel调色板数据)
  - [tp-builtins.el：内置层与辅助工具](#tp-builtinsel内置层与辅助工具)
- [钩子变量：唯一许可的反向调用](#钩子变量唯一许可的反向调用)
- [函数调用关系图](#函数调用关系图)
- [设计原则](#设计原则)

---

## 架构概述

tp 采用严格的线性分层：**每个模块只允许 `require` 并调用排在它前面的模块**，字节编译器强制检查这一依赖顺序。加载顺序即依赖顺序：

```
tp-core → tp-reactive → tp-layer → tp-ops → tp-search
        → tp-render → tp-stack → tp-palette → tp-builtins
```

```
┌────────────────────────────────────────────────────────────────┐
│  tp.el —— 总入口，按序 require 全部模块                          │
└────────────────────────────────────────────────────────────────┘
┌────────────────────────────────────────────────────────────────┐
│  tp-builtins.el   内置层（tp-link, tp-space, tp-headline …）、   │
│                   tp-palette-show、显示缓冲辅助宏                │
├────────────────────────────────────────────────────────────────┤
│  tp-palette.el    明/暗主题调色板数据、tp-parse-color            │
├────────────────────────────────────────────────────────────────┤
│  tp-stack.el      层栈操作（push/pop/move/merge/flatten …）      │
├────────────────────────────────────────────────────────────────┤
│  tp-render.el     响应式重渲染引擎 ──┐                          │
├──────────────────────────────────── │ ─────────────────────────┤
│  tp-search.el     tp-match-*/tp-regexp-*、tp-search、导航       │
├──────────────────────────────────── │ ─────────────────────────┤
│  tp-ops.el        tp-set/reset/add/get/at/remove/clear         │
│                   ◁╌╌ tp--tp-text-handler-function ╌╌╌╌┤       │
├──────────────────────────────────── │ ─────────────────────────┤
│  tp-layer.el      define-tp/define-tps、层注册表与解析          │
│                   ◁╌╌ tp--layer-refresh-function ╌╌╌╌╌╌┤       │
├──────────────────────────────────── │ ─────────────────────────┤
│  tp-reactive.el   响应式依赖注册表、变量监听、批量队列           │
│                   ◁╌╌ tp--reactive-update-function ╌╌╌╌┤       │
│                   ◁╌╌ tp--reactive-flush-function ╌╌╌╌╌┘       │
├────────────────────────────────────────────────────────────────┤
│  tp-core.el       区间遍历、plist/face 合并引擎、               │
│                   调试日志、$var 符号工具                       │
└────────────────────────────────────────────────────────────────┘

实线层级：上层模块调用下层模块（require 依赖）。
虚线（◁╌╌）：钩子变量 —— 下层模块预留的函数变量，
由 tp-render.el 在加载时安装实现（见下文）。
```

早期文档把"响应式系统"画在高级 API 之下、却又让它向上调用 `tp-search-map`，与自身的分层原则矛盾。现在这一矛盾已在代码层面消除：需要向上调用的逻辑全部收拢进 `tp-render.el`（位于 `tp-search.el` 之上，可以直接调用它）；下层模块（tp-reactive、tp-layer、tp-ops）通过**钩子变量**触发渲染，自身不依赖任何上层模块。

---

## 模块分层

### tp-core.el：基础工具

最底层模块，不依赖任何其他 tp 模块，提供区间遍历、合并引擎与调试能力。

#### 区间操作
| 函数 | 描述 | 主要调用者 |
|------|------|--------|
| `tp-intervals` | 获取区域内文本属性区间列表（裁剪到 [START, END)） | tp-intervals-map, tp-get |
| `tp-intervals-map` | 对区间应用函数 | 多个属性/层操作函数 |
| `tp--map-intervals` | 共享的裁剪式区间遍历引擎 | tp-intervals-map, tp-ops/tp-stack 的区域操作 |
| `tp-plist` | 获取区域中合并后的所有属性 | 用户 API |
| `tp-empty-p` | 检查对象是否没有文本属性 | 用户 API |

#### plist / face 合并引擎
| 函数 | 描述 | 主要调用者 |
|------|------|--------|
| `tp--deep-merge-plist` | 深度合并两个 plist | tp-add, tp--prepend-face 等 |
| `tp--prepend-face` | face 家族属性的合并逻辑 | tp-add, tp-match-add |
| `tp--merge-face-values` | 合并两个 face 值 | 合并引擎内部 |
| `tp--merge-duplicate-keys` | 合并 plist 中的重复键 | tp--parse-args |
| `tp--parse-face-list` | 解析 face 列表 | 合并引擎内部 |
| `tp--get-nested` | 按路径获取嵌套属性值 | tp-get, tp-at |

`tp-face-properties`（常量，`'(face font-lock-face mouse-face)`）定义参与 face 感知合并的属性家族。

#### `$var` 符号工具
| 函数 | 描述 |
|------|------|
| `tp--reactive-symbol-p` | 检查是否为 `$var` 响应式符号 |
| `tp--reactive-var-symbol` | `$var` 符号转变量符号 |
| `tp--collect-reactive-symbols` | 收集表达式中所有 `$var` 符号 |
| `tp--resolve-reactive-symbols` | 将 `$var` 解析为当前值（支持覆盖表） |
| `tp--extract-reactive-props` | 提取引用特定变量的属性 |

#### 调试工具
| 变量/函数 | 描述 |
|-----------|------|
| `tp-debug-mode` | 启用/禁用调试模式 |
| `tp-debug-echo` | 是否在 minibuffer 显示调试信息 |
| `tp-debug-log` | 记录调试信息 |
| `tp-debug-show` | 显示 *tp-debug* 缓冲区 |
| `tp-debug-clear` | 清除调试日志 |

另有辅助宏 `tp-with-current-buffer`。

---

### tp-reactive.el：响应式基础设施

只依赖 tp-core。维护响应式依赖注册表、变量监听器与批量更新队列；**不包含任何渲染逻辑**，重渲染通过钩子变量委托给 tp-render.el。

#### 依赖注册与管理
| 函数/变量 | 描述 |
|------|------|
| `tp-reactive-deps` | 变量 → 依赖它的层及属性 的注册表 |
| `tp--register-reactive-deps` | 注册响应式依赖 |
| `tp--unregister-reactive-deps` | 取消注册依赖（含 watchers/computed/data） |
| `tp--layer-has-reactive-deps-p` | 层是否有响应式依赖 |
| `tp--register-layer-watchers` / `tp--unregister-layer-watchers` | 注册/清除 `:watch` 回调 |
| `tp--register-layer-computed` / `tp--unregister-layer-computed` | 注册/清除 `:compute` 计算属性 |
| `tp--register-layer-data` / `tp--unregister-layer-data` | 注册/清除 `:data` 变量 |
| `tp--apply-initial-computed` | 计算 `:compute` 的初始值 |
| `tp--ensure-reactive-variables` | 确保 `$var` 对应的变量已定义 |
| `tp-reactive-reset` | 重置全部响应式注册表 |

#### 变量监听与批量更新
| 函数/宏 | 描述 |
|------|------|
| `tp--reactive-variable-watcher` | `add-variable-watcher` 回调；调用 `:watch` 后经 `tp--reactive-update-function` 委托重渲染 |
| `tp--invoke-layer-watchers` | 调用层的 `:watch` 回调 |
| `tp-with-batch-updates` | 批量更新宏 |
| `tp--queue-batch-update` | 将更新加入待处理队列 |
| `tp--flush-batch-updates` | 刷新队列，经 `tp--reactive-flush-function` 委托重渲染 |

钩子变量：`tp--reactive-update-function`、`tp--reactive-flush-function`（定义于此，由 tp-render.el 安装）。

---

### tp-layer.el：层定义与解析

依赖 tp-core、tp-reactive。提供 `define-tp` / `define-tps` 宏、层注册表、层名解析，以及层栈的数据结构原语。

#### 层定义
| 函数/宏 | 描述 | 依赖 |
|---------|------|------|
| `define-tp` | 定义单个自定义文本属性（层） | tp--define-layer-internal |
| `define-tps` | 定义自定义文本属性组（层组）；别名 `define-tp-group` | tp--define-layer-group-internal |
| `tp--define-layer-internal` | 层定义的运行时实现 | tp--parse-define-layer-args, tp--collect-reactive-symbols, tp--ensure-reactive-variables, tp--register-*, tp--layer-refresh |
| `tp--parse-define-layer-args` | 解析 `:props` / `:data` / `:compute` / `:watch` / `:transform` | - |
| `tp--parse-layer-group-element` | 解析层组元素 | tp--layer-group-element-format |
| `tp--define-layer-from-parsed` | 从解析结果定义层 | （与 define-tp 类似的依赖） |
| `tp--check-layer-cycle` | 检测循环层引用并报错 | tp--layer-expansion-stack |
| `tp--anonymous-layer-name-for` | 匿名响应式层的驻留（`equal` 的 props 复用注册项） | - |

#### 注册表与查询
| 函数/变量 | 描述 |
|------|------|
| `tp-layer-alist` / `tp-layer-groups` / `tp-layer-transforms` | 层、层组、转换函数注册表 |
| `tp--set-layer-props` / `tp--set-group-layers` | 写入注册表 |
| `tp-layer-props` / `tp-group-props` | 获取层/层组属性（`&optional INCLUDE-TP-NAME`，默认不含 `tp-name`；返回副本） |
| `tp-layer-props-with-arg` / `tp-group-props-with-arg` | 参数化层/层组的属性求值 |
| `tp-layer-parameterized-p` / `tp-group-parameterized-p` | 是否参数化 |
| `tp-layer-reset` | 重置层系统 |
| `tp-undefine-layer` / `tp-undefine-group` | 删除层/层组（含其响应式依赖与转换） |

#### 属性解析
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--resolve-props` | 解析属性（展开层名、`$var`、注册依赖） | tp-layer-props, tp--collect-reactive-symbols, tp--resolve-reactive-symbols, tp--register-reactive-deps |
| `tp--expand-layer-in-plist` | 展开 plist 中的层名键 | tp--is-layer-name-p |
| `tp--expand-layer-to-props-list` | 层名展开为属性列表 | tp--check-layer-cycle |

#### 层栈数据结构原语
| 函数 | 描述 |
|------|------|
| `tp--normalize-layer-spec` | 规范化层规格 |
| `tp--get-layer-stack` | 获取位置的层栈 |
| `tp--build-layer-props` | 从层列表构建属性 |
| `tp--layer-stack-to-list` | 将层栈转换为列表 |
| `tp--get-layer-by-idx-or-name` | 通过索引或名称查找层 |

钩子变量：`tp--layer-refresh-function`（定义于此，由 tp-render.el 安装为 `tp--update-layer-regions`）；`tp--layer-refresh` 是它的调用入口，层重定义后经它触发已应用区域的重渲染。

---

### tp-ops.el：核心属性操作

依赖 tp-core、tp-layer。面向用户的核心属性读写函数，直接调用 Emacs 原生文本属性 API。

#### 参数解析
| 函数 | 描述 | 调用者 |
|------|------|--------|
| `tp--parse-args` | 解析灵活的调用格式（整串/区域/层名） | tp-set, tp-reset, tp-add |
| `tp--apply-props-to-string` | 字符串路径的属性应用 | tp-set, tp-reset, tp-add |

#### 设置属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-set` | 设置文本属性（保留其他属性） | tp--parse-args, tp--handle-tp-text | tp-match-set, 层操作 |
| `tp-reset` | 完全替换所有文本属性 | tp--parse-args, tp--handle-tp-text | tp-match-reset |
| `tp-add` | 深度合并属性 | tp--parse-args, tp--deep-merge-plist, tp--prepend-face | tp-match-add |

#### 获取属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-get` | 获取范围内的属性值（返回区间列表） | tp--get-nested | 搜索函数 |
| `tp-at` | 获取单个位置的属性值 | tp--get-nested | 大多数高层函数 |
| `tp-member` | 区分"属性值为 nil"与"属性不存在"（plist-member 风格） | - | 用户 API |

#### 删除属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-remove` | 移除属性或子属性 | tp--remove-property, tp--remove-sub, tp--remove-*-from-string | 用户 API |
| `tp-clear` | 清除所有属性 | - | 用户 API |

钩子变量：`tp--tp-text-handler-function`（定义于此，由 tp-render.el 安装为 `tp--handle-tp-text-property`）；`tp--handle-tp-text` 是它的调用入口，未安装时 `tp-text` 属性按普通属性处理。

---

### tp-search.el：模式匹配与搜索

依赖 tp-core、tp-layer、tp-ops。提供模式匹配式属性应用、属性搜索与导航。

#### 模式匹配
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-match-set` / `tp-match-reset` / `tp-match-add` | 在字符串匹配处设置/重置/合并属性 | tp--match-apply |
| `tp-regexp-set` / `tp-regexp-reset` / `tp-regexp-add` | 在正则匹配处设置/重置/合并属性 | tp--regexp-apply |
| `tp--match-apply` / `tp--regexp-apply` | 字面/正则匹配的入口（含多模式支持） | tp--pattern-apply |
| `tp--pattern-apply` / `tp--pattern-apply-single` | 共享的模式匹配引擎（空模式/零宽模式安全） | tp-set/tp-reset/tp-add 风格的 apply-fn |
| `tp--deep-merge-apply` / `tp--reset-apply` | 传给引擎的合并/重置回调 | tp--deep-merge-plist 等 |

#### 搜索和导航
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-search-forward` | 向前搜索属性 | text-property-search-forward |
| `tp-search-backward` | 向后搜索属性 | tp--property-search-backward |
| `tp--property-search-backward` | 带等值谓词的向后搜索（与向前语义对称） | text-property-search-backward |
| `tp-forward` | 向前搜索 N 次并移动点 | tp-search-forward |
| `tp-backward` | 向后搜索 N 次并移动点 | tp-search-backward |
| `tp-search` | 收集所有匹配区间 | tp-intervals 等 |

#### 遍历与替换
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-forward-do` / `tp-backward-do` | 向前/向后搜索并对匹配执行函数 | tp--forward-do / tp--backward-do |
| `tp--forward-do` / `tp--backward-do` | 单方向遍历的内部实现 | tp--replace-match-text |
| `tp-search-map` | 对所有匹配应用函数（FUNCTION 接收 TEXT &optional START END IDX） | tp--search-do |
| `tp--search-do` | 搜索遍历的内部实现 | tp--replace-match-text |
| `tp--replace-match-text` | 共享的匹配文本替换助手（缓冲区支持变长替换；字符串变长时报错） | - |

---

### tp-render.el：响应式渲染引擎

依赖 tp-core、tp-reactive、tp-layer、tp-ops、tp-search。这是唯一"知道"渲染如何进行的模块：它可以直接调用 `tp-search-map`、`tp-add` 等前置模块的函数，并在加载末尾把自己的入口函数**安装**进下层模块预留的钩子变量。

#### 重渲染
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--update-layer-regions` | 重渲染携带某层的所有文本区域（替换该层自己的属性键，保留其他来源属性） | tp--layer-render-props, tp-search-map |
| `tp--update-layer-computed` | 更新 `:compute` 计算属性（nil 值可正常传播） | tp--resolve-reactive-symbols, tp--set-layer-props |
| `tp--layer-render-props` / `tp--layer-reactive-props` | 求取层的渲染属性 | tp-layer-props |

#### 响应式文本（tp-text）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--handle-tp-text-property` | 处理 `tp-text` 属性（文本替换） | tp--tp-text-replace |
| `tp--update-reactive-text` | 变量变化后更新响应式文本 | tp--replace-reactive-text-in-buffer |
| `tp--replace-reactive-text-in-buffer` | 在缓冲区中替换响应式文本 | - |
| `tp--tp-text-transform` | 应用 `:transform` 转换（首次渲染同样生效） | tp-layer-transforms |

#### 引擎入口与钩子安装
| 函数 | 描述 |
|------|------|
| `tp--reactive-apply-update` | 变量变化的完整处理：更新 computed、合并层定义、重渲染或入批量队列（嵌套写入经队列而非递归）。安装为 `tp--reactive-update-function` |
| `tp--reactive-flush-entry` | 批量队列刷新时的重渲染入口。安装为 `tp--reactive-flush-function` |

加载末尾执行安装：

```elisp
(setq tp--reactive-update-function #'tp--reactive-apply-update)
(setq tp--reactive-flush-function  #'tp--reactive-flush-entry)
(setq tp--tp-text-handler-function #'tp--handle-tp-text-property)
(setq tp--layer-refresh-function   #'tp--update-layer-regions)
```

---

### tp-stack.el：属性层栈操作

依赖 tp-core、tp-layer、tp-ops。所有栈变更函数建立在共享的裁剪式区域遍历之上，区域操作不会影响 [START, END) 之外的文本。

#### 内部助手
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--parse-layer-args` | 解析层操作的灵活参数 | - |
| `tp--stack-map-region` | 按区间遍历区域内层栈的共享引擎 | tp--map-intervals 风格遍历 |
| `tp--stack-build-props` | 从层列表构建栈属性（单层栈不携带 `tp-layers`） | - |
| `tp--put-layer-specs` | 展开层规格（层名/内联 plist/层名列表/参数化/层组） | tp--normalize-layer-spec, tp-group-props(-with-arg) |
| `tp--move-layer-in-stack` | 在栈中移动层 | tp--get-layer-by-idx-or-name |
| `tp--raise-layer-in-stack` | 在栈中上下移动层 | tp--move-layer-in-stack |
| `tp--switch-layers-in-stack` | 交换两个层的位置 | tp--get-layer-by-idx-or-name |

#### 层操作（公开 API）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-put-layer` | 在指定索引放置层（区域局部） | tp--put-layer-specs, tp--stack-map-region |
| `tp-push-layer` | 将层推到顶部 | tp-put-layer |
| `tp-delete-layer` | 删除层 | tp--stack-map-region |
| `tp-pop-layer` | 弹出顶层 | tp-delete-layer |
| `tp-move-layer` | 移动层到指定位置 | tp--move-layer-in-stack, tp--stack-map-region |
| `tp-raise-layer` | 上移/下移层 | tp--raise-layer-in-stack, tp--stack-map-region |
| `tp-rotate-layer` | 轮换层 | tp-move-layer |
| `tp-pin-layer` | 将层置顶 | tp-move-layer |
| `tp-switch-layer` | 交换两个层 | tp--switch-layers-in-stack, tp--stack-map-region |
| `tp-merge-layers` | 合并多个层（显式 nil 值保留） | tp--merge-layer-props, tp--stack-map-region |
| `tp-flatten-layers` | 扁平化所有层 | tp--merge-layer-props, tp--stack-map-region |

#### 层查询
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-layer-list` | 列出所有层名称 | tp--stack-map-region |
| `tp-layer-count` | 计算层数量 | tp--stack-map-region |
| `tp-layer-exists-p` | 检查层是否存在 | tp-layer-list |
| `tp-layer-top` | 获取顶层名称（覆盖整个请求区域） | tp--stack-map-region |
| `tp-region-layer-props` | 获取区域中特定层的属性 | tp--stack-map-region |

#### 层属性操作
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-add-to-layers` | 向特定层添加属性 | tp--deep-merge-plist, tp--stack-map-region |
| `tp-add-to-all-layers` | 向所有层添加属性 | tp-add-to-layers |

---

### tp-palette.el：调色板数据

只依赖 tp-core（及 subr-x）。明/暗主题双值调色板系统，`tp-palette-alist` 是唯一数据源。

| 函数/宏/变量 | 描述 |
|------|------|
| `define-tp-palette` | 定义调色板（重定义立即生效） |
| `tp-palette-alist` | 调色板注册表（唯一数据源） |
| `tp-parse-color` | 解析颜色规格（支持 `("light" . "dark")` 及单边 cons） |
| `tp-theme-dark-p` / `tp-theme-light-p` | 当前主题判断 |
| `tp-palette-fg-color` / `tp-palette-bg-color` / `tp-palette-border-color` | 取前景/背景/边框色 |
| `tp-palette-p` / `tp-palette-fg-p` / `tp-palette-bg-p` / `tp-palette-fbg-p` / `tp-palette-border-p` | 调色板谓词 |
| `tp-palette-pure` | 取纯色值 |

---

### tp-builtins.el：内置层与辅助工具

最上层模块，依赖 tp-core、tp-layer、tp-ops、tp-palette。提供开箱即用的内置层与展示/缓冲辅助。

| 定义 | 描述 |
|------|------|
| 内置层 | `tp-palette`、`tp-fg`、`tp-bg`、`tp-button`、`tp-underline`、`tp-delete`、`tp-link`、`tp-space`、`tp-headline`、`tp-action` 等（`define-tp` 定义；`tp-link` 的颜色在应用时解析，主题切换即时生效） |
| `tp-pop-to-buffer` / `tp-switch-to-buffer` | 显示带属性文本的缓冲辅助宏（q 绑定在缓冲区局部 minor-mode keymap 中） |
| `tp-palette-show` | 展示所有调色板 |
| `tp-suffix-symbol` | 符号加后缀助手 |

---

## 钩子变量：唯一许可的反向调用

分层规则的唯一例外是四个**钩子变量**：下层模块声明变量并在需要时 `funcall`，实现由 tp-render.el 在加载时安装。这样下层模块不必 `require` 上层模块，依赖图保持严格单向；而在未加载 tp-render 时，下层模块依然可用（钩子为 nil 时优雅降级）。

| 钩子变量 | 声明于 | 安装的实现（tp-render.el） | 用途 |
|----------|--------|---------------------------|------|
| `tp--reactive-update-function` | tp-reactive.el | `tp--reactive-apply-update` | 变量监听器触发的重计算与重渲染 |
| `tp--reactive-flush-function` | tp-reactive.el | `tp--reactive-flush-entry` | 批量更新队列刷新时的重渲染 |
| `tp--layer-refresh-function` | tp-layer.el | `tp--update-layer-regions` | 层重定义后刷新已应用区域 |
| `tp--tp-text-handler-function` | tp-ops.el | `tp--handle-tp-text-property` | `tp-set` 等操作中处理 `tp-text` 属性 |

---

## 函数调用关系图

（标注 `[模块]` 表示函数所在文件；`╌╌▷` 表示经钩子变量的间接调用。）

### tp-set 调用链
```
tp-set [tp-ops]
  ├── tp--parse-args [tp-ops]
  │     ├── tp--merge-duplicate-keys [tp-core]
  │     └── tp--resolve-props [tp-layer]
  │           ├── tp-layer-props
  │           ├── tp--collect-reactive-symbols [tp-core]
  │           ├── tp--resolve-reactive-symbols [tp-core]
  │           └── tp--register-reactive-deps [tp-reactive]
  ├── tp--handle-tp-text [tp-ops]
  │     ╌╌▷ tp--handle-tp-text-property [tp-render]（经钩子）
  ├── tp--apply-props-to-string [tp-ops]（整串形式，返回新字符串）
  └── set-text-properties / put-text-property（Emacs 原生，区域形式）
```

### tp-add 调用链
```
tp-add [tp-ops]
  ├── tp--parse-args [tp-ops]
  ├── tp--handle-tp-text [tp-ops] ╌╌▷ tp--handle-tp-text-property [tp-render]
  ├── text-properties-at（Emacs 原生）
  ├── tp--prepend-face [tp-core]（face 家族属性）
  │     └── tp--deep-merge-plist [tp-core]
  ├── tp--deep-merge-plist [tp-core]（其他嵌套属性）
  └── put-text-property（Emacs 原生）
```

### define-tp 调用链
```
define-tp [tp-layer]（宏）
  └── tp--define-layer-internal [tp-layer]
        ├── tp--parse-define-layer-args [tp-layer]
        ├── tp--collect-reactive-symbols [tp-core]
        ├── tp--unregister-reactive-deps [tp-reactive]
        ├── tp--ensure-reactive-variables [tp-reactive]
        ├── tp--register-layer-data [tp-reactive]
        │     └── add-variable-watcher（Emacs 原生）
        ├── tp--register-layer-computed [tp-reactive]
        ├── tp--apply-initial-computed [tp-reactive]
        ├── tp--register-reactive-deps [tp-reactive]
        ├── tp--register-layer-watchers [tp-reactive]
        ├── tp--resolve-reactive-symbols [tp-core]
        ├── tp--set-layer-props [tp-layer]
        └── tp--layer-refresh [tp-layer]
              ╌╌▷ tp--update-layer-regions [tp-render]（经钩子）
                    └── tp-search-map [tp-search]
                          └── put-text-property
```

### tp-push-layer 调用链
```
tp-push-layer [tp-stack]
  ├── tp--parse-layer-args [tp-stack]
  └── tp-put-layer [tp-stack]
        ├── tp--put-layer-specs [tp-stack]
        │     ├── tp--normalize-layer-spec [tp-layer]
        │     │     └── tp-layer-props [tp-layer]
        │     └── tp-group-props / tp-group-props-with-arg [tp-layer]
        └── tp--stack-map-region [tp-stack]（裁剪到 [START, END)）
              ├── tp--stack-build-props [tp-stack]
              └── set-text-properties（Emacs 原生）
```

### 响应式更新调用链
```
(setq some-reactive-var new-value)
  └── tp--reactive-variable-watcher [tp-reactive]
        ├── tp--invoke-layer-watchers [tp-reactive]（:watch 回调）
        └── ╌╌▷ tp--reactive-apply-update [tp-render]（经钩子）
              ├── tp--update-layer-computed [tp-render]
              │     ├── tp--resolve-reactive-symbols [tp-core]
              │     └── tp--set-layer-props [tp-layer]
              ├── tp--set-layer-props [tp-layer]（深合并回层定义；setq-local 不写全局）
              ├── tp--update-layer-regions [tp-render]（属性更新）
              │     └── tp-search-map [tp-search]
              │           └── put-text-property
              └── tp--update-reactive-text [tp-render]（tp-text 文本替换）
                    └── tp--replace-reactive-text-in-buffer [tp-render]

批量模式（tp-with-batch-updates）/ 更新中的嵌套写入：
  └── tp--queue-batch-update [tp-reactive]（入队，不递归）
        └── tp--flush-batch-updates [tp-reactive]（退出批量时）
              └── ╌╌▷ tp--reactive-flush-entry [tp-render]（经钩子）
                    ├── tp--update-layer-regions
                    └── tp--update-reactive-text
```

---

## 设计原则

1. **严格分层**：模块只允许 `require` 并调用排在它前面的模块，字节编译器强制检查依赖顺序
2. **钩子反转**：唯一许可的"向上调用"是四个钩子变量（`tp--tp-text-handler-function`、`tp--reactive-update-function`、`tp--reactive-flush-function`、`tp--layer-refresh-function`），由 tp-render.el 统一安装实现
3. **单一职责**：每个模块（和函数）只负责一件事
4. **复用优先**：共享引擎（`tp--map-intervals`、`tp--stack-map-region`、`tp--pattern-apply`、`tp--replace-match-text`）承载重复逻辑，高层函数复用而非复制
5. **统一接口**：所有核心属性函数支持相同的调用约定（整串/区域形式、层名、`$var`）
6. **响应式解耦**：tp-reactive/tp-layer/tp-ops 不依赖渲染引擎；不加载 tp-render 时钩子为 nil，各模块优雅降级
