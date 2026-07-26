# tp 代码架构文档

本文档描述 tp 库的模块分层结构与函数调用层次，从底层基础模块到上层功能模块的分层组织。

自 0.2.0 起，原来的单文件 tp.el 已拆分为九个分层模块，`tp.el` 只作为总入口（`(require 'tp)` 依次加载全部模块，用户接口不变）。0.3.0 进一步收紧了模块边界：`tp-text` 处理链下沉至 tp-ops、批量更新上收至 tp-render、层栈存储编解码与匿名层机制归位 tp-layer，钩子变量从四个减少到两个。各变更的缘由见 [CHANGELOG.md](../CHANGELOG.md)。

## 目录

- [架构概述](#架构概述)
- [模块分层](#模块分层)
  - [tp-core.el：基础工具](#tp-coreel基础工具)
  - [tp-reactive.el：响应式基础设施](#tp-reactiveel响应式基础设施)
  - [tp-layer.el：层定义、解析与层栈存储](#tp-layerel层定义解析与层栈存储)
  - [tp-ops.el：核心属性操作与 tp-text 处理链](#tp-opsel核心属性操作与-tp-text-处理链)
  - [tp-search.el：模式匹配与搜索](#tp-searchel模式匹配与搜索)
  - [tp-render.el：响应式渲染引擎](#tp-renderel响应式渲染引擎)
  - [tp-stack.el：属性层栈操作](#tp-stackel属性层栈操作)
  - [tp-palette.el：调色板数据](#tp-paletteel调色板数据)
  - [tp-builtins.el：内置层与辅助工具](#tp-builtinsel内置层与辅助工具)
- [钩子变量：唯一许可的反向调用](#钩子变量唯一许可的反向调用)
- [可变状态清单](#可变状态清单)
- [函数调用关系图](#函数调用关系图)
- [设计原则](#设计原则)

---

## 架构概述

tp 采用严格的线性分层：**每个模块只允许 `require` 并调用排在它前面的模块**，字节编译器强制检查这一依赖顺序。加载顺序即依赖顺序：

```
tp-core → tp-reactive → tp-layer → tp-ops → tp-search
        → tp-render → tp-stack → tp-palette → tp-builtins
```

注意加载顺序是依赖顺序的**上界**：并非每个模块都依赖它前面的全部模块。各模块实际 `require` 的 tp- 模块如下（逐一核对自源码头部）：

| 模块 | require 的 tp- 模块 |
|------|--------------------|
| tp-core | —（仅 cl-lib、dash、seq） |
| tp-reactive | tp-core |
| tp-layer | tp-core、tp-reactive |
| tp-ops | tp-core、tp-reactive、tp-layer |
| tp-search | tp-core、tp-reactive、tp-layer、tp-ops |
| tp-render | tp-core、tp-reactive、tp-layer、tp-ops、tp-search |
| tp-stack | tp-core、tp-reactive、tp-layer（**不依赖 tp-ops / tp-search / tp-render**） |
| tp-palette | —（不依赖任何 tp- 模块，仅 subr-x） |
| tp-builtins | tp-core、tp-layer、tp-ops、tp-palette |

```
┌────────────────────────────────────────────────────────────────┐
│  tp.el —— 总入口，按序 require 全部模块                          │
└────────────────────────────────────────────────────────────────┘
┌────────────────────────────────────────────────────────────────┐
│  tp-builtins.el   内置层（tp-link, tp-space, tp-headline …）、   │
│                   tp-palette-show、显示缓冲辅助宏                │
├────────────────────────────────────────────────────────────────┤
│  tp-palette.el    明/暗主题调色板数据、tp-parse-color            │
│                   （独立叶模块，不依赖任何 tp- 模块）             │
├────────────────────────────────────────────────────────────────┤
│  tp-stack.el      层栈操作（push/pop/move/hide/show/merge …）    │
├────────────────────────────────────────────────────────────────┤
│  tp-render.el     响应式重渲染引擎、最小差异 tp-text 编辑、       │
│                   批量更新（tp-with-batch-updates + flush）──┐   │
├─────────────────────────────────────────────────────────── │ ──┤
│  tp-search.el     tp-match-*/tp-regexp-*、tp-search、导航    │   │
├─────────────────────────────────────────────────────────── │ ──┤
│  tp-ops.el        tp-set/reset/add/get/at/remove/clear、    │   │
│                   tp-text 处理链（0.3.0 起在此，直接调用）    │   │
├─────────────────────────────────────────────────────────── │ ──┤
│  tp-layer.el      define-tp/define-tps、层注册表与解析、     │   │
│                   层栈存储编解码、匿名层机制与 GC            │   │
│                   ◁╌╌ tp--layer-refresh-function ╌╌╌╌╌╌╌╌╌╌┤   │
├─────────────────────────────────────────────────────────── │ ──┤
│  tp-reactive.el   响应式依赖注册表、变量监听、批量队列、      │   │
│                   层→缓冲区注册表                           │   │
│                   ◁╌╌ tp--reactive-update-function ╌╌╌╌╌╌╌╌╌┘   │
├────────────────────────────────────────────────────────────────┤
│  tp-core.el       区间遍历、plist/face 合并引擎、               │
│                   调试日志、$var 符号工具（无可变状态）          │
└────────────────────────────────────────────────────────────────┘

实线层级：上层模块调用下层模块（require 依赖）。
虚线（◁╌╌）：钩子变量 —— 下层模块预留的函数变量，
由 tp-render.el 在加载时安装实现（见下文）。
```

需要"向上调用"的逻辑全部收拢在 `tp-render.el`（位于 `tp-search.el` 之上，可以直接调用它）。0.2.0 时这类反向调用靠四个钩子变量实现；0.3.0 把其中两个消除在了代码层面——`tp-text` 处理链整体下沉进 tp-ops（`tp-set` 等直接调用，不再需要 `tp--tp-text-handler-function`；只加载到 tp-ops 的部分加载也能得到可用的 `tp-text` 文本替换），批量刷新整体上收进 tp-render（`tp--flush-batch-updates` 直接调用 `tp--reactive-flush-entry`，不再需要 `tp--reactive-flush-function`）。剩下的两个钩子对应真正源自下层的事件：变量监听器触发（tp-reactive）与层重定义触发（tp-layer）。

---

## 模块分层

### tp-core.el：基础工具

最底层模块，不依赖任何其他 tp 模块，提供区间遍历、合并引擎与调试能力。0.3.0 起 tp-core **不再持有任何可变状态**（匿名层计数器已迁至 tp-layer；仅剩 `tp-debug-mode` / `tp-debug-echo` 两个 defcustom 用户选项）。

#### 区间操作
| 函数 | 描述 | 主要调用者 |
|------|------|--------|
| `tp-intervals` | 获取区域内文本属性区间列表（裁剪到 [START, END)；可选 ABSOLUTE 参数返回缓冲区原生坐标，默认仍为相对坐标） | tp-intervals-map, tp-get |
| `tp-intervals-map` | 对区间应用函数（同样支持 ABSOLUTE） | 多个属性/层操作函数 |
| `tp--map-intervals` | 共享的裁剪式区间遍历引擎 | tp-intervals-map, tp-ops/tp-stack 的区域操作, tp-reactive 的缓冲区扫描 |
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
| `tp--merge-string-props-into-plist` | 将字符串内嵌属性并入 plist | tp-ops 的 tp-text 处理链 |
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

只依赖 tp-core。维护响应式依赖注册表、变量监听器、批量更新队列与 0.3.0 新增的**层→缓冲区注册表**；**不包含任何渲染逻辑**，重渲染通过钩子变量委托给 tp-render.el。批量更新的队列（`tp--batch-update-pending`、`tp--queue-batch-update`）定义在这里，但 `tp-with-batch-updates` 宏与刷新逻辑自 0.3.0 起位于 tp-render.el。

#### 依赖注册与管理
| 函数/变量 | 描述 |
|------|------|
| `tp-reactive-deps` | 变量 → 依赖它的层及属性 的注册表 |
| `tp--register-reactive-deps` | 注册响应式依赖 |
| `tp--unregister-reactive-deps` | 取消注册依赖（含 watchers/computed/data，并移除该层的缓冲区注册表条目） |
| `tp--layer-has-reactive-deps-p` | 层是否有响应式依赖 |
| `tp--register-layer-watchers` / `tp--unregister-layer-watchers` | 注册/清除 `:watch` 回调 |
| `tp--register-layer-computed` / `tp--unregister-layer-computed` | 注册/清除 `:compute` 计算属性 |
| `tp--register-layer-data` / `tp--unregister-layer-data` | 注册/清除 `:data` 变量 |
| `tp--apply-initial-computed` | 计算 `:compute` 的初始值 |
| `tp--ensure-reactive-variables` | 确保 `$var` 对应的变量已定义 |
| `tp-reactive-reset` | 重置全部响应式注册表（含批量队列与层→缓冲区注册表） |

#### 层→缓冲区注册表（0.3.0）
响应式更新不再全量扫描 `(buffer-list)`：每条会写入 `tp-name` 的缓冲区路径（tp-set 家族、栈变更函数、match/regexp 应用器）都把目标缓冲区登记到注册表，更新时只访问登记过的缓冲区。

| 函数/变量 | 描述 |
|------|------|
| `tp--layer-buffers` | 哈希表（`:test equal`）：层名 → 展示该层的缓冲区列表。键存在但值为空表示"已知：无缓冲区展示该层"，与键不存在（`unknown`）严格区分 |
| `tp-reactive--register-layer-buffer` | 幂等登记（公开写入口，tp-ops/tp-search/tp-stack 各自的注册助手最终都调用它）；首次使用时安装 `kill-buffer-hook` 清理器 |
| `tp-reactive-layer-buffers` | 查询某层的已登记存活缓冲区，或返回符号 `unknown`；惰性剔除已死缓冲区 |
| `tp-reactive--buffer-layer-names` | 栈感知的缓冲区扫描：直接 `tp-name` 与 `tp-layers` 栈存储内的层（被覆盖或被隐藏）都算在场。`tp-reactive-track-buffer` 与匿名层 GC 的存活检查共用它 |
| `tp-reactive-track-buffer` | 交互命令：扫描缓冲区并登记其中的全部层。用于弥补"插入已带属性的字符串"绕过登记路径的已知缺口 |
| `tp-reactive--prune-killed-buffer` / `tp-reactive--install-kill-buffer-hook` | kill-buffer 时从注册表剔除死缓冲区（条目保留为空列表，即"已知：无"） |

对 `unknown` 层，tp-render 的更新走一次**学习性**全扫描并登记实际找到的缓冲区；一处都没找到的层刻意保持 `unknown`，以便之后经非登记路径（如字符串插入）出现时仍能被下次扫描发现。

#### 变量监听与批量队列
| 函数 | 描述 |
|------|------|
| `tp--reactive-variable-watcher` | `add-variable-watcher` 回调；调用 `:watch` 后经 `tp--reactive-update-function` 委托重渲染 |
| `tp--invoke-layer-watchers` | 调用层的 `:watch` 回调 |
| `tp--queue-batch-update` | 将更新加入待处理队列 `tp--batch-update-pending`（刷新在 tp-render） |

钩子变量：`tp--reactive-update-function`（定义于此，由 tp-render.el 安装）。

---

### tp-layer.el：层定义、解析与层栈存储

依赖 tp-core、tp-reactive。提供 `define-tp` / `define-tps` 宏、层注册表、层名解析，以及 0.3.0 归位至此的**层栈存储编解码**与**匿名层完整生命周期**（铸造、驻留、注销、GC）。

#### 层定义
| 函数/宏 | 描述 | 依赖 |
|---------|------|------|
| `define-tp` | 定义单个自定义文本属性（层）；别名 `tp-define-layer` | tp--define-layer-internal |
| `define-tps` | 定义自定义文本属性组（层组）；别名 `define-tp-group`、`tp-define-group` | tp--define-layer-group-internal |
| `tp--define-layer-internal` | 层定义的运行时实现 | tp--parse-define-layer-args, tp--collect-reactive-symbols, tp--ensure-reactive-variables, tp--register-*, tp--layer-refresh |
| `tp--parse-define-layer-args` | 解析 `:props` / `:data` / `:compute` / `:watch` / `:transform` | - |
| `tp--parse-layer-group-element` | 解析层组元素 | tp--layer-group-element-format |
| `tp--define-layer-from-parsed` | 从解析结果定义层 | （与 define-tp 类似的依赖） |
| `tp--check-layer-cycle` | 检测循环层引用并报错 | tp--layer-expansion-stack |

0.3.0 起参数化层/层组的 ARGLIST 可以声明**任意个**参数（此前仅限一个）；`(LAYER ARG1 ... ARGN)` 与包裹形式 `(LAYER (ARG1 ... ARGN))` 在 `tp-set` 与 `tp-put-layer` 规格中均可用，实参数量不匹配会报出点名该层与两个数量的清晰错误。

#### 注册表与查询
| 函数/变量 | 描述 |
|------|------|
| `tp-layer-alist` / `tp-layer-groups` / `tp-layer-transforms` | 层、层组、转换函数注册表 |
| `tp--group-generated-layers` | 层组 → 其定义生成的层 的注册表（组重定义/注销时随之清理） |
| `tp--set-layer-props` / `tp--set-group-layers` | 写入注册表 |
| `tp-layer-props` / `tp-group-props` | 获取层/层组属性（`&optional INCLUDE-TP-NAME`，默认不含 `tp-name`；返回副本） |
| `tp-layer-props-with-arg` / `tp-group-props-with-arg` | 单参数形式（0.3.0 起是 -with-args 的薄封装） |
| `tp-layer-props-with-args` / `tp-group-props-with-args` | 多参数形式：ARGS 按位置绑定到层参数 |
| `tp-layer-arglist` | 返回参数化层的形参表副本（非参数化层返回 nil） |
| `tp-layer-parameterized-p` / `tp-group-parameterized-p` | 是否参数化 |
| `tp-describe-layer` | 交互命令：在 help 缓冲区展示层的存储格式、形参表、原始定义体、展开属性、响应式依赖、transform 与所属层组（数据采集在 `tp--describe-layer-data`） |
| `tp-layer-reset` | 重置层系统（连带调用 `tp-reactive-reset`；见[可变状态清单](#可变状态清单)） |
| `tp-undefine-layer` / `tp-undefine-group` | 删除层/层组（含其响应式依赖、转换与匿名层注册表条目） |

#### 属性解析
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--resolve-props` | 解析属性（展开层名、多参数规格、`$var`、注册依赖、驻留匿名层） | tp-layer-props(-with-args), tp--collect-reactive-symbols, tp--resolve-reactive-symbols, tp--anonymous-layer-name-for, tp--register-reactive-deps |
| `tp--expand-layer-in-plist` | 展开 plist 中的层名键 | tp--is-layer-name-p |

#### 匿名层机制与 GC（0.3.0 归位/新增）
| 函数/变量 | 描述 |
|------|------|
| `tp--anonymous-layer-counter` | 匿名层名计数器。**刻意不被任何 reset 清零**：脱离缓冲区的字符串可能仍携带旧的 `tp-anon-N` 属性值，计数器单调递增保证新铸名字永不与之混淆 |
| `tp--generate-anonymous-layer-name` | 生成唯一的 `tp-anon-N` 符号 |
| `tp--anonymous-layer-registry` | 匿名响应式层驻留表：`equal` 的 props 规格复用既有注册项 |
| `tp--anonymous-layer-name-for` | 驻留查询/铸造入口 |
| `tp--buffer-has-layer-region-p` | 栈感知的存活检查：直接 `tp-name` 或 `tp-layers` 内（被覆盖/被隐藏）皆算存活 |
| `tp-gc-anonymous-layers` | 交互命令：回收已无任何已登记存活缓冲区展示的匿名层；注册表状态为 `unknown` 的层（可能仅被游离字符串引用）保守保留 |

#### 层栈存储编解码
层栈在原始文本属性上的编码/解码知识集中在这里，tp-stack（栈操作）与 tp-render（响应式写穿）都向下调用它，互不 require。

| 函数 | 描述 |
|------|------|
| `tp--normalize-layer-spec` | 规范化层规格（含多参数 `(LAYER ARG1 ... ARGN)`） |
| `tp--build-layer-props` / `tp--layer-stack-to-list` | 旧式编解码原语（无隐藏层语义） |
| `tp--stack-hidden-p` | 层 plist 是否带 `tp-hidden` 标志 |
| `tp--stack-props-to-list` | 原始属性 → 有序层列表（顶层在前，含隐藏层）。有隐藏层时 `tp-layers` 持有完整栈，直接属性只是最顶可见层的渲染缓存 |
| `tp--stack-build-props` | 有序层列表 → 原始属性。单层栈不携带 `tp-layers`；含隐藏层时切换为"完整栈 + 渲染缓存"存储模式（全部隐藏时不渲染任何层属性） |
| `tp--get-layer-by-idx-or-name` | 通过索引或名称查找层 |

钩子变量：`tp--layer-refresh-function`（定义于此，由 tp-render.el 安装为 `tp--update-layer-regions`）；`tp--layer-refresh` 是它的调用入口，层重定义后经它触发已应用区域的重渲染。

---

### tp-ops.el：核心属性操作与 tp-text 处理链

依赖 tp-core、tp-reactive、tp-layer。面向用户的核心属性读写函数，直接调用 Emacs 原生文本属性 API。0.3.0 起 `tp-text` 处理链从 tp-render 下沉至此，`tp-set` 等**同模块直接调用**它（不再经钩子变量）——因此只加载到 tp-ops 的部分加载也能得到可用的 `tp-text` 文本替换。

#### 参数解析
| 函数 | 描述 | 调用者 |
|------|------|--------|
| `tp--parse-args` | 解析灵活的调用格式（整串/区域/层名/多参数层） | tp-set, tp-reset, tp-add |
| `tp--apply-props-to-string` | 字符串路径的属性应用 | tp-set, tp-reset, tp-add |
| `tp--ops-register-layer-buffer` | 应用带 `tp-name` 的属性到缓冲区后，登记到层→缓冲区注册表 | tp-set, tp-reset, tp-add |

#### tp-text 处理链（0.3.0 自 tp-render 迁入）
| 函数 | 描述 |
|------|------|
| `tp--handle-tp-text-property` | `tp-text` 属性的总入口：初始化/替换文本、双向同步响应式变量 |
| `tp--tp-text-replace` | 执行文本替换（缓冲区与字符串两条路径） |
| `tp--tp-text-transform` | 应用层的 `:transform`（首次渲染同样生效） |
| `tp--find-tp-text-reactive-var` | 找到层 `tp-text` 绑定的响应式变量 |
| `tp--merge-embedded-props` | 合并 tp-text 字符串内嵌属性与外部属性 |
| `tp--apply-reactive-text-props` | 把结果属性应用到替换文本（值未变的区段跳过写入，保持 buffer-modified 状态） |
| `tp--put-text-property-unless-equal` | 仅在值确实变化时写属性 |

#### 设置属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-set` | 设置文本属性（保留其他属性） | tp--parse-args, tp--handle-tp-text-property, tp--ops-register-layer-buffer | tp-match-set, 层操作 |
| `tp-reset` | 完全替换所有文本属性 | 同上 | tp-match-reset |
| `tp-add` | 深度合并属性 | 同上 + tp--deep-merge-plist, tp--prepend-face | tp-match-add |

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
| `tp-clear` | 清除所有属性（显式返回 nil） | - | 用户 API |

---

### tp-search.el：模式匹配与搜索

依赖 tp-core、tp-reactive、tp-layer、tp-ops（0.3.0 新增 tp-reactive 依赖：应用器写入缓冲区后经 `tp--search-register-layer-buffer` 登记层→缓冲区注册表）。提供模式匹配式属性应用、属性搜索与导航。

#### 模式匹配
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-match-set` / `tp-match-reset` / `tp-match-add` | 在字符串匹配处设置/重置/合并属性；0.3.0 起接受 START/END 界限（视同只存在该部分；颠倒的界限自动交换） | tp--match-apply |
| `tp-regexp-set` / `tp-regexp-reset` / `tp-regexp-add` | 在正则匹配处设置/重置/合并属性；0.3.0 起额外接受 SUBEXP（属性作用于每个匹配的该捕获组；超出组数报清晰错误） | tp--regexp-apply |
| `tp--match-apply` / `tp--regexp-apply` | 字面/正则匹配的入口（含多模式支持） | tp--pattern-apply |
| `tp--pattern-apply` / `tp--pattern-apply-single` | 共享的模式匹配引擎（空模式/零宽模式安全；承载 START/END/SUBEXP） | tp-set/tp-reset/tp-add 风格的 apply-fn |
| `tp--deep-merge-apply` / `tp--reset-apply` | 传给引擎的合并/重置回调（缓冲区路径顺带登记注册表） | tp--deep-merge-plist, tp--search-register-layer-buffer |
| `tp--search-register-layer-buffer` | 登记助手，转发到 `tp-reactive--register-layer-buffer` | tp-reactive |

#### 搜索和导航
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-forward` | 向前搜索 N 次并移动点；0.3.0 起接受 PREDICATE 与 NOT-CURRENT（默认保持 0.2.0 的对称 `equal` 匹配契约） | text-property-search-forward |
| `tp-backward` | 向后搜索 N 次并移动点（与向前语义对称，同样新增 PREDICATE/NOT-CURRENT） | tp--property-search-backward |
| `tp--property-search-backward` | 带谓词的向后搜索引擎 | text-property-search-backward |
| `tp--property-match-p` | 谓词归一化（nil/t → `equal`；函数 → `(funcall PRED VALUE PROP-VALUE)`） | - |
| `tp--string-property-matches` | 字符串路径的按段匹配收集器 | - |
| `tp-search` | 收集所有匹配区间 | tp-intervals 等 |
| `tp-search-forward` / `tp-search-backward` | **已废弃（0.3.0，make-obsolete）**：裸封装原语，nil-PREDICATE 默认语义与库内 `equal` 匹配相悖；请改用 `tp-forward` / `tp-backward`，或直接用 Emacs 原语 | text-property-search-* |

#### 遍历与替换
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-forward-do` / `tp-backward-do` | 向前/向后搜索并在第 TIMES 个匹配处执行函数（同样透传 PREDICATE/NOT-CURRENT） | tp--forward-do / tp--backward-do |
| `tp--forward-do` / `tp--backward-do` | 单方向遍历的内部实现 | tp--replace-match-text |
| `tp-search-map` | 对所有匹配应用函数（FUNCTION 接收 TEXT &optional START END IDX） | tp--search-do |
| `tp--search-do` | 搜索遍历的内部实现 | tp--replace-match-text |
| `tp--replace-match-text` | 共享的匹配文本替换助手（缓冲区支持变长替换；字符串变长时报错） | - |

---

### tp-render.el：响应式渲染引擎

依赖 tp-core、tp-reactive、tp-layer、tp-ops、tp-search。这是唯一"知道"渲染如何进行的模块：它直接调用 `tp-search-map`、`tp--tp-text-transform`、`tp--apply-reactive-text-props`（后两者位于 tp-ops——这条 require 是真实的下行调用，不只是加载顺序），并在加载末尾把自己的入口函数**安装**进下层模块预留的两个钩子变量。0.3.0 起批量更新宏与刷新逻辑也位于此。

#### 缓冲区遍历（0.3.0：注册表驱动）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--render-visit-buffer` | 单缓冲区访问接缝（测试可包裹它统计访问次数） | tp-with-current-buffer |
| `tp--map-layer-buffers` | 在可能展示该层的缓冲区中执行更新：WHERE 为缓冲区（setq-local）时只走它；否则查注册表只访问已登记缓冲区；`unknown` 层回退为一次学习性 `(buffer-list)` 全扫描并登记实际命中的缓冲区 | tp-reactive-layer-buffers, tp--buffer-has-layer-region-p |

#### 重渲染
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--update-layer-regions` | 重渲染携带某层的所有文本区域（替换该层自己的属性键，保留其他来源属性），并**写穿**到 `tp-layers` 栈存储 | tp--layer-render-props, tp-search-map, tp--write-layer-through-stack-storage |
| `tp--write-layer-through-stack-storage` | 把新属性写进栈存储里该层的条目（被覆盖或被隐藏的副本也保持最新，`tp-show-layer` 后渲染当前值而非陈旧快照；值未变的段不触碰缓冲区） | tp--stack-props-to-list, tp--stack-build-props [tp-layer] |
| `tp--merge-props-into-stack-entry` | 更新栈条目的键，保留其 `tp-hidden` 标志与栈位置 | - |
| `tp--update-layer-computed` | 更新 `:compute` 计算属性（nil 值可正常传播） | tp--resolve-reactive-symbols, tp--set-layer-props |
| `tp--layer-render-props` / `tp--layer-reactive-props` | 求取层的渲染属性 | tp-layer-props |

#### 响应式文本（tp-text）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--update-reactive-text` | 变量变化后更新响应式文本 | tp--replace-reactive-text-in-buffer, tp--map-layer-buffers |
| `tp--replace-reactive-text-in-buffer` | 在缓冲区中替换响应式文本（0.3.0：**最小差异编辑**——修剪公共前后缀只编辑差异区段，且先插入后删除，未变文本内的点位与标记不动；文本相同的更新完全不触碰缓冲区） | tp--edit-region-minimal-diff |
| `tp--edit-region-minimal-diff` | 最小差异编辑原语 | - |
| `tp--pos-holds-layer-in-storage-only-p` | 某位置的层是否只存在于栈存储（隐藏/被覆盖，跳过可见文本替换） | - |

#### 批量更新（0.3.0 自 tp-reactive 迁入）
| 函数/宏 | 描述 |
|------|------|
| `tp-with-batch-updates` | 批量更新宏：BODY 内的多次变量修改合并为一次刷新（队列变量仍在 tp-reactive，宏向下 let 绑定它们） |
| `tp--flush-batch-updates` | 刷新队列，按层去重后**直接调用** `tp--reactive-flush-entry`（不再经钩子） |
| `tp--reactive-flush-entry` | 单条刷新的工作函数（属性更新或 tp-text 替换） |

#### 引擎入口与钩子安装
| 函数 | 描述 |
|------|------|
| `tp--reactive-apply-update` | 变量变化的完整处理：更新 computed、合并层定义、重渲染或入批量队列（嵌套写入经队列而非递归）。尾部刷新置于 `unwind-protect` 清理段中，重渲染抛错也不会把队列条目困死。安装为 `tp--reactive-update-function` |

加载末尾执行安装（与源码逐字一致）：

```elisp
(setq tp--reactive-update-function #'tp--reactive-apply-update)
(setq tp--layer-refresh-function #'tp--update-layer-regions)
```

---

### tp-stack.el：属性层栈操作

依赖 tp-core、tp-reactive、tp-layer——**不依赖 tp-ops**（0.2.0 的幻影依赖已在 0.3.0 移除，独立字节编译无警告）。所有栈变更函数建立在共享的裁剪式区域遍历之上，区域操作不会影响 [START, END) 之外的文本；栈的存储编解码在 tp-layer（向下调用）。0.3.0 起所有栈变更函数**返回实际修改的属性段数量**（0 表示无匹配；`tp-put-layer`/`tp-push-layer` 例外，仍返回 OBJECT 或 `(START . END)`），每次改写后经 `tp--stack-register-layers` 登记层→缓冲区注册表。字符串形式**原地修改**字符串（与 `tp-set` 的复制语义不同，各函数 docstring 均有警示）。

#### 内部助手
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--parse-layer-args` | 解析层操作的灵活参数 | - |
| `tp--plist-remove` | 返回去掉某键的 plist 副本 | - |
| `tp--stack-map-region` | 按区间遍历区域内层栈的共享引擎（解码经 tp--stack-props-to-list，含隐藏层） | tp--map-intervals [tp-core], tp--stack-props-to-list [tp-layer] |
| `tp--stack-register-layers` | 把新栈中每个带 `tp-name` 的层（含被覆盖与隐藏的）登记到缓冲区注册表 | tp-reactive--register-layer-buffer [tp-reactive] |
| `tp--put-layer-specs` | 展开层规格（层名/内联 plist/层名列表/参数化/层组） | tp--normalize-layer-spec, tp-group-props(-with-arg) [tp-layer] |
| `tp--move-layer-in-stack` | 在栈中移动层 | tp--get-layer-by-idx-or-name |
| `tp--raise-layer-in-stack` | 在栈中上下移动层 | tp--move-layer-in-stack |
| `tp--switch-layers-in-stack` | 交换两个层的位置 | tp--get-layer-by-idx-or-name |

#### 层操作（公开 API）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-put-layer` | 在指定索引放置层（区域局部；0.3.0 新增尾参 NOERROR：未定义层名返回 nil 而非报错） | tp--put-layer-specs, tp--stack-map-region |
| `tp-push-layer` | 将层推到顶部（同样支持 NOERROR） | tp-put-layer |
| `tp-delete-layer` | 删除层 | tp--stack-map-region |
| `tp-pop-layer` | 弹出顶层 | tp-delete-layer |
| `tp-move-layer` | 移动层到指定位置 | tp--move-layer-in-stack, tp--stack-map-region |
| `tp-raise-layer` | 上移层 | tp--raise-layer-in-stack, tp--stack-map-region |
| `tp-lower-layer` | 下移层（0.3.0 新增，tp-raise-layer 的镜像） | tp--raise-layer-in-stack, tp--stack-map-region |
| `tp-rotate-layer` | 轮换层（0.3.0：规范顺序 `(START END DIRECTION [COUNT] [OBJECT])`，凭 `up`/`down` 符号无歧义分派；旧顺序永久兼容；单趟栈旋转实现） | tp--stack-map-region |
| `tp-pin-layer` | 将层一次性移到栈顶（不阻止后续 push 覆盖） | tp-move-layer |
| `tp-switch-layer` | 交换两个层 | tp--switch-layers-in-stack, tp--stack-map-region |
| `tp-hide-layer` | 隐藏层（0.3.0 新增）：层留在栈中、继续接收响应式更新但不渲染；隐藏可见顶层则显露下一可见层；全部隐藏时文本仅剩 `tp-layers` 记账属性 | tp--stack-map-region, tp--stack-build-props [tp-layer] |
| `tp-show-layer` | 取消隐藏（0.3.0 新增） | 同上 |
| `tp-merge-layers` | 合并多个层（显式 nil 值保留；隐藏的匹配层不贡献属性，全部匹配层均隐藏时合并结果保持隐藏） | tp--merge-layer-props, tp--stack-map-region |
| `tp-flatten-layers` | 扁平化所有层（只合并可见层；全部隐藏时得到裸文本） | tp--merge-layer-props, tp--stack-map-region |

#### 层查询
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-layer-list` | 列出所有层名称（含隐藏层） | tp--stack-map-region |
| `tp-layer-count` | 计算层数量（含隐藏层） | tp--stack-map-region |
| `tp-layer-exists-p` | 检查层是否存在 | tp-layer-list |
| `tp-layer-top` | 获取顶层名称（覆盖整个请求区域；按栈序报告最顶层，即使它被隐藏） | tp--stack-map-region |
| `tp-layer-stack-at` | 单个位置的完整有序层栈：`(NAME . PROPS)` 列表，顶层在前，隐藏层以 PROPS 中的 `tp-hidden t` 标识（0.3.0 新增） | tp--stack-props-to-list [tp-layer] |
| `tp-region-layer-props` | 获取区域中特定层的属性 | tp--stack-map-region |

#### 层属性操作
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-add-to-layers` | 向特定层添加属性 | tp--deep-merge-plist, tp--stack-map-region |
| `tp-add-to-all-layers` | 向所有层添加属性 | tp-add-to-layers |

---

### tp-palette.el：调色板数据

**不依赖任何 tp- 模块**（仅 subr-x），是独立的叶模块。明/暗主题双值调色板系统，`tp-palette-alist` 是唯一数据源。

| 函数/宏/变量 | 描述 |
|------|------|
| `define-tp-palette` | 定义调色板（重定义立即生效）；别名 `tp-define-palette` |
| `tp-palette-alist` | 调色板注册表（唯一数据源） |
| `tp-parse-color` | 解析颜色规格（支持 `("light" . "dark")` 及单边 cons） |
| `tp-theme-dark-p` / `tp-theme-light-p` | 当前主题判断 |
| `tp-palette-color` | 通用的主题解析取色器（0.3.0 新增的首选查询入口） |
| `tp-palette-has-p` | 谓词整合入口：KIND 取 `:fg`/`:bg`/`:border`/nil（0.3.0 新增） |
| `tp-palette-fg-color` / `tp-palette-bg-color` / `tp-palette-border-color` | 取前景/背景/边框色（兼容便捷函数） |
| `tp-palette-p` / `tp-palette-fg-p` / `tp-palette-bg-p` / `tp-palette-fbg-p` / `tp-palette-border-p` | 调色板谓词（兼容便捷函数） |
| `tp-palette-pure` | 取纯色值 |

---

### tp-builtins.el：内置层与辅助工具

最上层模块，依赖 tp-core、tp-layer、tp-ops、tp-palette。提供开箱即用的内置层与展示/缓冲辅助。

| 定义 | 描述 |
|------|------|
| 内置层 | `tp-palette`、`tp-fg`、`tp-bg`、`tp-button`、`tp-underline`、`tp-delete`、`tp-link`、`tp-space`、`tp-headline`、`tp-action` 等（`define-tp` 定义；`tp-link` 的颜色在应用时解析，主题切换即时生效） |
| `tp-pop-to-buffer` / `tp-switch-to-buffer` | 显示带属性文本的缓冲辅助宏（q 绑定在缓冲区局部 minor-mode keymap 中） |
| `tp-palette-show` | 展示所有调色板 |
| `tp--suffix-symbol` | 符号加后缀助手（0.3.0 起转为私有；`tp-suffix-symbol` 保留为废弃兼容别名） |

---

## 钩子变量：唯一许可的反向调用

分层规则的唯一例外是两个**钩子变量**：下层模块声明变量并在需要时 `funcall`，实现由 tp-render.el 在加载时安装。这样下层模块不必 `require` 上层模块，依赖图保持严格单向；而在未加载 tp-render 时，下层模块依然可用（钩子为 nil 时优雅降级：层可以定义与应用，只是没有自动重渲染）。

| 钩子变量 | 声明于 | 安装的实现（tp-render.el） | 用途 |
|----------|--------|---------------------------|------|
| `tp--reactive-update-function` | tp-reactive.el | `tp--reactive-apply-update` | 变量监听器触发的重计算与重渲染 |
| `tp--layer-refresh-function` | tp-layer.el | `tp--update-layer-regions` | 层重定义后刷新已应用区域 |

0.2.0 时钩子有四个；0.3.0 删掉了其中两个，代之以真实的模块内/下行调用：

- `tp--tp-text-handler-function`（原声明于 tp-ops）：整条 `tp-text` 处理链移入 tp-ops，`tp-set` 等直接调用 `tp--handle-tp-text-property`。副产品：只加载 tp-ops 的部分加载也能完成 `tp-text` 文本替换。
- `tp--reactive-flush-function`（原声明于 tp-reactive）：`tp-with-batch-updates` 与 `tp--flush-batch-updates` 移入 tp-render，刷新直接调用 `tp--reactive-flush-entry`。副产品：部分加载下批量刷新不再被静默丢弃，而是诚实地报 void-function。

留下的两个钩子对应真正**源自下层的事件**（变量被 set、层被重定义），无法在不打破分层的前提下改写为下行调用。

---

## 可变状态清单

各模块持有的可变运行时状态及其清理入口（0.3.0 全面核对）：

| 模块 | 状态 | 描述 | 清理 |
|------|------|------|------|
| tp-core | —— | **无可变状态**（仅 `tp-debug-mode`/`tp-debug-echo` 两个用户选项；调试日志写入 *tp-debug* 缓冲区，由 `tp-debug-clear` 清除） | - |
| tp-reactive | `tp-reactive-deps` | 变量 → 依赖层 注册表 | `tp-reactive-reset` |
| tp-reactive | `tp-layer-watchers` / `tp-layer-computed` / `tp-layer-data` | `:watch` / `:compute` / `:data` 注册表 | `tp-reactive-reset` |
| tp-reactive | `tp--batch-update-pending` | 批量更新队列（0.3.0 起也被 reset 清空，防止残留条目对新定义的层重放） | `tp-reactive-reset` |
| tp-reactive | `tp--layer-buffers` | 层→缓冲区注册表（哈希表，0.3.0 新增） | `tp-reactive-reset`（clrhash）；单层条目随 `tp-undefine-layer`/层重定义移除；死缓冲区经 kill-buffer-hook 与惰性访问剔除 |
| tp-reactive | `tp--batch-update-active` / `tp--reactive-updating` | 动态标志（let 绑定，非持久状态） | 随作用域退出 |
| tp-layer | `tp-layer-alist` / `tp-layer-groups` / `tp-layer-transforms` | 层、层组、转换注册表 | `tp-layer-reset` |
| tp-layer | `tp--group-generated-layers` | 层组生成的层 | `tp-layer-reset` |
| tp-layer | `tp--anonymous-layer-registry` | 匿名层驻留表 | `tp-layer-reset`；单条随 `tp-undefine-layer` / `tp-gc-anonymous-layers` |
| tp-layer | `tp--anonymous-layer-counter` | 匿名层名计数器——**刻意不清零**（任何 reset 都不动它）：游离字符串上残留的 `tp-anon-N` 名字永远不能与新铸层重名 | 从不 |

`tp-reactive-reset` 移除全部变量监听器并清空上表 tp-reactive 各行；`tp-layer-reset` 先调用 `tp-reactive-reset`，再清空 tp-layer 各注册表（计数器除外）。

---

## 函数调用关系图

（标注 `[模块]` 表示函数所在文件；`╌╌▷` 表示经钩子变量的间接调用。）

### tp-set 调用链
```
tp-set [tp-ops]
  ├── tp--parse-args [tp-ops]
  │     ├── tp--merge-duplicate-keys [tp-core]
  │     └── tp--resolve-props [tp-layer]
  │           ├── tp-layer-props / tp-layer-props-with-args [tp-layer]
  │           ├── tp--collect-reactive-symbols [tp-core]
  │           ├── tp--resolve-reactive-symbols [tp-core]
  │           ├── tp--anonymous-layer-name-for [tp-layer]（$var 匿名层驻留）
  │           └── tp--register-reactive-deps [tp-reactive]
  ├── tp--handle-tp-text-property [tp-ops]（0.3.0 起同模块直接调用，不再经钩子）
  │     └── tp--tp-text-transform / tp--tp-text-replace [tp-ops]
  ├── tp--apply-props-to-string [tp-ops]（整串形式，返回新字符串）
  ├── set-text-properties / put-text-property（Emacs 原生，区域形式）
  └── tp--ops-register-layer-buffer [tp-ops]（缓冲区目标）
        └── tp-reactive--register-layer-buffer [tp-reactive]
```

### tp-add 调用链
```
tp-add [tp-ops]
  ├── tp--parse-args [tp-ops]
  ├── tp--handle-tp-text-property [tp-ops]（直接调用）
  ├── text-properties-at（Emacs 原生）
  ├── tp--prepend-face [tp-core]（face 家族属性）
  │     └── tp--deep-merge-plist [tp-core]
  ├── tp--deep-merge-plist [tp-core]（其他嵌套属性）
  ├── put-text-property（Emacs 原生）
  └── tp--ops-register-layer-buffer [tp-ops]
        └── tp-reactive--register-layer-buffer [tp-reactive]
```

### define-tp 调用链
```
define-tp [tp-layer]（宏）
  └── tp--define-layer-internal [tp-layer]
        ├── tp--parse-define-layer-args [tp-layer]
        ├── tp--collect-reactive-symbols [tp-core]
        ├── tp--unregister-reactive-deps [tp-reactive]（连带移除旧的缓冲区注册表条目）
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
              ├── tp--stack-props-to-list [tp-layer]（解码既有栈，含隐藏层）
              ├── tp--stack-build-props [tp-layer]（编码新栈/渲染缓存）
              ├── set-text-properties（Emacs 原生）
              └── tp--stack-register-layers [tp-stack]
                    └── tp-reactive--register-layer-buffer [tp-reactive]
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
              │     └── tp--map-layer-buffers [tp-render]
              │           │（只访问注册表登记的缓冲区；unknown 层回退为
              │           │ 一次学习性全扫描并登记命中缓冲区）
              │           ├── tp-reactive-layer-buffers [tp-reactive]
              │           ├── tp--buffer-has-layer-region-p [tp-layer]（回退路径）
              │           └── 每缓冲区：
              │                 ├── tp-search-map [tp-search] → put-text-property
              │                 └── tp--write-layer-through-stack-storage [tp-render]
              │                       └── tp--stack-props-to-list /
              │                           tp--stack-build-props [tp-layer]
              │                          （隐藏/被覆盖的层副本同步刷新）
              └── tp--update-reactive-text [tp-render]（tp-text 文本替换）
                    └── tp--replace-reactive-text-in-buffer [tp-render]
                          └── tp--edit-region-minimal-diff [tp-render]
                             （最小差异、先插入后删除；文本相同则完全不动缓冲区）

批量模式（tp-with-batch-updates [tp-render]）/ 更新中的嵌套写入：
  └── tp--queue-batch-update [tp-reactive]（入队，不递归）
        └── tp--flush-batch-updates [tp-render]（退出批量/最外层更新结束时；
              置于 unwind-protect 清理段，重渲染抛错也会排空队列）
              └── tp--reactive-flush-entry [tp-render]（0.3.0 起同模块直接调用，不再经钩子）
                    ├── tp--update-layer-regions
                    └── tp--update-reactive-text
```

---

## 设计原则

1. **严格分层**：模块只允许 `require` 并调用排在它前面的模块，字节编译器强制检查依赖顺序；且只声明真实存在的依赖（0.3.0 移除了 tp-stack→tp-ops 的幻影依赖，tp-palette 不依赖任何 tp- 模块）
2. **钩子反转**：唯一许可的"向上调用"是两个钩子变量（`tp--reactive-update-function`、`tp--layer-refresh-function`），由 tp-render.el 统一安装实现；能改写为下行调用的反转（tp-text 链、批量刷新）已在 0.3.0 改写掉
3. **单一职责**：每个模块（和函数）只负责一件事；一个子系统的完整生命周期住在一个模块里（匿名层的铸造/驻留/注销/GC 全在 tp-layer，层栈存储格式知识全在 tp-layer 的编解码器）
4. **复用优先**：共享引擎（`tp--map-intervals`、`tp--stack-map-region`、`tp--pattern-apply`、`tp--replace-match-text`、`tp-reactive--buffer-layer-names`）承载重复逻辑，高层函数复用而非复制
5. **统一接口**：所有核心属性函数支持相同的调用约定（整串/区域形式、层名、`$var`、多参数层）
6. **响应式解耦**：tp-reactive/tp-layer/tp-ops 不依赖渲染引擎；不加载 tp-render 时钩子为 nil，各模块优雅降级（`tp-text` 替换自 0.3.0 起随 tp-ops 即可用）；重渲染只访问层→缓冲区注册表登记的缓冲区，未知层才回退全扫描
