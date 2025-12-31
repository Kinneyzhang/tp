# tp.el 代码架构文档

本文档描述 tp.el 的函数调用层次结构，从底层基础功能到上层 API 的分层组织。

## 目录

- [架构概述](#架构概述)
- [分层结构](#分层结构)
  - [第一层：基础工具函数](#第一层基础工具函数)
  - [第二层：核心属性操作](#第二层核心属性操作)
  - [第三层：属性层系统](#第三层属性层系统)
  - [第四层：响应式系统](#第四层响应式系统)
  - [第五层：高级 API](#第五层高级-api)
- [函数调用关系图](#函数调用关系图)

---

## 架构概述

tp.el 采用分层架构设计，每一层建立在下层功能之上：

```
┌─────────────────────────────────────────────────────────────────┐
│                    第五层：高级 API                               │
│  tp-match-set, tp-regexp-set, tp-forward-do, tp-search-map      │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                  第四层：响应式系统                               │
│  tp-define-layer, tp--reactive-variable-watcher,                │
│  tp--update-layer-regions, tp--register-reactive-deps           │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                  第三层：属性层系统                               │
│  tp-push-layer, tp-pop-layer, tp-rotate-layer,                  │
│  tp-layer-list, tp--build-layer-props                           │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                  第二层：核心属性操作                             │
│  tp-set, tp-reset, tp-add, tp-get, tp-at, tp-remove, tp-clear   │
└─────────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────────┐
│                  第一层：基础工具函数                             │
│  tp--parse-args, tp--deep-merge-plist, tp--get-nested,          │
│  tp-intervals, tp-empty-p                                        │
└─────────────────────────────────────────────────────────────────┘
```

---

## 分层结构

### 第一层：基础工具函数

这些是最底层的工具函数，不依赖于其他 tp.el 函数，主要提供参数解析、数据结构操作等基础能力。

#### 参数解析
| 函数 | 描述 | 调用者 |
|------|------|--------|
| `tp--parse-args` | 解析灵活的函数参数格式 | tp-set, tp-reset, tp-add |
| `tp--parse-layer-args` | 解析属性层操作的参数 | tp-put-layer 及其他层操作函数 |
| `tp--parse-define-layer-args` | 解析 tp-define-layer 的参数 | tp-define-layer |

#### 数据结构操作
| 函数 | 描述 | 调用者 |
|------|------|--------|
| `tp--deep-merge-plist` | 深度合并两个 plist | tp-add, tp--prepend-face |
| `tp--prepend-face` | 处理 face 属性的合并逻辑 | tp-add |
| `tp--get-nested` | 获取嵌套属性值 | tp-get, tp-at |
| `tp--remove-nested-keys` | 从 plist 中移除指定键 | tp--remove-property |

#### 区间操作
| 函数 | 描述 | 调用者 |
|------|------|--------|
| `tp-intervals` | 获取文本属性区间列表 | tp-intervals-map, tp-get |
| `tp-intervals-map` | 对区间应用函数 | 多个层操作函数 |
| `tp-empty-p` | 检查对象是否没有文本属性 | tp-put-layer |

---

### 第二层：核心属性操作

这些是核心的文本属性操作函数，直接调用 Emacs 原生的文本属性 API。

#### 设置属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-set` | 设置文本属性（保留其他属性） | tp--parse-args, tp--handle-tp-text-property | tp-match-set, 层操作 |
| `tp-reset` | 完全替换所有文本属性 | tp--parse-args, tp--handle-tp-text-property | tp-match-reset |
| `tp-add` | 深度合并属性 | tp--parse-args, tp--deep-merge-plist, tp--prepend-face | tp-match-add, tp--update-layer-regions |

#### 获取属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-get` | 获取范围内的属性值（返回区间列表） | tp--get-nested | 搜索函数 |
| `tp-at` | 获取单个位置的属性值 | tp--get-nested | 大多数高层函数 |
| `tp-plist` | 获取区域中的所有属性 | tp-intervals | 用户 API |

#### 删除属性
| 函数 | 描述 | 依赖 | 被依赖 |
|------|------|------|--------|
| `tp-remove` | 移除属性或子属性 | tp--remove-property, tp--remove-sub | 用户 API |
| `tp-clear` | 清除所有属性 | - | 用户 API |

---

### 第三层：属性层系统

属性层系统在核心属性操作之上，提供多层属性栈的管理能力。

#### 层栈操作（内部）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--get-layer-stack` | 获取位置的层栈 | - |
| `tp--build-layer-props` | 从层列表构建属性 | - |
| `tp--layer-stack-to-list` | 将层栈转换为列表 | - |
| `tp--get-layer-by-idx-or-name` | 通过索引或名称查找层 | - |
| `tp--move-layer-in-stack` | 在栈中移动层 | tp--get-layer-by-idx-or-name |
| `tp--raise-layer-in-stack` | 在栈中上下移动层 | tp--move-layer-in-stack |
| `tp--switch-layers-in-stack` | 交换两个层的位置 | tp--get-layer-by-idx-or-name |
| `tp--normalize-layer-spec` | 规范化层规格 | tp-layer-props |

#### 层操作（公开 API）
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-put-layer` | 在指定索引放置层 | tp--normalize-layer-spec, tp--build-layer-props, tp-intervals-map |
| `tp-push-layer` | 将层推到顶部 | tp-put-layer |
| `tp-delete-layer` | 删除层 | tp--get-layer-by-idx-or-name, tp-intervals-map |
| `tp-pop-layer` | 弹出顶层 | tp-delete-layer |
| `tp-move-layer` | 移动层到指定位置 | tp--move-layer-in-stack, tp-intervals-map |
| `tp-raise-layer` | 上移/下移层 | tp--raise-layer-in-stack, tp-intervals-map |
| `tp-rotate-layer` | 轮换层 | tp-move-layer |
| `tp-pin-layer` | 将层置顶 | tp-move-layer |
| `tp-switch-layer` | 交换两个层 | tp--switch-layers-in-stack, tp-intervals-map |
| `tp-merge-layers` | 合并多个层 | tp--get-layer-by-idx-or-name, tp-intervals-map |
| `tp-flatten-layers` | 扁平化所有层 | tp-intervals-map |

#### 层查询
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-layer-list` | 列出所有层名称 | tp-intervals-map |
| `tp-layer-count` | 计算层数量 | tp-intervals-map |
| `tp-layer-exists-p` | 检查层是否存在 | tp-region-layer-props |
| `tp-layer-top` | 获取顶层名称 | tp-intervals |
| `tp-region-layer-props` | 获取区域中特定层的属性 | tp-intervals-map |

#### 层属性操作
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-add-to-layers` | 向特定层添加属性 | tp--deep-merge-plist, tp-intervals-map |
| `tp-add-to-all-layers` | 向所有层添加属性 | tp-add-to-layers, tp-layer-count |

---

### 第四层：响应式系统

响应式系统提供当变量值改变时自动更新文本属性的能力。

#### 响应式变量处理
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--reactive-symbol-p` | 检查是否为响应式符号 | - |
| `tp--reactive-var-symbol` | 转换响应式符号为变量符号 | tp--reactive-symbol-p |
| `tp--collect-reactive-symbols` | 收集所有响应式符号 | tp--reactive-symbol-p |
| `tp--resolve-reactive-symbols` | 解析响应式符号为值 | tp--reactive-symbol-p, tp--reactive-var-symbol |
| `tp--extract-reactive-props` | 提取使用特定变量的属性 | tp--collect-reactive-symbols, tp--extract-reactive-value |
| `tp--ensure-reactive-variables` | 确保变量已定义 | tp--reactive-symbol-p, tp--reactive-var-symbol |

#### 依赖注册与管理
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--register-reactive-deps` | 注册响应式依赖 | tp--reactive-var-symbol, tp--extract-reactive-props |
| `tp--unregister-reactive-deps` | 取消注册依赖 | tp--unregister-layer-watchers, tp--unregister-layer-computed, tp--unregister-layer-data |
| `tp--register-layer-watchers` | 注册层的监听器 | - |
| `tp--register-layer-computed` | 注册计算属性 | - |
| `tp--register-layer-data` | 注册数据变量 | tp--data-var-symbol |
| `tp--unregister-layer-watchers` | 取消注册监听器 | - |
| `tp--unregister-layer-computed` | 取消注册计算属性 | - |
| `tp--unregister-layer-data` | 取消注册数据变量 | - |

#### 响应式更新
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--reactive-variable-watcher` | 变量监听器回调 | tp--invoke-layer-watchers, tp--update-layer-computed, tp--update-layer-regions, tp--update-reactive-text |
| `tp--invoke-layer-watchers` | 调用层的监听回调 | - |
| `tp--update-layer-computed` | 更新计算属性 | tp--resolve-reactive-symbols, tp--set-layer-props |
| `tp--update-layer-regions` | 更新使用层的文本区域 | tp-layer-props, tp-search-map, tp-add |
| `tp--update-reactive-text` | 更新响应式文本 | tp-layer-props, tp--replace-reactive-text-in-buffer |
| `tp--replace-reactive-text-in-buffer` | 在缓冲区中替换响应式文本 | - |

#### 层定义
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-define-layer` | 定义单个属性层 | tp--parse-define-layer-args, tp--collect-reactive-symbols, tp--ensure-reactive-variables, tp--register-* |
| `tp-define-layer-group` | 定义属性层组 | tp--parse-layer-group-element, tp--define-layer-from-parsed |
| `tp--define-layer-from-parsed` | 从解析结果定义层 | (与 tp-define-layer 类似的依赖) |
| `tp--set-layer-props` | 设置层属性 | - |
| `tp--set-group-layers` | 设置组的层列表 | - |
| `tp-layer-props` | 获取层属性 | - |
| `tp-group-props` | 获取组中所有层的属性 | tp-layer-props |
| `tp--resolve-props` | 解析属性（支持层名称） | tp-layer-props, tp--collect-reactive-symbols, tp--resolve-reactive-symbols, tp--register-reactive-deps |

#### 响应式文本
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp--handle-tp-text-property` | 处理 tp-text 属性 | - |

#### 批量更新
| 函数/宏 | 描述 | 依赖 |
|---------|------|------|
| `tp-with-batch-updates` | 批量更新宏 | tp--flush-batch-updates |
| `tp--flush-batch-updates` | 刷新待处理的批量更新 | tp--update-layer-regions, tp--update-reactive-text |

#### 值转换
| 变量/函数 | 描述 | 依赖 |
|-----------|------|------|
| `tp-layer-transforms` | 存储层转换函数的 alist | - |
| `:transform` 选项 | 在 tp-define-layer 中指定转换函数 | tp-layer-transforms |

#### 调试工具
| 变量/函数 | 描述 | 依赖 |
|-----------|------|------|
| `tp-debug-mode` | 启用/禁用调试模式 | - |
| `tp-debug-echo` | 是否在 minibuffer 显示调试信息 | - |
| `tp-debug-log` | 记录调试信息 | tp-debug-mode, tp-debug-echo |
| `tp-debug-show` | 显示 *tp-debug* 缓冲区 | - |
| `tp-debug-clear` | 清除调试日志 | - |

---

### 第五层：高级 API

这些是面向用户的高级 API，构建在前四层之上。

#### 模式匹配
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-match-set` | 在字符串匹配处设置属性 | tp--match-apply |
| `tp-match-reset` | 在匹配处重置所有属性 | tp--match-apply |
| `tp-match-add` | 在匹配处添加/合并属性 | tp--match-apply |
| `tp-regexp-set` | 在正则匹配处设置属性 | tp--regexp-apply |
| `tp-regexp-reset` | 在正则匹配处重置属性 | tp--regexp-apply |
| `tp-regexp-add` | 在正则匹配处添加属性 | tp--regexp-apply |
| `tp--match-apply` | 字符串匹配的内部实现 | tp-set/tp-reset/tp-add |
| `tp--regexp-apply` | 正则匹配的内部实现 | tp-set/tp-reset/tp-add |

#### 搜索和导航
| 函数 | 描述 | 依赖 |
|------|------|------|
| `tp-search-forward` | 向前搜索属性 | text-property-search-forward |
| `tp-search-backward` | 向后搜索属性 | text-property-search-backward |
| `tp-forward` | 向前搜索 N 次 | tp--forward-on-string, tp-search-forward |
| `tp-backward` | 向后搜索 N 次 | tp--backward-on-string, tp-search-backward |
| `tp-forward-do` | 向前搜索并对最后匹配执行函数 | tp--forward-do-on-string |
| `tp-backward-do` | 向后搜索并对最后匹配执行函数 | tp--backward-do-on-string |
| `tp-search` | 搜索所有匹配 | tp--search-do |
| `tp-search-map` | 对所有匹配应用函数 | tp--search-do |
| `tp--search-do` | 搜索的内部实现 | - |

---

## 函数调用关系图

### tp-set 调用链
```
tp-set
  ├── tp--parse-args
  │     └── tp--resolve-props
  │           ├── tp-layer-props
  │           ├── tp--collect-reactive-symbols
  │           ├── tp--resolve-reactive-symbols
  │           ├── tp--register-reactive-deps
  │           └── tp--build-layer-props (for groups)
  ├── tp--handle-tp-text-property
  └── put-text-property (Emacs 原生)
```

### tp-add 调用链
```
tp-add
  ├── tp--parse-args
  ├── tp--handle-tp-text-property
  ├── text-properties-at (Emacs 原生)
  ├── tp--prepend-face
  │     └── tp--deep-merge-plist
  ├── tp--deep-merge-plist
  └── put-text-property (Emacs 原生)
```

### tp-define-layer 调用链
```
tp-define-layer
  ├── tp--parse-define-layer-args
  ├── tp--collect-reactive-symbols
  ├── tp--unregister-reactive-deps
  │     ├── tp--unregister-layer-watchers
  │     ├── tp--unregister-layer-computed
  │     └── tp--unregister-layer-data
  ├── tp--ensure-reactive-variables
  ├── tp--register-layer-data
  ├── tp--register-layer-computed
  ├── tp--apply-initial-computed
  ├── tp--register-reactive-deps
  ├── tp--register-layer-watchers
  ├── tp--resolve-reactive-symbols
  ├── tp--set-layer-props
  └── tp--update-layer-regions
        ├── tp-layer-props
        └── tp-search-map
              └── tp-add
```

### tp-push-layer 调用链
```
tp-push-layer
  └── tp-put-layer
        ├── tp--normalize-layer-spec
        │     └── tp-layer-props
        ├── tp-group-props
        │     └── tp-layer-props
        ├── tp-empty-p
        ├── set-text-properties (Emacs 原生)
        └── tp-intervals-map
              └── tp-intervals
```

### 响应式更新调用链
```
(setq some-reactive-var new-value)
  └── tp--reactive-variable-watcher
        ├── tp--invoke-layer-watchers
        ├── tp--update-layer-computed
        │     ├── tp--resolve-reactive-symbols
        │     └── tp--set-layer-props
        ├── tp--update-layer-regions (属性更新)
        │     └── tp-search-map
        │           └── tp-add
        └── tp--update-reactive-text (文本替换)
              └── tp--replace-reactive-text-in-buffer
```

---

## 设计原则

1. **分层封装**：每层只依赖于下层功能，避免跨层调用
2. **单一职责**：每个函数只做一件事
3. **复用优先**：高层函数应该复用低层函数，避免重复代码
4. **统一接口**：所有核心属性函数支持相同的调用约定
5. **响应式解耦**：响应式系统独立于核心属性操作，可选择性使用
