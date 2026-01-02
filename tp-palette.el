;;; tp-palette.el --- Color palette definitions for tp.el -*- lexical-binding: t -*-

;; Copyright (C) 2024

;;; Commentary:

;; This file provides color palette definitions for tp.el.
;; Colors are designed to work well in both light and dark themes.
;; Each palette entry has the format:
;;   (:fg (LIGHT-FG . DARK-FG) :bg (LIGHT-BG . DARK-BG) :border (LIGHT-BORDER . DARK-BORDER))
;;
;; Color scheme inspired by:
;; - GitHub Primer Design System
;; - Tailwind CSS Color Palette
;; - Material Design Color System
;; - One Dark / One Light themes

;;; Code:

;; ============================================
;; 基础文本样式 (Basic Text Styles)
;; ============================================

(defvar tp-palette-text-primary
  '(:fg ("#24292f" . "#e6edf3") :bg ("#ffffff" . "#0d1117")
        :border ("#d0d7de" . "#30363d"))
  "主要文本样式，用于正文内容。")

(defvar tp-palette-text-secondary
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#161b22")
        :border ("#d8dee4" . "#30363d"))
  "次要文本样式，用于辅助说明文字。")

(defvar tp-palette-text-muted
  '(:fg ("#6e7781" . "#6e7681") :bg ("#f6f8fa" . "#161b22")
        :border ("#e1e4e8" . "#21262d"))
  "弱化文本样式，用于占位符、禁用状态。")

;; ============================================
;; 语义状态样式 (Semantic Status Styles)
;; ============================================

(defvar tp-palette-success
  '(:fg ("#1a7f37" . "#3fb950") :bg ("#dafbe1" . "#1b4721")
        :border ("#4ac26b" . "#238636"))
  "成功状态样式，用于成功提示、通过状态。")

(defvar tp-palette-warning
  '(:fg ("#9a6700" . "#d29922") :bg ("#fff8c5" . "#3d2e00")
        :border ("#d4a72c" . "#9e6a03"))
  "警告状态样式，用于警告提示、待处理状态。")

(defvar tp-palette-error
  '(:fg ("#cf222e" . "#f85149") :bg ("#ffebe9" . "#542426")
        :border ("#ff8182" . "#f85149"))
  "错误状态样式，用于错误提示、失败状态。")

(defvar tp-palette-info
  '(:fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" .  "#388bfd"))
  "信息提示样式，用于普通提示、链接。")

(defvar tp-palette-neutral
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#21262d")
        :border ("#d0d7de" .  "#30363d"))
  "中性状态样式，用于普通标签、默认状态。")

;; ============================================
;; 代码高亮样式 (Code Syntax Highlighting Styles)
;; Based on One Dark/Light and popular editor themes
;; ============================================

(defvar tp-palette-syntax-keyword
  '(:fg ("#a626a4" . "#c678dd") :bg nil
        :border ("#a626a4" . "#c678dd"))
  "关键字样式，如 if, else, def, class 等。紫色调。")

(defvar tp-palette-syntax-string
  '(:fg ("#50a14f" . "#98c379") :bg nil
        :border ("#50a14f" . "#98c379"))
  "字符串样式。绿色调。")

(defvar tp-palette-syntax-comment
  '(:fg ("#a0a1a7" . "#5c6370") :bg nil
        :border ("#a0a1a7" . "#5c6370"))
  "注释样式。灰色调。")

(defvar tp-palette-syntax-function
  '(:fg ("#4078f2" . "#61afef") :bg nil
        :border ("#4078f2" . "#61afef"))
  "函数名样式。蓝色调。")

(defvar tp-palette-syntax-variable
  '(:fg ("#e45649" . "#e06c75") :bg nil
        :border ("#e45649" . "#e06c75"))
  "变量名样式。红色调。")

(defvar tp-palette-syntax-constant
  '(:fg ("#986801" . "#d19a66") :bg nil
        :border ("#986801" . "#d19a66"))
  "常量样式。橙色调。")

(defvar tp-palette-syntax-type
  '(:fg ("#c18401" . "#e5c07b") :bg nil
        :border ("#c18401" . "#e5c07b"))
  "类型样式，如类名、结构体名。黄色调。")

(defvar tp-palette-syntax-number
  '(:fg ("#986801" . "#d19a66") :bg nil
        :border ("#986801" . "#d19a66"))
  "数字样式。橙色调。")

(defvar tp-palette-syntax-operator
  '(:fg ("#0184bc" . "#56b6c2") :bg nil
        :border ("#0184bc" . "#56b6c2"))
  "运算符样式。青色调。")

(defvar tp-palette-syntax-preprocessor
  '(:fg ("#a626a4" . "#c678dd") :bg nil
        :border ("#a626a4" . "#c678dd"))
  "预处理器样式，如 #include, #define。紫色调。")

(defvar tp-palette-syntax-builtin
  '(:fg ("#0184bc" . "#56b6c2") :bg nil
        :border ("#0184bc" . "#56b6c2"))
  "内置函数/方法样式。青色调。")

(defvar tp-palette-syntax-regexp
  '(:fg ("#50a14f" . "#98c379") :bg nil
        :border ("#50a14f" . "#98c379"))
  "正则表达式样式。绿色调。")

(defvar tp-palette-syntax-attribute
  '(:fg ("#986801" . "#d19a66") :bg nil
        :border ("#986801" . "#d19a66"))
  "属性/装饰器样式。橙色调。")

(defvar tp-palette-syntax-tag
  '(:fg ("#e45649" . "#e06c75") :bg nil
        :border ("#e45649" . "#e06c75"))
  "HTML/XML 标签样式。红色调。")

;; ============================================
;; UI 界面样式 (UI Styles)
;; ============================================

(defvar tp-palette-ui-default
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#ffffff" . "#0d1117")
        :border ("#d0d7de" . "#30363d"))
  "默认 UI 样式。")

(defvar tp-palette-ui-canvas
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#f6f8fa" . "#161b22")
        :border ("#d8dee4" . "#21262d"))
  "画布/次级背景样式。")

(defvar tp-palette-ui-overlay
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#ffffff" . "#21262d")
        :border ("#d0d7de" .  "#30363d"))
  "覆盖层/弹窗样式。")

(defvar tp-palette-ui-hover
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#eaeef2" . "#30363d")
        :border ("#d0d7de" . "#484f58"))
  "悬浮状态样式。")

(defvar tp-palette-ui-selected
  '(:fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" .  "#388bfd"))
  "选中状态样式。")

(defvar tp-palette-ui-active
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" .  "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "激活状态样式。")

(defvar tp-palette-ui-disabled
  '(:fg ("#8c959f" . "#484f58") :bg ("#f6f8fa" .  "#21262d")
        :border ("#e1e4e8" . "#30363d"))
  "禁用状态样式。")

;; ============================================
;; 按钮样式 (Button Styles)
;; ============================================

(defvar tp-palette-button-primary
  '(:fg ("#ffffff" . "#ffffff") :bg ("#1f883d" . "#238636")
        :border ("#1a7f37" . "#2ea043"))
  "主要按钮样式（绿色确认按钮）。")

(defvar tp-palette-button-secondary
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#f6f8fa" . "#21262d")
        :border ("#d0d7de" . "#30363d"))
  "次级按钮样式。")

(defvar tp-palette-button-danger
  '(:fg ("#ffffff" . "#ffffff") :bg ("#cf222e" .  "#da3633")
        :border ("#a40e26" . "#f85149"))
  "危险按钮样式（红色删除按钮）。")

(defvar tp-palette-button-outline
  '(:fg ("#0969da" . "#58a6ff") :bg nil
        :border ("#0969da" . "#58a6ff"))
  "轮廓按钮样式。")

;; ============================================
;; 输入框样式 (Input Styles)
;; ============================================

(defvar tp-palette-input-default
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#ffffff" . "#0d1117")
        :border ("#d0d7de" .  "#30363d"))
  "输入框默认样式。")

(defvar tp-palette-input-focus
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#ffffff" . "#0d1117")
        :border ("#0969da" . "#58a6ff"))
  "输入框聚焦样式。")

(defvar tp-palette-input-error
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#ffffff" . "#0d1117")
        :border ("#cf222e" . "#f85149"))
  "输入框错误样式。")

(defvar tp-palette-input-disabled
  '(:fg ("#8c959f" . "#484f58") :bg ("#f6f8fa" . "#21262d")
        :border ("#e1e4e8" . "#30363d"))
  "输入框禁用样式。")

;; ============================================
;; Diff 样式 (Version Control Diff Styles)
;; ============================================

(defvar tp-palette-diff-added
  '(:fg ("#116329" . "#3fb950") :bg ("#dafbe1" . "#1b4721")
        :border ("#4ac26b" . "#238636"))
  "新增行样式。")

(defvar tp-palette-diff-removed
  '(:fg ("#cf222e" .  "#f85149") :bg ("#ffebe9" .  "#542426")
        :border ("#ff8182" . "#f85149"))
  "删除行样式。")

(defvar tp-palette-diff-modified
  '(:fg ("#9a6700" . "#d29922") :bg ("#fff8c5" . "#3d2e00")
        :border ("#d4a72c" . "#9e6a03"))
  "修改行样式。")

(defvar tp-palette-diff-hunk-header
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" .  "#388bfd"))
  "Diff hunk 头部样式。")

;; ============================================
;; 模式行样式 (Modeline Styles)
;; ============================================

(defvar tp-palette-modeline-active
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#f6f8fa" . "#21262d")
        :border ("#d0d7de" . "#30363d"))
  "活动窗口模式行样式。")

(defvar tp-palette-modeline-inactive
  '(:fg ("#6e7781" . "#6e7681") :bg ("#eaeef2" . "#161b22")
        :border ("#e1e4e8" . "#21262d"))
  "非活动窗口模式行样式。")

(defvar tp-palette-modeline-highlight
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" .  "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "模式行高亮样式。")

;; ============================================
;; 搜索和匹配样式 (Search & Match Styles)
;; ============================================

(defvar tp-palette-search-match
  '(:fg ("#9a6700" . "#f0c239") :bg ("#fff8c5" . "#533d00")
        :border ("#d4a72c" . "#9e6a03"))
  "搜索匹配高亮样式。")

(defvar tp-palette-search-current
  '(:fg ("#24292f" . "#24292f") :bg ("#ffc83d" . "#e3b341")
        :border ("#bf8700" . "#d29922"))
  "当前搜索项高亮样式。")

(defvar tp-palette-lazy-highlight
  '(:fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" .  "#388bfd"))
  "延迟高亮样式。")

(defvar tp-palette-match-paren
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#add6ff" . "#264f78")
        :border ("#0969da" . "#58a6ff"))
  "括号匹配样式。")

;; ============================================
;; 选区和光标样式 (Selection & Cursor Styles)
;; ============================================

(defvar tp-palette-region
  '(:fg nil :bg ("#add6ff" . "#264f78")
        :border ("#54aeff" . "#388bfd"))
  "选区样式。")

(defvar tp-palette-cursor
  '(:fg ("#ffffff" . "#0d1117") :bg ("#24292f" . "#58a6ff")
        :border ("#24292f" . "#58a6ff"))
  "光标样式。")

(defvar tp-palette-line-highlight
  '(:fg nil :bg ("#f6f8fa" . "#161b22")
        :border ("#eaeef2" . "#21262d"))
  "当前行高亮样式。")

;; ============================================
;; Org-mode 标题样式 (Org-mode Heading Styles)
;; ============================================

(defvar tp-palette-org-heading-1
  '(:fg ("#0969da" . "#58a6ff") :bg ("#f0f7ff" . "#1a2634")
        :border ("#54aeff" . "#388bfd"))
  "Org 一级标题样式。")

(defvar tp-palette-org-heading-2
  '(:fg ("#8250df" . "#a371f7") :bg ("#fbefff" . "#271d36")
        :border ("#c297ff" . "#6e40c9"))
  "Org 二级标题样式。")

(defvar tp-palette-org-heading-3
  '(:fg ("#1a7f37" . "#3fb950") :bg ("#f0fff4" . "#1a2e1f")
        :border ("#4ac26b" . "#238636"))
  "Org 三级标题样式。")

(defvar tp-palette-org-heading-4
  '(:fg ("#953800" . "#ffa657") :bg ("#fff8f0" . "#2e2318")
        :border ("#ffb86c" . "#9e6a03"))
  "Org 四级标题样式。")

(defvar tp-palette-org-heading-5
  '(:fg ("#bf3989" . "#db61a2") :bg ("#fff0f7" . "#2e1f28")
        :border ("#f28cb1" . "#8b3d63"))
  "Org 五级标题样式。")

(defvar tp-palette-org-heading-6
  '(:fg ("#0598bc" . "#39c5cf") :bg ("#f0faff" . "#1a2e33")
        :border ("#56d4dd" . "#2d7d85"))
  "Org 六级标题样式。")

;; ============================================
;; Org-mode 其他样式 (Org-mode Other Styles)
;; ============================================

(defvar tp-palette-org-todo
  '(:fg ("#ffffff" . "#ffffff") :bg ("#cf222e" . "#da3633")
        :border ("#a40e26" . "#f85149"))
  "Org TODO 关键字样式。")

(defvar tp-palette-org-done
  '(:fg ("#ffffff" . "#ffffff") :bg ("#1a7f37" . "#238636")
        :border ("#116329" . "#2ea043"))
  "Org DONE 关键字样式。")

(defvar tp-palette-org-code
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#f6f8fa" . "#161b22")
        :border ("#d0d7de" . "#30363d"))
  "Org 行内代码样式。")

(defvar tp-palette-org-block
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#f6f8fa" .  "#161b22")
        :border ("#d0d7de" .  "#30363d"))
  "Org 代码块样式。")

(defvar tp-palette-org-quote
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#161b22")
        :border ("#d0d7de" . "#30363d"))
  "Org 引用块样式。")

(defvar tp-palette-org-link
  '(:fg ("#0969da" . "#58a6ff") :bg nil
        :border ("#0969da" . "#58a6ff"))
  "Org 链接样式。")

(defvar tp-palette-org-date
  '(:fg ("#8250df" . "#a371f7") :bg nil
        :border ("#8250df" . "#a371f7"))
  "Org 日期样式。")

(defvar tp-palette-org-tag
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" .  "#21262d")
        :border ("#d0d7de" .  "#888"))
  ;; #30363d
  "Org 标签样式。")

;; ============================================
;; 强调色样式 (Accent Color Styles)
;; ============================================

(defvar tp-palette-accent-blue
  '(:fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" . "#388bfd"))
  "蓝色强调样式。")

(defvar tp-palette-accent-purple
  '(:fg ("#8250df" . "#a371f7") :bg ("#fbefff" . "#271d36")
        :border ("#c297ff" . "#6e40c9"))
  "紫色强调样式。")

(defvar tp-palette-accent-pink
  '(:fg ("#bf3989" . "#db61a2") :bg ("#fff0f7" . "#2e1f28")
        :border ("#f28cb1" . "#8b3d63"))
  "粉色强调样式。")

(defvar tp-palette-accent-orange
  '(:fg ("#bc4c00" . "#f0883e") :bg ("#fff5eb" . "#2e2318")
        :border ("#ffb86c" . "#9e6a03"))
  "橙色强调样式。")

(defvar tp-palette-accent-green
  '(:fg ("#1a7f37" . "#3fb950") :bg ("#dafbe1" . "#1b4721")
        :border ("#4ac26b" . "#238636"))
  "绿色强调样式。")

(defvar tp-palette-accent-red
  '(:fg ("#cf222e" . "#f85149") :bg ("#ffebe9" . "#542426")
        :border ("#ff8182" . "#f85149"))
  "红色强调样式。")

(defvar tp-palette-accent-yellow
  '(:fg ("#9a6700" . "#d29922") :bg ("#fff8c5" . "#3d2e00")
        :border ("#d4a72c" . "#9e6a03"))
  "黄色强调样式。")

(defvar tp-palette-accent-cyan
  '(:fg ("#0598bc" . "#39c5cf") :bg ("#e8f9fc" . "#1a2e33")
        :border ("#56d4dd" . "#2d7d85"))
  "青色强调样式。")

;; ============================================
;; 通知/徽章样式 (Notification/Badge Styles)
;; ============================================

(defvar tp-palette-badge-primary
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" . "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "主要徽章样式。")

(defvar tp-palette-badge-secondary
  '(:fg ("#24292f" . "#c9d1d9") :bg ("#eaeef2" . "#30363d")
        :border ("#d0d7de" . "#484f58"))
  "次级徽章样式。")

(defvar tp-palette-badge-success
  '(:fg ("#ffffff" . "#ffffff") :bg ("#1a7f37" . "#238636")
        :border ("#116329" . "#2ea043"))
  "成功徽章样式。")

(defvar tp-palette-badge-warning
  '(:fg ("#24292f" . "#24292f") :bg ("#ffd33d" . "#d29922")
        :border ("#bf8700" . "#bb8009"))
  "警告徽章样式。")

(defvar tp-palette-badge-danger
  '(:fg ("#ffffff" . "#ffffff") :bg ("#cf222e" .  "#da3633")
        :border ("#a40e26" . "#f85149"))
  "危险徽章样式。")

;; ============================================
;; 彩虹括号样式 (Rainbow Delimiters Styles)
;; ============================================

(defvar tp-palette-rainbow-1
  '(:fg ("#e45649" . "#e06c75") :bg nil
        :border ("#e45649" . "#e06c75"))
  "彩虹括号第一层样式（红色）。")

(defvar tp-palette-rainbow-2
  '(:fg ("#986801" . "#d19a66") :bg nil
        :border ("#986801" . "#d19a66"))
  "彩虹括号第二层样式（橙色）。")

(defvar tp-palette-rainbow-3
  '(:fg ("#c18401" . "#e5c07b") :bg nil
        :border ("#c18401" . "#e5c07b"))
  "彩虹括号第三层样式（黄色）。")

(defvar tp-palette-rainbow-4
  '(:fg ("#50a14f" . "#98c379") :bg nil
        :border ("#50a14f" . "#98c379"))
  "彩虹括号第四层样式（绿色）。")

(defvar tp-palette-rainbow-5
  '(:fg ("#0184bc" . "#56b6c2") :bg nil
        :border ("#0184bc" . "#56b6c2"))
  "彩虹括号第五层样式（青色）。")

(defvar tp-palette-rainbow-6
  '(:fg ("#4078f2" . "#61afef") :bg nil
        :border ("#4078f2" . "#61afef"))
  "彩虹括号第六层样式（蓝色）。")

(defvar tp-palette-rainbow-7
  '(:fg ("#a626a4" . "#c678dd") :bg nil
        :border ("#a626a4" . "#c678dd"))
  "彩虹括号第七层样式（紫色）。")

(defvar tp-palette-rainbow-8
  '(:fg ("#bf3989" . "#db61a2") :bg nil
        :border ("#bf3989" . "#db61a2"))
  "彩虹括号第八层样式（粉色）。")

;; ============================================
;; 更多强调色 (Additional Accent Colors)
;; Inspired by Tailwind CSS color palette
;; ============================================

(defvar tp-palette-accent-teal
  '(:fg ("#0d9488" . "#2dd4bf") :bg ("#ccfbf1" . "#134e4a")
        :border ("#14b8a6" . "#0d9488"))
  "蓝绿色强调样式。")

(defvar tp-palette-accent-indigo
  '(:fg ("#4f46e5" . "#818cf8") :bg ("#e0e7ff" . "#312e81")
        :border ("#6366f1" . "#4f46e5"))
  "靛蓝色强调样式。")

(defvar tp-palette-accent-rose
  '(:fg ("#e11d48" . "#fb7185") :bg ("#ffe4e6" . "#4c0519")
        :border ("#f43f5e" . "#e11d48"))
  "玫瑰色强调样式。")

(defvar tp-palette-accent-amber
  '(:fg ("#d97706" . "#fbbf24") :bg ("#fef3c7" . "#451a03")
        :border ("#f59e0b" . "#d97706"))
  "琥珀色强调样式。")

(defvar tp-palette-accent-lime
  '(:fg ("#65a30d" . "#a3e635") :bg ("#ecfccb" . "#1a2e05")
        :border ("#84cc16" . "#65a30d"))
  "青柠色强调样式。")

(defvar tp-palette-accent-emerald
  '(:fg ("#059669" . "#34d399") :bg ("#d1fae5" . "#064e3b")
        :border ("#10b981" . "#059669"))
  "翡翠绿强调样式。")

(defvar tp-palette-accent-sky
  '(:fg ("#0284c7" . "#38bdf8") :bg ("#e0f2fe" . "#0c4a6e")
        :border ("#0ea5e9" . "#0284c7"))
  "天蓝色强调样式。")

(defvar tp-palette-accent-violet
  '(:fg ("#7c3aed" . "#a78bfa") :bg ("#ede9fe" . "#2e1065")
        :border ("#8b5cf6" . "#7c3aed"))
  "紫罗兰色强调样式。")

(defvar tp-palette-accent-fuchsia
  '(:fg ("#c026d3" . "#e879f9") :bg ("#fae8ff" . "#4a044e")
        :border ("#d946ef" . "#c026d3"))
  "洋红色强调样式。")

;; ============================================
;; 更多徽章样式 (Additional Badge Styles)
;; ============================================

(defvar tp-palette-badge-info
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" . "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "信息徽章样式。")

(defvar tp-palette-badge-purple
  '(:fg ("#ffffff" . "#ffffff") :bg ("#8250df" . "#8957e5")
        :border ("#6e40c9" . "#a371f7"))
  "紫色徽章样式。")

(defvar tp-palette-badge-pink
  '(:fg ("#ffffff" . "#ffffff") :bg ("#bf3989" . "#db61a2")
        :border ("#99306f" . "#f28cb1"))
  "粉色徽章样式。")

;; ============================================
;; 按钮扩展样式 (Additional Button Styles)
;; ============================================

(defvar tp-palette-button-info
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" . "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "信息按钮样式（蓝色）。")

;; ============================================
;; Markdown 样式 (Markdown Styles)
;; ============================================

(defvar tp-palette-markdown-heading
  '(:fg ("#0969da" . "#58a6ff") :bg nil
        :border ("#0969da" . "#58a6ff"))
  "Markdown 标题样式。")

(defvar tp-palette-markdown-bold
  '(:fg ("#24292f" . "#e6edf3") :bg nil
        :border ("#24292f" . "#e6edf3"))
  "Markdown 粗体样式。")

(defvar tp-palette-markdown-italic
  '(:fg ("#57606a" . "#8b949e") :bg nil
        :border ("#57606a" . "#8b949e"))
  "Markdown 斜体样式。")

(defvar tp-palette-markdown-code
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#f6f8fa" . "#161b22")
        :border ("#d0d7de" . "#30363d"))
  "Markdown 行内代码样式。")

(defvar tp-palette-markdown-link
  '(:fg ("#0969da" . "#58a6ff") :bg nil
        :border ("#0969da" . "#58a6ff"))
  "Markdown 链接样式。")

(defvar tp-palette-markdown-blockquote
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#161b22")
        :border ("#d0d7de" . "#30363d"))
  "Markdown 引用块样式。")

(defvar tp-palette-markdown-list
  '(:fg ("#9a6700" . "#d29922") :bg nil
        :border ("#9a6700" . "#d29922"))
  "Markdown 列表标记样式。")

;; ============================================
;; 灰度色阶 (Grayscale Palette)
;; Inspired by Tailwind CSS neutral colors
;; ============================================

(defvar tp-palette-gray-50
  '(:fg ("#24292f" . "#fafafa") :bg ("#fafafa" . "#171717")
        :border ("#e5e5e5" . "#262626"))
  "灰度 50 级别。")

(defvar tp-palette-gray-100
  '(:fg ("#24292f" . "#f5f5f5") :bg ("#f5f5f5" . "#1c1c1c")
        :border ("#e5e5e5" . "#262626"))
  "灰度 100 级别。")

(defvar tp-palette-gray-200
  '(:fg ("#24292f" . "#e5e5e5") :bg ("#e5e5e5" . "#262626")
        :border ("#d4d4d4" . "#404040"))
  "灰度 200 级别。")

(defvar tp-palette-gray-300
  '(:fg ("#24292f" . "#d4d4d4") :bg ("#d4d4d4" . "#404040")
        :border ("#a3a3a3" . "#525252"))
  "灰度 300 级别。")

(defvar tp-palette-gray-400
  '(:fg ("#24292f" . "#a3a3a3") :bg ("#a3a3a3" . "#525252")
        :border ("#737373" . "#737373"))
  "灰度 400 级别。")

(defvar tp-palette-gray-500
  '(:fg ("#ffffff" . "#737373") :bg ("#737373" . "#737373")
        :border ("#525252" . "#a3a3a3"))
  "灰度 500 级别。")

(defvar tp-palette-gray-600
  '(:fg ("#ffffff" . "#525252") :bg ("#525252" . "#a3a3a3")
        :border ("#404040" . "#d4d4d4"))
  "灰度 600 级别。")

(defvar tp-palette-gray-700
  '(:fg ("#ffffff" . "#404040") :bg ("#404040" . "#d4d4d4")
        :border ("#262626" . "#e5e5e5"))
  "灰度 700 级别。")

(defvar tp-palette-gray-800
  '(:fg ("#ffffff" . "#262626") :bg ("#262626" . "#e5e5e5")
        :border ("#171717" . "#f5f5f5"))
  "灰度 800 级别。")

(defvar tp-palette-gray-900
  '(:fg ("#ffffff" . "#171717") :bg ("#171717" . "#f5f5f5")
        :border ("#0a0a0a" . "#fafafa"))
  "灰度 900 级别。")

;; ============================================
;; 优先级样式 (Priority Styles)
;; ============================================

(defvar tp-palette-priority-highest
  '(:fg ("#ffffff" . "#ffffff") :bg ("#cf222e" . "#da3633")
        :border ("#a40e26" . "#f85149"))
  "最高优先级样式（红色）。")

(defvar tp-palette-priority-high
  '(:fg ("#ffffff" . "#ffffff") :bg ("#bc4c00" . "#f0883e")
        :border ("#953800" . "#ffa657"))
  "高优先级样式（橙色）。")

(defvar tp-palette-priority-medium
  '(:fg ("#24292f" . "#24292f") :bg ("#ffd33d" . "#d29922")
        :border ("#bf8700" . "#bb8009"))
  "中等优先级样式（黄色）。")

(defvar tp-palette-priority-low
  '(:fg ("#ffffff" . "#ffffff") :bg ("#1a7f37" . "#238636")
        :border ("#116329" . "#2ea043"))
  "低优先级样式（绿色）。")

(defvar tp-palette-priority-lowest
  '(:fg ("#ffffff" . "#ffffff") :bg ("#0969da" . "#1f6feb")
        :border ("#0550ae" . "#388bfd"))
  "最低优先级样式（蓝色）。")

;; ============================================
;; 进度状态样式 (Progress Status Styles)
;; ============================================

(defvar tp-palette-status-pending
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" . "#21262d")
        :border ("#d0d7de" . "#30363d"))
  "待处理状态样式（灰色）。")

(defvar tp-palette-status-in-progress
  '(:fg ("#0969da" . "#58a6ff") :bg ("#ddf4ff" . "#1f3d5c")
        :border ("#54aeff" . "#388bfd"))
  "进行中状态样式（蓝色）。")

(defvar tp-palette-status-review
  '(:fg ("#8250df" . "#a371f7") :bg ("#fbefff" . "#271d36")
        :border ("#c297ff" . "#6e40c9"))
  "审核中状态样式（紫色）。")

(defvar tp-palette-status-completed
  '(:fg ("#1a7f37" . "#3fb950") :bg ("#dafbe1" . "#1b4721")
        :border ("#4ac26b" . "#238636"))
  "已完成状态样式（绿色）。")

(defvar tp-palette-status-cancelled
  '(:fg ("#cf222e" . "#f85149") :bg ("#ffebe9" . "#542426")
        :border ("#ff8182" . "#f85149"))
  "已取消状态样式（红色）。")

(defvar tp-palette-status-on-hold
  '(:fg ("#9a6700" . "#d29922") :bg ("#fff8c5" . "#3d2e00")
        :border ("#d4a72c" . "#9e6a03"))
  "暂停状态样式（黄色）。")

;;; Utilities

(defun tp-palette--get-color (symbol key)
  "Get color value for KEY from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist.
KEY should be one of :fg, :bg, or :border.
Returns nil if SYMBOL is unbound or doesn't contain KEY."
  (setq symbol (intern (concat "tp-palette-"
                               (symbol-name symbol))))
  (when (and (symbolp symbol) (boundp symbol))
    (let ((plist (symbol-value symbol)))
      (when (plistp plist)
        (tp-parse-color (plist-get plist key))))))

(defun tp-palette-fg-color (symbol)
  "Get the foreground color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :fg key.
Returns nil if SYMBOL is unbound or doesn't contain :fg."
  (tp-palette--get-color symbol :fg))

(defun tp-palette-bg-color (symbol)
  "Get the background color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :bg key.
Returns nil if SYMBOL is unbound or doesn't contain :bg."
  (tp-palette--get-color symbol :bg))

(defun tp-palette-border-color (symbol)
  "Get the border color from palette SYMBOL.
SYMBOL should be a symbol bound to a palette plist with a :border key.
Returns nil if SYMBOL is unbound or doesn't contain :border."
  (tp-palette--get-color symbol :border))

(provide 'tp-palette)
;;; tp-palette.el ends here
