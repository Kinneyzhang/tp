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
;; ============================================

(defvar tp-palette-syntax-keyword
  '(:fg ("#cf222e" . "#ff7b72") :bg ("#fff5f5" . "#2d1f1f")
        :border ("#ffcecb" . "#5c3d3d"))
  "关键字样式，如 if, else, def, class 等。")

(defvar tp-palette-syntax-string
  '(:fg ("#0a3069" . "#a5d6ff") :bg ("#f0f7ff" . "#1a2634")
        :border ("#c8e1ff" . "#3d5a73"))
  "字符串样式。")

(defvar tp-palette-syntax-comment
  '(:fg ("#6e7781" . "#8b949e") :bg ("#f6f8fa" . "#161b22")
        :border ("#e1e4e8" . "#30363d"))
  "注释样式。")

(defvar tp-palette-syntax-function
  '(:fg ("#8250df" . "#d2a8ff") :bg ("#fbefff" . "#271d36")
        :border ("#d8b9ff" . "#553d7a"))
  "函数名样式。")

(defvar tp-palette-syntax-variable
  '(:fg ("#953800" . "#ffa657") :bg ("#fff8f0" . "#2e2318")
        :border ("#ffd8b5" . "#5c4427"))
  "变量名样式。")

(defvar tp-palette-syntax-constant
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#f0f6ff" . "#1a2433")
        :border ("#b6d7ff" . "#3d5573"))
  "常量样式。")

(defvar tp-palette-syntax-type
  '(:fg ("#116329" . "#7ee787") :bg ("#f0fff4" . "#1a2e1f")
        :border ("#a7f0ba" . "#3d6647"))
  "类型样式，如类名、结构体名。")

(defvar tp-palette-syntax-number
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#f0f6ff" . "#1a2433")
        :border ("#b6d7ff" . "#3d5573"))
  "数字样式。")

(defvar tp-palette-syntax-preprocessor
  '(:fg ("#cf222e" . "#ff7b72") :bg ("#fff5f5" . "#2d1f1f")
        :border ("#ffcecb" . "#5c3d3d"))
  "预处理器样式，如 #include, #define。")

(defvar tp-palette-syntax-builtin
  '(:fg ("#0550ae" . "#79c0ff") :bg ("#f0f6ff" . "#1a2433")
        :border ("#b6d7ff" . "#3d5573"))
  "内置函数/方法样式。")

(defvar tp-palette-syntax-regexp
  '(:fg ("#116329" . "#7ee787") :bg ("#f0fff4" . "#1a2e1f")
        :border ("#a7f0ba" . "#3d6647"))
  "正则表达式样式。")

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
  '(:fg ("#0969da" . "#58a6ff") :bg ("transparent" . "transparent")
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
  '(:fg ("unspecified" . "unspecified") :bg ("#add6ff" . "#264f78")
        :border ("#54aeff" . "#388bfd"))
  "选区样式。")

(defvar tp-palette-cursor
  '(:fg ("#ffffff" . "#0d1117") :bg ("#24292f" . "#58a6ff")
        :border ("#24292f" . "#58a6ff"))
  "光标样式。")

(defvar tp-palette-line-highlight
  '(:fg ("unspecified" . "unspecified") :bg ("#f6f8fa" . "#161b22")
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
  '(:fg ("#0969da" . "#58a6ff") :bg ("transparent" . "transparent")
        :border ("#0969da" . "#58a6ff"))
  "Org 链接样式。")

(defvar tp-palette-org-date
  '(:fg ("#8250df" . "#a371f7") :bg ("transparent" . "transparent")
        :border ("#8250df" . "#a371f7"))
  "Org 日期样式。")

(defvar tp-palette-org-tag
  '(:fg ("#57606a" . "#8b949e") :bg ("#f6f8fa" .  "#21262d")
        :border ("#d0d7de" .  "#30363d"))
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
  '(:fg ("#0969da" . "#58a6ff") :bg ("transparent" . "transparent")
        :border ("#0969da" . "#58a6ff"))
  "彩虹括号第一层样式。")

(defvar tp-palette-rainbow-2
  '(:fg ("#8250df" . "#a371f7") :bg ("transparent" . "transparent")
        :border ("#8250df" . "#a371f7"))
  "彩虹括号第二层样式。")

(defvar tp-palette-rainbow-3
  '(:fg ("#1a7f37" . "#3fb950") :bg ("transparent" . "transparent")
        :border ("#1a7f37" . "#3fb950"))
  "彩虹括号第三层样式。")

(defvar tp-palette-rainbow-4
  '(:fg ("#953800" . "#ffa657") :bg ("transparent" .  "transparent")
        :border ("#953800" . "#ffa657"))
  "彩虹括号第四层样式。")

(defvar tp-palette-rainbow-5
  '(:fg ("#bf3989" . "#db61a2") :bg ("transparent" . "transparent")
        :border ("#bf3989" . "#db61a2"))
  "彩虹括号第五层样式。")

(defvar tp-palette-rainbow-6
  '(:fg ("#0598bc" . "#39c5cf") :bg ("transparent" . "transparent")
        :border ("#0598bc" . "#39c5cf"))
  "彩虹括号第六层样式。")

;;; Utilities

(defun tp-palette-fg-color (symbol)
  (tp-parse-color (plist-get (symbol-value symbol) :fg)))

(defun tp-palette-bg-color (symbol)
  (tp-parse-color (plist-get (symbol-value symbol) :bg)))

(defun tp-palette-border-color (symbol)
  (tp-parse-color (plist-get (symbol-value symbol) :border)))

(provide 'tp-palette)
