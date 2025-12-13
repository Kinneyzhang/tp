# tp.el - Text Properties Library for Emacs

<p align="center">
  <strong>A powerful text properties manipulation library with an innovative layer system</strong>
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#quick-start">Quick Start</a> •
  <a href="#api-reference">API Reference</a> •
  <a href="#the-layer-system">Layer System</a> •
  <a href="README_CN.md">中文文档</a>
</p>

---

**tp.el** provides a convenient and unified API for manipulating Emacs text properties. Inspired by [ov.el](https://github.com/emacsorphanage/ov) for overlays, tp.el offers:

- **Unified API**: All property-setting functions work on both **strings** and **buffers**
- **Layer System**: Stack multiple property sets on the same text region
- **Pattern Matching**: Apply properties to text matching strings or regexps

## Features

- ✅ **Unified Object Support**: Functions like `tp-put`, `tp-match`, `tp-regexp` work on both strings and buffers
- ✅ **Innovative Layer System**: Stack, rotate, and manage multiple layers of properties
- ✅ **Layer Groups**: Define reusable sets of related layers
- ✅ **Search & Navigation**: Find and navigate through propertized text
- ✅ **Pattern Matching**: Apply properties to string/regexp matches
- ✅ **Clean API**: Consistent naming and calling conventions

## Requirements

- **Emacs 28.1+** (uses `object-intervals` function)
- **dash.el** (list manipulation utilities)

## Installation

```elisp
;; Add to your load-path
(add-to-list 'load-path "/path/to/tp")
(require 'tp)
```

Or with `use-package`:

```elisp
(use-package tp
  :load-path "/path/to/tp")
```

---

## Quick Start

### Setting Properties

```elisp
;; On current buffer (properties as a list)
(tp-put 1 10 '(face bold help-echo "Hello!"))

;; On a specific buffer
(tp-put 1 10 '(face bold) some-buffer)

;; On a string with range (0-indexed)
(tp-put 0 5 '(face bold) "Hello World")
;; => #("Hello World" 0 5 (face bold))

;; On entire string (flat properties)
(tp-put "Hello World" 'face 'bold 'help-echo "test")
;; => #("Hello World" 0 11 (face bold help-echo "test"))
```

### Getting Properties

```elisp
;; Get specific property at position
(tp-get 5 'face)  ; => bold

;; Get specific property from range
(tp-get 1 10 'face)  ; => bold

;; Get all properties from range
(tp-get 1 10)  ; => (face bold help-echo "Hello!")

;; Get all properties at point
(tp-at 5)  ; => (face bold help-echo "Hello!")
```

### Fine-grained Property Manipulation

```elisp
;; Get sub-property from face
(tp-get-sub 1 'face :foreground)  ; => "red"

;; Set sub-property on face
(tp-put-sub 1 6 'face :foreground "blue")

;; Remove sub-property from face
(tp-remove-sub 1 6 'face :foreground)
```

### Pattern Matching

```elisp
;; Apply properties to all occurrences of "TODO" in buffer
(tp-match "TODO" 'face 'warning)

;; Apply to string
(tp-match "world" "Hello world world" 'face 'bold)
;; => #("Hello world world" 6 11 (face bold) 12 17 (face bold))

;; Using regexp
(tp-regexp "\\b[0-9]+\\b" 'face 'font-lock-number-face)
```

---

## API Reference

### Core Property Functions

#### `tp-put` - Set Text Properties

Set text properties on a string or buffer region.

```elisp
;; Current buffer (properties as a list)
(tp-put START END '(PROPERTY VALUE ...))

;; Specific buffer or string
(tp-put START END '(PROPERTY VALUE ...) OBJECT)

;; Entire string (flat properties)
(tp-put STRING PROPERTY VALUE ...)
```

**Examples:**

```elisp
;; Set face on buffer region
(tp-put 1 10 '(face bold))  ; => (1 . 10)

;; Set multiple properties
(tp-put 1 10 '(face bold help-echo "Click me"))

;; Set on specific buffer
(tp-put 1 10 '(face italic) my-buffer)

;; Set properties on a string (0-indexed)
(setq my-string (tp-put 0 5 '(face italic) "Hello World"))
;; => #("Hello World" 0 5 (face italic))

;; Set properties on entire string
(tp-put "Hello" 'face 'bold 'mouse-face 'highlight)
;; => #("Hello" 0 5 (face bold mouse-face highlight))
```

---

#### `tp-get` - Get Property Value

Get property value(s) from position or range.

```elisp
;; Single position
(tp-get POSITION PROPERTY)
(tp-get POSITION PROPERTY OBJECT)

;; Range - specific property
(tp-get START END PROPERTY)
(tp-get START END PROPERTY OBJECT)

;; Range - all properties
(tp-get START END)
(tp-get START END OBJECT)
```

**Examples:**

```elisp
;; Get from current buffer
(tp-get 5 'face)           ; => bold

;; Get from string (0-indexed)
(tp-get 0 'face my-string) ; => italic

;; Get from range
(tp-get 1 10 'face)        ; => bold

;; Get all properties from range
(tp-get 1 10)              ; => (face bold help-echo "test")
```

---

#### Fine-grained Property Functions

For manipulating sub-properties within complex properties like `face` or `display`:

```elisp
;; Get sub-property
(tp-get-sub POSITION PROPERTY SUB-PROPERTY &optional OBJECT)

;; Set sub-property
(tp-put-sub START END PROPERTY SUB-PROPERTY VALUE &optional OBJECT)

;; Remove sub-property
(tp-remove-sub START END PROPERTY SUB-PROPERTY &optional OBJECT)
```

**Examples:**

```elisp
;; Get :foreground from face
(tp-get-sub 1 'face :foreground)  ; => "red"

;; Set :weight on face
(tp-put-sub 1 6 'face :weight 'bold)

;; Remove :background from face
(tp-remove-sub 1 6 'face :background)
```

---

#### `tp-at` - Get All Properties

```elisp
(tp-at &optional POINT OBJECT)
```

Get all text properties at POINT as a plist.

**Examples:**

```elisp
(tp-at 5)  ; => (face bold help-echo "test")
(tp-at 0 my-string)  ; Get from string
```

---

#### `tp-remove` - Remove Property

```elisp
(tp-remove START END PROPERTY &optional OBJECT)
```

Remove a specific property from a region.

**Examples:**

```elisp
(tp-remove 1 10 'face)  ; Remove face property
```

---

#### `tp-remove-list` - Remove Multiple Properties

```elisp
(tp-remove-list START END PROPERTIES &optional OBJECT)
```

Remove multiple properties at once.

**Examples:**

```elisp
(tp-remove-list 1 10 '(face help-echo mouse-face))
```

---

#### `tp-clear` - Clear All Properties

```elisp
(tp-clear &optional START END OBJECT)
```

Clear all text properties from a region.

**Examples:**

```elisp
(tp-clear 1 10)     ; Clear region
(tp-clear)          ; Clear entire buffer
```

---

### Propertize Functions

#### `tp-propertize` - Create Propertized String

```elisp
;; Create propertized string
(tp-propertize STRING PROPERTY VALUE ...)
(tp-propertize STRING '(PROPERTY VALUE ...))

;; Apply to region of object
(tp-propertize OBJECT START END PROPERTY VALUE ...)
```

**Examples:**

```elisp
;; Simple usage - returns propertized string
(tp-propertize "Hello" 'face 'bold)
;; => #("Hello" 0 5 (face bold))

;; With property list
(tp-propertize "World" '(face italic help-echo "greeting"))

;; Apply to substring
(tp-propertize "Hello World" 6 11 'face 'underline)
```

---

#### `tp-layer-propertize` - Apply Layer to Object

```elisp
(tp-layer-propertize OBJECT LAYER &optional START END)
```

Apply a predefined layer's properties to an object.

**Examples:**

```elisp
;; Define a layer first
(tp-layer-define highlight '(face (:background "yellow")))

;; Apply to string
(tp-layer-propertize "Important" 'highlight)

;; Apply to substring
(tp-layer-propertize "Hello World" 'highlight 0 5)

;; Apply to buffer region
(tp-layer-propertize (current-buffer) 'highlight 1 10)
```

---

#### `tp-group-propertize` - Apply Layer Group

```elisp
(tp-group-propertize OBJECT LAYER-GROUP &optional START END)
```

Apply all layers from a layer group to an object.

---

### Pattern Matching Functions

#### `tp-match` - Match String

```elisp
;; Buffer
(tp-match PATTERN PROPERTY VALUE ...)

;; String or Buffer object
(tp-match PATTERN OBJECT PROPERTY VALUE ...)
```

Set properties on all occurrences of a string pattern.

**Examples:**

```elisp
;; In buffer - returns list of (START . END) pairs
(tp-match "TODO" 'face 'warning)
;; => ((10 . 14) (50 . 54) ...)

;; On string - returns modified string
(tp-match "o" "Hello World" 'face 'bold)
;; => #("Hello World" 4 5 (face bold) 7 8 (face bold))
```

---

#### `tp-regexp` - Match Regexp

```elisp
;; Buffer
(tp-regexp PATTERN PROPERTY VALUE ...)

;; String or Buffer object
(tp-regexp PATTERN OBJECT PROPERTY VALUE ...)
```

Set properties on all matches of a regular expression.

**Examples:**

```elisp
;; Highlight all numbers in buffer
(tp-regexp "[0-9]+" 'face 'font-lock-number-face)

;; On string
(tp-regexp "[A-Z]+" "Hello WORLD" 'face 'bold)
;; => #("Hello WORLD" 6 11 (face bold))
```

---

### Search & Navigation Functions

#### `tp-forward` / `tp-backward`

```elisp
(tp-forward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
(tp-backward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
```

Search forward/backward for text with PROPERTY.

**Examples:**

```elisp
;; Find next text with 'marker property
(tp-forward 'marker)

;; Find next text where 'type equals 'heading
(tp-forward 'type 'heading)
```

---

#### `tp-next` / `tp-prev`

```elisp
(tp-next &optional POINT PROPERTY VALUE)
(tp-prev &optional POINT PROPERTY VALUE)
```

Get the next/previous position with text properties.

---

#### `tp-goto-next` / `tp-goto-prev`

```elisp
(tp-goto-next &optional PROPERTY VALUE)
(tp-goto-prev &optional PROPERTY VALUE)
```

Move point to next/previous text with PROPERTY.

---

#### `tp-regions-map` / `tp-strings-map`

```elisp
(tp-regions-map FUNCTION PROPERTY &optional VALUE PREDICATE COLLECT)
(tp-strings-map FUNCTION PROPERTY &optional VALUE PREDICATE COLLECT)
```

Apply a function to all regions/strings with PROPERTY.

**Examples:**

```elisp
;; Upcase all marked text
(tp-strings-map
 (lambda (str idx)
   (message "Found: %s at index %d" str idx))
 'marker)
```

---

### Query Functions

#### `tp-in` - Find Regions with Property

```elisp
(tp-in PROPERTY &optional VALUE START END)
```

Get all regions with PROPERTY in current buffer.

**Examples:**

```elisp
;; Get all regions with 'marker property
(tp-in 'marker)
;; => ((1 5 (marker t ...)) (10 15 (marker t ...)))

;; Filter by value
(tp-in 'type 'heading)
```

---

#### `tp-all` - Get All Propertized Regions

```elisp
(tp-all &optional START END)
```

Get all regions with any text properties.

---

#### `tp-intervals` - Get Property Intervals

```elisp
(tp-intervals START END &optional OBJECT)
```

Get all text property intervals in a region.

---

#### `tp-empty-p` - Check for Properties

```elisp
(tp-empty-p OBJECT)
```

Return t if OBJECT has no text properties.

---

#### `tp-plist` - Get Merged Properties

```elisp
(tp-plist START END &optional OBJECT)
```

Get a merged plist of all properties in a region.

---

## The Layer System

The **layer system** is tp.el's innovative feature that allows stacking multiple sets of properties on the same text region. Only the **top layer** is visible, but lower layers are preserved and can be revealed through rotation or pinning.

### Layer Concept

```
┌─────────────────────────────┐
│   TOP LAYER (visible)       │  ← What you see
├─────────────────────────────┤
│   Middle Layer (hidden)     │  ← Preserved
├─────────────────────────────┤
│   Bottom Layer (hidden)     │  ← Preserved
└─────────────────────────────┘
```

### Layer Definition Functions

#### `tp-layer-define` - Define a Layer

```elisp
(tp-layer-define NAME PROPERTIES)
```

Define a named layer with properties.

**Examples:**

```elisp
(tp-layer-define highlight
  '(face (:background "yellow" :foreground "black")))

(tp-layer-define error
  '(face (:background "red" :foreground "white")
    help-echo "Error!"))

(tp-layer-define info
  '(face (:background "blue" :foreground "white")))
```

---

#### `tp-group-define` - Define Layer Group

```elisp
(tp-group-define NAME
  LAYER1 PROPERTIES1
  LAYER2 PROPERTIES2
  ...)
```

Define a group of related layers.

**Examples:**

```elisp
(tp-group-define status-colors
  status-ok      '(face (:foreground "green"))
  status-warning '(face (:foreground "orange"))
  status-error   '(face (:foreground "red")))
```

---

#### `tp-layer-props` / `tp-group-props`

```elisp
(tp-layer-props LAYER-NAME)
(tp-group-props GROUP-NAME)
```

Get properties for a layer or all layers in a group.

---

#### `tp-layer-undefine` / `tp-group-undefine`

```elisp
(tp-layer-undefine NAME)
(tp-group-undefine NAME)
```

Remove layer or group definition.

---

#### `tp-layer-reset`

```elisp
(tp-layer-reset)
```

Clear all layer and group definitions.

---

### Layer Manipulation Functions

#### `tp-layer-push` - Add Layer

```elisp
(tp-layer-push START END NAME &optional OBJECT)
```

Push a layer to the top of the stack.

**Examples:**

```elisp
(tp-layer-define base '(face default))
(tp-layer-define highlight '(face (:background "yellow")))

;; Push base layer first
(tp-layer-push 1 10 'base)

;; Push highlight on top (now visible)
(tp-layer-push 1 10 'highlight)
```

---

#### `tp-layer-delete` - Remove Layer

```elisp
(tp-layer-delete START END NAME &optional OBJECT)
```

Delete a layer from anywhere in the stack.

**Examples:**

```elisp
;; Remove the highlight layer
(tp-layer-delete 1 10 'highlight)
;; base layer is now visible
```

---

#### `tp-layer-rotate` - Cycle Layers

```elisp
(tp-layer-rotate START END &optional OBJECT)
```

Rotate layers - top goes to bottom, next becomes visible.

**Examples:**

```elisp
;; Stack: highlight (top) -> base (bottom)
(tp-layer-rotate 1 10)
;; Stack: base (top) -> highlight (bottom)
```

---

#### `tp-layer-pin` - Bring Layer to Top

```elisp
(tp-layer-pin START END NAME &optional OBJECT)
```

Move a specific layer to the top.

**Examples:**

```elisp
;; Make 'base the top layer
(tp-layer-pin 1 10 'base)
```

---

#### `tp-layer-hide` / `tp-layer-show`

```elisp
(tp-layer-hide START END NAME &optional OBJECT)
(tp-layer-show START END NAME &optional OBJECT)
```

Hide layer (move to bottom) or show layer (move to top).

---

#### `tp-layer-merge`

```elisp
(tp-layer-merge START END LAYER1 LAYER2 NEW-NAME &optional OBJECT)
```

Merge two layers into one new layer.

---

### Layer Query Functions

#### `tp-layer-list` - List All Layers

```elisp
(tp-layer-list START END &optional OBJECT)
```

Get list of all layer names in region.

**Examples:**

```elisp
(tp-layer-list 1 10)  ; => (highlight base)
```

---

#### `tp-layer-count`

```elisp
(tp-layer-count START END &optional OBJECT)
```

Count layers in region.

---

#### `tp-layer-exists-p`

```elisp
(tp-layer-exists-p START END NAME &optional OBJECT)
```

Check if layer exists in region.

---

#### `tp-layer-top`

```elisp
(tp-layer-top START END &optional OBJECT)
```

Get name of the top (visible) layer.

---

## Practical Examples

### Syntax Highlighting with Multiple Layers

```elisp
;; Define layers for different highlighting purposes
(tp-layer-define code-base
  '(face font-lock-keyword-face))

(tp-layer-define code-error
  '(face (:underline (:color "red" :style wave))
    help-echo "Syntax error"))

(tp-layer-define code-debug
  '(face (:background "dark blue")))

;; Apply base highlighting
(tp-layer-push 1 100 'code-base)

;; Add error highlight on problematic code
(tp-layer-push 50 60 'code-error)

;; Toggle between error and normal view
(defun toggle-error-view ()
  (interactive)
  (tp-layer-rotate 50 60))
```

### Status Indicator

```elisp
(tp-group-define task-status
  status-todo     '(face (:foreground "gray"))
  status-progress '(face (:foreground "yellow"))
  status-done     '(face (:foreground "green")))

;; Cycle through statuses
(defun cycle-task-status ()
  (interactive)
  (tp-layer-rotate (line-beginning-position) (line-end-position)))
```

### Temporary Highlights

```elisp
(tp-layer-define temp-highlight
  '(face (:background "yellow")))

(defun flash-region (start end)
  "Flash a region temporarily."
  (tp-layer-push start end 'temp-highlight)
  (run-with-timer 0.5 nil
                  (lambda ()
                    (tp-layer-delete start end 'temp-highlight))))
```

---

## Aliases

For convenience, tp.el provides these aliases:

| Alias | Original Function |
|-------|-------------------|
| `tp-set` | `tp-put` |
| `tp-layer-properties` | `tp-layer-props` |
| `tp-layer-group-define` | `tp-group-define` |
| `tp-layer-group-properties` | `tp-group-props` |
| `tp-layer-group-propertize` | `tp-group-propertize` |
| `tp-layer-group-undefine` | `tp-group-undefine` |

---

## License

GNU General Public License v2 or later.

---

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

---

<p align="center">
  <em>tp.el - Making text properties powerful and easy to use</em>
</p>
