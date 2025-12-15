# tp.el - Text Properties Library for Emacs

<p align="center">
  <strong>A powerful text properties manipulation library with an innovative property layer system</strong>
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#quick-start">Quick Start</a> •
  <a href="#api-reference">API Reference</a> •
  <a href="#the-property-layer-system">Property Layer System</a> •
  <a href="README_CN.md">中文文档</a>
</p>

---

## Overview

**tp.el** is a library that comprehensively enhances Emacs text property manipulation. It is not just a simple wrapper around native text property APIs (like `put-text-property`, `get-text-property`), but provides many **functional extensions that native functions do not have**.

Inspired by [ov.el](https://github.com/emacsorphanage/ov) for overlays, tp.el innovates in the following areas:

### Core Innovations

1. **Unified API Parameter Conventions**: All functions support multiple flexible calling patterns, working seamlessly with both strings and buffers
2. **Fine-grained Sub-property Operations**: Support path-style access, modification, and deep merging of nested properties
3. **Innovative Property Layer System**: Stack and manage multiple sets of properties on the same text region with layered control
4. **Pattern Matching Batch Operations**: Batch apply properties via string or regular expression matching
5. **Enhanced Search & Navigation**: Rich property search and traversal functionality

## Features

### Unified API Parameter Conventions

Native Emacs APIs have different functions and parameter orders for strings and buffers. tp.el unifies all of this:

- ✅ **Three Calling Conventions**: All core functions (`tp-set`, `tp-get`, `tp-remove`, etc.) support three flexible calling patterns:
  ```elisp
  ;; 1. Current buffer
  (tp-set START END '(face bold))
  ;; 2. Specific buffer or string
  (tp-set START END '(face bold) OBJECT)
  ;; 3. Entire string (flat properties)
  (tp-set STRING 'face 'bold 'help-echo "tip")
  ```
- ✅ **Unified Object Support**: The same function works with both strings and buffers, no need to remember different APIs

### Three Property Operation Semantics

Native APIs only have simple set and get. tp.el provides three clear operation semantics:

- ✅ **`tp-reset`**: Complete replacement - clears all existing properties, sets new ones
- ✅ **`tp-set`**: Partial replacement - only replaces specified properties, preserves others
- ✅ **`tp-add`**: Deep merge - intelligently merges nested properties instead of simple overwrite

```elisp
;; Deep merge example
(tp-set 1 10 '(face (:foreground "red")))
(tp-add 1 10 '(face (:background "blue")))
;; Result: face is (:foreground "red" :background "blue")
;; Native API would completely overwrite, but tp-add merges intelligently
```

### Fine-grained Sub-property Operations

**This is functionality that native APIs completely lack**. tp.el supports fine-grained reading, modification, and deletion of nested properties:

- ✅ **Path-style Access**: Access deeply nested property values through path syntax
  ```elisp
  ;; Get nested properties
  (tp-get str 'face :underline :style)  ; => wave
  (tp-at 5 '(face :box :color))         ; => "blue"
  
  ;; Get multiple nested keys
  (tp-get str 'face :underline '(:color :style))
  ;; => ((:color "green" :style wave))
  ```
- ✅ **Sub-property Deletion**: Precisely remove specific keys from nested properties
  ```elisp
  ;; Only delete :style from :underline, preserve :color
  (tp-remove 1 10 '(face :underline (:style)))
  ```
- ✅ **Deep Merge**: `tp-add` recursively merges nested plist structures
- ✅ **Smart Face Merging**: Symbol faces are automatically prepended to face lists, plist faces are deep merged

### Innovative Property Layer System

**This is tp.el's most innovative feature**, completely unsupported by native Emacs. The property layer system allows stacking multiple sets of properties on the same text region:

- ✅ **Property Layer Stack Concept**: Multiple property layers stack like a stack, only the top layer is visible, lower layers are preserved
- ✅ **Property Layer Definition & Reuse**: Define reusable property layers and layer groups via `tp-define-layer`
- ✅ **Rich Property Layer Operations**:
  - Placement: `tp-put-layer` (specific position), `tp-push-layer` (top)
  - Deletion: `tp-delete-layer` (by name/index), `tp-pop-layer` (top layer)
  - Movement: `tp-raise-layer` (up/down), `tp-rotate-layer` (rotate), `tp-pin-layer` (pin to top), `tp-switch-layer` (swap)
  - Merging: `tp-merge-layers` (merge specified layers), `tp-flatten-layers` (flatten all layers)
- ✅ **Property Layer Queries**: `tp-layer-list`, `tp-layer-count`, `tp-layer-exists-p`, `tp-layer-top`

```elisp
;; Property layer usage example
(tp-define-layer highlight (face (:background "yellow")))
(tp-define-layer error (face (:foreground "red")))

;; Stack multiple property layers
(tp-push-layer 1 10 'highlight)
(tp-push-layer 1 10 'error)  ; error is now visible

;; Rotate display
(tp-rotate-layer 1 10)  ; highlight is now visible
```

### Pattern Matching & Batch Operations

Native APIs require manual searching and looping. tp.el provides convenient pattern matching functionality:

- ✅ **String Matching**: `tp-match-set`, `tp-match-reset`, `tp-match-add`
- ✅ **Regexp Matching**: `tp-regexp-set`, `tp-regexp-reset`, `tp-regexp-add`
- ✅ **Three Semantic Variants**: Each match type supports set/reset/add operation semantics

```elisp
;; Highlight all TODOs
(tp-match-set "TODO" '(face warning))

;; Regexp match all numbers
(tp-regexp-set "[0-9]+" '(face font-lock-number-face))

;; Add properties with deep merge
(tp-match-add "TODO" '(face (:underline t)))
```

### Enhanced Search & Navigation

- ✅ **Range Search**: `tp-search` returns a list of all matching intervals
- ✅ **N-times Search**: `tp-forward`/`tp-backward` support searching forward/backward N times
- ✅ **Search and Execute**: `tp-forward-do`/`tp-backward-do` search and execute function on matched text
- ✅ **Batch Transform**: `tp-search-map` applies transformation function to all matches

```elisp
;; Search all markers
(tp-search my-string 'marker)  ; => ((0 5 t) (12 17 t))

;; Upcase all marker text
(tp-search-map #'upcase my-string 'marker)
```

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

## API Reference

### API Quick Reference

A complete overview of all tp.el functions organized by category:

#### Core Property Functions
| Function | Description |
|----------|-------------|
| [`tp-set`](#tp-set---set-text-properties) | Set text properties (replaces specified properties only) |
| [`tp-reset`](#tp-reset---replace-all-properties) | Replace ALL text properties |
| [`tp-add`](#tp-add---addmerge-properties) | Add/merge properties with deep merge support |
| [`tp-get`](#tp-get---get-property-value) | Get property value(s) from range or string |
| [`tp-at`](#tp-at---get-property-at-position) | Get property value(s) at a single position |
| [`tp-remove`](#tp-remove---remove-property) | Remove a property or sub-property |
| [`tp-clear`](#tp-clear---clear-all-properties) | Clear all text properties from a region |

#### Pattern Matching Functions
| Function | Description |
|----------|-------------|
| [`tp-match-set`](#tp-match-set---match-string) | Set properties on string pattern matches |
| [`tp-match-reset`](#tp-match-reset---match-and-reset) | Reset all properties on string matches |
| [`tp-match-add`](#tp-match-add---match-and-add) | Add/merge properties on string matches |
| [`tp-regexp-set`](#tp-regexp-set---match-regexp) | Set properties on regexp matches |
| [`tp-regexp-reset`](#tp-regexp-reset---regexp-and-reset) | Reset all properties on regexp matches |
| [`tp-regexp-add`](#tp-regexp-add---regexp-and-add) | Add/merge properties on regexp matches |

#### Search & Navigation Functions
| Function | Description |
|----------|-------------|
| [`tp-search-forward`](#tp-search-forward--tp-search-backward) | Raw wrapper for text-property-search-forward |
| [`tp-search-backward`](#tp-search-forward--tp-search-backward) | Raw wrapper for text-property-search-backward |
| [`tp-forward`](#tp-forward--tp-backward) | Search forward N times for text with property (buffers and strings) |
| [`tp-backward`](#tp-forward--tp-backward) | Search backward N times for text with property (buffers and strings) |
| [`tp-forward-do`](#tp-forward-do--tp-backward-do) | Apply function to matched text for N forward matches (with optional start point) |
| [`tp-backward-do`](#tp-forward-do--tp-backward-do) | Apply function to matched text for N backward matches (with optional start point) |
| [`tp-search`](#tp-search---search-all-matches) | Search all matching properties in range or string |
| [`tp-search-map`](#tp-search-map---apply-function-to-matched-text) | Apply function to matched text for all matches |

#### Property Layer Definition Functions
| Function | Description |
|----------|-------------|
| [`tp-define-layer`](#tp-define-layer---define-layers) | Define a layer or layer group |
| [`tp-layer-props`](#tp-layer-props--tp-group-props) | Get properties for a layer |
| [`tp-group-props`](#tp-layer-props--tp-group-props) | Get properties for all layers in a group |
| [`tp-layer-undefine`](#tp-layer-undefine--tp-group-undefine) | Remove layer definition |
| [`tp-group-undefine`](#tp-layer-undefine--tp-group-undefine) | Remove group definition |
| [`tp-layer-reset`](#tp-layer-reset) | Clear all layer/group definitions |

#### Property Layer Placement Functions
| Function | Description |
|----------|-------------|
| [`tp-put-layer`](#tp-put-layer---set-layer-at-index) | Set layer at specific index position |
| [`tp-push-layer`](#tp-push-layer---push-layer-to-top) | Push layer to top of stack |

#### Property Layer Deletion Functions
| Function | Description |
|----------|-------------|
| [`tp-delete-layer`](#tp-delete-layer---delete-layer-by-nameindex) | Delete layer by name or index |
| [`tp-pop-layer`](#tp-pop-layer---pop-top-layer) | Remove top layer |

#### Property Layer Movement Functions
| Function | Description |
|----------|-------------|
| [`tp-raise-layer`](#tp-raise-layer---move-layer-updown) | Move layer up/down by N positions |
| [`tp-rotate-layer`](#tp-rotate-layer---cycle-layers) | Cycle layers (top goes to bottom) |
| [`tp-pin-layer`](#tp-pin-layer---pin-layer-to-top) | Pin a layer to top (make visible) |
| [`tp-switch-layer`](#tp-switch-layer---switch-two-layers) | Swap positions of two layers |

#### Property Layer Merging Functions
| Function | Description |
|----------|-------------|
| [`tp-merge-layers`](#tp-merge-layers---merge-multiple-layers) | Merge specified layers into a new layer |
| [`tp-flatten-layers`](#tp-flatten-layers---flatten-all-layers) | Flatten all layers into a single layer |

#### Property Layer Query Functions
| Function | Description |
|----------|-------------|
| [`tp-layer-list`](#tp-layer-list---list-all-layers) | List all layer names in region |
| [`tp-layer-count`](#tp-layer-count) | Count layers in region |
| [`tp-layer-exists-p`](#tp-layer-exists-p) | Check if layer exists in region |
| [`tp-layer-top`](#tp-layer-top) | Get name of top (visible) layer |

---

### Core Property Functions

#### `tp-set` - Set Text Properties

Set text properties on a string or buffer region. Replaces only the specified properties, preserving others.

```elisp
;; Current buffer (properties as a list)
(tp-set START END '(PROPERTY VALUE ...))

;; Specific buffer or string
(tp-set START END '(PROPERTY VALUE ...) OBJECT)

;; Entire string (flat properties)
(tp-set STRING PROPERTY VALUE ...)
```

**Examples:**

```elisp
;; Set face on buffer region
(tp-set 1 10 '(face bold))  ; => (1 . 10)

;; Set multiple properties
(tp-set 1 10 '(face bold help-echo "Click me"))

;; Set on specific buffer
(tp-set 1 10 '(face italic) my-buffer)

;; Set properties on a string (0-indexed)
(setq my-string (tp-set 0 5 '(face italic) "Hello World"))
;; => #("Hello World" 0 5 (face italic))

;; Set properties on entire string
(tp-set "Hello" 'face 'bold 'mouse-face 'highlight)
;; => #("Hello" 0 5 (face bold mouse-face highlight))
```

---

#### `tp-reset` - Replace All Properties

Completely replace ALL text properties with the specified ones.

```elisp
(tp-reset START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-reset STRING PROPERTY VALUE ...)
```

**Examples:**

```elisp
;; Replace all properties in region
(tp-reset 1 10 '(face bold))  ; Any existing properties are removed

;; On string
(tp-reset "Hello" 'face 'italic)
```

---

#### `tp-add` - Add/Merge Properties

Add or update properties with deep merge support for nested plists.

```elisp
(tp-add START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-add STRING PROPERTY VALUE ...)
```

**Examples:**

```elisp
;; Add properties (preserves existing, merges nested)
(tp-add 1 10 '(help-echo "tooltip"))

;; Deep merge face properties
(tp-set 1 10 '(face (:foreground "red")))
(tp-add 1 10 '(face (:background "blue")))
;; Result: face is (:foreground "red" :background "blue")

;; Face prepending - symbol faces are prepended to face list
(tp-set "Hello" 'face 'bold)
(tp-add "Hello" 'face 'shadow)
;; Result: face is (shadow bold)
```

---

#### `tp-get` - Get Property Value

Get property value(s) from range or string, with support for nested sub-properties.

Returns a list of `(START END VALUE)` intervals, allowing you to see all property values across the range.

For single position queries, use `tp-at` instead.

```elisp
;; Range - specific property (returns list of intervals)
(tp-get START END PROPERTY)
(tp-get START END PROPERTY OBJECT)

;; Range with property path as list
(tp-get START END '(PROPERTY) OBJECT)
(tp-get START END '(PROPERTY SUB-KEY ...) OBJECT)

;; Range with deeply nested property path
(tp-get START END '(PROPERTY SUB-KEY SUB-SUB-KEY ...) OBJECT)

;; Range extracting multiple keys from nested property
(tp-get START END '(PROPERTY SUB-KEY (KEY1 KEY2 ...)) OBJECT)

;; Range - all properties (returns list of intervals)
(tp-get START END)
(tp-get START END OBJECT)

;; Entire string (returns list of intervals)
(tp-get STRING)
(tp-get STRING PROPERTY)
(tp-get STRING PROPERTY SUB-KEY ...)
(tp-get STRING PROPERTY SUB-KEY '(KEY1 KEY2 ...))
(tp-get STRING '(PROPERTY SUB-KEY ...))
```

**Examples:**

```elisp
;; Get from range - returns list of (START END VALUE) intervals
(tp-get 1 10 'face)        ; => ((1 6 bold))

;; Get with multiple intervals
(setq str (copy-sequence "Hello World Hello"))
(tp-set 0 5 '(face bold) str)
(tp-set 12 17 '(face italic) str)
(tp-get 0 17 'face str)    ; => ((0 5 bold) (12 17 italic))

;; Get with property path as list
(setq my-string (copy-sequence "Hello World Hello World"))
(put-text-property 5 20 'face '(:underline (:style wave)) my-string)
(tp-get 5 20 '(face :underline :style) my-string)  ; => ((5 20 wave))

;; Get deeply nested property from entire string
(setq str (copy-sequence "Hello World"))
(put-text-property 0 5 'face '(:underline (:color "green")) str)
(put-text-property 6 11 'face '(:underline (:color "yellow")) str)
(tp-get str 'face :underline :color)  ; => ((0 5 "green") (6 11 "yellow"))

;; Get multiple keys from nested property
(setq str (copy-sequence "Hello World"))
(put-text-property 0 5 'face '(:underline (:color "green" :style wave)) str)
(put-text-property 6 11 'face '(:underline (:color "yellow" :style line)) str)
(tp-get str 'face :underline '(:color :style))
;; => ((0 5 (:color "green" :style wave)) (6 11 (:color "yellow" :style line)))

;; Get all properties from range
(tp-get 1 10)              ; => ((1 6 (face bold help-echo "test")))

;; Get from entire string - returns list of intervals
(setq str (copy-sequence "Hello World Hello"))
(tp-set 0 5 '(face bold) str)
(tp-set 12 17 '(face italic) str)
(tp-get str)               ; => ((0 5 (face bold)) (12 17 (face italic)))
(tp-get str 'face)         ; => ((0 5 bold) (12 17 italic))
```

---

#### `tp-at` - Get Property at Position

```elisp
;; Get all properties at position
(tp-at POS)
(tp-at POS OBJECT)

;; Get specific property at position
(tp-at POS PROPERTY)
(tp-at POS PROPERTY OBJECT)

;; Get nested sub-property at position
(tp-at POS '(PROPERTY SUB-KEY ...))
(tp-at POS '(PROPERTY SUB-KEY ...) OBJECT)
```

Get text properties at POS, optionally filtered by PROPERTY.

For single-position property queries (previously done with `tp-get`), use `tp-at`.

**Examples:**

```elisp
;; Get all properties at position 5 in current buffer
(tp-at 5)  ; => (face bold help-echo "test")

;; Get all properties at position 0 in string
(setq my-string (tp-set "Hello" 'face 'italic 'help-echo "greeting"))
(tp-at 0 my-string)  ; => (face italic help-echo "greeting")

;; Get specific property at position
(tp-at 5 'face)  ; => bold
(tp-at 0 'face my-string)  ; => italic

;; Get nested sub-property at position
(tp-at 5 '(face :foreground))  ; => "red"
(tp-at 5 '(face :box :color))  ; => "blue"
(tp-at 5 '(display :width))    ; => 10

;; Get nested sub-property from string
(setq str (copy-sequence "Hello"))
(put-text-property 0 5 'face '(:foreground "red" :underline t) str)
(tp-at 0 '(face :foreground) str)  ; => "red"
```

---

#### `tp-remove` - Remove Property

Remove a property or nested sub-property from a region or entire string.

```elisp
;; Remove entire property (buffer)
(tp-remove START END PROPERTY &optional OBJECT)

;; Remove sub-property (buffer)
(tp-remove START END '(PROPERTY SUB-KEY) &optional OBJECT)

;; Remove nested sub-properties (buffer)
(tp-remove START END '(PROPERTY SUB-KEY (NESTED-KEYS...)) &optional OBJECT)

;; Remove from entire string
(tp-remove STRING PROP1 PROP2 ...)
(tp-remove STRING PROPERTY SUB-KEY)
(tp-remove STRING PROPERTY SUB-KEY '(NESTED-KEYS...))
```

**Examples:**

```elisp
;; Remove entire property
(tp-remove 1 10 'face)

;; Remove sub-property from face
(tp-remove 1 10 '(face :underline))

;; Remove specific nested keys, keep others
(tp-remove 1 10 '(face :underline (:style :position)))
;; Removes :style and :position from :underline
;; If :color exists in :underline, it's preserved

;; Remove from entire string
(tp-remove "Hello World" 'face 'help-echo)  ; Remove multiple properties
(tp-remove "Hello World" 'face :underline)  ; Remove sub-property
(tp-remove "Hello World" 'face :underline '(:style :position))  ; Remove nested
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

### Pattern Matching Functions

#### `tp-match-set` - Match String

```elisp
;; Buffer
(tp-match-set PATTERN '(PROPERTY VALUE ...))

;; String or Buffer object
(tp-match-set PATTERN OBJECT '(PROPERTY VALUE ...))

;; Pattern as (PATTERN STRING) format
(tp-match-set '(PATTERN STRING) '(PROPERTY VALUE ...))
```

Set properties on all occurrences of a string pattern.

**Examples:**

```elisp
;; In buffer - returns list of (START . END) pairs
(tp-match-set "TODO" '(face warning))
;; => ((10 . 14) (50 . 54) ...)

;; On string - returns modified string
(tp-match-set "o" "Hello World" '(face bold))
;; => #("Hello World" 4 5 (face bold) 7 8 (face bold))

;; Using (PATTERN STRING) format
(tp-match-set '("world" "Hello world") '(face bold))
;; => #("Hello world" 6 11 (face bold))
```

---

#### `tp-match-reset` - Match and Reset

Reset (completely replace) all properties on matches.

```elisp
(tp-match-reset PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

**Examples:**

```elisp
(tp-match-reset "TODO" '(face warning))
;; Replaces ALL properties on matched text
```

---

#### `tp-match-add` - Match and Add

Add/merge properties on matches with deep merge support.

```elisp
(tp-match-add PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

**Examples:**

```elisp
(tp-match-add "TODO" '(face (:underline t)))
;; Merges with existing properties
```

---

#### `tp-regexp-set` - Match Regexp

```elisp
;; Buffer
(tp-regexp-set PATTERN '(PROPERTY VALUE ...))

;; String or Buffer object
(tp-regexp-set PATTERN OBJECT '(PROPERTY VALUE ...))
```

Set properties on all matches of a regular expression.

**Examples:**

```elisp
;; Highlight all numbers in buffer
(tp-regexp-set "[0-9]+" '(face font-lock-number-face))

;; On string
(tp-regexp-set "[A-Z]+" "Hello WORLD" '(face bold))
;; => #("Hello WORLD" 6 11 (face bold))
```

---

#### `tp-regexp-reset` - Regexp and Reset

Reset (completely replace) all properties on regexp matches.

```elisp
(tp-regexp-reset PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

---

#### `tp-regexp-add` - Regexp and Add

Add/merge properties on regexp matches with deep merge support.

```elisp
(tp-regexp-add PATTERN '(PROPERTY VALUE ...) &optional OBJECT)
```

---

### Search & Navigation Functions

#### `tp-search-forward` / `tp-search-backward`

```elisp
(tp-search-forward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
(tp-search-backward PROPERTY &optional VALUE PREDICATE NOT-CURRENT)
```

Raw wrappers for Emacs's `text-property-search-forward` and `text-property-search-backward`.
These are low-level search functions that work directly with prop-match objects.

---

#### `tp-forward` / `tp-backward`

```elisp
(tp-forward PROPERTY &optional VALUE OBJECT N)
(tp-backward PROPERTY &optional VALUE OBJECT N)
```

Search forward/backward N times for text with PROPERTY.

- **N** is the number of searches, defaulting to 1.
- **VALUE** is the optional value to match.
- **OBJECT** can be a buffer or string; nil defaults to current buffer.
- For buffers, returns the prop-match object from the last successful search.
- For strings, returns a list of (START END VALUE) for all matches found.

**Examples:**

```elisp
;; Find next text with 'marker property
(tp-forward 'marker)

;; Find next text where 'type equals 'heading
(tp-forward 'type 'heading)

;; Search forward 3 times
(tp-forward 'marker nil nil 3)

;; Search in a string
(setq my-string (copy-sequence "Hello World Hello"))
(tp-set 0 5 '(marker t) my-string)
(tp-set 12 17 '(marker t) my-string)
(tp-forward 'marker nil my-string 2)
;; => ((0 5 t) (12 17 t))
```

---

#### `tp-forward-do` / `tp-backward-do`

```elisp
(tp-forward-do FUNCTION PROPERTY &optional VALUE OBJECT POINT N)
(tp-backward-do FUNCTION PROPERTY &optional VALUE OBJECT POINT N)
```

Search forward/backward N times for text with PROPERTY and apply FUNCTION to matched text.

- **FUNCTION** receives the matched text as its only argument.  The return value
  of FUNCTION replaces the matched text in the string or buffer.
- **N** is the number of searches, defaulting to 1.
- **OBJECT** can be a buffer or string; nil defaults to current buffer.
- **POINT** is the starting position for search; for buffers nil means current point,
  for strings nil means 0 (forward) or end of string (backward).
- Returns the number of successful matches.

**Examples:**

```elisp
;; Upcase matched text in buffer (starting from current point)
(tp-forward-do #'upcase 'marker nil nil nil 3)

;; Upcase matched text in string (starting from position 0)
(setq my-string (copy-sequence "hello world hello"))
(tp-set 0 5 '(marker t) my-string)
(tp-set 12 17 '(marker t) my-string)
(tp-forward-do #'upcase 'marker nil my-string nil 2)
;; my-string is now "HELLO world HELLO"

;; Start search from specific position
(setq my-string (copy-sequence "hello world hello"))
(tp-set 0 5 '(marker t) my-string)
(tp-set 12 17 '(marker t) my-string)
(tp-forward-do #'upcase 'marker nil my-string 6 2)
;; Only matches from position 6 onward are processed
;; my-string is now "hello world HELLO"

;; Custom transformation
(tp-forward-do
 (lambda (text)
   (concat "[" text "]"))
 'marker nil nil nil 3)
```

---

#### `tp-search` - Search All Matches

```elisp
;; Buffer/string region
(tp-search START END PROPERTY &optional VALUE OBJECT)

;; Entire string
(tp-search STRING PROPERTY &optional VALUE)
```

Search for all text with PROPERTY in a buffer/string range or entire string.

Returns a list of (START END VALUE) for all matching regions.

**Examples:**

```elisp
;; Find all 'marker properties in buffer range
(tp-search 1 100 'marker)
;; => ((5 10 t) (20 25 t) ...)

;; Find all 'type properties with value 'heading in string
(tp-search my-string 'type 'heading)
;; => ((0 10 heading) (50 60 heading) ...)

;; Filter by value
(tp-search 1 100 'type 'heading)
```

---

#### `tp-search-map` - Apply Function to Matched Text

```elisp
;; Buffer/string region
(tp-search-map FUNCTION START END PROPERTY &optional VALUE OBJECT)

;; Entire string
(tp-search-map FUNCTION STRING PROPERTY &optional VALUE)
```

Apply FUNCTION to matched text for all matches of PROPERTY.

- **FUNCTION** receives the matched text as its only argument.  The return value
  of FUNCTION replaces the matched text in the string or buffer.
- Returns the number of matches processed.

**Examples:**

```elisp
;; Upcase all markers in string
(tp-search-map #'upcase my-string 'marker)

;; Upcase all markers in buffer range
(tp-search-map #'upcase 1 100 'marker)

;; Custom transformation
(tp-search-map
 (lambda (text)
   (concat "[" text "]"))
 my-string 'marker)
```

---

## The Property Layer System

The **property layer system** is tp.el's innovative feature that allows stacking multiple sets of properties on the same text region. Only the **top layer** is visible, but lower layers are preserved and can be revealed through rotation or pinning.

### Property Layer Concept

```
┌─────────────────────────────┐
│   TOP LAYER (visible)       │  ← idx=0, What you see
├─────────────────────────────┤
│   Middle Layer (hidden)     │  ← idx=1, Preserved
├─────────────────────────────┤
│   Bottom Layer (hidden)     │  ← idx=-1, Preserved
└─────────────────────────────┘
```

### Property Layer Definition

#### `tp-define-layer` - Define Layer(s)

Define a single layer or a group of multiple layers.

**Single Layer:**

```elisp
(tp-define-layer layer-name
  (face (:background "cyan") line-prefix ">>"))
```

**Multiple Layers (Layer Group):**

```elisp
(tp-define-layer my-group
  layer-1                                    ; Reference existing layer
  (face (:background "red") line-prefix ">>")    ; Anonymous layer
  (face (:background "green" :weight bold)))     ; Another anonymous layer
```

The first layer in the definition is the top layer (visible by default).

**Examples:**

```elisp
;; Define individual layers
(tp-define-layer highlight
  (face (:background "yellow" :foreground "black")))

(tp-define-layer error
  (face (:background "red" :foreground "white")
   help-echo "Error!"))

(tp-define-layer info
  (face (:background "blue" :foreground "white")))

;; Define a layer group
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

### Property Layer Placement

#### `tp-put-layer` - Set Layer at Index

```elisp
;; Buffer/string region
(tp-put-layer START END LAYER IDX OBJECT)

;; Entire string
(tp-put-layer STRING LAYER IDX)
```

Set layer(s) at a specific index position in the layer stack.

- `IDX = 0`: Top (visible layer)
- `IDX = -1`: Bottom
- Other values insert at that position

**Examples:**

```elisp
(tp-define-layer base (face default))
(tp-define-layer highlight (face (:background "yellow")))

;; Put base layer at top
(tp-put-layer 1 10 'base 0)

;; Put highlight at index 1 (below top)
(tp-put-layer 1 10 'highlight 1)

;; Put layer at bottom
(tp-put-layer 1 10 'info -1)
```

---

#### `tp-push-layer` - Push Layer to Top

```elisp
;; Buffer/string region
(tp-push-layer START END LAYER OBJECT)

;; Entire string
(tp-push-layer STRING LAYER)
```

Push a layer to the top of the stack (equivalent to `tp-put-layer ... 0`).

**Examples:**

```elisp
(tp-define-layer base (face default))
(tp-define-layer highlight (face (:background "yellow")))

;; Push base layer first
(tp-push-layer 1 10 'base)

;; Push highlight on top (now visible)
(tp-push-layer 1 10 'highlight)
```

---

### Property Layer Deletion

#### `tp-delete-layer` - Delete Layer by Name/Index

```elisp
;; Buffer/string region
(tp-delete-layer START END LAYER-NAME/IDX OBJECT)

;; Entire string
(tp-delete-layer STRING LAYER-NAME/IDX)
```

Delete a layer from anywhere in the stack by name or index.

**Examples:**

```elisp
;; Remove by name
(tp-delete-layer 1 10 'highlight)

;; Remove top layer (idx=0)
(tp-delete-layer 1 10 0)

;; Remove bottom layer
(tp-delete-layer 1 10 -1)
```

---

#### `tp-pop-layer` - Pop Top Layer

```elisp
;; Buffer/string region
(tp-pop-layer START END OBJECT)

;; Entire string
(tp-pop-layer STRING)
```

Remove the top layer (equivalent to `tp-delete-layer ... 0`).

---

### Property Layer Movement

#### `tp-raise-layer` - Move Layer Up/Down

```elisp
;; Buffer/string region
(tp-raise-layer START END IDX/LAYER-NAME N OBJECT)

;; Entire string
(tp-raise-layer STRING IDX/LAYER-NAME N)
```

Raise a layer by N positions. Positive N moves toward top, negative moves toward bottom.

**Examples:**

```elisp
;; Move layer1 up by 2 positions
(tp-raise-layer 1 10 'layer1 2)

;; Move layer at idx 2 down by 1 position
(tp-raise-layer 1 10 2 -1)
```

---

#### `tp-rotate-layer` - Cycle Layers

```elisp
;; Buffer/string region
(tp-rotate-layer START END OBJECT)

;; Entire string
(tp-rotate-layer STRING)
```

Rotate layers - top goes to bottom, next becomes visible.

**Examples:**

```elisp
;; Stack: highlight (top) -> base (bottom)
(tp-rotate-layer 1 10)
;; Stack: base (top) -> highlight (bottom)
```

---

#### `tp-pin-layer` - Pin Layer to Top

```elisp
;; Buffer/string region
(tp-pin-layer START END IDX/LAYER-NAME OBJECT)

;; Entire string
(tp-pin-layer STRING IDX/LAYER-NAME)
```

Move a specific layer to the top (make it visible).

**Examples:**

```elisp
;; Make 'base the top layer
(tp-pin-layer 1 10 'base)
```

---

#### `tp-switch-layer` - Switch Two Layers

```elisp
;; Buffer/string region
(tp-switch-layer START END IDX1/NAME1 IDX2/NAME2 OBJECT)

;; Entire string
(tp-switch-layer STRING IDX1/NAME1 IDX2/NAME2)
```

Swap positions of two layers.

**Examples:**

```elisp
;; Switch layer1 and layer2
(tp-switch-layer 1 10 'layer1 'layer2)
```

---

### Property Layer Merging

#### `tp-merge-layers` - Merge Multiple Layers

```elisp
;; Buffer/string region
(tp-merge-layers START END NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...) OBJECT)

;; Entire string
(tp-merge-layers STRING NEW-LAYER-NAME '(IDX1 LAYER-NAME1 IDX2 ...))
```

Merge specified layers into a new layer. Earlier layers in the list take precedence.

**Examples:**

```elisp
;; Merge layer1 and layer2 into merged-layer
(tp-merge-layers 1 10 'merged-layer '(layer1 layer2))

;; Merge by index
(tp-merge-layers 1 10 'merged '(0 1 2))
```

---

#### `tp-flatten-layers` - Flatten All Layers

```elisp
;; Buffer/string region
(tp-flatten-layers START END NAME OBJECT)

;; Entire string
(tp-flatten-layers STRING NAME)
```

Flatten all layers into a single layer with the given name.

**Examples:**

```elisp
;; Flatten all layers into 'flat-layer
(tp-flatten-layers 1 10 'flat-layer)

;; Flatten with nil name (unnamed layer)
(tp-flatten-layers 1 10 nil)
```

---

### Property Layer Query Functions

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
(tp-define-layer code-base
  (face font-lock-keyword-face))

(tp-define-layer code-error
  (face (:underline (:color "red" :style wave))
   help-echo "Syntax error"))

(tp-define-layer code-debug
  (face (:background "dark blue")))

;; Apply base highlighting
(tp-push-layer 1 100 'code-base)

;; Add error highlight on problematic code
(tp-push-layer 50 60 'code-error)

;; Toggle between error and normal view
(defun toggle-error-view ()
  (interactive)
  (tp-rotate-layer 50 60))
```

### Status Indicator

```elisp
;; Define status layers as a group
(tp-define-layer status-todo (face (:foreground "gray")))
(tp-define-layer status-progress (face (:foreground "yellow")))
(tp-define-layer status-done (face (:foreground "green")))
(tp-define-layer task-status status-todo status-progress status-done)

;; Cycle through statuses
(defun cycle-task-status ()
  (interactive)
  (tp-rotate-layer (line-beginning-position) (line-end-position)))
```

### Temporary Highlights

```elisp
(tp-define-layer temp-highlight
  (face (:background "yellow")))

(defun flash-region (start end)
  "Flash a region temporarily."
  (tp-push-layer start end 'temp-highlight)
  (run-with-timer 0.5 nil
                  (lambda ()
                    (tp-delete-layer start end 'temp-highlight))))
```

---

## Aliases

For convenience, tp.el provides these aliases:

| Alias | Original Function |
|-------|-------------------|
| `tp-layer-properties` | `tp-layer-props` |
| `tp-layer-group-properties` | `tp-group-props` |
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
