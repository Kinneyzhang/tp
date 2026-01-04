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
  <a href="#reactive-text-properties">Reactive Text Properties</a> •
  <a href="README_CN.md">中文文档</a>
</p>

---

## Table of Contents

- [Overview](#overview)
  - [Core Innovations](#core-innovations)
- [Features](#features)
  - [Unified API Parameter Conventions](#unified-api-parameter-conventions)
  - [Three Property Operation Semantics](#three-property-operation-semantics)
  - [Fine-grained Sub-property Operations](#fine-grained-sub-property-operations)
  - [Innovative Property Layer System](#innovative-property-layer-system)
  - [Pattern Matching & Batch Operations](#pattern-matching--batch-operations)
  - [Reactive Text Properties](#-reactive-text-properties)
  - [Enhanced Search & Navigation](#enhanced-search--navigation)
- [Requirements](#requirements)
- [Installation](#installation)
- [API Reference](#api-reference)
  - [API Quick Reference](#api-quick-reference)
  - [Core Property Functions](#core-property-functions)
    - [tp-set](#tp-set---set-text-properties)
    - [tp-reset](#tp-reset---replace-all-properties)
    - [tp-add](#tp-add---addmerge-properties)
    - [tp-get](#tp-get---get-property-value)
    - [tp-at](#tp-at---get-property-at-position)
    - [tp-remove](#tp-remove---remove-property)
    - [tp-clear](#tp-clear---clear-all-properties)
  - [Pattern Matching Functions](#pattern-matching-functions)
    - [tp-match-set](#tp-match-set---match-string)
    - [tp-match-reset](#tp-match-reset---match-and-reset)
    - [tp-match-add](#tp-match-add---match-and-add)
    - [tp-regexp-set](#tp-regexp-set---match-regexp)
    - [tp-regexp-reset](#tp-regexp-reset---regexp-and-reset)
    - [tp-regexp-add](#tp-regexp-add---regexp-and-add)
  - [Search & Navigation Functions](#search--navigation-functions)
    - [tp-search-forward / tp-search-backward](#tp-search-forward--tp-search-backward)
    - [tp-forward / tp-backward](#tp-forward--tp-backward)
    - [tp-forward-do / tp-backward-do](#tp-forward-do--tp-backward-do)
    - [tp-search](#tp-search---search-all-matches)
    - [tp-search-map](#tp-search-map---apply-function-to-matched-text)
- [The Property Layer System](#the-property-layer-system)
  - [Custom Text Properties vs Text Property Layers](#custom-text-properties-vs-text-property-layers)
  - [Property Layer Concept](#property-layer-concept)
  - [Property Layer Definition](#property-layer-definition)
    - [define-tp / define-tps](#define-tp--define-tps---define-custom-text-properties)
    - [tp-layer-props / tp-group-props](#tp-layer-props--tp-group-props)
    - [tp-undefine-layer / tp-undefine-group](#tp-undefine-layer--tp-undefine-group)
    - [tp-layer-reset](#tp-layer-reset)
    - [tp-reactive-reset](#tp-reactive-reset)
  - [Property Layer Placement](#property-layer-placement)
    - [tp-put-layer](#tp-put-layer---set-layer-at-index)
    - [tp-push-layer](#tp-push-layer---push-layer-to-top)
  - [Property Layer Deletion](#property-layer-deletion)
    - [tp-delete-layer](#tp-delete-layer---delete-layer-by-nameindex)
    - [tp-pop-layer](#tp-pop-layer---pop-top-layer)
  - [Property Layer Movement](#property-layer-movement)
    - [tp-move-layer](#tp-move-layer---move-layer-to-position)
    - [tp-raise-layer](#tp-raise-layer---move-layer-updown)
    - [tp-rotate-layer](#tp-rotate-layer---cycle-layers)
    - [tp-pin-layer](#tp-pin-layer---pin-layer-to-top)
    - [tp-switch-layer](#tp-switch-layer---switch-two-layers)
  - [Property Layer Merging](#property-layer-merging)
    - [tp-merge-layers](#tp-merge-layers---merge-multiple-layers)
    - [tp-flatten-layers](#tp-flatten-layers---flatten-all-layers)
  - [Property Layer Query Functions](#property-layer-query-functions)
    - [tp-layer-list](#tp-layer-list---list-all-layers)
    - [tp-layer-count](#tp-layer-count)
    - [tp-layer-exists-p](#tp-layer-exists-p)
    - [tp-layer-top](#tp-layer-top)
    - [tp-add-to-layers](#tp-add-to-layers---add-properties-to-specific-layers)
    - [tp-add-to-all-layers](#tp-add-to-all-layers---add-properties-to-all-layers)
  - [Utility Functions](#utility-functions)
    - [tp-intervals](#tp-intervals---get-text-property-intervals)
    - [tp-intervals-map](#tp-intervals-map---apply-function-to-intervals)
    - [tp-plist](#tp-plist---get-all-properties-in-region)
    - [tp-empty-p](#tp-empty-p---check-if-object-has-properties)
    - [tp-region-layer-props](#tp-region-layer-props---get-layer-properties-in-region)
- [Reactive Text Properties](#reactive-text-properties)
  - [Core Concept](#core-concept)
  - [How It Works](#how-it-works)
  - [Defining Reactive Layers](#defining-reactive-layers)
  - [:data - Additional Reactive State](#data---additional-reactive-state)
  - [:compute - Computed Properties](#compute---computed-properties)
  - [:watch - Side Effect Callbacks](#watch---side-effect-callbacks)
  - [:transform - Value Transformation](#transform---value-transformation)
  - [Anonymous Reactive Layers](#anonymous-reactive-layers)
  - [Layer Name Resolution in APIs](#layer-name-resolution-in-apis)
  - [Reactive Layer Groups](#reactive-layer-groups)
  - [Batched Updates](#batched-updates)
  - [Debug Mode](#debug-mode)
  - [Resetting Reactive State](#resetting-reactive-state)
  - [Complete Example: Theme-Aware Text](#complete-example-theme-aware-text)
- [Practical Examples](#practical-examples)
  - [Syntax Highlighting with Multiple Layers](#syntax-highlighting-with-multiple-layers)
  - [Status Indicator](#status-indicator)
  - [Temporary Highlights](#temporary-highlights)
- [License](#license)
- [Contributing](#contributing)

---

## Overview

**tp.el** is a library that comprehensively enhances Emacs text property manipulation. It is not just a simple wrapper around native text property APIs (like `put-text-property`, `get-text-property`), but provides many **functional extensions that native functions do not have**. tp.el innovates in the following areas:

### Core Innovations

1. **Unified API Parameter Conventions**: All functions support multiple flexible calling patterns, working seamlessly with both strings and buffers
2. **Fine-grained Sub-property Operations**: Support path-style access, modification, and deep merging of nested properties
3. **Innovative Property Layer System**: Stack and manage multiple sets of properties on the same text region with layered control
4. **🆕 Reactive Text Properties**: Automatically update text properties when variable values change - a groundbreaking feature inspired by modern reactive UI frameworks
5. **Pattern Matching Batch Operations**: Batch apply properties via string or regular expression matching
6. **Enhanced Search & Navigation**: Rich property search and traversal functionality

## Features

### Unified API Parameter Conventions

Native Emacs APIs have different functions and parameter orders for strings and buffers. tp.el unifies all of this:

- ✅ **Three Calling Conventions**: All core functions (`tp-set`, `tp-get`, `tp-remove`, etc.) support three flexible calling patterns:
  ```elisp
  ;; 1. Current buffer
  (tp-set START END '(face bold))
  ;; 2. Specific buffer or string
  (tp-set START END '(face bold) OBJECT)
  ;; 3. Entire string (flat properties or layer name)
  (tp-set STRING 'face 'bold 'help-echo "tip")
  (tp-set STRING 'layer-name)
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
  (tp-remove 1 10 '(face :underline :style))
  ```
- ✅ **Deep Merge**: `tp-add` recursively merges nested plist structures
- ✅ **Smart Face Merging**: Symbol faces are automatically prepended to face lists, plist faces are deep merged
- ✅ **Automatic Duplicate Property Merging in Single Call**: When the same property (e.g., `face`) is specified multiple times in a single `tp-set`/`tp-add`/`tp-reset` call, they are automatically merged

```elisp
;; Merge multiple faces in a single call
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; Result: face is ((:background "green" :foreground "red") bold)

;; Later values override earlier ones for the same sub-property
(tp-set "emacs"
        'face '(:foreground "red")
        'face '(:foreground "yellow"))
;; Result: foreground is "yellow"

;; Use with tp-palette layer
(tp-set "emacs"
        'tp-palette 'info
        'face '(:foreground "red"))
;; Result: tp-palette's face is merged with (:foreground "red")
```

### Innovative Property Layer System

**This is tp.el's most innovative feature**, completely unsupported by native Emacs. The property layer system allows stacking multiple sets of properties on the same text region:

- ✅ **Property Layer Stack Concept**: Multiple property layers stack like a stack, only the top layer is visible, lower layers are preserved
- ✅ **Property Layer Definition & Reuse**: Define reusable property layers and layer groups via `define-tp` and `define-tps`
- ✅ **Rich Property Layer Operations**:
  - Placement: `tp-put-layer` (specific position), `tp-push-layer` (top)
  - Deletion: `tp-delete-layer` (by name/index), `tp-pop-layer` (top layer)
  - Movement: `tp-raise-layer` (up/down), `tp-rotate-layer` (rotate), `tp-pin-layer` (pin to top), `tp-switch-layer` (swap)
  - Merging: `tp-merge-layers` (merge specified layers), `tp-flatten-layers` (flatten all layers)
- ✅ **Property Layer Queries**: `tp-layer-list`, `tp-layer-count`, `tp-layer-exists-p`, `tp-layer-top`

```elisp
;; Property layer usage example
(define-tp highlight () '(face (:background "yellow")))
(define-tp error () '(face (:foreground "red")))

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

### 🆕 Reactive Text Properties

**This is tp.el's most innovative new feature** - reactive text properties automatically update when variable values change. Inspired by modern reactive UI frameworks like Vue.js, this feature brings reactive programming to Emacs text properties:

- ✅ **Reactive Variables**: Use `$`-prefixed symbols (like `$my-color`) in property definitions - they automatically resolve to variable values
- ✅ **Automatic Updates**: When a reactive variable changes, all text regions using that variable are automatically updated
- ✅ **:data for Additional State**: Define additional reactive variables that aren't directly used in properties but can trigger updates
- ✅ **:compute for Derived Values**: Create computed properties that derive their values from other reactive variables (like Vue's computed properties)
- ✅ **:watch for Side Effects**: Execute callbacks when reactive variables change (like Vue's watch)

```elisp
;; Define a layer with reactive properties
(defvar my-color "red")  ;; Reactive variable

(define-tp my-highlight ()
  :props '(face (:foreground $my-color)))

;; Apply the layer
(tp-push-layer 1 10 'my-highlight)

;; Later, just change the variable - text updates automatically!
(setq my-color "blue")  ;; All text with my-highlight layer updates to blue!

;; Advanced example with :data, :compute, and :watch
(define-tp full-name-layer ()
  :props '(help-echo $full-name face (:foreground $name-color))
  :data ((first-name . "John") (last-name . "Doe"))  ;; With initial values
  :compute ((full-name (lambda () (concat first-name " " last-name))))
  :watch ((first-name (lambda (new old layer)
                        (message "Name changed from %s to %s" old new)))))
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
| [`tp-forward-do`](#tp-forward-do--tp-backward-do) | Apply function to last match in forward search (with optional start/end range) |
| [`tp-backward-do`](#tp-forward-do--tp-backward-do) | Apply function to last match in backward search (with optional start/end range) |
| [`tp-search`](#tp-search---search-all-matches) | Search all matching properties in range or string |
| [`tp-search-map`](#tp-search-map---apply-function-to-matched-text) | Apply function to all matches (with optional start/end range) |

#### Property Layer Definition Functions
| Function | Description |
|----------|-------------|
| [`define-tp`](#define-tp--define-tps---define-custom-text-properties) | Define custom text property (layer) with optional parameter |
| [`define-tps`](#define-tp--define-tps---define-custom-text-properties) | Define custom text property group (layer group) with optional parameter |
| [`tp-layer-props`](#tp-layer-props--tp-group-props) | Get properties for a layer |
| [`tp-group-props`](#tp-layer-props--tp-group-props) | Get properties for all layers in a group |
| [`tp-undefine-layer`](#tp-undefine-layer--tp-undefine-group) | Remove layer definition |
| [`tp-undefine-group`](#tp-undefine-layer--tp-undefine-group) | Remove group definition |
| [`tp-layer-reset`](#tp-layer-reset) | Clear all layer/group definitions |
| [`tp-reactive-reset`](#tp-reactive-reset) | Clear all reactive dependencies and watchers |

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
| [`tp-move-layer`](#tp-move-layer---move-layer-to-position) | Move a layer from one position to another |
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
| [`tp-region-layer-props`](#tp-region-layer-props---get-layer-properties-in-region) | Get properties for a specific layer in region |

#### Property Layer Manipulation Functions
| Function | Description |
|----------|-------------|
| [`tp-add-to-layers`](#tp-add-to-layers---add-properties-to-specific-layers) | Add/merge properties to specific layers by index or name |
| [`tp-add-to-all-layers`](#tp-add-to-all-layers---add-properties-to-all-layers) | Add/merge properties to all existing layers |

#### Utility Functions
| Function | Description |
|----------|-------------|
| [`tp-intervals`](#tp-intervals---get-text-property-intervals) | Get all text property intervals in a region |
| [`tp-intervals-map`](#tp-intervals-map---apply-function-to-intervals) | Apply function to all intervals in a region |
| [`tp-plist`](#tp-plist---get-all-properties-in-region) | Get all properties present in a region |
| [`tp-empty-p`](#tp-empty-p---check-if-object-has-properties) | Check if object has no text properties |

---

### Core Property Functions

#### `tp-set` - Set Text Properties

Set text properties on a string or buffer region. Replaces only the specified properties, preserving others.

```elisp
;; Current buffer (properties as a list)
(tp-set START END '(PROPERTY VALUE ...))
(tp-set START END LAYER-NAME)

;; Specific buffer or string
(tp-set START END '(PROPERTY VALUE ...) OBJECT)
(tp-set START END LAYER-NAME OBJECT)

;; Entire string (flat properties or layer name)
(tp-set STRING PROPERTY VALUE ...)
(tp-set STRING LAYER-NAME)
```

LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.

**Examples:**

```elisp
;; Set face on buffer region
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold)))
;; => (1 . 10)

;; Set multiple properties
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "Click me")))
;; => (1 . 10)

;; Use a defined layer name
(define-tp warning-style ()
  '(face (:foreground "orange" :weight bold)))
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 'warning-style))
;; => (1 . 10)

;; Set on specific buffer
(let ((my-buffer (generate-new-buffer "*test*")))
  (with-current-buffer my-buffer
    (insert "Hello World"))
  (tp-set 1 10 '(face italic) my-buffer)
  (kill-buffer my-buffer))
;; => (1 . 10)

;; Set properties on a string (0-indexed)
(let ((my-string (tp-set 0 5 '(face italic) "Hello World")))
  my-string)
;; => #("Hello World" 0 5 (face italic))

;; Set properties on entire string
(tp-set "Hello" 'face 'bold 'mouse-face 'highlight)
;; => #("Hello" 0 5 (face bold mouse-face highlight))

;; Use a defined layer name on entire string
(define-tp my-style ()
  :props '(face (:foreground $my-color))
  :data ((my-color . "blue")))
(tp-set " " 'my-style)
;; => #(" " 0 1 (tp-name my-style face (:foreground "blue") ...))

;; Merge multiple faces in a single call (duplicate properties auto-merged)
(tp-set "emacs"
        'face 'bold
        'face '(:background "green")
        'face '(:foreground "red"))
;; => Three faces merged into one: ((:background "green" :foreground "red") bold)

;; Later values override earlier ones for the same sub-property
(tp-set "emacs"
        'face '(:foreground "red")
        'face '(:foreground "yellow"))
;; => face's :foreground is "yellow" (later overrides earlier)

;; Use with tp-palette layer, merging extra face properties
(tp-set "emacs"
        'tp-palette 'info
        'face '(:foreground "red"))
;; => tp-palette's face is merged with (:foreground "red"), :foreground is overridden
```

---

#### `tp-reset` - Replace All Properties

Completely replace ALL text properties with the specified ones.

```elisp
(tp-reset START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-reset START END LAYER-NAME &optional OBJECT)
(tp-reset STRING PROPERTY VALUE ...)
```

LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.

**Examples:**

```elisp
;; Replace all properties in region
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(help-echo "old"))  ; Set existing property
  (tp-reset 1 10 '(face bold))      ; Any existing properties are removed
  (tp-at 1))
;; => (face bold)  ; help-echo is gone

;; On string
(tp-reset "Hello" 'face 'italic)
;; => #("Hello" 0 5 (face italic))

;; Use a defined layer name
(define-tp error-style ()
  '(face (:foreground "red" :weight bold)))
(with-temp-buffer
  (insert "Hello World")
  (tp-reset 1 10 'error-style))
;; => (1 . 10)  ; All properties replaced with error-style
```

---

#### `tp-add` - Add/Merge Properties

Add or update properties with deep merge support for nested plists.

```elisp
(tp-add START END '(PROPERTY VALUE ...) &optional OBJECT)
(tp-add START END LAYER-NAME &optional OBJECT)
(tp-add STRING PROPERTY VALUE ...)
```

LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.

**Examples:**

```elisp
;; Add properties (preserves existing, merges nested)
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-add 1 10 '(help-echo "tooltip"))
  (tp-at 1))
;; => (face bold help-echo "tooltip")

;; Deep merge face properties
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red")))
  (tp-add 1 10 '(face (:background "blue")))
  (tp-at 1 'face))
;; => (:foreground "red" :background "blue")

;; Face prepending - symbol faces are prepended to face list
(let ((str (tp-set "Hello" 'face 'bold)))
  (tp-add str 'face 'shadow)
  (tp-at 0 'face str))
;; => (shadow bold)

;; Use a defined layer name
(define-tp highlight-style ()
  '(face (:background "yellow")))
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-add 1 10 'highlight-style)
  (tp-at 1))
;; => Properties merged with highlight-style
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
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-get 1 10 'face))
;; => ((1 6 bold))

;; Get with multiple intervals
(let ((str (copy-sequence "Hello World Hello")))
  (tp-set 0 5 '(face bold) str)
  (tp-set 12 17 '(face italic) str)
  (tp-get 0 17 'face str))
;; => ((0 5 bold) (12 17 italic))

;; Get with property path as list
(let ((my-string (copy-sequence "Hello World Hello World")))
  (tp-set 5 20 '(face (:underline (:style wave))) my-string)
  (tp-get 5 20 '(face :underline :style) my-string))
;; => ((5 20 wave))

;; Get deeply nested property from entire string
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 5 '(face (:underline (:color "green"))) str)
  (tp-set 6 11 '(face (:underline (:color "yellow"))) str)
  (tp-get str 'face :underline :color))
;; => ((0 5 "green") (6 11 "yellow"))

;; Get multiple keys from nested property
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 5 '(face (:underline (:color "green" :style wave))) str)
  (tp-set 6 11 '(face (:underline (:color "yellow" :style line))) str)
  (tp-get str 'face :underline '(:color :style)))
;; => ((0 5 (:color "green" :style wave)) (6 11 (:color "yellow" :style line)))

;; Get all properties from range
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold help-echo "test"))
  (tp-get 1 10))
;; => ((1 6 (face bold help-echo "test")))

;; Get from entire string - returns list of intervals
(let ((str (copy-sequence "Hello World Hello")))
  (tp-set 0 5 '(face bold) str)
  (tp-set 12 17 '(face italic) str)
  (list (tp-get str)              ; => ((0 5 (face bold)) (12 17 (face italic)))
        (tp-get str 'face)))      ; => ((0 5 bold) (12 17 italic))
;; => (((0 5 (face bold)) (12 17 (face italic))) ((0 5 bold) (12 17 italic)))
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
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "test"))
  (tp-at 5))
;; => (face bold help-echo "test")

;; Get all properties at position 0 in string
(let ((my-string (tp-set "Hello" 'face 'italic 'help-echo "greeting")))
  (tp-at 0 my-string))
;; => (face italic help-echo "greeting")

;; Get specific property at position
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-at 5 'face))
;; => bold

;; Get specific property at position in string
(let ((my-string (tp-set "Hello" 'face 'italic)))
  (tp-at 0 'face my-string))
;; => italic

;; Get nested sub-property at position
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red" :box (:color "blue"))))
  (list (tp-at 5 '(face :foreground))
        (tp-at 5 '(face :box :color))))
;; => ("red" "blue")

;; Get nested sub-property from string
(let ((str (copy-sequence "Hello")))
  (tp-set 0 5 '(face (:foreground "red" :underline t)) str)
  (tp-at 0 '(face :foreground) str))
;; => "red"
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
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold help-echo "test"))
  (tp-remove 1 10 'face)
  (tp-at 1))
;; => (help-echo "test")

;; Remove sub-property from face
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:foreground "red" :underline t)))
  (tp-remove 1 10 '(face :underline))
  (tp-at 1 'face))
;; => (:foreground "red")

;; Remove specific nested keys, keep others
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face (:underline (:style wave :position t :color "blue"))))
  (tp-remove 1 10 '(face :underline (:style :position)))
  (tp-at 1 '(face :underline)))
;; => (:color "blue")  ; :style and :position removed, :color preserved

;; Remove from entire string - multiple properties
(let ((str (tp-set "Hello World" 'face 'bold 'help-echo "tip")))
  (tp-remove str 'face 'help-echo)
  (tp-at 0 str))
;; => nil

;; Remove sub-property from string
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 11 '(face (:foreground "red" :underline t)) str)
  (tp-remove str 'face :underline)
  (tp-at 0 'face str))
;; => (:foreground "red")

;; Remove nested keys from string
(let ((str (copy-sequence "Hello World")))
  (tp-set 0 11 '(face (:underline (:style wave :color "blue"))) str)
  (tp-remove str 'face :underline '(:style))
  (tp-at 0 '(face :underline) str))
;; => (:color "blue")
```

---

#### `tp-clear` - Clear All Properties

```elisp
(tp-clear &optional START END OBJECT)
```

Clear all text properties from a region.

**Examples:**

```elisp
;; Clear region
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 10 '(face bold))
  (tp-clear 1 10)
  (tp-at 1))
;; => nil

;; Clear entire buffer
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 12 '(face bold))
  (tp-clear)
  (tp-at 5))
;; => nil
```

---

### Pattern Matching Functions

#### `tp-match-set` - Match String

```elisp
(tp-match-set PATTERN PLIST &optional OBJECT)
(tp-match-set PATTERN LAYER-NAME &optional OBJECT)
```

Set properties on all occurrences of a string pattern.
PATTERN can be a string (single pattern) or a list of strings (multiple patterns).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

**Examples:**

```elisp
;; In buffer - returns list of (START . END) pairs
(with-temp-buffer
  (insert "TODO: fix this. TODO: also this.")
  (tp-match-set "TODO" '(face warning)))
;; => ((1 . 5) (17 . 21))

;; On string - returns modified string
(tp-match-set "o" '(face bold) "Hello World")
;; => #("Hello World" 4 5 (face bold) 7 8 (face bold))

;; Multiple patterns - match both "world" and "Hello"
(with-temp-buffer
  (insert "Hello world, Hello again")
  (tp-match-set '("world" "Hello") '(face bold)))
;; => ((1 . 6) (7 . 12) (14 . 19))  ; Matches "Hello", "world", "Hello"

;; Multiple patterns on string
(tp-match-set '("Hello" "world") '(face bold) "Hello world")
;; => #("Hello world" 0 5 (face bold) 6 11 (face bold))

;; Use a defined layer name
(define-tp todo-style ()
  '(face (:foreground "orange" :weight bold)))
(with-temp-buffer
  (insert "TODO: fix this. TODO: also this.")
  (tp-match-set "TODO" 'todo-style))
;; => ((1 . 5) (17 . 21))
```

---

#### `tp-match-reset` - Match and Reset

Reset (completely replace) all properties on matches.
PATTERN can be a string or list of strings (multiple patterns).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

```elisp
(tp-match-reset PATTERN PLIST &optional OBJECT)
(tp-match-reset PATTERN LAYER-NAME &optional OBJECT)
```

**Examples:**

```elisp
;; Replaces ALL properties on matched text
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-set 1 5 '(help-echo "original"))  ; Set existing property
  (tp-match-reset "TODO" '(face warning))
  (tp-at 1))
;; => (face warning)  ; help-echo is removed

;; Multiple patterns
(with-temp-buffer
  (insert "TODO: fix. FIXME: also fix.")
  (tp-match-reset '("TODO" "FIXME") '(face warning)))
;; => ((1 . 5) (12 . 17))

;; Use a defined layer name
(define-tp alert-style ()
  '(face (:background "red" :foreground "white")))
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-match-reset "TODO" 'alert-style))
;; => ((1 . 5))
```

---

#### `tp-match-add` - Match and Add

Add/merge properties on matches with deep merge support.
PATTERN can be a string or list of strings (multiple patterns).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

```elisp
(tp-match-add PATTERN PLIST &optional OBJECT)
(tp-match-add PATTERN LAYER-NAME &optional OBJECT)
```

**Examples:**

```elisp
;; Merges with existing properties
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-set 1 5 '(help-echo "important"))
  (tp-match-add "TODO" '(face (:underline t)))
  (tp-at 1))
;; => (face (:underline t) help-echo "important")

;; Multiple patterns
(with-temp-buffer
  (insert "TODO: fix. FIXME: also fix.")
  (tp-match-add '("TODO" "FIXME") '(face (:underline t))))
;; => ((1 . 5) (12 . 17))

;; Use a defined layer name
(define-tp underline-style ()
  '(face (:underline (:color "blue" :style wave))))
(with-temp-buffer
  (insert "TODO: fix this")
  (tp-match-add "TODO" 'underline-style))
;; => ((1 . 5))
```

---

#### `tp-regexp-set` - Match Regexp

```elisp
(tp-regexp-set PATTERN PLIST &optional OBJECT)
(tp-regexp-set PATTERN LAYER-NAME &optional OBJECT)
```

Set properties on all matches of a regular expression.
PATTERN can be a string (single regexp) or a list of strings (multiple regexps).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

**Examples:**

```elisp
;; Highlight all numbers in buffer
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" '(face font-lock-number-face))
  (list (tp-at 5 'face) (tp-at 13 'face)))
;; => (font-lock-number-face font-lock-number-face)

;; On string
(tp-regexp-set "[A-Z]+" '(face bold) "Hello WORLD")
;; => #("Hello WORLD" 6 11 (face bold))

;; Multiple regexps - match both numbers and uppercase letters
(tp-regexp-set '("[0-9]+" "[A-Z]+") '(face bold) "abc 123 XYZ")
;; => #("abc 123 XYZ" 4 7 (face bold) 8 11 (face bold))

;; Use a defined layer name
(define-tp number-style ()
  '(face (:foreground "green")))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-set "[0-9]+" 'number-style))
;; => ((5 . 8) (13 . 16))
```

---

#### `tp-regexp-reset` - Regexp and Reset

Reset (completely replace) all properties on regexp matches.
PATTERN can be a string or list of strings (multiple regexps).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

```elisp
(tp-regexp-reset PATTERN PLIST &optional OBJECT)
(tp-regexp-reset PATTERN LAYER-NAME &optional OBJECT)
```

**Examples:**

```elisp
;; Reset all properties on regexp matches
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "original"))
  (tp-regexp-reset "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold)  ; help-echo is removed

;; On string
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "original") str)
  (tp-regexp-reset "[0-9]+" '(face italic) str)
  (tp-at 4 str))
;; => (face italic)

;; Use a defined layer name
(define-tp code-number ()
  '(face (:foreground "cyan")))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-reset "[0-9]+" 'code-number))
;; => ((5 . 8) (13 . 16))
```

---

#### `tp-regexp-add` - Regexp and Add

Add/merge properties on regexp matches with deep merge support.
PATTERN can be a string or list of strings (multiple regexps).
PLIST is a property list like `'(face bold help-echo "tip")`.
LAYER-NAME can be a symbol representing a layer defined by `define-tp` or a group defined by `define-tps`.
OBJECT is a buffer or string; nil means current buffer.

```elisp
(tp-regexp-add PATTERN PLIST &optional OBJECT)
(tp-regexp-add PATTERN LAYER-NAME &optional OBJECT)
```

**Examples:**

```elisp
;; Add properties to regexp matches (preserves existing)
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-set 5 8 '(help-echo "number"))
  (tp-regexp-add "[0-9]+" '(face bold))
  (tp-at 5))
;; => (face bold help-echo "number")

;; On string
(let ((str (copy-sequence "abc 123 def")))
  (tp-set 4 7 '(help-echo "number") str)
  (tp-regexp-add "[0-9]+" '(face italic) str)
  (tp-at 4 str))
;; => (face italic help-echo "number")

;; Use a defined layer name
(define-tp bold-underline ()
  '(face (:weight bold :underline t)))
(with-temp-buffer
  (insert "abc 123 def 456")
  (tp-regexp-add "[0-9]+" 'bold-underline))
;; => ((5 . 8) (13 . 16))
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
(with-temp-buffer
  (insert "Hello World Test")
  (tp-set 7 12 '(marker t))
  (goto-char 1)
  (let ((match (tp-forward 'marker)))
    (when match
      (prop-match-beginning match))))
;; => 7

;; Find next text where 'type equals 'heading
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(type heading))
  (goto-char 1)
  (let ((match (tp-forward 'type 'heading)))
    (when match
      (prop-match-value match))))
;; => heading

;; Search in a string
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

Search forward/backward for text with PROPERTY and apply FUNCTION **only to the last match**.

- **FUNCTION** receives `(TEXT &optional START END)` where TEXT is the matched text, START and END are the positions of the match. The return value of FUNCTION replaces the matched text in the string or buffer.
- **PROPERTY** is the text property to search for.
- **VALUE** is the optional value to match; nil means search for PROPERTY without matching value.
- **OBJECT** can be a buffer or string; nil defaults to current buffer.
- **TIMES** is the number of searches, defaulting to 1. The function searches TIMES times but only applies FUNCTION to the last (Nth) match found.
- **START** and **END** define the search range; defaults are object start and end.
- Returns the number of successful matches.

**Examples:**

```elisp
;; Upcase only the last (2nd) match in string
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward-do #'upcase 'marker nil my-string 2)
  my-string)
;; => "hello world HELLO"  ; Only the 2nd match is upcased

;; Search within a range (only matches in range 6-17)
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-forward-do #'upcase 'marker nil my-string 2 6 17)
  my-string)
;; => "hello world HELLO"  ; Only 1 match in range 6-17

;; Using function with start and end parameters
;; The function receives position info; use upcase to keep same length
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
;; => ("hello world HELLO" (12 17))  ; Only the last match is transformed

;; Backward search - upcase only the last (2nd) match
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-backward-do #'upcase 'marker nil my-string 2)
  my-string)
;; => "HELLO world hello"  ; The first match (last when searching backward) is upcased
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
(with-temp-buffer
  (insert "Hello World Test Again")
  (tp-set 1 6 '(marker t))
  (tp-set 13 17 '(marker t))
  (tp-search 1 22 'marker))
;; => ((1 6 t) (13 17 t))

;; Find all 'type properties with value 'heading in string
(let ((my-string (copy-sequence "Title Here Body Text")))
  (tp-set 0 10 '(type heading) my-string)
  (tp-search my-string 'type 'heading))
;; => ((0 10 heading))

;; Filter by value
(with-temp-buffer
  (insert "Heading1 Body Heading2")
  (tp-set 1 9 '(type heading))
  (tp-set 10 14 '(type body))
  (tp-set 15 23 '(type heading))
  (tp-search 1 23 'type 'heading))
;; => ((1 9 heading) (15 23 heading))
```

---

#### `tp-search-map` - Apply Function to Matched Text

```elisp
(tp-search-map FUNCTION PROPERTY &optional VALUE OBJECT START END)
```

Apply FUNCTION to all matches of PROPERTY in OBJECT.

- **FUNCTION** receives `(TEXT &optional START END IDX)` where:
  - TEXT is the matched text
  - START and END are the positions of the match
  - IDX is the 0-based index of the current match
  The return value of FUNCTION replaces the matched text in the string or buffer.
- **PROPERTY** is the text property to search for.
- **VALUE** is the optional value to match; nil means search for PROPERTY without matching value.
- **OBJECT** can be a buffer or string; nil defaults to current buffer.
- **START** and **END** define the search range; defaults are object start and end.
- Returns the number of matches processed.

**Examples:**

```elisp
;; Upcase all markers in string
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string)
  my-string)
;; => "HELLO world HELLO"

;; Search only in a range
(let ((my-string (copy-sequence "hello world hello")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-set 12 17 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string 0 10)
  my-string)
;; => "HELLO world hello"  ; Only first match in range 0-10

;; Custom transformation with start, end, and index
;; The function receives position info; use upcase to keep same length
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

;; Custom transformation without optional parameters
(let ((my-string (copy-sequence "hello world")))
  (tp-set 0 5 '(marker t) my-string)
  (tp-search-map #'upcase 'marker nil my-string)
  my-string)
;; => "HELLO world"
```

---

## The Property Layer System

The **property layer system** is tp.el's innovative feature that allows stacking multiple sets of properties on the same text region. Only the **top layer** is visible, but lower layers are preserved and can be revealed through rotation or pinning.

### Custom Text Properties vs Text Property Layers

tp.el unifies the concepts of "custom text properties" and "text property layers":

#### Concept Distinction

1. **Custom Text Properties**: Defined using `define-tp`, when set using `tp-set`/`tp-reset`/`tp-add`, they are treated as regular text properties that can be mixed with built-in Emacs text properties (like `face`, `display`, etc.).

2. **Text Property Layers**: Also defined using `define-tp`, but when set using `tp-put-layer`/`tp-push-layer`, layer-related properties are introduced (`tp-name`, `tp-layers`), enabling layer stacking and operations.

3. **Custom Text Property Groups**: Defined using `define-tps`, they contain multiple related text properties that can be used individually or as a group.

#### Definition and Usage

```elisp
;; Define custom text property (layer)
(define-tp tp-highlight ()
  '(face (:background "yellow")))

;; Use as regular text property (no tp-name or layer attributes)
(tp-set 1 10 '(tp-highlight t))
;; Result: Only face property, no tp-name

;; Use as text property layer (introduces layer-related properties)
(tp-push-layer 1 10 'tp-highlight)
;; Result: Both face and tp-name properties, supports layer operations
```

#### When to Use Which

- **`tp-set`/`tp-reset`/`tp-add`**: When you only need to set text properties without layer stacking functionality. Suitable for simple property setting scenarios.

- **`tp-push-layer`/`tp-put-layer`**: When you need to stack multiple sets of properties on the same text region and perform layer operations like rotation, deletion, etc.

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

#### `define-tp` / `define-tps` - Define Custom Text Properties

##### `define-tp` - Define Single Custom Text Property (Layer)

Define a custom text property. The name does not need to be quoted. Supports three formats:

**Format 1 - Non-parameterized (empty argument list, simple properties):**

```elisp
(define-tp tp-bold ()
  '(face bold))

;; Usage:
(tp-set "emacs" 'tp-bold t)
(tp-set 0 5 '(tp-bold t) "emacs")
```

**Format 2 - Parameterized (with single argument):**

```elisp
(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

;; Usage:
(tp-set "emacs" 'tp-space 2)
(tp-set 0 5 '(tp-space 2) "emacs")
```

**Format 3 - With reactive features (:props, :data, :compute, :watch, :transform):**

```elisp
(define-tp my-reactive-layer ()
  :props '(face (:foreground $my-color))
  :data '((my-color . "red"))
  :compute '((full-name (lambda () (concat first-name " " last-name))))
  :watch '((my-color (lambda (new old layer) (message "Color changed!"))))
  :transform (lambda (text) (upcase text)))

;; Usage:
(tp-push-layer 1 10 'my-reactive-layer)
;; Changing the variable automatically updates the text
(setq my-color "blue")
```

**Reactive Keywords:**

- **:props** - Property list where `$`-prefixed symbols are reactive variables
- **:data** - Additional reactive variables (can include initial values)
- **:compute** - Computed properties that derive values from other variables
- **:watch** - Watchers that execute callbacks when variables change
- **:transform** - Transform function to process `tp-text` values before display

##### `define-tps` - Define Custom Text Property Group (Layer Group)

Define multiple related custom text properties. The name does not need to be quoted. Properties in the group can be used individually or with the group name to set multiple layers.

**Format 1 - Non-parameterized (empty argument list):**

```elisp
(define-tps tp-moon-phases ()
  '(display "🌑")
  '(display "🌕"))

;; Usage:
(tp-set 1 6 'tp-moon-phases)
```

**Supported layer definition formats within the group:**

1. **Anonymous layers** (named as NAME-0, NAME-1, etc.):
   ```elisp
   '(face (:background "yellow"))
   ```

2. **Named layers with cons-cell** (named as NAME-suffix):
   ```elisp
   '("highlight" . (face (:background "yellow")))
   ```

3. **Named layers with :props keyword**:
   ```elisp
   '("highlight" :props (face (:background "yellow")))
   ```

4. **Named layers with reactive features** (:props, :data, :watch, :compute):
   ```elisp
   '("reactive" :props (face (:foreground $my-color))
                :data ((my-color . "red"))
                :watch ((my-color (lambda (new old layer) (message "Changed!")))))
   ```

**Examples:**

```elisp
;; Define non-parameterized custom text property
(define-tp tp-highlight ()
  '(face (:background "yellow")))

;; Define parameterized custom text property
(define-tp tp-color (color)
  `(face (:foreground ,color)))

;; Define property group
(define-tps tp-status ()
  '("success" . (face (:foreground "green")))
  '("warning" . (face (:foreground "orange")))
  '("error" . (face (:foreground "red"))))

;; Use custom text properties
(tp-set "Hello" 'tp-highlight t)        ; Non-parameterized
(tp-set "Hello" 'tp-color "blue")       ; Parameterized
(tp-set 1 6 'tp-status)                 ; Use layer group

;; Use as layers (supports stacking operations)
(tp-push-layer 1 10 'tp-highlight)
```

The first layer in the definition is the top layer (visible by default).

**More Examples:**

```elisp
;; Define status layers, then group them
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

;; Define a layer group with named layers
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
```

---

#### `tp-layer-props` / `tp-group-props`

```elisp
(tp-layer-props LAYER-NAME)
(tp-group-props GROUP-NAME)
```

Get properties for a layer or all layers in a group.

**Examples:**

```elisp
;; Get layer properties
(progn
  (setq tp-layer-alist nil)
  (define-tp my-layer ()
    '(face bold help-echo "tip"))
  (tp-layer-props 'my-layer))
;; => (face bold help-echo "tip" tp-name my-layer)

;; Get group properties
(progn
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
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

Remove layer or group definition.

**Examples:**

```elisp
;; Undefine a layer
(progn
  (setq tp-layer-alist nil)
  (define-tp temp-layer () '(face bold))
  (tp-undefine-layer 'temp-layer)
  (tp-layer-props 'temp-layer))
;; => nil

;; Undefine a group
(progn
  (setq tp-layer-alist nil)
  (setq tp-layer-groups nil)
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

Clear all layer and group definitions, including all reactive dependencies and watchers.

**Examples:**

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

Clear all reactive text property watchers and dependencies, without affecting layer definitions.

This is useful when you want to remove all reactive bindings but keep the layer definitions intact.

**Examples:**

```elisp
;; Define a reactive layer
(progn
  (defvar my-reactive-color "red")
  (define-tp reactive-layer ()
  :props '(face (:foreground $my-reactive-color)))
  ;; Clear reactive bindings only
  (tp-reactive-reset)
  ;; Layer still exists, but changing my-reactive-color no longer updates it
  (tp-layer-props 'reactive-layer))
;; => (face (:foreground "red") tp-name reactive-layer)
```

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
;; Put base layer at top
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 'base 0)
    (tp-at 1 'tp-name)))
;; => base

;; Put highlight at index 1 (below top)
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

;; Put layer at bottom
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp info () '(face (:foreground "blue")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-put-layer 1 10 'base 0)
    (tp-put-layer 1 10 'info -1)
    (tp-layer-top 1 10)))
;; => base  ; info is at bottom, base is visible
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
;; Push base layer first
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-at 1 'tp-name)))
;; => base

;; Push highlight on top (now visible)
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

;; Remove top layer (idx=0)
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

;; Remove bottom layer
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

#### `tp-pop-layer` - Pop Top Layer

```elisp
;; Buffer/string region
(tp-pop-layer START END OBJECT)

;; Entire string
(tp-pop-layer STRING)
```

Remove the top layer (equivalent to `tp-delete-layer ... 0`).

**Examples:**

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

### Property Layer Movement

#### `tp-move-layer` - Move Layer to Position

```elisp
;; Buffer/string region
(tp-move-layer START END FROM-ID TO-IDX OBJECT)

;; Entire string
(tp-move-layer STRING FROM-ID TO-IDX)
```

Move a layer from one position to another in the layer stack.

- `FROM-ID` identifies the layer to move: an integer index or a layer name symbol
- `TO-IDX` is the target position (integer index)
- Index 0 means top (visible), -1 means bottom
- Both indices refer to positions before the move

This is the generic layer movement function used internally by `tp-raise-layer`, `tp-rotate-layer`, `tp-pin-layer`, and `tp-switch-layer`.

**Examples:**

```elisp
;; Move layer at index 2 to index 0 (top)
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
    ;; Stack: layer3 (0), layer2 (1), layer1 (2)
    (tp-move-layer 1 10 2 0)
    (tp-layer-top 1 10)))
;; => layer1

;; Move layer by name to bottom
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; Stack: layer2 (top), layer1 (bottom)
    (tp-move-layer 1 10 'layer2 -1)
    (tp-layer-top 1 10)))
;; => layer1

;; Move on string
(let ((str (copy-sequence "Hello")))
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (tp-push-layer str 'layer1)
  (tp-push-layer str 'layer2)
  ;; layer2 is on top
  (tp-move-layer str 'layer1 0)
  (tp-at 0 'tp-name str))
;; => layer1
```

---

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
;; Move layer1 up by 2 positions (to top)
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
    ;; Stack: layer3 (top), layer2, layer1 (bottom)
    (tp-raise-layer 1 10 'layer1 2)
    (tp-layer-top 1 10)))
;; => layer1

;; Move layer at idx 0 down by 1 position
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; Stack: layer2 (idx 0), layer1 (idx 1)
    (tp-raise-layer 1 10 0 -1)
    (tp-layer-top 1 10)))
;; => layer1
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
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    ;; Stack: highlight (top) -> base (bottom)
    (tp-rotate-layer 1 10)
    ;; Stack: base (top) -> highlight (bottom)
    (tp-layer-top 1 10)))
;; => base
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
(progn
  (tp-layer-reset)
  (define-tp base () '(face default))
  (define-tp highlight () '(face (:background "yellow")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'base)
    (tp-push-layer 1 10 'highlight)
    ;; highlight is on top
    (tp-pin-layer 1 10 'base)
    (tp-layer-top 1 10)))
;; => base
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
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; layer2 is on top
    (tp-switch-layer 1 10 'layer1 'layer2)
    ;; Now layer1 is on top
    (tp-layer-top 1 10)))
;; => layer1
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

;; Merge by index
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

;; Flatten with nil name (unnamed layer)
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

### Property Layer Query Functions

#### `tp-layer-list` - List All Layers

```elisp
(tp-layer-list START END &optional OBJECT)
```

Get list of all layer names in region.

**Examples:**

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

Count layers in region.

**Examples:**

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

Check if layer exists in region.

**Examples:**

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

Get name of the top (visible) layer.

**Examples:**

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

#### `tp-add-to-layers` - Add Properties to Specific Layers

```elisp
;; Buffer/string region
(tp-add-to-layers IDX-OR-LAYER-NAME-LIST START END PLIST &optional OBJECT)

;; Entire string
(tp-add-to-layers IDX-OR-LAYER-NAME-LIST STRING PROP VAL ...)
```

Add or merge properties to specific layers in a region or string.

- **IDX-OR-LAYER-NAME-LIST** is a list of layer indices (integers) or layer names (symbols). For indices: 0 means top layer, -1 means bottom layer.
- Properties are deeply merged into the specified layers (nested plists are merged, not replaced).
- OBJECT defaults to current buffer for region form.
- Returns the modified string or nil for buffer operations.

**Examples:**

```elisp
(progn
  (tp-layer-reset)
  (define-tp layer1 () '(face (:foreground "red")))
  (define-tp layer2 () '(face (:foreground "blue")))
  (with-temp-buffer
    (insert "Hello World")
    (tp-push-layer 1 10 'layer1)
    (tp-push-layer 1 10 'layer2)
    ;; Add underline to both layers
    (tp-add-to-layers '(0 1) 1 10 '(face (:underline t)))
    (tp-at 5)))
;; Both layers now have underline merged with their colors
```

---

#### `tp-add-to-all-layers` - Add Properties to All Layers

```elisp
;; Buffer/string region
(tp-add-to-all-layers START END PLIST &optional OBJECT)

;; Entire string
(tp-add-to-all-layers STRING PROP VAL ...)
```

Add or merge properties to all layers in a region or string.

- Properties are deeply merged into all existing layers.
- OBJECT defaults to current buffer for region form.
- Returns the modified string or nil for buffer operations.

**Examples:**

```elisp
(let ((str (copy-sequence "Hello World")))
  (define-tp layer1 () '(face bold))
  (define-tp layer2 () '(face italic))
  (tp-push-layer 0 5 'layer1 str)
  (tp-push-layer 0 5 'layer2 str)
  ;; Add underline to all layers
  (tp-add-to-all-layers 0 5 '(face (:underline t)) str)
  str)
```

---

#### `tp-intervals` - Get Text Property Intervals

```elisp
(tp-intervals START END &optional OBJECT)
```

Get all text property intervals from START to END in OBJECT.

- Returns a list of (START END PROPERTIES) for each interval.
- Uses `object-intervals` (requires Emacs 28.1+).
- OBJECT can be a buffer or string; nil defaults to current buffer.

**Examples:**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold))
  (tp-set 7 12 '(face italic))
  (tp-intervals 1 12))
;; => ((0 5 (face bold)) (6 11 (face italic)))
```

---

#### `tp-intervals-map` - Apply Function to Intervals

```elisp
(tp-intervals-map FUNCTION START END &optional OBJECT)
```

Apply FUNCTION to all intervals between START and END in OBJECT.

- FUNCTION receives four arguments: interval-start, interval-end, top-props (visible layer properties), and below-props-lst (list of hidden layers).
- OBJECT can be a buffer or string; nil defaults to current buffer.
- Returns list of function results (nil values are removed).

**Examples:**

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

#### `tp-region-layer-props` - Get Layer Properties in Region

```elisp
(tp-region-layer-props START END LAYER-NAME &optional OBJECT)
```

Return layer properties for LAYER-NAME in region from START to END.

- Returns a list of (START END PROPERTIES) for matching intervals.
- OBJECT defaults to current buffer.

**Examples:**

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

#### `tp-plist` - Get All Properties in Region

```elisp
;; Buffer/string region
(tp-plist START END &optional OBJECT)

;; Entire string
(tp-plist STRING)
```

Get a property list of all properties present in a region or string.

- Returns a plist containing all properties found in the range.
- OBJECT defaults to current buffer for region form.

**Examples:**

```elisp
(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 6 '(face bold help-echo "Tip"))
  (tp-set 7 12 '(face italic))
  (tp-plist 1 12))
;; => (face bold help-echo "Tip" face italic)
```

---

#### `tp-empty-p` - Check if Object Has Properties

```elisp
(tp-empty-p &optional OBJECT)
```

Return t if OBJECT has no text properties.

- OBJECT can be a string or buffer; nil defaults to current buffer.
- Uses `object-intervals` (requires Emacs 28.1+).

**Examples:**

```elisp
(tp-empty-p "plain text")  ; => t
(let ((str (copy-sequence "text")))
  (tp-set str 'face 'bold)
  (tp-empty-p str))  ; => nil
```

---

## Practical Examples

### Syntax Highlighting with Multiple Layers

```elisp
;; Complete example that can be run in a buffer
(progn
  (tp-layer-reset)
  ;; Define layers for different highlighting purposes
  (define-tp code-base ()
  '(face font-lock-keyword-face))
  (define-tp code-error ()
  '(face (:underline (:color "red" :style wave))
     help-echo "Syntax error"))
  (define-tp code-debug ()
  '(face (:background "dark blue")))
  (with-temp-buffer
    (insert (make-string 100 ?x))  ; Create 100-char buffer
    ;; Apply base highlighting
    (tp-push-layer 1 100 'code-base)
    ;; Add error highlight on problematic code
    (tp-push-layer 50 60 'code-error)
    ;; Check the top layer at position 55
    (tp-layer-top 50 60)))
;; => code-error

;; Toggle function (for use in real buffers)
(defun toggle-error-view (start end)
  "Toggle between error and normal view."
  (interactive "r")
  (tp-rotate-layer start end))
```

### Status Indicator

```elisp
;; Complete example with layer group
(progn
  (tp-layer-reset)
  ;; Define status layers as a group
  (define-tp status-todo () '(face (:foreground "gray")))
  (define-tp status-progress () '(face (:foreground "yellow")))
  (define-tp status-done () '(face (:foreground "green")))
  (define-tps task-status 'status-todo 'status-progress 'status-done)
  ;; Check group is defined
  (length (tp-group-props 'task-status)))
;; => 3

;; Cycle through statuses (for use in real buffers)
(defun cycle-task-status ()
  "Cycle through task status layers on current line."
  (interactive)
  (tp-rotate-layer (line-beginning-position) (line-end-position)))
```

### Temporary Highlights

```elisp
;; Define temporary highlight layer
(progn
  (tp-layer-reset)
  (define-tp temp-highlight ()
  '(face (:background "yellow")))
  (tp-layer-props 'temp-highlight))
;; => (face (:background "yellow") tp-name temp-highlight)

;; Flash function (for use in real buffers)
(defun flash-region (start end)
  "Flash a region temporarily."
  (tp-push-layer start end 'temp-highlight)
  (run-with-timer 0.5 nil
                  (lambda (s e)
                    (tp-delete-layer s e 'temp-highlight))
                  start end))
```

---

## Reactive Text Properties

> 📖 **For a comprehensive guide with detailed examples, see [Reactive Text Properties Complete Guide](docs/reactive-text-properties-en.md)**
>
> 📖 **For advanced optimization features, see [Reactive System Optimization](docs/reactive-optimization-en.md)**

**Reactive Text Properties** is tp.el's groundbreaking innovation that brings reactive programming paradigms to Emacs text properties. Inspired by modern frontend frameworks like Vue.js, this feature enables text properties to automatically update when underlying variable values change.

### Core Concept

Traditional text property manipulation requires manually updating all affected text regions whenever you want to change a property value. With reactive text properties, you simply define a variable relationship once, and tp.el handles all updates automatically:

```elisp
;; Traditional approach (manual updates required)
(defvar my-color "red")
(tp-set 1 10 '(face (:foreground "red")))
;; To change color, you must manually update every region:
(setq my-color "blue")
(tp-set 1 10 '(face (:foreground "blue")))  ; Manual!

;; Reactive approach (automatic updates)
(defvar my-color "red")
(define-tp my-layer ()
  :props '(face (:foreground $my-color)))
(tp-push-layer 1 10 'my-layer)
;; Just change the variable - all text updates automatically!
(setq my-color "blue")  ; All regions with my-layer update instantly!
```

### How It Works

1. **Reactive Variables**: Any symbol prefixed with `$` in `:props` is treated as a reactive variable. The `$` is stripped to get the actual variable name.

2. **Variable Watchers**: tp.el uses Emacs's `add-variable-watcher` to monitor changes to reactive variables.

3. **Automatic Updates**: When a reactive variable changes via `setq`, all text regions using layers that depend on that variable are automatically updated with the new property values.

### Defining Reactive Layers

#### Basic Reactive Layer

```elisp
(defvar highlight-color "yellow")

(define-tp my-highlight ()
  :props '(face (:background $highlight-color)))

(with-temp-buffer
  (insert "Hello World")
  (tp-push-layer 1 10 'my-highlight)
  ;; Text is highlighted in yellow
  
  (setq highlight-color "cyan")
  ;; Text is now highlighted in cyan - automatically!
)
```

#### Multiple Reactive Variables

```elisp
(defvar fg-color "white")
(defvar bg-color "black")

(define-tp themed-text ()
  :props '(face (:foreground $fg-color :background $bg-color)))

;; Changing either variable updates the text
(setq fg-color "yellow")  ; Updates foreground
(setq bg-color "navy")    ; Updates background
```

### :data - Additional Reactive State

The `:data` keyword defines additional reactive variables that aren't directly used in `:props` but can trigger computed value updates or be watched:

```elisp
(define-tp user-info ()
  :props '(help-echo $full-name)
  :data '(first-name last-name)  ; Not used directly in props
  :compute '((full-name (lambda () (concat first-name " " last-name)))))
```

**With Initial Values:**

You can specify initial values using cons cells:

```elisp
(define-tp user-info ()
  :props '(help-echo $full-name)
  :data '((first-name . "John") (last-name . "Doe"))
  :compute '((full-name (lambda () (concat first-name " " last-name)))))

;; first-name is now "John", last-name is now "Doe"
```

### :compute - Computed Properties

The `:compute` keyword creates derived values that are automatically recalculated when their dependencies change:

```elisp
(define-tp progress-display ()
  :props '(display $progress-text face (:foreground $progress-color))
  :data '((current . 0) (total . 100))
  :compute '((progress-text (lambda () (format "%d%%" (/ (* current 100) total))))
             (progress-color (lambda ()
                               (cond ((< current 30) "red")
                                     ((< current 70) "yellow")
                                     (t "green"))))))

;; Update progress
(setq current 50)
;; progress-text becomes "50%" and progress-color becomes "yellow" automatically!
```

### :watch - Side Effect Callbacks

The `:watch` keyword lets you execute callbacks when reactive variables change:

```elisp
(define-tp monitored-layer ()
  :props '(face (:foreground $status-color))
  :watch ((status-color 
           (lambda (new-val old-val layer-name)
             (message "Layer %s: color changed from %s to %s" 
                      layer-name old-val new-val)))))

(setq status-color "red")
;; Message: "Layer monitored-layer: color changed from nil to red"

(setq status-color "green")
;; Message: "Layer monitored-layer: color changed from red to green"
```

### :transform - Value Transformation

The `:transform` keyword allows you to register a transformation function that processes `tp-text` values before they are displayed. This is useful for formatting numbers, dates, or other values:

```elisp
;; Number formatting
(define-tp price-display ()
  :props '(tp-text $price)
  :data '((price . "99.9"))
  :transform (lambda (text)
               (format "$%.2f" (string-to-number text))))
;; 99.9 displays as $99.00

;; Date formatting
(define-tp date-display ()
  :props '(tp-text $timestamp)
  :data '((timestamp . "1703865600"))
  :transform (lambda (text)
               (format-time-string "%Y-%m-%d" 
                 (seconds-to-time (string-to-number text)))))

;; Uppercase conversion
(define-tp uppercase-text ()
  :props '(tp-text $content)
  :data '((content . "hello"))
  :transform #'upcase)
;; "hello" displays as "HELLO"
```

The transform function:
- Receives the raw `tp-text` string value
- Returns the transformed string for display
- Is applied both on initial display and reactive updates
- Errors in transform functions are caught and logged

### Anonymous Reactive Layers

You can use reactive variables even without `define-tp`. When you use `$`-prefixed symbols in an anonymous plist, tp.el automatically generates a unique layer name:

```elisp
(defvar my-face-color "blue")

;; Anonymous reactive layer - tp-name is auto-generated
(tp-set 1 10 '(face (:foreground $my-face-color)))

;; The layer is now reactive - changing the variable updates the text
(setq my-face-color "red")
```

### Layer Name Resolution in APIs

All text property APIs (`tp-set`, `tp-match-set`, `tp-regexp-set`, etc.) now accept layer names directly:

```elisp
(define-tp warning-style ()
  :props '(face (:foreground "orange" :weight bold)))

;; Use layer name instead of plist
(tp-set 1 10 'warning-style)

;; Works with all matching functions
(tp-match-set "TODO" 'warning-style)
(tp-regexp-set "[0-9]+" 'warning-style)
```

### Reactive Layer Groups

Layer groups can also use reactive features:

```elisp
(define-tps status-indicators
  '("success" :props (face (:foreground $success-color))
              :data ((success-color . "green")))
  '("warning" :props (face (:foreground $warning-color))
              :data ((warning-color . "orange")))
  '("error"   :props (face (:foreground $error-color))
              :data ((error-color . "red"))))
```

### Batched Updates

When modifying multiple reactive variables simultaneously, each `setq` triggers a separate buffer update. Use `tp-with-batch-updates` to consolidate all changes and apply them once at the end:

```elisp
(define-tp themed-text ()
  :props '(face (:foreground $fg-color :background $bg-color))
  :data '((fg-color . "white") (bg-color . "black")))

(with-temp-buffer
  (insert "Hello World")
  (tp-set 1 12 'themed-text)
  
  ;; Without batching: each setq triggers a buffer update
  (setq fg-color "yellow")  ; First update
  (setq bg-color "navy")    ; Second update
  
  ;; With batching: all changes applied once at the end
  (tp-with-batch-updates
    (setq fg-color "red")
    (setq bg-color "blue")))  ; Only one update
```

Benefits of batched updates:
- Reduces redundant buffer modifications
- Improves performance when changing multiple variables
- Ensures consistent state when multiple variables are interdependent

### Debug Mode

tp.el provides a debug mode to help understand reactive update flow:

```elisp
;; Enable debug mode
(setq tp-debug-mode t)

;; Also show debug info in minibuffer (optional)
(setq tp-debug-echo t)

;; View debug log
(tp-debug-show)

;; Clear debug log
(tp-debug-clear)
```

Debug log includes:
- Variable change notifications (old → new value)
- Layer update tracking
- Batch update start/end
- Transform application info

Example debug output:
```
[12:34:56.789] Variable my-color changed: "red" -> "blue" (where: global)
[12:34:56.790]   Updating layer test-layer (tp-text affected: no)
```

### Resetting Reactive State

To clear all reactive dependencies and watchers:

```elisp
(tp-reactive-reset)  ; Clears only reactive state

(tp-layer-reset)     ; Clears layers, groups, AND reactive state
```

### Complete Example: Theme-Aware Text

```elisp
;; Define theme variables
(defvar theme-fg "white")
(defvar theme-bg "black")
(defvar theme-accent "cyan")

;; Define theme-aware layers
(define-tp code-keyword ()
  :props '(face (:foreground $theme-accent :weight bold)))

(define-tp code-comment ()
  :props '(face (:foreground "gray" :slant italic)))

(define-tp code-string ()
  :props '(face (:foreground "green")))

;; Apply layers to code
(tp-match-set '("defun" "defvar" "let" "if" "when") 'code-keyword)
(tp-regexp-set ";.*$" 'code-comment)
(tp-regexp-set "\"[^\"]*\"" 'code-string)

;; Switch to light theme - just change variables!
(defun switch-to-light-theme ()
  (interactive)
  (setq theme-fg "black")
  (setq theme-bg "white")
  (setq theme-accent "blue"))

;; Switch to dark theme
(defun switch-to-dark-theme ()
  (interactive)
  (setq theme-fg "white")
  (setq theme-bg "black")
  (setq theme-accent "cyan"))
```

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
