# tp.el - Text Properties Library for Emacs

A convenient wrapper around Emacs text properties with an innovative **layer system** that allows setting multiple layers of text properties on the same text region.

Inspired by [ov.el](https://github.com/emacsorphanage/ov) for overlays.

## Features

- **Simple API** for text property manipulation (similar to ov.el for overlays)
- **Innovative tp-layer system** for multi-layer text properties
- **Layer groups** for defining reusable property sets
- **Search and navigation** functions for text properties
- **Match and regexp** functions for applying properties to text patterns

## Installation

```elisp
(require 'tp)
```

**Requirements:**
- Emacs 28.1+ (uses `object-intervals` function)
- `dash` (for list manipulation utilities)

## Quick Start

### Basic Text Properties

```elisp
;; Set properties on a region
(tp-put 10 20 'face 'warning 'help-echo "Hello!")

;; Get property at point
(tp-get 15 'face)  ; => warning

;; Remove a property
(tp-remove 10 20 'face)

;; Clear all properties in region
(tp-clear 10 20)

;; Get all properties at point
(tp-at 15)  ; => (face warning help-echo "Hello!")
```

### Match and Regexp

```elisp
;; Apply properties to all matches of a string
(tp-match "TODO" 'face 'warning)

;; Apply properties to all matches of a regexp
(tp-regexp "\\bfunction\\b" 'face 'font-lock-function-name-face)
```

### The Layer System

The layer system allows you to stack multiple sets of properties on the same text. Only the top layer is visible, but you can rotate, pin, or delete layers to reveal hidden ones.

```elisp
;; Define reusable layers
(tp-layer-define highlight
  '(face (:background "yellow" :foreground "black")))

(tp-layer-define urgent
  '(face (:background "red" :foreground "white")))

(tp-layer-define info
  '(face (:background "blue" :foreground "white")))

;; Push layers onto text (first pushed is bottom, last is top)
(tp-layer-push 1 10 'highlight)
(tp-layer-push 1 10 'urgent)  ; urgent is now visible

;; Rotate layers (urgent moves to bottom, highlight becomes visible)
(tp-layer-rotate 1 10)

;; Pin a specific layer to top
(tp-layer-pin 1 10 'urgent)

;; Delete a layer
(tp-layer-delete 1 10 'highlight)
```

### Layer Groups

Define groups of layers that work together:

```elisp
(tp-layer-group-define my-status-group
  status-normal '(face (:background "green" :foreground "black"))
  status-warning '(face (:background "yellow" :foreground "black"))
  status-error '(face (:background "red" :foreground "white")))

;; Apply all layers from a group to a string
(tp-layer-group-propertize "Status" 'my-status-group)
```

## API Reference

### Basic Text Property Functions

| Function | Description |
|----------|-------------|
| `tp-put (start end &rest properties)` | Set text properties on region |
| `tp-get (position property &optional object)` | Get property value at position |
| `tp-remove (start end property &optional object)` | Remove a property from region |
| `tp-remove-list (start end properties &optional object)` | Remove multiple properties |
| `tp-clear (&optional start end object)` | Clear all properties in region |
| `tp-at (&optional point object)` | Get all properties at point |
| `tp-plist (start end &optional object)` | Get merged plist for region |

### Match and Regexp Functions

| Function | Description |
|----------|-------------|
| `tp-match (string &rest properties)` | Set properties on string matches |
| `tp-regexp (regexp &rest properties)` | Set properties on regexp matches |

### Layer Definition Functions

| Function | Description |
|----------|-------------|
| `tp-layer-define (name properties)` | Define a named layer |
| `tp-layer-group-define (name &rest layers)` | Define a layer group |
| `tp-layer-properties (layer-name)` | Get properties for a layer |
| `tp-layer-group-properties (group-name)` | Get properties for all layers in group |
| `tp-layer-undefine (name)` | Remove a layer definition |
| `tp-layer-group-undefine (name)` | Remove a layer group definition |
| `tp-layer-reset ()` | Clear all layer definitions |

### Layer Manipulation Functions

| Function | Description |
|----------|-------------|
| `tp-layer-push (start end name &optional object)` | Push layer to top of stack |
| `tp-layer-delete (start end name &optional object)` | Delete layer from stack |
| `tp-layer-rotate (start end &optional object)` | Rotate layers (top to bottom) |
| `tp-layer-pin (start end name &optional object)` | Pin layer to top |
| `tp-layer-set (start end name &optional object)` | Name the current top layer |
| `tp-layer-hide (start end name &optional object)` | Move layer to bottom |
| `tp-layer-show (start end name &optional object)` | Move layer to top (alias for pin) |
| `tp-layer-merge (start end layer1 layer2 new-name &optional object)` | Merge two layers |

### Layer Query Functions

| Function | Description |
|----------|-------------|
| `tp-layer-list (start end &optional object)` | List all layer names in region |
| `tp-layer-count (start end &optional object)` | Count layers in region |
| `tp-layer-exists-p (start end name &optional object)` | Check if layer exists |
| `tp-layer-top (start end &optional object)` | Get name of top layer |
| `tp-region-layer-props (start end layer-name &optional object)` | Get layer properties in region |

### Propertize String Functions

| Function | Description |
|----------|-------------|
| `tp-propertize (string &rest properties)` | Propertize string |
| `tp-layer-propertize (string layer)` | Apply layer to string |
| `tp-layer-group-propertize (string layer-group)` | Apply layer group to string |

### Search Functions

| Function | Description |
|----------|-------------|
| `tp-forward (property &optional value predicate not-current)` | Search forward for property |
| `tp-backward (property &optional value predicate not-current)` | Search backward for property |
| `tp-forward-do (function property &optional ...)` | Search forward and apply function |
| `tp-backward-do (function property &optional ...)` | Search backward and apply function |
| `tp-regions-map (function property &optional ...)` | Apply function to all matching regions |
| `tp-strings-map (function property &optional ...)` | Apply function to all matching strings |

### Navigation Functions

| Function | Description |
|----------|-------------|
| `tp-next (&optional point property value)` | Get next position with property |
| `tp-prev (&optional point property value)` | Get previous position with property |
| `tp-goto-next (&optional property value)` | Move to next property |
| `tp-goto-prev (&optional property value)` | Move to previous property |

### Query Functions

| Function | Description |
|----------|-------------|
| `tp-in (property &optional value start end)` | Get regions with property |
| `tp-all (&optional start end)` | Get all regions with properties |
| `tp-intervals (start end &optional object)` | Get all property intervals |
| `tp-empty-p (object)` | Check if object has no properties |

## The Layer Concept

The layer system stores multiple sets of properties in a stack structure:

```
┌─────────────────────────┐
│   TOP LAYER (visible)   │  <- Properties you see
├─────────────────────────┤
│      Middle Layer       │  <- Hidden, but preserved
├─────────────────────────┤
│      Bottom Layer       │  <- Hidden, but preserved
└─────────────────────────┘
```

Each layer is identified by a name (symbol) and contains:
- Standard text properties (face, display, help-echo, etc.)
- A special `tp-name` property for identification
- A `tp-layers` property containing the list of layers below

### Layer Operations

- **Push**: Add a new layer on top
- **Delete**: Remove a layer from anywhere in the stack
- **Rotate**: Move top layer to bottom (cycles visibility)
- **Pin**: Move any layer to the top
- **Hide**: Move a layer to the bottom
- **Merge**: Combine two layers into one

## Examples

### Syntax Highlighting with Layers

```elisp
;; Define layers for different highlighting purposes
(tp-layer-define code-syntax
  '(face font-lock-keyword-face))

(tp-layer-define code-error
  '(face (:underline (:color "red" :style wave))))

(tp-layer-define code-selection
  '(face (:background "light blue")))

;; Apply base syntax highlighting
(tp-layer-push 1 100 'code-syntax)

;; Add error highlighting on top (doesn't remove syntax highlighting)
(tp-layer-push 1 100 'code-error)

;; Toggle between showing error and syntax
(tp-layer-rotate 1 100)
```

### Interactive Layer Switching

```elisp
(defun my-toggle-layers ()
  "Toggle between different property layers on current line."
  (interactive)
  (tp-layer-rotate (line-beginning-position)
                   (line-end-position)))

(global-set-key (kbd "C-c t") 'my-toggle-layers)
```

## License

GNU General Public License v2 or later.
