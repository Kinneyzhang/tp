# tp.el Reactive System Optimization Documentation

This document describes the optimizations and enhancements made to the tp.el reactive system based on practical experience from the [twidget](https://github.com/Kinneyzhang/twidget.git) project.

## Optimization Suggestions Evaluation

The following evaluates and documents the implementation status of six optimization suggestions for the tp.el reactive system:

### 1. Granular Reactive Updates

**Suggestion**: Support partial updates within a region - only updating the reactive portion while preserving surrounding text properties.

**Evaluation**: Already implemented. tp.el uses `tp-intervals-map` and interval-based update mechanisms to support fine-grained property updates. Updates only affect regions with specific `tp-name` properties.

### 2. Reactive Symbol Cleanup ✅ Already Implemented

**Suggestion**: Add a mechanism to unregister reactive symbols when widgets are destroyed.

**Evaluation**: Already implemented. The `tp--unregister-reactive-deps` function handles cleanup:
- Called automatically when a layer is redefined
- Called automatically when a layer is undefined (`tp-undefine-layer`)
- Cleans up variable watchers, computed properties, and data variables

**Key functions**:
- `tp--unregister-reactive-deps`
- `tp--unregister-layer-watchers`
- `tp--unregister-layer-computed`
- `tp--unregister-layer-data`

### 3. Scoped Reactivity ✅ Already Implemented

**Suggestion**: Add instance/context scoping for reactive variables.

**Evaluation**: Already implemented. The `where` parameter supports buffer-local updates in:
- `tp--update-layer-regions`
- `tp--update-reactive-text`

When using `setq-local`, updates only affect the specific buffer.

### 4. Batched Updates 🆕 New Feature

**Suggestion**: When multiple reactive values change simultaneously, batch updates to avoid redundant buffer modifications.

**Implementation**: Added `tp-with-batch-updates` macro:

```elisp
;; Using batch updates
(tp-with-batch-updates
  (setq my-color "red")
  (setq my-size 14)
  (setq my-text "Hello"))
;; All updates applied to buffer once at the end
```

**Key functions and variables**:
- `tp-with-batch-updates` - Batch update macro
- `tp--batch-update-active` - Flag indicating batch mode
- `tp--batch-update-pending` - List of pending updates
- `tp--flush-batch-updates` - Apply all pending updates

### 5. Value Transformation 🆕 New Feature

**Suggestion**: Allow registering transformation functions that run when tp-text updates.

**Implementation**: Added `:transform` option:

```elisp
;; Define a layer with transformation
(tp-define-layer 'currency-display
  :props '(face bold tp-text $amount)
  :data '((amount . "100"))
  :transform (lambda (text)
               (format "$%s.00" text)))

;; After application, 100 displays as $100.00
```

**Key functions and variables**:
- `tp-layer-transforms` - Stores layer transform functions
- Transforms applied in `tp--handle-tp-text-property` and `tp--update-reactive-text`

### 6. Debug Mode 🆕 New Feature

**Suggestion**: Add a debug mode to trace reactive updates.

**Implementation**: Added debug functionality:

```elisp
;; Enable debug mode
(setq tp-debug-mode t)

;; Also show debug info in minibuffer
(setq tp-debug-echo t)

;; View debug log
(tp-debug-show)

;; Clear debug log
(tp-debug-clear)
```

**Key functions and variables**:
- `tp-debug-mode` - Enable/disable debug mode
- `tp-debug-echo` - Whether to echo debug info to minibuffer
- `tp-debug-log` - Log debug information
- `tp-debug-show` - Show debug buffer
- `tp-debug-clear` - Clear debug log

Debug log includes:
- Variable change notifications (old → new value)
- Layer update tracking
- Batch update start/end
- Transform application info

## New Features in Detail

### Batch Updates (tp-with-batch-updates)

When modifying multiple reactive variables simultaneously, use batch updates to avoid multiple buffer updates:

```elisp
(tp-define-layer 'themed-text
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

### Value Transformation (:transform)

Transform functions allow processing tp-text values before display:

```elisp
;; Number formatting
(tp-define-layer 'price-display
  :props '(tp-text $price)
  :data '((price . "99.9"))
  :transform (lambda (text)
               (format "$%.2f" (string-to-number text))))

;; Date formatting
(tp-define-layer 'date-display
  :props '(tp-text $timestamp)
  :data '((timestamp . "1703865600"))
  :transform (lambda (text)
               (format-time-string "%Y-%m-%d" 
                 (seconds-to-time (string-to-number text)))))

;; Uppercase conversion
(tp-define-layer 'uppercase-text
  :props '(tp-text $content)
  :data '((content . "hello"))
  :transform #'upcase)
```

### Debug Mode

Debug mode helps developers understand the reactive update flow:

```elisp
;; Enable full debugging
(setq tp-debug-mode t)
(setq tp-debug-echo t)

;; Define and use a reactive layer
(tp-define-layer 'test-layer
  :props '(face (:foreground $my-color))
  :data '((my-color . "red")))

(with-temp-buffer
  (insert "Test")
  (tp-set 1 5 'test-layer)
  (setq my-color "blue"))

;; Example debug output:
;; [12:34:56.789] Variable my-color changed: "red" -> "blue" (where: global)
;; [12:34:56.790]   Updating layer test-layer (tp-text affected: no)
```

## Architecture Notes

These optimizations follow tp.el's layered architecture principles:

1. **Debug Mode** - Basic utility layer functionality
2. **Batch Updates** - Implemented in the reactive system layer
3. **Value Transformation** - Implemented in layer definition and reactive text handling

All new features integrate seamlessly with the existing reactive system without breaking existing APIs.

## Function Reference

| Function/Variable | Description |
|------------------|-------------|
| `tp-debug-mode` | Enable debug mode |
| `tp-debug-echo` | Enable minibuffer debug output |
| `tp-debug-log` | Log debug information |
| `tp-debug-show` | Show debug buffer |
| `tp-debug-clear` | Clear debug log |
| `tp-with-batch-updates` | Batch update macro |
| `tp-layer-transforms` | Layer transform function storage |
| `:transform` | Transform option in layer definition |
