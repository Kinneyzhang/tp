# tp.el Complete Guide to Reactive Text Properties

> Bringing modern frontend framework reactive programming paradigms to the Emacs text properties world

## Introduction

In traditional Emacs development, managing text properties has always been a tedious task. Whenever you want to change a property value, you need to manually find all related text regions and update them one by one. This approach is not only error-prone but also difficult to maintain.

**Reactive Text Properties** is one of the most innovative features in the tp.el library. It borrows the reactive programming concepts from modern frontend frameworks like Vue.js and React, allowing Emacs text properties to **automatically respond to variable changes**.

Imagine: you define the relationship between a variable and a property once, and from then on, whenever you change the variable's value, all text regions using that variable will **automatically update**. This is the magic of reactive text properties!

## From Traditional to Reactive

### Pain Points of the Traditional Approach

Let's first look at how the traditional approach handles dynamic text properties:

```lisp
;; Traditional approach: define a color variable
(defvar my-color "red")

(tp-pop-to-buffer "*tp-test*"
  (insert "Hello World")
  (tp-set 1 12 `(face (:foreground ,my-color)))
  
  ;; Here comes the problem: when you want to change the color...
  (setq my-color "blue")
  ;; The text doesn't update automatically! You must manually reapply:
  (tp-set 1 12 `(face (:foreground ,my-color))))
```

The problems with this approach are obvious:
1. **Manual tracking**: You need to remember which text regions use which variables
2. **Easy to miss**: In complex applications, it's easy to forget to update some regions
3. **Code redundancy**: Update logic is scattered throughout the code

### The Elegance of Reactive Approach

Now let's see how the reactive approach solves these problems:

```lisp
;; Reactive approach: define a color variable
(defvar my-color "red")

;; Define a reactive layer using $my-color to reference the variable
(define-tp my-highlight ()
  '(face (:foreground $my-color)))

;; Apply to text
(tp-pop-to-buffer "*tp-test*"
  (insert "Hello World")
  (tp-set 1 12 'my-highlight)
  
  ;; Now, just change the variable!
  (setq my-color "blue")
  ;; Magic happens: the text automatically turns blue!
  )
```

Isn't that amazing? Let's dive deep into how this powerful feature works.

## Core Concepts

### Reactive Variables

In tp.el, any symbol starting with `$` is treated as a **reactive variable**. For example:
- `$my-color` → references variable `my-color`
- `$font-size` → references variable `font-size`
- `$theme-background` → references variable `theme-background`

When you use these `$`-prefixed symbols in property definitions, tp.el will:
1. Automatically resolve the variable's current value
2. Register a watcher to monitor variable changes
3. When the variable changes, automatically update all related text regions

## Basic Usage

### Your First Reactive Layer

Let's start with a simple example:

```lisp
;; Define a global variable
(defvar highlight-bg "yellow")

;; Define a reactive layer
(define-tp simple-highlight ()
  '(face (:background $highlight-bg)))

;; Create a test buffer and apply the layer
(tp-pop-to-buffer "*tp-test*"
  (insert "This is text that needs highlighting")
  (tp-set 1 (point-max) 'simple-highlight)
  ;; => "Initial background color: yellow"
  ;; Change the variable
  (setq highlight-bg "cyan")
  ;; => "Updated background color: cyan"
  )
```

### Multiple Reactive Variables

A layer can reference multiple reactive variables:

```lisp
;; Define multiple variables
(defvar fg-color "white")
(defvar bg-color "darkGreen")
(defvar underline-color "red")

;; Define a layer using multiple variables
(define-tp multi-var-layer ()
  '(face ( :foreground $fg-color 
           :background $bg-color
           :underline (:color $underline-color))))

;; Test
(tp-pop-to-buffer "*tp-test*"
  (insert "Multi-variable reactive example")
  (tp-set 1 (point-max) 'multi-var-layer)
  
  ;; Changing any variable triggers an update
  (setq fg-color "yellow")      ; Foreground turns yellow
  (setq bg-color "navy")        ; Background turns navy
  (setq underline-color "lime") ; Underline turns lime green
  )
```

## Advanced Features: :data, :compute, and :watch

tp.el's reactive system borrows from Vue's API, providing three powerful keywords:

### :data - Define Additional Reactive State

Sometimes you need reactive variables that aren't directly used in `:props`. This is where `:data` comes in.

Main uses of `:data`:
1. Define auxiliary variables that don't appear directly in properties
2. Provide initial values for variables
3. Work together with `:compute`

### :compute - Computed Properties

`:compute` lets you define **derived values**—their values are computed from other variables:

```lisp
;; Complete computed properties example
(define-tp computed-greeting ()
  :props '(display $full-greeting face (:foreground $status-color))
  :data '((user-name . "John")
          (greeting-prefix . "Hello"))
  :compute '((full-greeting (lambda () 
                              (format "%s, %s! Welcome back." 
                                      greeting-prefix user-name)))
             (status-color (lambda ()
                             (if (string= user-name "Admin")
                                 "red"
                               "green")))))

;; Test
(tp-pop-to-buffer "*tp-test*"
  (insert "Test text")
  (tp-set 1 (point-max) 'computed-greeting)
  ;; Initial state
  (message "full-greeting = %s" full-greeting)
  ;; => "Hello, John! Welcome back."
  (message "status-color = %s" status-color)
  ;; => "green"
  ;; Change user-name
  (setq user-name "Admin")
  ;; Computed properties update automatically!
  (message "full-greeting = %s" full-greeting)
  ;; => "Hello, Admin! Welcome back."
  (message "status-color = %s" status-color)
  ;; => "red"
  ;; Change greeting-prefix
  (setq greeting-prefix "Hi")
  (message "full-greeting = %s" full-greeting))
;; => "Hi, Admin! Welcome back."
```

### :watch - Watch Variable Changes

`:watch` lets you execute **side effect** operations when variables change:

```lisp
;; Layer with watchers
(define-tp watched-layer ()
  :props '(face (:foreground $status-color))
  :data '((status-color . "green"))
  :watch '((status-color 
            (lambda (new-val old-val layer-name)
              (message "[%s] Color changed from %s to %s" 
                       layer-name old-val new-val)))))

;; Test
(tp-pop-to-buffer "*tp-test*"
  (insert "Test text")
  (tp-set 1 (point-max) 'watched-layer)
  
  ;; Change color - triggers watcher
  (setq status-color "yellow")
  ;; Message: "[watched-layer] Color changed from green to yellow"
  
  (setq status-color "red"))
;; Message: "[watched-layer] Color changed from yellow to red"
```

Typical uses for `:watch`:
- Logging
- Updating external state
- Triggering notifications
- Performing cleanup operations

## Complete Practical Examples

### Example 1: Dynamic Color Status Indicator

This example shows how to create an indicator that automatically changes color based on status:

```lisp
(tp-layer-reset)

;; Define status color variables
(defvar status-color "gray")
(defvar status-text "Not Started")

;; Define status indicator layer
(define-tp status-indicator ()
  '(face (:background $status-color) display $status-text))

;; Define status update function
(defun set-status (status)
  "Set status, automatically update color and text"
  (pcase status
    ('pending  (setq status-color "gray"   status-text "Pending"))
    ('running  (setq status-color "blue"   status-text "Running"))
    ('success  (setq status-color "green"  status-text "Success"))
    ('warning  (setq status-color "orange" status-text "Warning"))
    ('error    (setq status-color "red"    status-text "Error"))))

;; Test the status indicator
(tp-pop-to-buffer "*tp-test*"
  (insert "Status")
  (tp-set 1 (point-max) 'status-indicator)
  
  ;; Simulate status changes
  (set-status 'pending)
  (message "Status: %s, Color: %s" status-text status-color)
  ;; => "Status: Pending, Color: gray"
  
  (set-status 'running)
  (message "Status: %s, Color: %s" status-text status-color)
  ;; => "Status: Running, Color: blue"
  
  (set-status 'success)
  (message "Status: %s, Color: %s" status-text status-color))
;; => "Status: Success, Color: green"
```

### Example 2: Theme Switching System

This example shows how to create a switchable theme system:

```lisp
(tp-layer-reset)

;; Define theme color variables
(defvar keyword-color nil)
(defvar string-color nil)

;; Define theme-related reactive layers
(define-tp themed-keyword ()
  '(face (:foreground $keyword-color :weight bold)))

(define-tp themed-string ()
  '(face (:foreground $string-color)))

;; Define theme switching functions
(defun switch-to-dark-theme ()
  "Switch to dark theme"
  (interactive)
  (setq keyword-color "light blue"
        string-color "green")
  (message "Switched to dark theme"))

(defun switch-to-light-theme ()
  "Switch to light theme"
  (interactive)
  (setq keyword-color "blue"
        string-color "dark green")
  (message "Switched to light theme"))

;; Test theme switching
(tp-pop-to-buffer "*tp-test*"
  (insert "(defun hello () \"greeting\")")
  
  ;; Apply different theme layers
  (tp-match-set "defun" 'themed-keyword)
  (tp-regexp-set "\".+\"" 'themed-string)

  (switch-to-dark-theme)
  ;; Initially using dark theme
  (message "Keyword color: %s" keyword-color)
  (message "String color: %s" string-color)
  
  ;; Switch to light theme
  (switch-to-light-theme)
  ;; Text updates automatically!
  (message "Keyword color: %s" keyword-color)
  (message "String color: %s" string-color))
```

## Anonymous Reactive Layers

Besides using `define-tp` to define named layers, you can also use reactive variables directly in property lists. tp.el will automatically generate unique names for these anonymous layers:

```lisp
(tp-layer-reset)

(defvar inline-color "purple")

(tp-pop-to-buffer "*tp-test*"
  (insert "Anonymous reactive layer example")
  
  ;; Use $inline-color directly, no need to pre-define a layer
  (tp-set 1 (point-max) '(face (:foreground $inline-color)))
  
  ;; Text is now purple
  (message "Color: %s" (plist-get (tp-at 1 'face) :foreground))
  ;; => "purple"
  
  ;; Change the variable
  (setq inline-color "orange")
  
  ;; Text automatically turns orange
  (message "Color: %s" (plist-get (tp-at 1 'face) :foreground)))
;; => "orange"
```

Anonymous reactive layers are suitable for simple scenarios where you don't need to reuse the same layer definition in multiple places.

## Reactive Text (tp-text)

Besides reactive text **properties**, tp.el also supports reactive **text content** itself. Through the special `tp-text` property, you can make the text content reactive too—when the bound variable changes, the text content automatically updates.

### Basic Usage

The `tp-text` property has two ways to use:

#### 1. Initialize with Current Text

When `tp-text` is `nil`, it will be automatically set to the current region's text content:

```lisp
(tp-pop-to-buffer "*tp-test*"
  (insert "Hello World")
  ;; When tp-text is nil, auto-initialize to current text "Hello"
  (tp-set 1 6 '(face bold tp-text nil))
  ;; Now tp-text value is "Hello"
  (message "tp-text = %s" (tp-at 1 'tp-text)))
;; => "Hello"
```

#### 2. Replace Text Content

When `tp-text` is a string, it replaces the text in the region while preserving other text properties:

```lisp
(tp-pop-to-buffer "*tp-test*"
  (insert "Hello World")
  ;; When tp-text is a string, replace the text content
  (tp-set 1 6 '(face bold tp-text "Hi"))
  ;; Text becomes "Hi World", and "Hi" still has bold style
  (message "buffer = %s" (buffer-string)))
;; => "Hi World"
```

### Reactive Text Layers

The real power of `tp-text` comes from combining it with reactive variables:

```lisp
;; Define a reactive variable
(defvar my-dynamic-text "Loading...")

;; Define a layer containing tp-text
(define-tp dynamic-content ()
  :props '(face (:foreground "blue") tp-text $my-dynamic-text))

;; Apply to text
(tp-pop-to-buffer "*tp-test*"
  (insert "placeholder")
  (tp-set 1 12 'dynamic-content)
  ;; Text now shows "Loading..."
  (message "Initial text: %s" (buffer-string))
  ;; => "Loading... "
  
  ;; Change the variable
  (setq my-dynamic-text "Data loaded successfully!")
  ;; Text updates automatically!
  (message "After update: %s" (buffer-string)))
;; => "Data loaded successfully! "
```

### Using :compute for Dynamic Text

`tp-text` can be combined with `:compute` to create dynamic text derived from other variables:

```lisp
(define-tp greeting-layer ()
  :props '(face (:foreground "green") tp-text $full-greeting)
  :data '((user-name . "Guest")
          (greeting-prefix . "Welcome"))
  :compute '((full-greeting
              (lambda ()
                (format "%s, %s!" greeting-prefix user-name)))))

;; Apply to text
(tp-pop-to-buffer "*tp-test*"
  (insert "placeholder")
  (tp-set 1 12 'greeting-layer)
  ;; Shows "Welcome, Guest!"
  (message "Initial: %s" (buffer-string))
  
  ;; Change user name
  (setq user-name "John")
  ;; Text automatically updates to "Welcome, John!"
  (message "After update: %s" (buffer-string)))
```

### Anonymous Reactive Text

You can also use reactive `tp-text` directly in property lists without defining a layer:

```lisp
(defvar inline-text "Original content")

(tp-pop-to-buffer "*tp-test*"
  (insert "placeholder")
  ;; Directly use reactive tp-text
  (tp-set 1 12 '(face bold tp-text $inline-text))
  ;; Shows "Original content"
  
  ;; Change the variable
  (setq inline-text "New content")
  ;; Text automatically updates to "New content"
  )
```

### Important Notes

1. **tp-text only affects buffer text**: For string objects, since Emacs string length is fixed, `tp-text` won't replace string content.
2. **Preserves existing properties**: When using `tp-set` or `tp-add` to set `tp-text`, existing text properties are preserved.
3. **Non-reactive properties don't add tp-name**: If there are no reactive variables (`$` prefix) in the text properties, `tp-name` and other reactive-specific properties won't be added, maintaining native text property behavior.

## Value Transformation with :transform

The `:transform` keyword allows you to register a transformation function that processes `tp-text` values before they are displayed. This is useful for formatting numbers, dates, or other values:

```lisp
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

> 📖 **For more optimization features like batched updates and debug mode, see [Reactive System Optimization](reactive-optimization-en.md)**

## Summary

tp.el's reactive text properties feature brings a modern reactive programming experience to Emacs development. By using `$`-prefixed reactive variables, `:data` to define state, `:compute` for derived values, `:watch` to monitor changes, and `:transform` for value formatting, you can build a more dynamic and maintainable text property system.

Key points:
1. **Reactive Variables**: Use `$` prefix to reference variables
2. **:props**: Define properties containing reactive variables
3. **:data**: Define additional reactive state and initial values
4. **:compute**: Define computed properties derived from other variables
5. **:watch**: Watch variable changes and execute side effects
6. **:transform**: Transform tp-text values before display
7. **Automatic Updates**: Change variable values, all related text updates automatically
8. **Reactive Text (tp-text)**: Make text content itself reactive
