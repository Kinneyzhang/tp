### text properties layer
- tp-layer-set (name start end &optional object)
- tp-layer-push (name start end properties &optional object)
- tp-layer-delete (name start end &optional object)
- tp-layer-rotate (start end &optional object)
- tp-layer-pin (name start end &optional object)
- tp-layer-define (name properties)
- tp-layer-group-define (name &rest layers)

### text properties propertize
- tp-propertize (string properties &optional layer)
- tp-layer-propertize (string layer)
- tp-layer-group-propertize (string layer-group)

### text properties search
- tp-forward (property &optional value predicate not-current)
- tp-backward (property &optional value predicate not-current)
- tp-forward-do (function property &optional value predicate not-current)
- tp-backward-do (function property &optional value predicate not-current)
- tp-regions-map (function property &optional value predicate collect)
- tp-strings-map (function property &optional value predicate collect)
