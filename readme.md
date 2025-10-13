最上面的层会被渲染到 buffer 中。

### text properties layer
- `tp-layer-set (name start end &optional object)`
将 object 在 start 到 end 范围内的文本当前展示的文本属性层命名为 name。

- `tp-layer-push (name start end properties &optional object)`
给 object 在 start 到 end 范围内的文本设置 properties 并 push 到最上层。

- `tp-layer-delete (name start end &optional object)`
删除 object 在 start 到 end 范围内的文本名称为 name 的层。

- `tp-layer-rotate (start end &optional object)`
循环移动 object 在 start 到 end 范围内的文本的属性层。即每次将最上面的层移到最下面，交替使用每个层的文本属性。

- `tp-layer-pin (name start end &optional object)`
将 object 在 start 到 end 范围内的文本的名称为 name 的层移到最上面。

- `tp-layer-define (name properties)`
定义 properties 为名称为 name 的文本属性层。如果已经存在，则会使用 properties 覆其属性定义。

- `tp-layer-group-define (name &rest layers)`


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
