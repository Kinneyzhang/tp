# Changelog

All notable changes to the tp library are documented here.

## 0.2.0 (2026-07-26)

### Architecture

- **tp.el was split into layered modules.** `(require 'tp)` still loads
  everything; nothing changes for users. Each module depends only on
  the ones before it, and the byte compiler enforces the order:

  | Module | Responsibility |
  |---|---|
  | `tp-core.el` | Intervals, plist/face merge engine, debug logging, `$var` utilities |
  | `tp-reactive.el` | Reactive dependency registry, variable watchers, batching queue |
  | `tp-layer.el` | `define-tp` / `define-tps`, layer registry and resolution |
  | `tp-ops.el` | `tp-set` / `tp-reset` / `tp-add` / `tp-get` / `tp-at` / `tp-remove` / `tp-clear` |
  | `tp-search.el` | `tp-match-*`, `tp-regexp-*`, `tp-search`, navigation |
  | `tp-render.el` | Reactive re-rendering engine (installs itself into lower modules) |
  | `tp-stack.el` | Layer stack operations (push/pop/move/merge/flatten/...) |
  | `tp-palette.el` | Light/dark color palette data |
  | `tp-builtins.el` | Built-in layers, palette gallery, display-buffer helpers |

- The library now byte-compiles cleanly (previously `define-tp`
  macro-expansion failed at compile time).
- New shared engine `tp--map-intervals`: a clipping interval walker that
  underlies region operations; property edits can no longer bleed
  outside the requested region.
- New public constant `tp-face-properties` (`'(face font-lock-face
  mouse-face)`): the property family that gets face-aware merging.

### Fixed

Core operations:

- `(require 'text-property-search)` was missing; `tp-backward` signaled
  `void-function` in batch/fresh sessions.
- `tp-remove` string form silently dropped the 3rd and later properties.
- String-form removal helpers sampled properties at position 0 and
  smeared them across the range, destroying neighboring intervals; they
  now work per-interval.
- `tp-clear` computed default bounds from the current buffer even when
  clearing a string (silent no-op or range error).
- `(tp-get STRING START END ...)` returned nil silently; it now behaves
  like the buffer region form.
- `tp-intervals` returned unclipped intervals (including negative
  offsets); it now clips to `[START, END)`.
- Region-form calls with flat prop/val arguments — `(tp-set 1 4 'face
  'bold)` — silently discarded the value and failed later; they now
  signal a clear error immediately.
- Face-family prepend semantics in `tp-add` covered only `face`; they
  now cover `font-lock-face` and `mouse-face` too.
- `tp--parse-face-list` no longer invents a `(:key nil)` pair for a
  trailing bare keyword.

Built-ins and palette:

- Emacs 28.1 compatibility restored (`plistp` is Emacs 29+; a compat
  shim is used, and `subr-x` is required where needed).
- `tp-pop-to-buffer` / `tp-switch-to-buffer` no longer bind `q` in the
  shared major-mode keymap (a buffer-local minor-mode keymap is used)
  and no longer capture a `buffer` variable from the caller.
- `tp-link` resolved its palette color once at load time; the color is
  now resolved at application time, so theme switches are honored.
- `define-tp-palette` no longer generates per-palette defvars;
  `tp-palette-alist` is the single source of truth and palette
  redefinition takes effect immediately.
- `tp-headline` emitted invalid `(:height nil)` for integer heights.
- `tp-space` now produces the documented pixel `(space :width (N))`
  spec.
- `tp-parse-color` accepts one-sided cons colors like `("white" . nil)`.

Layer definitions (tp-layer):

- Parameterized `define-tps` groups: the documented format returned nil
  props via `tp-group-props-with-arg`; all documented element shapes now
  resolve correctly.
- Cyclic layer references signal a clear error naming the cycle
  (previously crashed with `excessive-lisp-nesting`); diamond-shaped
  reuse is not a false positive.
- `define-tp` errors at macro-expansion time when extra body forms are
  present (previously silently discarded all but the first).
- `$`-symbols in parameterized layer bodies resolve to their variables'
  current values at evaluation time (previously leaked literally into
  the output props); parameterized layers remain non-reactive, and the
  choice is documented.
- `tp-layer-props` / `tp-group-props` and their `-with-arg` variants
  return copies; mutating a returned plist can no longer corrupt the
  registry.
- `:transform` in `define-tps` group elements is honored (was silently
  dropped).
- Group redefinition and `tp-undefine-group` clean up the layers the
  group generated, including their reactive deps and transforms
  (previously orphaned).
- The group-element parser errors on unknown keywords instead of
  advancing by one and re-reading a value as a key.
- Anonymous reactive layers are interned: an `equal` `$var` props spec
  reuses the existing registry entry instead of minting a new one on
  every `tp-set` (unbounded leak fixed).

Layer stacks (tp-stack):

- All stack mutators were rewritten onto a shared clipped region walker;
  region ops no longer alter text outside `[START, END)`, and
  `tp-put-layer` is region-local instead of switching behavior on
  whole-object emptiness.
- The documented inline-plist spec (`(face bold ...)`) and
  list-of-layer-names spec (`'(layer-a layer-b)`) for `tp-put-layer`
  work (previously errored), handled at the call site.
- `tp-region-layer-props` no longer double-offsets string positions.
- `tp-merge-layers` / `tp-flatten-layers` no longer drop explicitly-nil
  values (presence is checked with `plist-member`).
- Single-layer stacks no longer carry a garbage `(tp-layers nil)`
  property, and its absence is tolerated everywhere.
- `tp-layer-top` respects the requested region instead of reading only
  the first interval.

Search and navigation (tp-search):

- `tp-backward` buffer paths passed no predicate to
  `text-property-search-backward`, so matching was inverted relative to
  `tp-forward`; backward now mirrors forward's equal-matching
  semantics. The legacy test that codified the inverted behavior
  (`tp-test-backward`) was updated to the symmetric contract.
- Empty and zero-width patterns no longer loop forever in the
  match/regexp apply engines.
- Length-changing replacements work in buffers in `tp-forward-do` /
  `tp-backward-do` / `tp-search-map` (previously signaled
  `args-out-of-range` via `store-substring`). On strings — which cannot
  change length in place — a length-changing replacement signals a
  clear error instead of silently truncating or leaving residue;
  same-length string replacements are unchanged.
- `tp-search-map` with a non-current buffer OBJECT operates on that
  buffer (previously read and mutated the current buffer) and no longer
  corrupts buffers on length-changing replacements.
- `tp-match-add` buffer path uses face-family-aware merging like the
  string path, so existing faces are preserved.
- `tp-search-map` can remove properties on strings (nil-props ranges
  were previously skipped).
- The triplicated ~38-line replacement lambda was extracted into one
  shared helper.

Reactive rendering (tp-reactive / tp-render):

- Sub-region `tp-text` on a string no longer discards the rest of the
  string.
- Computed-variable updates deep-merge resolved props with the layer
  definition, preserving sibling static attributes.
- Reactive refresh replaces the re-rendered layer's own property keys
  instead of accumulating (bold→italic no longer yields
  `(italic bold)`), while preserving other layers' properties.
- `setq-local` re-renders the buffer without leaking buffer-local
  values into the global layer definition.
- Reactive `tp-text` replacement preserves unrelated existing
  properties.
- Computed values of nil propagate (nil was conflated with the error
  sentinel).
- Variable-watcher reentrancy: nested `set` calls inside the update
  path queue their re-render through the batch queue instead of
  recursing.
- Batched updates union their WHERE and tp-text flags at flush time
  instead of freezing the first change's.
- Reactive strings keep per-interval props on re-render (previously
  only position-0 props survived and were smeared).
- `:transform` applies on the first render too, not only on updates.

Test infrastructure:

- The test fixture now tears down with `unwind-protect` and resets all
  registries including `tp-layer-transforms` (previously leaked across
  tests); the suite passes in randomized order.
- `tp-tests.el` header and `provide` renamed to match its file name.

### Known divergences

- On shortfall (fewer than TIMES matches in the range),
  `tp-forward-do` / `tp-backward-do` string paths still apply the
  function to the last available match while buffer paths apply
  nothing. Two legacy tests codify the string behavior
  (`tp-test-forward-do-on-string-with-range` and its backward twin), so
  it was left unchanged; unifying it is a pending semantics decision.

### Added

- `tp-member`: like `tp-at`, but distinguishes "property present with
  value nil" from "property absent" (plist-member-style result).
- `Makefile` with `test` / `compile` / `clean` targets.
- Per-module regression test suites: `tp-core-tests.el`,
  `tp-ops-tests.el`, `tp-builtins-tests.el`, `tp-layer-tests.el`,
  `tp-stack-tests.el`, `tp-search-tests.el`, `tp-render-tests.el` —
  the combined suite grew from 280 to 439 tests.

### Changed

- License clarified to GPLv3+ in file headers, matching the shipped
  LICENSE file (headers previously said v2+).

## 0.1.0

Initial release (monolithic tp.el).
