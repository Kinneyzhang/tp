# Changelog

All notable changes to the tp library are documented here.

## 0.3.0 (2026-07-27)

### Added

Layer stack:

- **Layer visibility**: `tp-hide-layer` / `tp-show-layer` — a hidden
  layer stays in the stack (and keeps receiving reactive updates) but
  does not render; hiding the visible top reveals the next visible
  layer, and with every layer hidden the text renders bare.
  `tp-flatten-layers` merges only visible layers; `tp-merge-layers`
  excludes hidden matched layers' props.
- `tp-lower-layer` (mirror of `tp-raise-layer`) and a
  family-consistent `tp-rotate-layer` calling order
  `(START END DIRECTION [COUNT] [OBJECT])`, selected unambiguously by
  the symbols `up` / `down`; the legacy order keeps working.
- `tp-layer-stack-at` — the full ordered stack at one position as
  `(NAME . PROPS)` conses, hidden layers marked by a `tp-hidden`
  entry.
- Stack mutators return the number of property runs they modified
  (including `tp-merge-layers` / `tp-flatten-layers`), and layer-name
  lookups gained optional NOERROR arguments where they previously
  signaled.
- `tp-describe-layer` — interactive help-buffer description of a
  layer: storage format, arglist, stored body, expanded props,
  reactive deps, transform, owning group.

Reactive engine:

- **Layer→buffer registry**: reactive updates now visit only the
  buffers registered as showing the affected layer instead of scanning
  the whole `(buffer-list)`; every buffer-mutating write path
  registers (tp-set family, stack mutators, match/regexp appliers),
  killed buffers are pruned, and an unknown layer falls back to one
  learning full scan. `tp-reactive-layer-buffers` exposes the
  registry; `tp-reactive-track-buffer` closes the
  insert-a-propertized-string gap.
- **Minimal-diff `tp-text` re-render**: only the differing span is
  edited (insert-before-delete), so point and markers in unchanged
  text stay put and identical-text updates no longer touch the buffer
  at all (buffer-modified flag preserved).
- `tp-gc-anonymous-layers` — collects interned anonymous layers that
  no registered live buffer still shows (stack-aware: buried and
  hidden layers count as alive; string-only layers are conservatively
  kept).

Search and matching:

- `tp-regexp-set/reset/add` accept SUBEXP: properties apply to that
  capture group per match (non-participating groups contribute
  nothing); SUBEXP beyond the pattern's group count signals a clear
  error.
- `tp-match-*` / `tp-regexp-*` accept START/END bounds with
  as-if-only-that-portion semantics; reversed bounds are swapped.
- `tp-forward` / `tp-backward` / `tp-forward-do` / `tp-backward-do`
  accept PREDICATE and NOT-CURRENT, passed through to the
  text-property-search machinery; defaults keep the 0.2.0 symmetric
  equal-matching contract exactly.

Layer definitions:

- **Multi-argument parameterized layers**: `define-tp` / `define-tps`
  arglists may declare any number of parameters;
  `(LAYER ARG1 ... ARGN)` and wrapped `(LAYER (ARG1 ... ARGN))` specs
  work in `tp-set` and `tp-put-layer`; new `tp-layer-props-with-args`
  / `tp-group-props-with-args` / `tp-layer-arglist`. Wrong-arity
  calls signal clear errors naming the layer and both counts.
- Prefix-conforming aliases `tp-define-layer` / `tp-define-group` /
  `tp-define-palette` for discoverability (`C-h f tp-…`).

Core and palette:

- `tp-intervals` / `tp-intervals-map` accept an optional ABSOLUTE
  argument returning native buffer coordinates (feedable straight
  back into `tp-set`); the range-relative default is unchanged.
- `tp-palette-color` (generic theme-resolved accessor) and
  `tp-palette-has-p` consolidate the palette query surface; all
  existing query functions remain.

### Fixed

All six were found by an adversarial architecture/API review of the
new 0.3.0 code and confirmed with minimal reproductions before fixing:

- The reactive buffer registry only registered `tp-set`-family writes;
  layers applied via `tp-push-layer`, `tp-match-set`, etc. never
  re-rendered on variable updates.
- Reactive updates wrote only the rendered top layer; hidden or
  buried layers kept stale props (visible again on `tp-show-layer`).
- `tp-gc-anonymous-layers` and `tp-reactive-track-buffer` scanned only
  direct `tp-name` properties, so a layer buried in a stack (or
  hidden) could be wrongly collected / missed.
- `tp-flatten-layers` / `tp-merge-layers` rendered hidden layers'
  properties despite `tp-hide-layer`'s documented contract.
- Minimal-diff `tp-text` edits deleted before inserting, so markers at
  the suffix boundary drifted to the wrong character.
- An error escaping a reactive update could strand queued batch
  entries (now drained under `unwind-protect`; `tp-reactive-reset`
  clears the queue).

### Changed

- **Module boundaries tightened** (behavior identical under
  `(require 'tp)`): the `tp-text` handler chain moved from tp-render
  into tp-ops — partial loads now get working `tp-text` replacement —
  and `tp-with-batch-updates` moved up into tp-render; two of the four
  upward hook variables are gone
  (`tp--tp-text-handler-function`, `tp--reactive-flush-function`).
  The layer-stack storage codec and the anonymous-layer machinery now
  live in tp-layer; tp-stack's phantom dependency on tp-ops is gone;
  67 lines of dead code deleted. tp-core holds no mutable state.
- String forms of all 16 stack mutators document that they modify the
  string in place (unlike `tp-set`'s copy semantics); unifying this is
  on the 0.4 ledger.

### Deprecated

- `tp-search-forward` / `tp-search-backward` (0.3.0) — thin wrappers
  whose nil-PREDICATE default contradicts the rest of the library's
  equal-matching; use `tp-forward` / `tp-backward`, or the Emacs
  primitives for raw access.
- `tp-suffix-symbol` (0.3.0) — internal helper now private as
  `tp--suffix-symbol`; a compatibility alias remains.

### Infrastructure

- GitHub Actions CI: Emacs 28.1 / 29.4 / 30.1 matrix running
  byte-compilation with warnings-as-errors, the full ERT suite, a
  shuffled-order rerun of every test (`make test-shuffled`,
  `tp-run-shuffled.el`; `SHUFFLE_SEED=N` reproduces an order), and the
  README doctests.
- The whole tree byte-compiles with zero warnings (57 fixed:
  docstring rewraps and quoting, `defvar` declarations for reactive
  test variables, prefixed doctest counters, one impossible `eq`
  comparison corrected to `equal`).
- Autoload cookies for the interactive commands (`tp-debug-show`,
  `tp-debug-clear`, `tp-reactive-reset`, `tp-layer-reset`,
  `tp-palette-show`, `tp-clear`) and the `define-tp` / `define-tps`
  macros.
- Two doctest assertions made property-order-insensitive (Emacs 28
  prints text-property plists in a different order than 29+).

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
- `tp-forward-do` / `tp-backward-do` shortfall is now all-or-nothing on
  both paths: TIMES targets the TIMES-th match specifically, so when
  fewer matches exist nothing is applied and the available count is
  returned. String paths previously acted on the last available match —
  the wrong target; the two legacy tests codifying that
  (`tp-test-forward-do-on-string-with-range` and its backward twin)
  were updated.

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

### Added

- `tp-member`: like `tp-at`, but distinguishes "property present with
  value nil" from "property absent" (plist-member-style result).
- `Makefile` with `test` / `doctest` / `compile` / `clean` targets.
- `tp-doctest.el`: executable documentation tests — 63 assertions
  reproducing README examples and comparing against their exact
  documented outputs (`make doctest`).
- Per-module regression test suites: `tp-core-tests.el`,
  `tp-ops-tests.el`, `tp-builtins-tests.el`, `tp-layer-tests.el`,
  `tp-stack-tests.el`, `tp-search-tests.el`, `tp-render-tests.el` —
  the combined suite grew from 280 to 439 tests.

### Changed

- License clarified to GPLv3+ in file headers, matching the shipped
  LICENSE file (headers previously said v2+).

## 0.1.0

Initial release (monolithic tp.el).
