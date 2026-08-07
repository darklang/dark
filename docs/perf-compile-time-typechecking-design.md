# Moving type checking off the hot path

A design for splitting Dark's runtime type checker into the half that can move to compile time and the half that can't, and for wiring the compile-time half into the op-apply step that already compiles package code.

Short version: **runtime type checking is two jobs wearing one coat. Ablation says the checking half is worth -20% allocation and -14% wall. The instantiation half is load-bearing and has to stay. The place to put the static checker already exists (`applyAddFn`), the store is content-addressed so a verdict never goes stale, and the dependency DAG for invalidation is already a table.** This is weeks of work, not days, and most of the risk is in soundness rather than plumbing.

---

## 1. What the runtime checker actually does

Two things, at every function call, interleaved in the same code path.

**1.A. Checking.** Does this argument's value match this parameter's declared type? `checkFnParam` and `checkFnResult` in `LibExecution/TypeChecker.fs`, driven from the Apply path in `Interpreter.fs`. This is safety: it turns "the interpreter does something incoherent three frames later" into

```
Darklang.Stdlib.Dict.keys's 1st parameter `dict` expects Dict<_>,
but got Darklang.Stdlib.Option.Option<Dict<_>>
```

I hit exactly that error while writing a benchmark workload for this report, so it earns its keep today.

**A live example, found while regression-hunting this branch.** `Darklang.WIP.AI.OpenAI.Audio.Transport.delete` calls `Stdlib.HttpClient.request "DELETE" url headers []`, but `request`'s fourth parameter is a `Blob`, not a list. That function is parsed, hashed, compiled to instructions and sitting in the store right now. It loads without complaint and fails only if someone calls it:

```
Darklang.Stdlib.HttpClient.request's 4th parameter `body` expects Blob, but got List<_> (List<_> [])
```

That's the case for the checker in one line: the error is perfectly good, it just arrives years late. It's also the case for the *runtime* half staying until the checker exists, because right now that message is the only thing standing between this and undefined behaviour.

**1.B. Instantiation.** Binding type variables in the type symbol table, so that `'a` in a signature has a concrete meaning inside the frame. This looks like type checking and lives in the same functions, but it is not a check, it is *information the runtime needs to do its job*.

The distinction isn't theoretical. I ablated each separately:

| ablation | result |
|---|---|
| `checkFnParam` + `checkFnResult` become no-ops | **-20% allocation, -14% wall.** Everything still runs. |
| instantiation removed | The CLI does not start. `Json.parse` fails with `Unsupported type in JSON: 'a` |

`Json.parse` needs to know the concrete type it is parsing *into*. Delete the binding and there is nothing to parse into. That is the whole argument for why this is a split and not a deletion.

One more number that shapes everything below: **~88% of calls are into functions whose signature contains a type variable.** Any design that fast-paths "the monomorphic case" is optimizing 12% of traffic.

## 2. Where the static checker goes

This is the part that is easier than it sounds, because the infrastructure is already there.

**2.A. There is already an ahead-of-time compile step.** `applyAddFn` in `LibDB/PackageOpPlayback.fs` runs `PT2RT.PackageFn.toRT` and stores the result:

```sql
CREATE TABLE package_functions (
  hash TEXT PRIMARY KEY,
  pt_def BLOB NOT NULL,       -- ProgramTypes: has the type annotations
  rt_instrs BLOB NOT NULL,    -- compiled instructions
  ...
)
```

So package code is *already* compiled when it is saved, not when it is loaded. A type checker is another pass in the same place, and its verdict is another column.

**2.B. Content addressing means a verdict never goes stale.** The hash *is* the definition. If `f0c29619...` checked clean once, it checked clean forever. There is no invalidation problem for the item itself, which is the thing that makes incremental type checking miserable in most systems.

**2.C. The dependency DAG is already a table.** `package_dependencies(item_hash, depends_on_hash, ...)` with indexes in both directions, currently 20,608 edges over 4,246 functions, 881 types and 434 values. That gives topological order for a full check and reverse reachability for "what do I re-check when a signature changes".

**2.D. There is already a pre-lowering validation pass to model it on.** `LibParser/Validation.fs` has the shape: a `Mode` (`Script | Package | Test`), an `IssueCode` enum, structural invariants checked before lowering. Its header says explicitly "It does not resolve names, check types, or evaluate expressions." The type checker is its sibling, one stage later, after name resolution.

## 3. The gradual boundary, concretely

Gradual typing is the goal, so the question isn't "can we delete the runtime check" but "where is the boundary between checked and unchecked code, and who pays at the seam".

Proposal: **a call skips the runtime argument check when both the caller and the callee are statically verified.** Three states per package item:

- `Checked` -- the static pass proved the signature and body consistent.
- `Unchecked` -- never checked, or the checker declined (see 4.A).
- `Failed` -- the static pass found a real error. Worth surfacing at save time, not call time.

Checks must stay wherever a value enters from outside a verified region:

- **Scripts and `eval`.** Not stored, not checked ahead of time, so every call *out of* a script into a package fn keeps its check. This is the common interactive case and it is fine: script code is small and calls a handful of package fns per statement, so the per-call cost is noise there.
- **Deserialization boundaries.** `Json.parse`, DB reads, HTTP request bodies, anything that constructs a `Dval` from bytes. These produce values whose type is asserted, not derived, and the assertion has to be checked somewhere.
- **Builtins returning `Unknown`.** Any builtin whose return `ValueType` isn't fully known reintroduces uncertainty and its consumers need the check.
- **`Unchecked` callees.** Obviously.

The nice property: this degrades correctly. A store where nothing has been checked behaves exactly like today. Each item that passes the static pass removes checks from its own call sites, incrementally, with no flag day.

## 4. The parts I'd expect to hurt

**4.A. Soundness, and the direction to be wrong in.** If the static checker says `Checked` and it is wrong, the runtime skips the check and the interpreter does something incoherent instead of raising a clean error. That trades a good error message for a bad crash, which is a bad trade for a language whose pitch includes good errors.

The rule has to be: **the checker under-approximates.** Anything it can't prove is `Unchecked`, and `Unchecked` costs performance, not correctness. That makes the initial version allowed to be dumb, and makes "what fraction of the package set is `Checked`" the metric to drive.

**4.B. Static types and runtime ValueTypes are not the same lattice.** The runtime works in `ValueType`, which has `Unknown`, and where an empty list literal is `Known (KTList Unknown)`. The static side works in `TypeReference`, which has `TVariable`. These meet in `unifyValueType`, and the meeting is subtle enough that `inferTVarsFromArg` deliberately refuses to bind a type variable to a partially-`Unknown` shape, because doing so locks in a constraint that downstream concretely-typed callers then fail. A static checker has to reason about the same thing without a value in hand, and I don't think anyone should assume the existing `TypeChecker` code can be reused as-is for it.

**4.C. Polymorphism is the common case, not the exception.** 88% again. The checker needs real inference over type variables, not signature equality. That is the bulk of the work and the bulk of the risk.

**4.D. Package values.** `package_values` holds `Dval`s evaluated at grow time. Their types come from evaluation rather than from a declaration, so they sit awkwardly between the two worlds. Probably start by treating every value as an unchecked boundary and revisit.

## 5. Phasing

Each phase is independently useful and independently revertable, which matters because phase 3 is where the payoff is and phases 1-2 are where the confidence comes from.

**Phase 1. Split checking from instantiation in the interpreter.** No behaviour change, no performance change. Today the two are interleaved inside `checkFnParam` and the Apply path; separate them into two named operations so that "skip the check, keep the binding" is a one-line condition rather than a refactor. This also makes the -20% ablation reproducible on demand instead of being a number in a note.

**Phase 2. Write the static checker, and run it without enforcing.** A pass over `pt_def` in dependency order. Output is a report, not a stored verdict: how many of the 4,246 functions check clean, how many the checker declines, how many it thinks are *wrong* (expect some of these to be checker bugs and some to be real). Run it in CI over the whole package set. This is the phase that tells you whether the whole idea is viable, and it ships nothing user-visible.

**Phase 3. Store the verdict and wire the runtime skip, behind a flag.** Add the column, populate it in `applyAddFn`, thread the state into the Apply path, gate the skip on a config flag defaulting to off. Measure. The ablation predicts -20% allocation and -14% wall as the *ceiling*, and the real number will be lower in proportion to how much of the call graph is `Checked` on both sides.

**Phase 4. Default it on**, once the checked fraction is high enough for the win to be real and the checker has been running in CI long enough to trust.

Rough effort, and I want to be honest rather than encouraging: phase 1 is a couple of days, phase 3 is a few days, phase 4 is a day plus a lot of waiting. **Phase 2 is the project** -- a type checker for a polymorphic language, sound in the conservative direction, is weeks. The fact that the plumbing around it is easy shouldn't make the middle look easy.

## 6. Things worth deciding before starting

- **Is `Failed` a save-time error or a warning?** Refusing to save a function that doesn't type check is the obvious answer and the wrong one for a gradually-typed system, at least early on, when most `Failed` results will be checker bugs. I'd warn first and tighten later.
- **Does the checker run on the branch or on the item?** Items are global and content-addressed, but name resolution is branch-scoped, so "does this name resolve" is branch-dependent while "do these types line up" is not. Worth being deliberate that the checker operates on already-resolved references.
- **What does the LSP do with this?** There is a `languageTools/lsp-server` and the checker is exactly the thing that would make it useful for type errors while typing. That is arguably a bigger user-facing win than the performance, and it might be the better reason to build it.

## 7. An adjacent gap this exposes

While proving that type errors still surface correctly, I noticed the call stack in a runtime error names functions by content hash:

```
Call stack (last call at bottom):
- Package Function f0c29619f9752d894f08a417e36c04814b4a33842816b2e6ad586d9bf50db352
- Package Function 9a373e3f3ed8bd0900430cc60549560e848b19e85f751446c9e23b4a80956d9f
```

Unrelated to performance, but it's in the same code and it makes every type error harder to act on than it needs to be. Worth fixing whether or not any of the above happens.
