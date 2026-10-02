# Working on Dark when the packages live in a store

How to add a builtin, use it from Dark, reference a new package type or function from F#, try all
of it locally, and get it to everybody else.

The one-line version: **the store is the source of truth for Dark code, git is the source of truth
for F#, and a git branch carries the package work its F# depends on so the two merge as one thing.**

Read `docs/dev-setup` first if the container is not up. `dark docs packages` is the short version of
this from inside the CLI.

---

## Where a build's packages come from

Two answers, and `package-set.txt` at the root says which:

    commit unset     built from `packages/` on disk, by reloading it
    commit <hash>    fetched as a SEED from a package server, at that commit

`scripts/build/prepare-package-set` is the one place that answers the question, and CI's
package-reloading jobs go through it. It also checks that the kernel and the package set agree
before letting the build continue.

It ships `unset` today, which is the reloading behaviour that has always been there. Everything
below works in both modes except where it says otherwise.

---

## Adding a builtin and using it from Dark

A builtin is F#; the Dark that calls it is a package item. They are two sides of one change and
they belong in one PR.

1. Add the fn to the `fns` list in the right `Builtins/Libs/<module>.fs`. `AGENTS.md` says how to
   pick the subproject and why you return structured Dark values rather than strings.
2. Wrap it, once, in a Dark package fn. `tests/builtin` enforces exactly one wrapper and at least
   one caller: a builtin with two `Builtin.x` references fails, and so does one with none.
3. `scripts/dev/build`. The build now ends with:

       All 206 kernel refs resolve, and all 734 builtins this package set calls
       exist in this kernel.

   That second half is the check that matters here. It reads `package_builtin_deps`, which the fold
   fills from the real call graph -- not a grep over text, which is what it replaces and which stops
   being possible once `packages/` is gone.

**Removing or renaming a builtin is two steps, and the check enforces the order.** Land the package
change that stops calling it, move the pin forward, and only then remove the builtin. Doing it the
other way round fails the build with:

    this package set calls 1 builtin(s) this kernel does not have:
      Builtin.someBuiltinWeDeleted (v0)

Adding one is safe in a single step, because an older package set simply does not call it.

---

## Referencing a new package type or function from F#

This is the case that needs a branch, and the only one that does.

F# names package items through `PackageRefs`. Those resolve **from the store, by name**, with the
hash in `package-ref-hashes.txt` as a fallback and a shape check in between: a candidate whose
signature (fn) or declaration (type) does not match what this build was compiled against is refused,
loudly, and the pinned version is used. A type nobody has pinned yet -- one your branch has just
authored -- has nothing to compare against, so the store's answer is taken. That is what makes this
work at all.

    git checkout -b add-foo
    dark branch add-foo                      # same name; they travel together

    dark type /Darklang.LanguageTools.Foo '{ n: Int64 }'
    #   authored on the dark branch. Invisible from main, and from everyone else.

    #   ...add `let foo = p [] "Foo"` in PackageRefs.fs and use it...

    scripts/dev/build                        # compiles; regenerates refs; runs `refs check`
    scripts/packages/bundle export           # ~1KB of JSON: package-branch.json
    git commit -a                            # F# + package-ref-hashes.txt + the bundle

The bundle is how your package work reaches anyone else. Export is explicit, like `git add`.

**Order matters in one small way:** author the type before the F# that uses it RUNS. Adding the ref
and building is fine -- a ref is a lazy closure -- but the first code path that reaches an
unresolvable one raises.

### What your coworker does

Nothing special. They check out the branch and build.

    git checkout add-foo
    scripts/dev/build     # prepare-package-set imports the bundle automatically

Importing is automatic because checking out the git branch IS asking for that branch's package
code, and it is idempotent -- ops are content addressed, so a second import says "up to date".

**If they are on the wrong dark branch, the build tells them, completely, at build time:**

    1 kernel ref(s) do not resolve against this package set:
      type Darklang.LanguageTools.Foo

    git is on `add-foo` and there is a dark branch called `add-foo`,
    but you are on dark main. Try `dark switch add-foo`.

That is deliberate: the refs are lazy, so without this you would find out whenever some unrelated
command happened to reach that code path.

### Reviewing it

    scripts/packages/bundle show --source

Prints what the bundle changes, with the full declarations, by importing into a throwaway store.
Reviewing somebody's branch should not mean taking their ops into the store you work in.

---

## Trying it locally before it goes anywhere

Everything above is local already. Your store is yours: there are no restrictions on what you can
author or rebind on your own machine, including under `Darklang.*`.

To try the two-machine shape without a server, a second store is enough:

    dark branch export add-foo /tmp/b.json     # from one
    dark branch import /tmp/b.json             # into the other

To try it against a real server, run one:

    DARK_MATTER_WRITE_SECRET=<a long secret> dark serve Darklang.Matter.router --port 9090

`scripts/testing/gates seed-serving` and `gates server-folds` do exactly this and are the worked
examples if you want to see the whole thing driven end to end.

---

## Getting it to everybody else

    dark push                 your own namespace, straight to the server's main
    dark branch push <name>   anything else, including `Darklang.*`

**Your namespace is yours to publish to freely.** Log in, write a function, push it. No branch, no
PR, no waiting.

**`Darklang.*` is reviewed**, and the server refuses a direct push that binds it:

    403: this server does not accept pushes that bind Darklang.Stdlib.List.foo
    into its main. That namespace is reviewed: `dark branch push` instead, and
    it lands on main when the change is merged.

A branch push is always accepted, because a branch is isolated, nobody runs it, and review is what
moves it to main. That is also why an abandoned branch costs nothing: close the PR and the bundle
goes with it.

The server FOLDS what you push, so it shows up in `/m`, `/p` and in seeds. It never RUNS it: a
pushed `val` is folded, browsable and servable, and evaluated only on the machine that fetches it.

---

## The pin, and when it moves

`package-set.txt` names the commit a build's packages come from. It moves when somebody re-pins:

    scripts/packages/pin head --url <server>    # pin to what it has now
    scripts/packages/pin --show
    scripts/packages/pin --unset                # back to building from packages/

Every pin is checked before it is written, so a commit that does not resolve is refused then rather
than at the next CI run.

Re-pinning also regenerates `package-ref-hashes.txt`, and that is the only thing that does.
Ordinary package work leaves it alone, which is what stops two branches that both touch packages
conflicting in a 206-line generated file. The diff a pin bump produces is the point: one reviewable
list of every kernel identity that moved.

A pinned seed is cached per machine under `~/.darklang/seeds`, so the network is needed once per
pin, not once per clone.

---

## When the op-log format changes

A store does not get rebuilt from text any more, so it has to be carried forward in place.

    dark store                 format, which commit it was cut at, which build cut it
    dark store upgrade         move the op log to this build's format
    dark store rollback <n>    put back the copy `upgrade` took

`upgrade` copies the store first and does the rewrite in one transaction. It only rewrites BLOBS: a
change that moves content hashes is a much larger migration and it refuses rather than half-doing
it. It is a no-op until the first real format bump.

If you are running a build that reads an OLDER format than your store, it says so and names the file
to move back -- a `mv` needs no working binary, and by then you do not have one.

---

## Things that will bite

- **`dark log` on a branch shows that branch's OPS, not main's commits.** Ask `dark --branch main
  log` when you want commits.
- **A bundle left behind after its branch merges** is how somebody imports work from weeks ago.
  Going back to dark main removes it; `bundle export` on main will clean up a stale one.
- **Your main can drift ahead of the pin** if you `dark pull`. Then your branch may rest on commits
  nobody else has, and your bundle will not be enough for them. `bundle export` warns when it sees
  this.
- **Two branches adding the same name with different shapes** both merge, last-writer-wins picks
  one, and the loser's F# then references a name whose declaration is not what it compiled against.
  The compatibility check catches it at the second merge, so main goes red rather than silently
  wrong -- but it is a merge conflict git cannot see.
