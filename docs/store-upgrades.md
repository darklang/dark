# Store upgrades

How a dark carries an existing store (`~/.darklang/data.db`, or a dev clone's `rundir/data.db`) to the shape it expects, and what to do when your change needs that. Read it before touching a table, a column, a stored blob's layout, or anything under `backend/migrations/`.

---

## What happens when a dark opens a store

`LibDB.Releases.upgrade`, called by the published CLI (`Cli/EmbeddedResources.fs`) and by LocalExec alike:

1. the base schema's tables (`backend/migrations/schema/*.sql`), `CREATE TABLE IF NOT EXISTS`, so a table that already exists keeps the shape it had;
2. the upgrade units this store hasn't run (`backend/migrations/upgrades/`), in order;
3. the base schema's indexes, once every column they name exists.

A published binary then tops the store up with its own packages, folds them, and checks the store holds every package item it pins (`Seed.missingPins`). Only then is the store stamped current (`store_stamp_v0`). If any step fails, a published dark puts back the copy taken before step 1 (`backups/data.db.before-upgrade-to-<build>`) and exits 3, saying what failed (or, if putting it back fails too, says where the copy is). A store is never left half-upgraded and stamped.

An upgrade is one way in place: the store's blobs are in this dark's format afterwards, which an older dark can't read. The way back is the backup, which is the store exactly as it was; the success line names it.

Which stores open:

- From v0.0.35 on: upgraded, keeping everything.
- Last upgraded by a dev build between v0.0.34 and v0.0.35 (no `branch_ops`, but older than any release since): upgraded. Ops written in that window's vocabulary that this dark can't read are kept as they were, unapplied, and the upgrade says how many.
- v0.0.34 and earlier (the ones with a `branch_ops` table, from before the source-control rewrite): refused, data put back. Their ops say format 1 in a vocabulary today's reader can't decode, so carrying them forward would keep the furniture and lose the person's functions.
- A store a NEWER dark wrote, in a format this one can't read: not opened; the message points at the dark that wrote it.

`dark store` says which build upgraded a store, which upgrades it has had (and any this dark doesn't carry), and its ops per format. `dark store upgrades` lists every upgrade with its time.

---

## Which tool for which change

- A new table, a new column, a new index: a SHAPE unit. `scripts/migrations/new <tag>` makes `backend/migrations/upgrades/<stamp>_<tag>/` with `upgrade.toml` and `shape.sql`.
- Rewriting rows, moving data between tables: a unit with `step.fs` instead of `shape.sql` (a unit has one or the other; for both, write two units, the second after the first). `let run () : unit`, in module `LibDB.UpgradeSteps.U<directory name>`.
- A change to what a blob serializer writes, or to what a reader accepts: a FORMAT bump. See the blob formats section; it is not optional.
- A change to a merged base schema file: never. It changes fresh stores only, and every older store keeps the old shape forever (that is how `propagation_policy` outlived its feature). Write a unit.

A unit's `upgrade.toml` takes two keys and refuses any other, by name. Its leading comment is where it says what it does, and is the only place that says so, so write one:

    # what this upgrade does, and why it is safe against a store that already has it
    after = ["20261008_000001_..."]     # the units it must follow
    touches = ["traces"]                # the tables it changes

Which files it has says what kind it is: `shape.sql` for a change of shape, `step.fs` for code. One or the other; a unit with neither or both is refused, naming the directory.

`shape.sql` holds statements safe to run twice, plus one form SQLite can't say: `add-column <table> <column> <declaration>`, which looks before it acts.

Rules the runner enforces:

- Order is `after`, then name. Two units touching the same table must be ordered; if neither reaches the other, the runner refuses and names the line to add to the later one. That's an edit to your own unit, never to a shared file. Usually `after` names the last unit to touch your table: `grep -l '"<table>"' backend/migrations/upgrades/*/upgrade.toml`.
- A shape unit runs in one transaction with its record: all of it or none of it, and not recorded unless it all ran. A unit with `step.fs` is recorded after its code returns; make the code safe to run again.
- A store remembers a unit by its directory name. Never rename one.

The list of units is generated at build (`scripts/build/_gen-upgrade-registry`), not committed, so two PRs that each add a unit don't conflict.

---

## Blob formats

Package ops, toplevels, function bodies and values are stored as binary blobs (`LibSerialization/Binary/`), each behind a header carrying its format version (`BaseFormat.CurrentVersion`).

A format is a contract with two halves, and both are tested:

- today's writer produces the committed bytes: `backend/testfiles/serialization-artifacts/<type>.bin`, one file per stored type holding every test value's bytes;
- every file ever committed still decodes: `serialization-artifacts/history/`, kept forever under each file's hash.

The values are in `backend/tests/Tests/Serialization.TestValues.fs`, chosen so every tag a live writer can emit is reached. Dev regenerates the files on every test run, so a change shows as a git diff; CI doesn't, so a change nobody meant fails there. The files are committed, not generated, because they are the record of what an earlier writer produced: regenerating them from today's code would make every check a tautology.

So, when you change a serializer:

1. Bump `CurrentVersion`. Keep the old reader beside the new writer, dispatching on the version the reader is handed.
2. Add a format unit after the last one, whose `step.fs` is `let run () : unit = LibDB.Upgrades.rewriteBlobsToCurrentFormat ()`. It rewrites every op and toplevel older than the current format, in place, keeping each op's id, and leaves an op it can't read as it was. If any package body (`pt_def`) is older, it empties the projection tables and marks the log unapplied, so the fold rebuilds them in the current format before the store is stamped: today's readers raise on an older body, and the fold itself reads one.
3. Add test values for the new shape, and commit the regenerated `<type>.bin` and the new `history/` file together.

Never:

- delete a file from `history/`, or a case a reader decodes, retired or not. A red "every golden ever committed still decodes" means a reader was lost. This has happened: a commit removed the retired `DecisionKind.Propagation` reader and its test value together, and stores holding pin/follow decisions stopped reading them.
- change what the bytes mean without changing the number. The source-control rewrite did, so a v0.0.34 store's ops say format 1 in a vocabulary today's reader can't decode.
- build a test value from anything outside the format: no `gid ()` (random ids), no hash from the gitignored `package-ref-hashes.txt`. Either makes a golden change with no format change.

---

## Testing an upgrade

- `./scripts/run-backend-tests --filter tests/Upgrades`: the runner (order, the problems it refuses, transactions, the blob rewrite).
- A test that empties the package tables runs against the store every other test shares, and the fold alone leaves values unevaluated: put it in `tests/OpsProjections`, which is sequenced and runs late, and compare a binding fingerprint with a plain `Seed.rebuildProjections`, not a count (other tests edit `locations` directly). In `tests/Upgrades` one such test took the tests after it down with `ValueNotFound`.
- `scripts/dev/build` runs your unit against the dev store; `rundir/logs/migrations.log` lists what ran, and `./scripts/run-cli store` shows the store afterwards.
- `./scripts/testing/gates old-release-stores`, after `scripts/build/build-release-cli-exes.sh`. It downloads pinned releases, lets each author a function and a config value in a store of its own, then opens that store with this tree's published binary. A refusal must put the data back and leave the old release able to open it. An upgrade must keep the work, keep every op id, leave no op reference dangling, put every op it can read and every package body at the current format (exactly as many ops may stay older as the upgrade says it couldn't read), and leave the store's tables, columns and indexes equal to a fresh store's; and the backup must be the store exactly as it was, which the old release still opens. Every answer is checked as an exact line and an exit code, never a substring: a crash's stack trace contains `+ 0x42`. One pinned store is not a release: v0.0.35's store reshaped as a dev build before it left it, because every release store already had a column that such a store lacks. It is the only thing that exercises the shipped path, so run it after anything here and after a rebase.
- To pin another release, add a line to the list at the top of `gate_old_release_stores` in `scripts/testing/_gates-store`: tag, `upgrades` or `refuses`, the sha256 of the unzipped linux-x64 binary, and the release asset's name.

Walking one by hand: a release binary keeps its store beside itself (`<exe dir>/.darklang`), so copy it into an empty directory and run it with `env -i HOME=<that dir> PATH=/usr/bin:/bin`. Never point an experiment at `~/.darklang`: that is a real person's store.

---

## Traps

- `INSERT OR IGNORE` into a table with a `NOT NULL` column you didn't fill drops every row without a word. That is how the July store lost the release's ops and was stamped anyway. Insert, then count what landed.
- An index in a base schema file can name a column only a unit adds, which is why indexes come after units.
- Inside the dev container, an early release reads `package-ref-hashes.txt` from `/home/dark/app/`, which is this clone, and dies on today's pins. The gate renames that path inside its copy of the binary; do the same when walking one by hand.
- A same-shaped record anywhere in the store IS your type, since types are content-addressed. A test type `{ text: String }` showed up in `dark db list` as somebody's `TranslationJson`. Use field names nobody else would.
- Fantomas rewrote an exception abbreviation (`exception X = Y.X`) into a new exception with no fields. Don't alias exceptions; and build once after formatting.
- `scripts/dev/build` after reverting an experiment can say "nothing has changed since the last successful build" while `scripts/dev/status` still shows the experiment's failure. `scripts/dev/build backend/global.json` forces a real one.
