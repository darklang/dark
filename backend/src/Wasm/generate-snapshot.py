#!/usr/bin/env python3
"""Generate the package snapshot the WASM REPL loads at startup.

Dumps all package ops from rundir/data.db as framed binary —
[36-byte ascii uuid][4-byte LE length][op blob] per op — matching the
reader in Repl.fs (LoadPackagesFromUrl). Re-run whenever packages change.

A manual script, not a builtin: the snapshot is a temporary seam (see the
CLEANUP marker in Repl.fs) and dies with it.
"""
import pathlib, shutil, sqlite3, struct

ROOT = pathlib.Path(__file__).resolve().parents[3]  # …/app
DB = ROOT / "rundir" / "data.db"
OUT = ROOT / "backend" / "src" / "Wasm" / "wwwroot" / "packages.snapshot"
# the copy the browser actually loads; refresh it too so a package-only
# change doesn't force a republish
PUBLISHED = ROOT / "rundir" / "wasm-repl" / "wwwroot" / "packages.snapshot"

# Committed main-branch ops only: uncommitted WIP (commit_hash IS NULL) and
# side-branch ops must not leak into the shipped snapshot.
rows = sqlite3.connect(DB).execute("""
    SELECT id, op_blob FROM package_ops
    WHERE commit_hash IS NOT NULL
      AND branch_id = (SELECT id FROM branches WHERE name = 'main')
    ORDER BY rowid ASC""").fetchall()
with open(OUT, "wb") as f:
    for id_, blob in rows:
        f.write(str(id_).encode())
        f.write(struct.pack("<I", len(blob)))
        f.write(blob)
print(f"wrote {len(rows)} package ops to {OUT}")
if PUBLISHED.parent.is_dir():
    shutil.copyfile(OUT, PUBLISHED)
    print(f"refreshed published copy at {PUBLISHED}")
