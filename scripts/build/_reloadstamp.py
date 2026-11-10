"""Whether the build's package reload would rebuild the store it already has.

A reload replaces the dev store's package ops with what `packages/` produces. Run again with the
same inputs into a store nobody has written to since, it produces the same store, for about
80 CPU-seconds. So the build skips it when both hold:
  - every input the reload reads hashes the same as at the last reload (`inputs()`), and
  - the store's op log is exactly as that reload left it (`ops_fingerprint()`).

The second matters as much as the first. A reload is also how a store gets back to the files after
ops were written to it, by hand (`dark fn`, `dark edit`) or by a test, and any op written since,
on any branch, means the reload runs as it always has.

The record sits beside the store, in `<store>.reload-stamp.json`, as dark-edits-in-dev's record
does. Only a reload the build ran, or an `apply-package-edits` standing in for one (`applied`), once it
succeeds, writes it; every reload deletes it as it
starts, so one that fails or is killed half way leaves no record that could vouch for the store.
`scripts/build/reload-packages` always reloads.
"""

import hashlib
import json
import os
import shutil
import sqlite3

REF_HASHES = "backend/src/LibExecution/package-ref-hashes.txt"


def local_exec_dir(optimize):
  if optimize:
    return "backend/Build/out/LocalExec/Release/net10.0/publish"
  return "backend/Build/out/LocalExec/Debug/net10.0"


def db_path():
  return os.path.join(os.environ["HOME"], ".darklang", os.getenv("DARK_CONFIG_DB_NAME", "data.db"))


def record_path():
  return db_path() + ".reload-stamp.json"


def _files_under(root, suffix=""):
  out = []
  for dirpath, _, names in os.walk(root):
    out += [os.path.join(dirpath, n) for n in names if n.endswith(suffix)]
  return sorted(out)


def _sha_file(path):
  h = hashlib.sha256()
  with open(path, "rb") as f:
    for block in iter(lambda: f.read(1 << 20), b""):
      h.update(block)
  return h.hexdigest()


def _dotnet_runtimes():
  """The shared runtimes LocalExec runs on; it ships only its own assemblies."""
  dotnet = shutil.which("dotnet")
  if dotnet is None:
    return []
  shared = os.path.join(os.path.dirname(os.path.realpath(dotnet)), "shared")
  try:
    return sorted(f"{fw}/{v}" for fw in os.listdir(shared) for v in os.listdir(os.path.join(shared, fw)))
  except OSError:
    return []


def inputs(optimize):
  """A hash of everything the reload reads, apart from package-ref-hashes.txt, which it also
  writes (see `record`):
    - the LocalExec it runs, and every file beside it: its own code, the parser, the checker and
      the builtins with their signatures, all compiled in; and the .NET runtimes it runs on;
    - its flags: Debug or Release, and every DARK_CONFIG_ setting (the store, the root dir);
    - every `.dark` file under packages/, a superset of the ones it loads;
    - the schema and its migrations, which the build's migration step applies first;
    - the two scripts between the build and LocalExec.
  Taken before the reload runs, so a file saved during it shows as changed afterwards.
  """
  h = hashlib.sha256()

  def add(label, value):
    h.update(f"{label}\0{value}\n".encode())

  add("configuration", "Release" if optimize else "Debug")
  for k in sorted(k for k in os.environ if k.startswith("DARK_CONFIG_")):
    add("env", f"{k}={os.environ[k]}")
  for rt in _dotnet_runtimes():
    add("runtime", rt)
  files = (_files_under(local_exec_dir(optimize))
           + _files_under("packages", ".dark")
           + _files_under("backend/migrations")
           + ["scripts/build/reload-packages", "scripts/run-local-exec"])
  for f in files:
    add(f, _sha_file(f))
  return h.hexdigest()


def _ref_hashes():
  return _sha_file(REF_HASHES) if os.path.exists(REF_HASHES) else ""


def ops_fingerprint():
  """Every op in the store, in order, with the commit it is in. Writing, deleting or committing an
  op moves it. None when there is no store to read."""
  if not os.path.exists(db_path()):
    return None
  try:
    con = sqlite3.connect(f"file:{db_path()}?mode=ro", uri=True, timeout=10)
  except sqlite3.Error:
    return None
  try:
    h = hashlib.sha256()
    n = 0
    for row in con.execute("SELECT id, branch_id, commit_hash FROM package_ops ORDER BY rowid"):
      h.update("|".join(str(c) for c in row).encode() + b"\n")
      n += 1
    return f"{n}:{h.hexdigest()}"
  except sqlite3.Error:
    return None
  finally:
    con.close()


def _read_record():
  try:
    with open(record_path()) as f:
      return json.load(f)
  except (OSError, ValueError):
    return None


def unchanged(inputs_hash):
  """True only when a reload now would rebuild the store as it stands."""
  rec = _read_record()
  if rec is None:
    return False
  fingerprint = ops_fingerprint()
  return (fingerprint is not None
          and rec.get("inputs") == inputs_hash
          and rec.get("ref_hashes") == _ref_hashes()
          and rec.get("ops") == fingerprint)


def applied(inputs_hash, apply):
  """Run <apply>, the dev build's `apply-package-edits`, in place of a reload, and record it as one
  when it succeeds. The record goes first, so an apply that fails or stops half way leaves none."""
  try:
    os.remove(record_path())
  except FileNotFoundError:
    pass
  ok = apply()
  if ok:
    record(inputs_hash)
  return ok


def record(inputs_hash):
  """After a successful reload: what it read, and the store it left."""
  rec = {"inputs": inputs_hash, "ref_hashes": _ref_hashes(), "ops": ops_fingerprint()}
  tmp = record_path() + ".tmp"
  with open(tmp, "w") as f:
    json.dump(rec, f)
  os.replace(tmp, record_path())

