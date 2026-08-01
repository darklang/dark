#!/usr/bin/env python3.12
"""What the last successful build was built from, file by file.

Timestamps alone are not enough to answer "has this changed". `git checkout`, a
rebase, a stash pop and a bare `touch` all move mtimes without changing a byte, and
every one of those is routine. Going by mtime, an agent that switches branches and
switches back pays for a full 75-second build that changes nothing.

So this records content: a hash per file, with mtime and size as a fast path so the
common case costs a stat rather than a read. What the build actually consumed is
snapshotted when it starts and committed only if it succeeds, which means a file
saved mid-build correctly shows up as changed afterwards.
"""

import hashlib
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import _buildplan  # noqa: E402

VERSION = 1


def rundir():
  return os.getenv("DARK_CONFIG_RUNDIR", "rundir")


def path():
  return os.path.join(rundir(), "build-index.json")


def load():
  """The index from the last successful build, or None."""
  try:
    with open(path()) as f:
      data = json.load(f)
  except (FileNotFoundError, json.JSONDecodeError):
    return None
  if data.get("version") != VERSION:
    return None
  return data.get("files") or {}


def _hash(abspath):
  h = hashlib.blake2b(digest_size=16)
  with open(abspath, "rb") as f:
    for chunk in iter(lambda: f.read(1 << 20), b""):
      h.update(chunk)
  return h.hexdigest()


def snapshot(previous=None, root="."):
  """Hash every source file, reusing `previous` where mtime and size still match.

  Returns {relpath: [mtime_ns, size, hash]}.
  """
  previous = previous or {}
  root_abs = os.path.abspath(root)
  out = {}

  for rel in _buildplan.source_files(root=root):
    abspath = os.path.join(root_abs, rel)
    try:
      st = os.stat(abspath)
    except OSError:
      continue
    mtime, size = st.st_mtime_ns, st.st_size

    cached = previous.get(rel)
    if cached and cached[0] == mtime and cached[1] == size:
      out[rel] = cached  # unchanged since we last looked; no need to read it
      continue

    try:
      out[rel] = [mtime, size, _hash(abspath)]
    except OSError:
      continue

  return out


def changed(index=None, roots=None, root="."):
  """Source files whose content differs from the last successful build.

  None means there's no index to compare against, which is not the same as "nothing
  changed" and callers should treat it as "build everything".
  """
  index = index if index is not None else load()
  if index is None:
    return None

  current = snapshot(index, root=root)

  def in_roots(rel):
    return roots is None or any(rel == r or rel.startswith(r + os.sep) for r in roots)

  diffs = [rel for rel, entry in current.items()
           if in_roots(rel) and (rel not in index or index[rel][2] != entry[2])]

  # A deleted file changes the build too: a removed .dark has to leave the package DB.
  diffs += [rel for rel in index if in_roots(rel) and rel not in current]

  return sorted(set(diffs))


def merge(previous, snapshot, covered):
  """The new baseline after a build that covered only some of the tree.

  `covered(relpath) -> bool` says whether this build actually accounted for that
  file. A build of one .dark file doesn't make a changed .fs any newer, so its entry
  has to keep the old content and stay listed as changed.
  """
  previous = previous or {}
  out = dict(previous)

  for rel, entry in snapshot.items():
    old = previous.get(rel)
    if old is not None and old[2] == entry[2]:
      out[rel] = entry  # same content; refresh mtime so we don't rehash it again
    elif covered(rel):
      out[rel] = entry

  for rel in list(out):
    if rel not in snapshot and covered(rel):
      del out[rel]

  return out


def record_covered(paths):
  """Mark these paths as accounted for, leaving every other entry alone.

  For files that map to no build action: a changed test fixture needs nothing done
  to it, but if nothing ever records that, it stays in the changed set and leaves
  the tree looking permanently behind.
  """
  paths = set(paths)
  if not paths:
    return
  previous = load()
  if previous is None:
    return  # no baseline to amend; the next full build makes one
  save(merge(previous, snapshot(previous), lambda rel: rel in paths))


def save(files):
  """Commit a snapshot as the new baseline. Written atomically."""
  os.makedirs(rundir(), exist_ok=True)
  tmp = path() + ".tmp"
  with open(tmp, "w") as f:
    json.dump({"version": VERSION, "files": files}, f)
    f.write("\n")
  os.replace(tmp, path())


def clear():
  try:
    os.remove(path())
  except FileNotFoundError:
    pass
