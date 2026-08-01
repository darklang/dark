"""Which build actions a set of changed files implies.

Split out of scripts/build/compile so that `scripts/dev/plan` can print the mapping
without running it. The docs used to describe this mapping in prose, which meant it
could drift; now there is one copy and you can ask it.

Pure: nothing here runs a build or touches the filesystem, apart from reading
.gitignore and stat-ing files to find what changed.
"""

import os
import subprocess

import pathspec

def in_ci():
  """Read at call time, not import time, so tests can exercise both branches.

  As a constant it silently encoded whichever environment happened to import this
  module, which is how two tests passed here and failed in CI.
  """
  return os.getenv("CI") == "true"


def packages_from_seed():
  return os.getenv("DARK_CONFIG_PACKAGES_SOURCE") == "seed"


class Should:
  """The actions a build will take. Field order is the order execute() runs them."""

  # Ordered so callers can print a plan that matches what happens.
  ACTIONS = [
    "fsharp_tool_restore",
    "fsharp_paket_restore",
    "fsharp_paket_install",
    "backend_full_build",
    "backend_quick_build",
    "run_migrations",
    "reload_all_packages",
    "backend_test",
    "circleci_validate",
    "shellcheck",
    "yamllint",
  ]

  def __init__(self):
    self.fsharp_tool_restore = False
    self.fsharp_paket_restore = False
    self.fsharp_paket_install = False
    self.backend_quick_build = False
    self.backend_full_build = False
    self.backend_test = False
    self.reload_all_packages = False
    self.circleci_validate = False
    self.run_migrations = False
    self.shellcheck = []
    self.yamllint = []
    # Files that route to no action at all: docs, test fixtures, anything mark()
    # doesn't recognise. Collected rather than printed, so a hundred of them are one
    # line in the plan instead of a hundred interleaved with the build's output.
    self.unrouted = []

  def chosen(self):
    """The actions that are on, in the order they will run."""
    return [a for a in self.ACTIONS if getattr(self, a)]

  def is_empty(self):
    return self.chosen() == []


def is_script(f):
  filetype = subprocess.check_output(["file", f], encoding="utf-8")
  return "Bourne-Again" in filetype


def mark(should, f):
  if f == "backend/global.json" or \
      f == "backend/.config/dotnet-tools.json" or \
      f == "backend/Directory.Build.props":
    should.fsharp_tool_restore = True

  elif "backend/paket.dependencies" == f:
    should.fsharp_paket_install = True

  elif f.startswith("backend/") and f.endswith("paket.references"):
    should.fsharp_paket_restore = True

  elif f.startswith("backend/") and \
    (f.endswith(".fsproj") or f.endswith(".sln")):
    should.backend_full_build = True

  elif f.startswith("backend/") and \
       (f.endswith(".fs") or f.endswith(".fsi")):
    if in_ci():
      # CI has no incremental state to protect, and wants the stricter build.
      should.backend_full_build = True
    else:
      should.backend_quick_build = True

  elif f.startswith("backend/tests/"):
    should.backend_test = True

  elif f.startswith("packages/") and f.endswith(".dark"):
    if packages_from_seed():
      should.unrouted.append(f)  # packages come from the seed DB, not from disk
    else:
      should.reload_all_packages = True

  elif f.startswith("backend/migrations/"):
    should.run_migrations = True

  elif f == "scripts/build/run-migrations":
    should.shellcheck += [f]
    should.run_migrations = True

  elif f == ".circleci/config.yml":
    should.circleci_validate = True

  elif (f.startswith("scripts/") or f.endswith(".sh")) and is_script(f):
    should.shellcheck += [f]

  elif f.endswith(".yml") or f.endswith(".yaml"):
    should.yamllint += [f]

  else:
    should.unrouted.append(f)

  return should


def actions_for(f, run_tests=False):
  """The actions a single file directly needs. Used to decide, after a build, which
  files it can honestly claim to have covered."""
  should = Should()
  mark(should, f)
  needed = set(should.chosen())
  if not run_tests:
    # Tests only run when asked, so a file that only wants them needs nothing.
    needed.discard("backend_test")
  return needed


def satisfied_by(succeeded):
  """What a set of successful actions amounts to.

  A full build compiles everything a quick build would, so a file asking for a quick
  build is satisfied by a full one. Without this, a full build leaves every .fs in
  the tree looking unbuilt, since none of them asked for the action that ran.
  """
  s = set(succeeded)
  if "backend_full_build" in s:
    s.add("backend_quick_build")
  return s


def expand(should, run_tests=False):
  """What will run if every step succeeds.

  execute() cascades on success (a build that fails doesn't trigger the reload that
  would follow it), so this is the optimistic case, which is the useful one to show
  before starting. What actually ran is recorded in build-state.json afterwards.

  Returns a fresh Should; the argument isn't modified.
  """
  s = Should()
  s.__dict__.update({k: (list(v) if isinstance(v, list) else v)
                     for k, v in should.__dict__.items()})

  if s.fsharp_tool_restore:
    s.fsharp_paket_restore = True
    s.backend_full_build = True
  if s.fsharp_paket_restore or s.fsharp_paket_install:
    s.backend_full_build = True

  if s.backend_full_build or s.backend_quick_build:
    if s.backend_full_build:
      s.backend_quick_build = False  # execute() does the same; no need for both
    s.run_migrations = True
    if run_tests:
      s.backend_test = True

  if s.run_migrations and not packages_from_seed():
    s.reload_all_packages = True

  # backend_test is set by execute() on any build, but run_test() no-ops without
  # --test, so a plan that lists it would be lying.
  if not run_tests:
    s.backend_test = False

  return s


def get_ignored_files():
  """.gitignore plus files that shouldn't drive a build."""
  with open(".gitignore", 'r') as file:
    gitignores = file.readlines()

  # Two commas were missing here, and Python quietly concatenated the neighbours, so
  # `*.tmp*`, `*~` and `devcontainer.json` were never ignored and editor temp files
  # reached the build. One pattern per line, trailing comma, so the next one is loud.
  ignores = [
    ".git/",
    "scripts/build/compile",
    "backend/testfiles/serialization-artifacts/",
    "containers/",
    "Dockerfile",
    ".devcontainer/devcontainer.json",
    "*.tmp*",
    "*.md",
    "*.sw",
    "*/.#",
    "*~",
    "backend/src/LibExecution/package-ref-hashes.txt",
  ]

  all_ignores = gitignores + ignores
  return pathspec.GitIgnoreSpec.from_lines(all_ignores)


_spec = None


def spec():
  global _spec
  if _spec is None:
    _spec = get_ignored_files()
  return _spec


def relative(f, root="/home/dark/app"):
  return os.path.relpath(f, root)


def keep(files, root="/home/dark/app"):
  """Filter to the files a build should care about, relative to the repo root."""
  kept = []
  for f in files:
    f = relative(f, root)
    if not spec().match_file(f):
      kept.append(f)
  return kept


# Directories that never hold build inputs. Walking them costs more than the
# .gitignore match that would exclude them anyway.
PRUNE = {".git", "node_modules", "obj", "bin"}


def source_files(roots=None, root="."):
  """Every file a build cares about, repo-relative.

  Walks rather than asking git, so a file you've written but not added still counts.
  `roots` narrows the walk to some subdirectories; None means the whole repo.
  """
  out = []
  root_abs = os.path.abspath(root)
  starts = [os.path.join(root_abs, r) for r in roots] if roots else [root_abs]

  for start in starts:
    if not os.path.isdir(start):
      continue
    for dirpath, dirnames, filenames in os.walk(start):
      rel_dir = os.path.relpath(dirpath, root_abs)
      if rel_dir == ".":
        rel_dir = ""
      dirnames[:] = [
        d for d in dirnames
        if d not in PRUNE and not spec().match_file(os.path.join(rel_dir, d) + "/")
      ]
      for name in filenames:
        rel = os.path.join(rel_dir, name) if rel_dir else name
        if not spec().match_file(rel):
          out.append(rel)
  return out


def changed_since(when, roots=None, root="."):
  """Repo-relative source files with an mtime after `when` (epoch seconds)."""
  changed = []
  root_abs = os.path.abspath(root)
  for rel in source_files(roots, root):
    try:
      if os.stat(os.path.join(root_abs, rel)).st_mtime > when:
        changed.append(rel)
    except FileNotFoundError:
      continue
  return sorted(changed)
