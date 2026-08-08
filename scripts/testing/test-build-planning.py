#!/usr/bin/env python3.12
"""What the build decides to do, and whether it's safe to run something.

  scripts/testing/test-build-planning.py

Covers the three modules behind `scripts/dev/build`: which actions a set of changed
files implies, whether a file counts as changed, and whether the tree is currently
built. Everything else now trusts those answers instead of grepping logs, so they're
worth testing directly; the alternative is finding out during a 75-second build.

No dev container needed and nothing is compiled: each test builds a small fake tree
in a temp directory and asks the modules about it.
"""

import os
import shutil
import sys
import tempfile
import json
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "build"))

import _buildindex  # noqa: E402
import _buildplan  # noqa: E402
import _buildstate  # noqa: E402


class TempTree:
  """A throwaway repo root, with the modules pointed at it."""

  def __enter__(self):
    self.dir = tempfile.mkdtemp(prefix="dev-build-test-")
    self.prev_cwd = os.getcwd()
    self.prev_rundir = os.environ.get("DARK_CONFIG_RUNDIR")

    os.makedirs(os.path.join(self.dir, "rundir"))
    self.write(".gitignore", "rundir/\nbackend/Build/\n")
    os.chdir(self.dir)
    os.environ["DARK_CONFIG_RUNDIR"] = os.path.join(self.dir, "rundir")

    # The ignore spec is cached per process and reads .gitignore from the cwd.
    _buildplan._spec = None
    return self

  def __exit__(self, *_):
    os.chdir(self.prev_cwd)
    if self.prev_rundir is None:
      os.environ.pop("DARK_CONFIG_RUNDIR", None)
    else:
      os.environ["DARK_CONFIG_RUNDIR"] = self.prev_rundir
    _buildplan._spec = None
    shutil.rmtree(self.dir, ignore_errors=True)

  def write(self, rel, contents):
    full = os.path.join(self.dir, rel)
    os.makedirs(os.path.dirname(full), exist_ok=True)
    with open(full, "w") as f:
      f.write(contents)
    return full

  def with_build_outputs(self):
    """Pretend a build already produced its binaries. check() looks for them."""
    for alternatives in _buildstate.EXPECTED_OUTPUTS:
      self.write(alternatives[0], "binary")
    return self


ALL = lambda _: True  # noqa: E731 - every file was covered


class Env:
  """Set an env var for the duration of a block, and put it back after."""

  def __init__(self, **vars):
    self.vars = vars

  def __enter__(self):
    self.previous = {k: os.environ.get(k) for k in self.vars}
    for k, v in self.vars.items():
      if v is None:
        os.environ.pop(k, None)
      else:
        os.environ[k] = v

  def __exit__(self, *_):
    for k, v in self.previous.items():
      if v is None:
        os.environ.pop(k, None)
      else:
        os.environ[k] = v


class TestMark(unittest.TestCase):
  """The file-to-action mapping."""

  def actions(self, path):
    should = _buildplan.Should()
    _buildplan.mark(should, path)
    return should

  def test_fsharp_source_takes_a_quick_build_in_dev_and_a_full_one_in_ci(self):
    # Both branches, explicitly. Asserting only the one this happens to run in is
    # how these passed locally and failed in CI.
    with Env(CI=None):
      self.assertTrue(
        self.actions("backend/src/LibDB/Queries.fs").backend_quick_build)
    with Env(CI="true"):
      self.assertTrue(
        self.actions("backend/src/LibDB/Queries.fs").backend_full_build)

  def test_fsproj_takes_a_full_build(self):
    self.assertTrue(self.actions("backend/src/LibDB/LibDB.fsproj").backend_full_build)

  def test_dark_package_reloads_packages(self):
    self.assertTrue(self.actions("packages/darklang/stdlib/list.dark")
                    .reload_all_packages)

  def test_migration_runs_migrations(self):
    self.assertTrue(self.actions("backend/migrations/001-init.sql").run_migrations)

  def test_unrecognised_file_routes_nowhere(self):
    should = self.actions("backend/testfiles/execution/foo.dark")
    self.assertEqual(should.chosen(), [])
    self.assertEqual(should.unrouted, ["backend/testfiles/execution/foo.dark"])


class TestExpand(unittest.TestCase):
  """What a plan cascades to, assuming each step works."""

  def expand(self, path, **kw):
    should = _buildplan.Should()
    _buildplan.mark(should, path)
    return _buildplan.expand(should, **kw).chosen()

  def test_fsharp_change_reaches_the_package_reload(self):
    with Env(CI=None):
      self.assertEqual(
        self.expand("backend/src/LibDB/Queries.fs"),
        ["backend_quick_build", "run_migrations", "reload_all_packages"])
    with Env(CI="true"):
      self.assertEqual(
        self.expand("backend/src/LibDB/Queries.fs"),
        ["backend_full_build", "run_migrations", "reload_all_packages"])

  def test_full_build_replaces_the_quick_one(self):
    actions = self.expand("backend/src/LibDB/LibDB.fsproj")
    self.assertIn("backend_full_build", actions)
    self.assertNotIn("backend_quick_build", actions)

  def test_tests_are_not_planned_unless_asked_for(self):
    self.assertNotIn("backend_test", self.expand("backend/src/LibDB/Queries.fs"))
    self.assertIn("backend_test",
                  self.expand("backend/src/LibDB/Queries.fs", run_tests=True))


class TestSatisfiedBy(unittest.TestCase):

  def test_a_full_build_satisfies_a_file_wanting_a_quick_one(self):
    # Otherwise every .fs in the tree looks unbuilt after a full build, because
    # none of them asked for the action that ran.
    needed = _buildplan.actions_for("backend/src/LibDB/Queries.fs")
    self.assertTrue(needed.issubset(_buildplan.satisfied_by({"backend_full_build"})))

  def test_a_quick_build_does_not_satisfy_a_file_wanting_a_full_one(self):
    needed = _buildplan.actions_for("backend/src/LibDB/LibDB.fsproj")
    self.assertFalse(needed.issubset(_buildplan.satisfied_by({"backend_quick_build"})))


class TestIgnores(unittest.TestCase):

  def test_editor_temp_files_are_ignored(self):
    # Two missing commas in this list used to concatenate the neighbouring patterns,
    # so these three reached the build.
    with TempTree():
      spec = _buildplan.spec()
      self.assertTrue(spec.match_file("packages/x.dark.tmp.123.abc"))
      self.assertTrue(spec.match_file("scripts/thing~"))
      self.assertTrue(spec.match_file(".devcontainer/devcontainer.json"))

  def test_gitignored_paths_are_ignored(self):
    with TempTree():
      self.assertTrue(_buildplan.spec().match_file("rundir/logs/build.log"))


class TestIndex(unittest.TestCase):
  """Content-addressed change detection."""

  def test_no_index_means_build_everything(self):
    with TempTree():
      self.assertIsNone(_buildindex.changed())

  def test_a_touched_file_with_the_same_contents_is_not_changed(self):
    # The whole reason the index hashes rather than stats: git checkout, rebase and
    # stash pop all move mtimes without changing a byte.
    with TempTree() as t:
      path = t.write("backend/src/A.fs", "let a = 1\n")
      _buildindex.save(_buildindex.snapshot())
      os.utime(path, (0, 0))
      self.assertEqual(_buildindex.changed(), [])

  def test_an_edited_file_is_changed(self):
    with TempTree() as t:
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildindex.save(_buildindex.snapshot())
      t.write("backend/src/A.fs", "let a = 2\n")
      self.assertEqual(_buildindex.changed(), ["backend/src/A.fs"])

  def test_a_deleted_file_is_changed(self):
    with TempTree() as t:
      path = t.write("packages/x.dark", "let x = 1\n")
      _buildindex.save(_buildindex.snapshot())
      os.remove(path)
      self.assertEqual(_buildindex.changed(), ["packages/x.dark"])

  def test_roots_narrow_the_comparison(self):
    with TempTree() as t:
      t.write("backend/src/A.fs", "1")
      t.write("scripts/thing.sh", "#!/bin/bash\n")
      _buildindex.save(_buildindex.snapshot())
      t.write("scripts/thing.sh", "#!/bin/bash\necho hi\n")
      self.assertEqual(_buildindex.changed(roots=["backend"]), [])
      self.assertEqual(_buildindex.changed(), ["scripts/thing.sh"])

  def test_a_file_needing_no_action_stops_being_offered(self):
    # A changed test fixture routes to nothing. If nothing records that, it stays in
    # the changed set forever and the tree reads as permanently behind.
    with TempTree() as t:
      t.write("backend/testfiles/execution/x.dark", "1")
      _buildindex.save(_buildindex.snapshot())
      t.write("backend/testfiles/execution/x.dark", "2")
      self.assertEqual(_buildindex.changed(),
                       ["backend/testfiles/execution/x.dark"])

      _buildindex.record_covered(["backend/testfiles/execution/x.dark"])
      self.assertEqual(_buildindex.changed(), [])

  def test_recording_one_file_does_not_claim_the_others(self):
    with TempTree() as t:
      t.write("backend/testfiles/execution/x.dark", "1")
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildindex.save(_buildindex.snapshot())
      t.write("backend/testfiles/execution/x.dark", "2")
      t.write("backend/src/A.fs", "let a = 2\n")

      _buildindex.record_covered(["backend/testfiles/execution/x.dark"])
      self.assertEqual(_buildindex.changed(), ["backend/src/A.fs"])

  def test_recording_with_no_baseline_is_a_no_op(self):
    with TempTree() as t:
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildindex.record_covered(["backend/src/A.fs"])
      # Still no index, so the next build is still a full one rather than one that
      # thinks a single recorded file means the tree is accounted for.
      self.assertIsNone(_buildindex.changed())

  def test_merge_refreshes_by_content_not_timestamp(self):
    # The headline guarantee: a file whose mtime moved but whose bytes didn't is not
    # a change. merge() has its own fast path, so this needs testing separately from
    # changed(); a mutation making merge compare mtimes went undetected without it.
    with TempTree() as t:
      path = t.write("backend/src/A.fs", "let a = 1\n")
      _buildindex.save(_buildindex.snapshot())

      os.utime(path, (1, 1))  # same bytes, different timestamp
      previous = _buildindex.load()
      merged = _buildindex.merge(previous, _buildindex.snapshot(previous),
                                 lambda _: False)  # covering nothing

      # Still current despite covering nothing, because the content is identical.
      _buildindex.save(merged)
      self.assertEqual(_buildindex.changed(), [])

      t.write("backend/src/A.fs", "let a = 2\n")
      merged = _buildindex.merge(_buildindex.load(),
                                 _buildindex.snapshot(_buildindex.load()),
                                 lambda _: False)
      _buildindex.save(merged)
      self.assertEqual(_buildindex.changed(), ["backend/src/A.fs"])

  def test_merge_leaves_uncovered_files_looking_changed(self):
    # A build of one .dark file mustn't claim to have built a changed .fs.
    with TempTree() as t:
      t.write("backend/src/A.fs", "let a = 1\n")
      t.write("packages/x.dark", "let x = 1\n")
      _buildindex.save(_buildindex.snapshot())

      t.write("backend/src/A.fs", "let a = 2\n")
      t.write("packages/x.dark", "let x = 2\n")

      previous = _buildindex.load()
      snapshot = _buildindex.snapshot(previous)
      only_packages = lambda rel: rel.startswith("packages/")  # noqa: E731
      _buildindex.save(_buildindex.merge(previous, snapshot, only_packages))

      self.assertEqual(_buildindex.changed(), ["backend/src/A.fs"])


class TestState(unittest.TestCase):

  def test_a_finished_build_is_ok_and_the_tree_is_clean(self):
    with TempTree() as t:
      t.with_build_outputs()
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildstate.begin(["backend_quick_build"], ["backend/src/A.fs"], "build")
      _buildstate.finish(True, ["backend_quick_build"])
      _buildindex.save(_buildindex.snapshot())

      ready, fatal, _ = _buildstate.check()
      self.assertTrue(ready)
      self.assertFalse(fatal)

  def test_a_failed_build_is_fatal_for_anything_downstream(self):
    with TempTree() as t:
      # Outputs present and a source that's outstanding against the index, so this
      # exercises the failed-build path rather than the missing-binary one.
      t.with_build_outputs()
      t.write("backend/src/A.fs", "broken\n")
      _buildstate.begin(["backend_quick_build"], [], "build")
      _buildstate.finish(False, ["backend_quick_build"], "backend_quick_build")

      ready, fatal, lines = _buildstate.check()
      self.assertFalse(ready)
      self.assertTrue(fatal)
      self.assertIn("backend_quick_build", " ".join(lines))

  def test_reverting_a_change_that_broke_the_build_unsticks_you(self):
    # A failed status describes an attempt, not the tree. Break a file, fail a build,
    # revert it: the build correctly has nothing to do, so if `failed` stayed fatal
    # the status would never clear and run-cli would refuse forever.
    with TempTree() as t:
      t.with_build_outputs()
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildstate.begin(["backend_quick_build"], [], "build")
      _buildstate.finish(True, ["backend_quick_build"])
      _buildindex.save(_buildindex.snapshot())

      t.write("backend/src/A.fs", "broken\n")
      _buildstate.begin(["backend_quick_build"], [], "build")
      _buildstate.finish(False, ["backend_quick_build"], "backend_quick_build")

      # Still broken on disk: refusing is right.
      ready, fatal, _ = _buildstate.check()
      self.assertFalse(ready)
      self.assertTrue(fatal)

      t.write("backend/src/A.fs", "let a = 1\n")  # revert
      ready, fatal, _ = _buildstate.check()
      self.assertTrue(ready, "reverting should unstick you without a build")
      self.assertFalse(fatal)

  def test_an_edit_after_a_build_warns_but_is_not_fatal(self):
    # Editing and then running is normal, so this must not block the CLI.
    with TempTree() as t:
      t.with_build_outputs()
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildstate.begin(["backend_quick_build"], [], "build")
      _buildstate.finish(True, ["backend_quick_build"])
      _buildindex.save(_buildindex.snapshot())

      t.write("backend/src/A.fs", "let a = 2\n")
      ready, fatal, lines = _buildstate.check()
      self.assertFalse(ready)
      self.assertFalse(fatal)
      self.assertIn("behind the tree", " ".join(lines))

  def test_either_configuration_counts_as_built(self):
    # The question is whether something wiped backend/Build, not which
    # configuration you built. Listing only Debug refused `run-cli --published`.
    with TempTree() as t:
      for alternatives in _buildstate.EXPECTED_OUTPUTS:
        t.write(alternatives[1], "release binary")
      self.assertEqual(_buildstate.missing_outputs(), [])

  def test_a_wiped_build_directory_beats_a_clean_index(self):
    # The index tracks sources, so wiping backend/Build leaves every source
    # matching. Without this the build says "nothing has changed" while run-cli
    # says "no binary, run the build", and there's no way out of that loop.
    with TempTree() as t:
      t.with_build_outputs()
      t.write("backend/src/A.fs", "let a = 1\n")
      _buildstate.begin(["backend_quick_build"], [], "build")
      _buildstate.finish(True, ["backend_quick_build"])
      _buildindex.save(_buildindex.snapshot())

      self.assertEqual(_buildstate.missing_outputs(), [])
      ready, fatal, _ = _buildstate.check()
      self.assertTrue(ready)

      os.remove(_buildstate.EXPECTED_OUTPUTS[0][0])
      self.assertEqual(_buildindex.changed(), [])  # sources really are unchanged
      ready, fatal, lines = _buildstate.check()
      self.assertFalse(ready)
      self.assertTrue(fatal)
      self.assertIn("build output is missing", " ".join(lines))

  def test_a_lint_only_build_does_not_claim_the_tree_is_built(self):
    # shellcheck passing doesn't make the binaries any newer. If it moved the mark,
    # a .fs edited in the same second would look built.
    self.assertFalse(_buildstate.built_successfully(["shellcheck"]))
    self.assertTrue(_buildstate.built_successfully(["backend_quick_build"]))

  def test_a_skipped_build_step_does_not_count_as_built(self):
    self.assertFalse(
      _buildstate.built_successfully(["backend_quick_build"],
                                     failed_action="backend_quick_build",
                                     skipped=["reload_all_packages"]))

  def test_an_interrupted_build_is_reported_rather_than_believed(self):
    with TempTree():
      _buildstate.begin(["backend_quick_build"], [], "build")
      state = _buildstate.read()
      state["pid"] = 2 ** 22  # a pid that cannot be running
      _buildstate._write(state)

      ready, fatal, lines = _buildstate.check()
      self.assertFalse(ready)
      self.assertTrue(fatal)
      self.assertIn("didn't finish", " ".join(lines))


class TestReleaseImage(unittest.TestCase):
  """The release image pins the .NET SDK, and so does global.json.

  They have to agree. global.json sets rollForward: disable, so a mismatch
  isn't a soft warning: the release build refuses to run at all, with an error
  that names an SDK version rather than the Dockerfile. Cheap to catch here.
  """

  def _repo_root(self):
    # scripts/testing/<this file> -> three levels up is the repo root.
    here = os.path.abspath(__file__)
    return os.path.dirname(os.path.dirname(os.path.dirname(here)))

  def test_release_dockerfile_sdk_matches_global_json(self):
    root = self._repo_root()
    dockerfile = os.path.join(root, "Dockerfile.aot-release")
    globaljson = os.path.join(root, "backend", "global.json")
    if not os.path.exists(dockerfile):
      self.skipTest("no Dockerfile.aot-release")

    with open(globaljson) as f:
      pinned = json.load(f)["sdk"]["version"]

    with open(dockerfile) as f:
      contents = f.read()
    match = re.search(r"ARG DOTNET_SDK_VERSION=([0-9][^\s]*)", contents)
    self.assertIsNotNone(
      match, "Dockerfile.aot-release should pin the SDK via ARG DOTNET_SDK_VERSION")
    self.assertEqual(
      match.group(1), pinned,
      "Dockerfile.aot-release SDK version must match backend/global.json")


if __name__ == "__main__":
  unittest.main(verbosity=2)
