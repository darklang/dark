#!/usr/bin/env python3
"""scripts/deployment/ci-superseded against throwaway git repos; no network needed."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "deployment" / "ci-superseded"


def git(cwd, *args):
  return subprocess.run(
    ["git", *args], cwd=cwd, check=True, capture_output=True, text=True,
    env={**os.environ, "GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
         "GIT_COMMITTER_EMAIL": "t@t"}).stdout.strip()


class SupersededTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    root = Path(self.temporary.name)
    self.origin = root / "origin.git"
    self.work = root / "work"
    git(root, "init", "-q", "--bare", "-b", "main", str(self.origin))
    git(root, "init", "-q", "-b", "main", str(self.work))
    git(self.work, "remote", "add", "origin", self.origin.as_uri())
    self.commit("first")
    self.built = self.commit("the commit this pipeline builds")

  def commit(self, message):
    git(self.work, "commit", "-q", "--allow-empty", "-m", message)
    git(self.work, "push", "-q", "-f", "origin", "HEAD:main")
    return git(self.work, "rev-parse", "HEAD")

  def run_script(self, branch="main", tag="", sha=None):
    # What the setup job has: this commit and its parent, fetched shallow.
    checkout = Path(self.temporary.name) / "checkout"
    subprocess.run(["rm", "-rf", str(checkout)], check=True)
    git(Path(self.temporary.name), "init", "-q", str(checkout))
    git(checkout, "remote", "add", "origin", self.origin.as_uri())
    git(checkout, "fetch", "-q", "--depth=2", "origin", sha or self.built)
    git(checkout, "checkout", "-q", "--detach", "FETCH_HEAD")
    env = {**os.environ, "CIRCLE_BRANCH": branch, "CIRCLE_TAG": tag, "CIRCLE_SHA1": sha or self.built}
    result = subprocess.run([str(SCRIPT)], cwd=checkout, env=env, capture_output=True, text=True)
    self.assertEqual(result.returncode, 0, result.stderr)
    return result.stdout.strip()

  def test_main_unchanged_builds(self):
    self.assertEqual(self.run_script(), "")

  def test_newer_main_commit_supersedes(self):
    self.commit("a")
    tip = self.commit("b")
    self.assertEqual(self.run_script(), tip)

  def test_branches_and_tags_always_build(self):
    self.commit("newer")
    self.assertEqual(self.run_script(branch="pull/123"), "")
    self.assertEqual(self.run_script(branch="", tag="v0.0.1"), "")

  def test_a_tip_ci_will_skip_does_not_supersede(self):
    self.commit("docs only [skip ci]")
    self.assertEqual(self.run_script(), "")
    self.commit("title\n\nbody says [ci skip]")
    self.assertEqual(self.run_script(), "")

  def test_a_tip_that_does_not_contain_this_commit_does_not_supersede(self):
    git(self.work, "checkout", "-q", "--orphan", "other")
    self.commit("rewritten history")
    self.assertEqual(self.run_script(), "")

  def test_no_reachable_origin_builds(self):
    self.commit("newer")
    subprocess.run(["rm", "-rf", str(self.origin)], check=True)
    self.assertEqual(self.run_script_without_origin(), "")

  def run_script_without_origin(self):
    checkout = Path(self.temporary.name) / "bare-checkout"
    git(Path(self.temporary.name), "init", "-q", str(checkout))
    git(checkout, "remote", "add", "origin", self.origin.as_uri())
    env = {**os.environ, "CIRCLE_BRANCH": "main", "CIRCLE_TAG": "", "CIRCLE_SHA1": self.built}
    result = subprocess.run([str(SCRIPT)], cwd=checkout, env=env, capture_output=True, text=True)
    self.assertEqual(result.returncode, 0, result.stderr)
    return result.stdout.strip()


if __name__ == "__main__":
  unittest.main()
