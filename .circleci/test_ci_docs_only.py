#!/usr/bin/env python3
"""scripts/deployment/ci-docs-only against throwaway git repos; no network needed."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "deployment" / "ci-docs-only"
ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
       "GIT_COMMITTER_EMAIL": "t@t"}


def git(cwd, *args):
  return subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True, text=True,
                        env={**os.environ, **ENV}).stdout.strip()


class DocsOnlyTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    self.root = Path(self.temporary.name)
    self.origin = self.root / "origin.git"
    self.work = self.root / "work"
    git(self.root, "init", "-q", "--bare", "-b", "main", str(self.origin))
    git(self.root, "init", "-q", "-b", "main", str(self.work))
    git(self.work, "remote", "add", "origin", self.origin.as_uri())
    self.commit({"README.md": "x", "backend/src/A.fs": "x", "docs/a.md": "x"})
    git(self.work, "push", "-q", "origin", "HEAD:main")
    git(self.work, "checkout", "-q", "-b", "topic")

  def commit(self, files, message="change"):
    for name, text in files.items():
      path = self.work / name
      path.parent.mkdir(parents=True, exist_ok=True)
      path.write_text(text)
      git(self.work, "add", name)
    git(self.work, "commit", "-q", "-m", message)
    return git(self.work, "rev-parse", "HEAD")

  def run_script(self, branch="pull/1", tag=""):
    sha = git(self.work, "rev-parse", "HEAD")
    git(self.work, "push", "-q", "-f", "origin", "HEAD:refs/heads/topic")
    # What the setup job has: this commit and its parent, fetched shallow.
    checkout = self.root / "checkout"
    subprocess.run(["rm", "-rf", str(checkout)], check=True)
    git(self.root, "init", "-q", str(checkout))
    git(checkout, "remote", "add", "origin", self.origin.as_uri())
    git(checkout, "fetch", "-q", "--depth=2", "origin", sha)
    git(checkout, "checkout", "-q", "--detach", "FETCH_HEAD")
    env = {**os.environ, "CIRCLE_BRANCH": branch, "CIRCLE_TAG": tag, "CIRCLE_SHA1": sha,
           "CIRCLE_PULL_REQUEST": ""}
    result = subprocess.run([str(SCRIPT)], cwd=checkout, env=env, capture_output=True, text=True)
    self.assertEqual(result.returncode, 0, result.stderr)
    return result.stdout.strip()

  def test_prose_only_skips(self):
    self.commit({"docs/a.md": "y", "README.md": "y", "docs/perf/new.md": "y"})
    self.commit({"later/notes/x.dark": "y", "later/plan.txt": "y"})
    self.assertEqual(self.run_script(), "true")

  def test_any_other_file_builds(self):
    self.commit({"docs/a.md": "y"})
    self.commit({"backend/src/A.fs": "y"})
    self.assertEqual(self.run_script(), "")

  def test_code_changed_earlier_in_the_branch_still_builds(self):
    self.commit({"backend/src/A.fs": "y"})
    self.commit({"docs/a.md": "y"})
    self.assertEqual(self.run_script(), "")

  def test_markdown_the_build_reads_builds(self):
    self.commit({"backend/src/Cli/README-to-embed.md": "y"})
    self.assertEqual(self.run_script(), "")
    self.commit({"packages/darklang/notes.md": "y"})
    self.assertEqual(self.run_script(), "")

  def test_non_markdown_under_docs_builds(self):
    self.commit({"docs/walkthrough-fixture.dark": "y"})
    self.assertEqual(self.run_script(), "")

  def test_main_and_tags_always_build(self):
    self.commit({"docs/a.md": "y"})
    self.assertEqual(self.run_script(branch="main"), "")
    self.assertEqual(self.run_script(branch="", tag="v0.0.1"), "")

  def test_a_branch_with_nothing_new_builds(self):
    self.assertEqual(self.run_script(), "")


if __name__ == "__main__":
  unittest.main()
