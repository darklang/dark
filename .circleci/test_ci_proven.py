#!/usr/bin/env python3
"""scripts/deployment/ci-proven against throwaway git repos and a stand-in for CircleCI's API;
no network needed."""

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


ROOT = Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "deployment" / "ci-proven"
ENV = {"GIT_AUTHOR_NAME": "t", "GIT_AUTHOR_EMAIL": "t@t", "GIT_COMMITTER_NAME": "t",
       "GIT_COMMITTER_EMAIL": "t@t"}

# Stands in for curl: answers with $FAKE_API_ANSWER, records the URL it was asked for, and
# fails like curl -f does when $FAKE_API_FAIL is set.
FAKE_CURL = """#!/usr/bin/env bash
for arg; do url=$arg; done
echo "$url" >> "$FAKE_API_LOG"
[[ -z "${FAKE_API_FAIL:-}" ]] || exit 22
cat "$FAKE_API_ANSWER"
"""


def git(cwd, *args):
  return subprocess.run(["git", *args], cwd=cwd, check=True, capture_output=True, text=True,
                        env={**os.environ, **ENV}).stdout.strip()


def job(revision, name="ok-to-deploy", workflow="build-and-deploy", status="success"):
  return {"vcs_revision": revision, "status": status, "build_url": f"https://ci/{name}/{revision[:7]}",
          "workflows": {"workflow_name": workflow, "job_name": name}}


class ProvenTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    self.root = Path(self.temporary.name)
    self.origin = self.root / "origin.git"
    self.work = self.root / "work"
    git(self.root, "init", "-q", "--bare", "-b", "main", str(self.origin))
    # GitHub serves a commit by hash even after a force push leaves no ref at it.
    git(self.origin, "config", "uploadpack.allowAnySHA1InWant", "true")
    git(self.origin, "config", "uploadpack.allowFilter", "true")
    git(self.root, "init", "-q", "-b", "main", str(self.work))
    git(self.work, "remote", "add", "origin", self.origin.as_uri())
    self.commit({"a.txt": "a"}, "first")
    git(self.work, "push", "-q", "origin", "HEAD:main")
    bin_dir = self.root / "bin"
    bin_dir.mkdir()
    (bin_dir / "curl").write_text(FAKE_CURL)
    (bin_dir / "curl").chmod(0o755)
    self.path = f"{bin_dir}:{os.environ['PATH']}"
    self.log = self.root / "api.log"
    self.answer = self.root / "answer.json"
    self.answer_with([])

  def commit(self, files, message="change"):
    for name, text in files.items():
      (self.work / name).write_text(text)
      git(self.work, "add", name)
    git(self.work, "commit", "-q", "--allow-empty", "-m", message)
    return git(self.work, "rev-parse", "HEAD")

  def answer_with(self, jobs):
    self.answer.write_text(json.dumps(jobs))

  def pr_head(self, files, number=7):
    """A PR branch off main, pushed where the PR's ref would be."""
    git(self.work, "checkout", "-q", "-b", f"pr{number}", "main")
    head = self.commit(files, "the PR")
    git(self.work, "push", "-q", "origin", f"HEAD:refs/pull/{number}/head")
    git(self.work, "checkout", "-q", "main")
    return head

  def merge(self, head, number=7):
    git(self.work, "merge", "-q", "--no-ff", "-m",
        f"Merge pull request #{number} from someone/topic\n\nThe PR", head)
    git(self.work, "push", "-q", "origin", "HEAD:main")
    return git(self.work, "rev-parse", "HEAD")

  def run_script(self, sha, branch="main", tag="", fail=False):
    # What the setup job has: this commit and its parents, fetched shallow.
    checkout = self.root / "checkout"
    subprocess.run(["rm", "-rf", str(checkout)], check=True)
    git(self.root, "init", "-q", str(checkout))
    git(checkout, "remote", "add", "origin", self.origin.as_uri())
    git(checkout, "fetch", "-q", "--depth=2", "origin", sha)
    git(checkout, "checkout", "-q", "--detach", "FETCH_HEAD")
    env = {**os.environ, "PATH": self.path, "CIRCLE_BRANCH": branch, "CIRCLE_TAG": tag,
           "CIRCLE_SHA1": sha, "CIRCLE_PROJECT_USERNAME": "darklang",
           "CIRCLE_PROJECT_REPONAME": "dark", "FAKE_API_LOG": str(self.log),
           "FAKE_API_ANSWER": str(self.answer)}
    if fail:
      env["FAKE_API_FAIL"] = "1"
    result = subprocess.run([str(SCRIPT)], cwd=checkout, env=env, capture_output=True, text=True)
    self.assertEqual(result.returncode, 0, result.stderr)
    return result.stdout.strip()

  def asked_for(self):
    return self.log.read_text().split() if self.log.exists() else []

  # Main

  def test_an_up_to_date_merge_is_proven_by_its_green_pr(self):
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job(head, name="build"), job(head)])
    self.assertEqual(self.run_script(merge), f"https://ci/ok-to-deploy/{head[:7]}")
    self.assertEqual(self.asked_for(), [
      "https://circleci.com/api/v1.1/project/github/darklang/dark/tree/pull%2F7"
      "?limit=100&shallow=true&filter=successful"])

  def test_a_merge_of_an_out_of_date_pr_builds(self):
    head = self.pr_head({"b.txt": "b"})
    self.commit({"c.txt": "c"}, "main moved on")
    merge = self.merge(head)
    self.answer_with([job(head)])
    self.assertEqual(self.run_script(merge), "")

  def test_a_pr_that_has_not_passed_does_not_prove(self):
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job(head, name="build", status="success")])
    self.assertEqual(self.run_script(merge), "")
    self.answer_with([job(head, status="failed")])
    self.assertEqual(self.run_script(merge), "")

  def test_only_a_real_build_proves(self):
    # A pipeline that was itself skipped has an ok-to-deploy in another workflow.
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job(head, workflow="proven")])
    self.assertEqual(self.run_script(merge), "")

  def test_another_green_commit_of_the_pr_does_not_prove_a_merge(self):
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job("0" * 40)])
    self.assertEqual(self.run_script(merge), "")

  def test_a_main_commit_that_is_not_a_pr_merge_builds(self):
    sha = self.commit({"b.txt": "b"}, "pushed straight to main")
    git(self.work, "push", "-q", "origin", "HEAD:main")
    self.answer_with([job(sha)])
    self.assertEqual(self.run_script(sha), "")
    self.assertEqual(self.asked_for(), [])

  def test_a_failed_request_builds(self):
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job(head)])
    self.assertEqual(self.run_script(merge, fail=True), "")
    self.answer.write_text("not json")
    self.assertEqual(self.run_script(merge), "")

  def test_tags_always_build(self):
    head = self.pr_head({"b.txt": "b"})
    merge = self.merge(head)
    self.answer_with([job(head)])
    self.assertEqual(self.run_script(merge, branch="", tag="v0.0.1"), "")

  # Branches and PRs

  def test_a_push_of_a_tree_the_branch_already_passed_is_proven(self):
    first = self.pr_head({"b.txt": "b"})
    git(self.work, "checkout", "-q", "pr7")
    git(self.work, "commit", "-q", "--amend", "-m", "the PR, reworded")
    again = git(self.work, "rev-parse", "HEAD")
    git(self.work, "push", "-q", "-f", "origin", "HEAD:refs/pull/7/head")
    self.answer_with([job(first)])
    self.assertEqual(self.run_script(again, branch="pull/7"), f"https://ci/ok-to-deploy/{first[:7]}")

  def test_a_push_that_changes_the_tree_builds(self):
    first = self.pr_head({"b.txt": "b"})
    git(self.work, "checkout", "-q", "pr7")
    second = self.commit({"b.txt": "changed"}, "more")
    git(self.work, "push", "-q", "origin", "HEAD:refs/pull/7/head")
    self.answer_with([job(first)])
    self.assertEqual(self.run_script(second, branch="pull/7"), "")

  def test_a_commit_is_not_its_own_proof(self):
    head = self.pr_head({"b.txt": "b"})
    self.answer_with([job(head)])
    self.assertEqual(self.run_script(head, branch="pull/7"), "")


if __name__ == "__main__":
  unittest.main()
