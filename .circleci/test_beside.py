#!/usr/bin/env python3
""".circleci/beside: a background step's verdict reaches the step that waits for it."""

import os
from pathlib import Path
import subprocess
import tempfile
import threading
import time
import unittest


SCRIPT = Path(__file__).resolve().parent / "beside"


class BesideTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    self.env = {**os.environ, "BESIDE_DIR": self.temporary.name}

  def run_beside(self, *args):
    return subprocess.run([str(SCRIPT), *args], env=self.env, capture_output=True, text=True,
                          stdin=subprocess.DEVNULL)

  def test_a_pass_is_a_pass(self):
    start = self.run_beside("start", "ok", "--", "bash", "-c", "echo hello")
    self.assertEqual(start.returncode, 0)
    self.assertIn("hello", start.stdout)
    wait = self.run_beside("wait", "ok")
    self.assertEqual(wait.returncode, 0, wait.stderr)
    self.assertNotIn("hello", wait.stdout + wait.stderr)

  def test_a_failure_fails_the_wait_with_its_output(self):
    self.assertEqual(self.run_beside("start", "bad", "--", "bash", "-c",
                                     "echo the reason; exit 3").returncode, 3)
    wait = self.run_beside("wait", "bad")
    self.assertEqual(wait.returncode, 3)
    self.assertIn("the reason", wait.stderr)

  def test_the_wait_blocks_until_the_command_ends(self):
    started = threading.Event()

    def start():
      started.set()
      self.run_beside("start", "slow", "--", "bash", "-c", "sleep 2; exit 4")

    thread = threading.Thread(target=start)
    thread.start()
    started.wait()
    time.sleep(0.5)
    began = time.monotonic()
    wait = self.run_beside("wait", "slow")
    thread.join()
    self.assertEqual(wait.returncode, 4)
    self.assertGreater(time.monotonic() - began, 1)

  def test_a_command_past_its_limit_fails_the_wait(self):
    proc = subprocess.Popen([str(SCRIPT), "start", "stuck", "--", "sleep", "30"], env=self.env,
                            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                            stdin=subprocess.DEVNULL)
    self.addCleanup(proc.kill)
    time.sleep(0.5)
    wait = self.run_beside("wait", "stuck", "2s")
    self.assertEqual(wait.returncode, 1)
    self.assertIn("still running after 2s", wait.stderr)

  def test_usage_errors(self):
    self.assertEqual(self.run_beside("start", "x", "echo").returncode, 2)
    self.assertEqual(self.run_beside("frob").returncode, 2)


if __name__ == "__main__":
  unittest.main()
