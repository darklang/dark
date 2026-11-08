#!/usr/bin/env python3
"""Exercise the real artifact validator with fake executables; no build needed."""

import os
from pathlib import Path
import pty
import signal
import subprocess
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parent.parent

# Each check's watchdog, unless a test is about timing out. No check of the fake CLI comes near
# it however loaded the machine is, so a run that takes this long waited one out.
TIMER = "120"
# How long the fixture's stray children live: as long as a child holding the output pipe would
# keep the validator from finishing if it were left running.
CHILD_SECONDS = "300"
# A validator that waits out a watchdog or a stray child takes at least TIMER. One that stops them
# takes well under a second, and a few seconds on a busy machine. So finishing at all within this
# is the check, with a wide margin on both sides, rather than a tight limit that load can break.
FINISH_WITHIN = 60


class ArtifactValidationTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    self.root = Path(self.temporary.name)
    self.artifact = self.root / "fake-cli"
    self.artifact.write_text("""#!/usr/bin/env bash
if [[ "$1" == status ]]; then
  case "$FIXTURE_MODE" in
    fail) echo 'fixture error' >&2; exit 23 ;;
    killed) kill -KILL "$$" ;;
    background)
      sleep "$FIXTURE_CHILD_SECONDS" &
      echo $! > "$FIXTURE_CHILD_PID"
      ;;
    hang)
      sleep "$FIXTURE_CHILD_SECONDS" &
      echo $! > "$FIXTURE_CHILD_PID"
      wait
      ;;
  esac
fi
# The CLI test suite. In CI the validator's stdin is a pty; the suite must not get it.
if [[ "$1" == test ]]; then
  if [[ -t 0 ]]; then echo 'suite fixture: stdin is a terminal'; exit 9; fi
  case "$FIXTURE_SUITE" in
    fail) echo 'FAIL Darklang.Cli.Tests.TestSuite.fixture'; echo '0 passed, 1 failed'; exit 1 ;;
    hang)
      sleep "$FIXTURE_CHILD_SECONDS" &
      echo $! > "$FIXTURE_CHILD_PID"
      wait
      ;;
  esac
  echo '1 passed, 0 failed'
  exit 0
fi
# A store where the real binary keeps one: ~/.darklang.
mkdir -p "$HOME/.darklang"
touch "$HOME/.darklang/data.db"
# The child-store check runs a second copy from inside `eval`; run what it asks for.
if [[ "$1" == eval && "$2" == *executeWithStdOutOrStdErr* ]]; then
  q='"'
  inner=${2#*$q}
  bash -c "${inner%$q*}" > /dev/null
fi
# A wrong answer that still contains the right one.
if [[ "$FIXTURE_MODE" == wrong && "$1" == eval ]]; then echo 13; else echo 3; fi
""")
    self.artifact.chmod(0o755)
    # Record the actual timer PIDs to verify they are stopped and reaped.
    self.bin = self.root / "bin"
    self.bin.mkdir()
    sleep = self.bin / "sleep"
    sleep.write_text("""#!/usr/bin/env bash
echo "$$" >> "$FIXTURE_TIMER_PIDS"
exec /bin/sleep "$@"
""")
    sleep.chmod(0o755)
    # The suite needs sqlite3 on PATH; the fake suite doesn't use it.
    sqlite3 = self.bin / "sqlite3"
    sqlite3.write_text("#!/usr/bin/env bash\n")
    sqlite3.chmod(0o755)

  def run_validator(self, mode, check_timeout=TIMER, compare=False, suite="pass",
                    suite_timeout=TIMER, stdin=subprocess.DEVNULL):
    # Every run must finish within FINISH_WITHIN; the tests about not waiting rely on that.
    env = {**os.environ, "IN_DEV_CONTAINER": "true", "FIXTURE_MODE": mode,
           "FIXTURE_SUITE": suite,
           "CHECK_TIMEOUT": check_timeout, "SUITE_TIMEOUT": suite_timeout,
           "FIXTURE_CHILD_PID": str(self.root / "child.pid"),
           "FIXTURE_CHILD_SECONDS": CHILD_SECONDS,
           "FIXTURE_TIMER_PIDS": str(self.root / "timers.pid"),
           "PATH": str(self.bin) + os.pathsep + os.environ["PATH"]}
    command = ["bash", str(ROOT / "scripts/testing/validate-cli-artifact"),
               str(self.artifact)]
    if compare:
      command += ["--compare-against", str(self.artifact)]
    process = subprocess.Popen(command, cwd=ROOT, env=env, stdin=stdin,
                               stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT, text=True,
                               start_new_session=True)
    try:
      output, _ = process.communicate(timeout=FINISH_WITHIN)
    except subprocess.TimeoutExpired:
      self.fail(f"validator did not finish within {FINISH_WITHIN}s: it waited out a watchdog "
                "or a child holding its output")
    finally:
      # Fixture cleanup only, including children left by a regressed watchdog.
      try:
        os.killpg(process.pid, signal.SIGKILL)
      except ProcessLookupError:
        pass
      process.communicate()
    # A broken EXIT trap still exits 0, so a green result is not enough.
    self.assertNotRegex(output, r"validate-cli-artifact: line \d+:", output)
    return process.returncode, output

  def assert_process_stopped(self, pid):
    # An orphan may briefly remain a zombie until init reaps it; it cannot run
    # or hold the output pipe. ps works on Linux and macOS.
    deadline = time.monotonic() + 10
    while time.monotonic() < deadline:
      result = subprocess.run(["ps", "-p", str(pid), "-o", "stat="],
                              capture_output=True, text=True)
      if result.returncode != 0 or result.stdout.strip().startswith("Z"):
        return
      time.sleep(0.01)
    self.fail(f"validator left process {pid} running")

  def test_success_returns_without_waiting_for_watchdog_and_stops_timers(self):
    rc, output = self.run_validator("success")
    self.assertEqual(rc, 0, output)
    self.assertIn("ok   suite (exit 0)", output)
    self.assertIn("All checks passed.", output)
    timers = self.root / "timers.pid"
    if timers.exists():
      for pid in timers.read_text().split():
        self.assert_process_stopped(int(pid))

  def test_command_failure_keeps_exit_code_and_output(self):
    rc, output = self.run_validator("fail")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL status (first run) (exit 23)", output)
    self.assertIn("fixture error", output)
    self.assertNotIn("no answer", output)

  def test_wrong_answer_fails_even_when_it_contains_the_right_one(self):
    rc, output = self.run_validator("wrong")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL eval (output was not '3')", output)
    self.assertIn("FAIL eval outside the checkout (exit 0, wanted 3)", output)

  def test_sigkill_is_not_misreported_as_a_timeout(self):
    rc, output = self.run_validator("killed")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL status (first run) (exit 137)", output)
    self.assertNotIn("no answer", output)

  def test_timeout_stops_command_and_child_holding_output_pipe(self):
    rc, output = self.run_validator("hang", check_timeout="0.2")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL status (first run) (no answer in 0.2s)", output)
    self.assert_process_stopped(int((self.root / "child.pid").read_text()))

  def test_output_comparison_still_works_without_waiting_for_each_timer(self):
    rc, output = self.run_validator("success", compare=True)
    self.assertEqual(rc, 0, output)
    self.assertIn("Output comparison against", output)

  def test_success_cleans_up_background_child_holding_output_pipe(self):
    rc, output = self.run_validator("background")
    self.assertEqual(rc, 0, output)
    self.assert_process_stopped(int((self.root / "child.pid").read_text()))

  def test_suite_failure_fails_the_artifact_with_its_output(self):
    rc, output = self.run_validator("success", suite="fail")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL suite (exit 1)", output)
    self.assertIn("FAIL Darklang.Cli.Tests.TestSuite.fixture", output)

  def test_suite_that_never_finishes_fails_as_a_timeout_and_is_stopped(self):
    rc, output = self.run_validator("success", suite="hang", suite_timeout="0.2")
    self.assertEqual(rc, 1, output)
    self.assertIn("FAIL suite (no answer in 0.2s)", output)
    self.assert_process_stopped(int((self.root / "child.pid").read_text()))

  def test_suite_does_not_get_the_terminal_ci_gives_the_step(self):
    controller, terminal = pty.openpty()
    self.addCleanup(os.close, controller)
    try:
      rc, output = self.run_validator("success", stdin=terminal)
    finally:
      os.close(terminal)
    self.assertEqual(rc, 0, output)
    self.assertNotIn("stdin is a terminal", output)
    self.assertIn("ok   suite (exit 0)", output)


if __name__ == "__main__":
  unittest.main()
