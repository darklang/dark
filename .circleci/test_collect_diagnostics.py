#!/usr/bin/env python3
"""Check complete diagnostics and automatic failure snapshots; no container needed."""

import gzip
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path
import sqlite3
import tarfile
import tempfile
import unittest
from unittest.mock import patch

import collect_diagnostics as diagnostics


class DiagnosticsTests(unittest.TestCase):
  def setUp(self):
    self.temporary = tempfile.TemporaryDirectory()
    self.addCleanup(self.temporary.cleanup)
    self.root = Path(self.temporary.name) / "rundir"
    self.root.mkdir()
    self.output = self.root / diagnostics.OUTPUT

  def write(self, relative, content):
    path = self.root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_bytes(content)
    return path

  def archived_files(self):
    with tarfile.open(self.output / "diagnostics.tar.gz", "r:gz") as archive:
      return {entry.name: archive.extractfile(entry).read()
              for entry in archive.getmembers()}

  def test_success_keeps_diagnostics_but_not_databases_or_linked_files(self):
    files = {
      "logs/fsharp-tests.log": b"tests passed",
      "package-tests/test.log": b"package tests passed",
      "shipped-tree-typechecks/report.json": b'{"failed": 0}',
      "shipped-tree-typechecks/stderr.txt": b"diagnostic",
      "test_results/backend.xml": b"<testsuite/>",
    }
    for name, contents in files.items():
      self.write(name, contents)
    self.write("data.db", b"working database")
    self.write("backups-test/backup.db", b"temporary database")
    self.write("unrelated.txt", b"not a diagnostic")
    outside = Path(self.temporary.name) / "outside.log"
    outside.write_text("not a diagnostic")
    (self.root / "linked.log").symlink_to(outside)
    (self.root / "linked-directory").symlink_to(outside.parent)

    diagnostics.collect(self.root)

    self.assertEqual(self.archived_files(), files)
    self.assertFalse((self.output / "databases").exists())
    self.assertEqual((self.root / "data.db").read_bytes(), b"working database")

  def test_large_logs_keep_middle_errors_and_all_bytes_beyond_old_budget(self):
    contents = (b"start\n" + b"x" * (11 * diagnostics.MIB)
                + b"\nERROR: the useful evidence is in the middle\n"
                + b"y" * (11 * diagnostics.MIB) + b"\nfailed\n")
    self.write("logs/fsharp-tests.log", contents)
    diagnostics.collect(self.root)
    self.assertEqual(self.archived_files()["logs/fsharp-tests.log"], contents)
    manifest = json.loads((self.output / "manifest.json").read_text())
    self.assertEqual(manifest["payload_bytes"], len(contents))
    self.assertEqual(manifest["archive_bytes"],
                     (self.output / "diagnostics.tar.gz").stat().st_size)
    self.assertLess(manifest["archive_bytes"], len(contents))

  def test_every_report_is_kept_even_beyond_old_file_count_limit(self):
    expected = {}
    for number in range(205):
      name = f"test_results/{number}.xml"
      expected[name] = f'<testsuite name="{number}"/>'.encode()
      self.write(name, expected[name])
    diagnostics.collect(self.root)
    self.assertEqual(self.archived_files(), expected)
    for name, contents in expected.items():
      self.assertEqual((self.root / name).read_bytes(), contents)

  def test_empty_rundir_still_produces_a_manifest(self):
    diagnostics.collect(self.root / "missing")
    manifest = self.root / "missing" / diagnostics.OUTPUT / "manifest.json"
    self.assertEqual(json.loads(manifest.read_text())["payload_bytes"], 0)

  def test_recollection_replaces_old_logs_without_archiving_its_own_output(self):
    log = self.write("logs/old.log", b"old diagnostic")
    diagnostics.collect(self.root)
    log.unlink()
    self.write("logs/new.log", b"new diagnostic")
    diagnostics.collect(self.root)
    self.assertEqual(self.archived_files(), {"logs/new.log": b"new diagnostic"})

  def record(self, name, status="failed"):
    self.write(f"ci-checks/{name}.json", json.dumps({
      "check": name, "status": status, "exit_code": 1 if status == "failed" else None,
      "databases": diagnostics.CHECK_DATABASES[name],
    }).encode())

  def database(self, relative):
    path = self.root / relative
    path.parent.mkdir(parents=True, exist_ok=True)
    with sqlite3.connect(path) as connection:
      connection.execute("CREATE TABLE evidence (message TEXT)")
      connection.execute("INSERT INTO evidence VALUES ('failure detail')")
    return path

  def test_successful_checks_and_unrelated_databases_are_not_saved(self):
    self.database("data.db")
    self.database("test-data.db")
    self.database("trait-rival/data.db")
    self.database("backups-test/data.db")
    self.record("backend-tests")
    self.record("gate-trait-choice-survives-rival", "passed")
    diagnostics.failure_databases(self.root)
    saved = list((self.output / "databases").rglob("*.gz"))
    self.assertEqual([p.relative_to(self.output / "databases").as_posix()
                      for p in saved], ["test-data.db.gz"])

  def test_failed_split_run_saves_both_halves_stores(self):
    self.database("test-data.db")
    self.database("split-sequenced/test-data.db")
    self.record("backend-tests")
    diagnostics.failure_databases(self.root)
    saved = sorted(p.relative_to(self.output / "databases").as_posix()
                   for p in (self.output / "databases").rglob("*.gz"))
    self.assertEqual(saved, ["split-sequenced/test-data.db.gz", "test-data.db.gz"])

  def test_successful_run_has_no_database_artifacts(self):
    self.database("test-data.db")
    rc = diagnostics.record_check(self.root, "backend-tests",
                                  [sys.executable, "-c", "pass"])
    self.assertEqual(rc, 0)
    diagnostics.failure_databases(self.root)
    self.assertFalse((self.output / "databases").exists())

  def test_failure_snapshot_includes_committed_wal_and_survives_collection(self):
    source = sqlite3.connect(self.root / "test-data.db")
    self.addCleanup(source.close)
    source.execute("PRAGMA journal_mode=WAL")
    source.execute("CREATE TABLE evidence (message TEXT)")
    source.execute("INSERT INTO evidence VALUES ('failure detail')")
    source.commit()
    self.assertGreater((self.root / "test-data.db-wal").stat().st_size, 0)
    rc = diagnostics.record_check(self.root, "backend-tests",
                                  [sys.executable, "-c", "raise SystemExit(7)"])
    self.assertEqual(rc, 7)
    diagnostics.failure_databases(self.root)
    diagnostics.collect(self.root)
    restored = Path(self.temporary.name) / "restored.db"
    restored.write_bytes(gzip.decompress(
      (self.output / "databases/test-data.db.gz").read_bytes()))
    with sqlite3.connect(restored) as snapshot:
      self.assertEqual(snapshot.execute("SELECT * FROM evidence").fetchall(),
                       [("failure detail",)])

  def test_missing_corrupt_and_outside_databases_are_reported(self):
    for name in ["backend-tests", "gate-trait-choice-survives-rival", "packages"]:
      self.record(name)
    self.write("test-data.db", b"not sqlite")
    outside = Path(self.temporary.name) / "outside.db"
    outside.write_bytes(b"do not upload")
    (self.root / "data.db").symlink_to(outside)
    diagnostics.failure_databases(self.root)
    results = json.loads((self.output / "databases.json").read_text())["databases"]
    # 4, not 3: backend-tests lists two databases, and both are unavailable here (the
    # main one is corrupt, and the split one was never created).
    self.assertEqual(len(results), 4)
    self.assertTrue(all(r["status"].startswith("unavailable:") for r in results))
    self.assertEqual(list(self.output.rglob("*.db.gz")), [])

  def test_backup_timeout_is_reported_instead_of_a_partial_snapshot(self):
    self.database("test-data.db")
    self.record("backend-tests")
    with patch.object(diagnostics, "DATABASE_TIMEOUT", -1):
      diagnostics.failure_databases(self.root)
    results = json.loads((self.output / "databases.json").read_text())["databases"]
    self.assertIn("exceeded", results[0]["status"])
    self.assertEqual(list(self.output.rglob("*.db.gz")), [])

  def test_interrupted_check_is_saved_and_timeout_exit_is_preserved(self):
    self.database("trait-rival/data.db")
    self.record("gate-trait-choice-survives-rival", "running")
    self.database("test-data.db")
    rc = diagnostics.record_check(self.root, "backend-tests", [
      "timeout", "0.1", sys.executable, "-c", "import time; time.sleep(10)"])
    self.assertEqual(rc, 124)
    diagnostics.failure_databases(self.root)
    self.assertTrue((self.output / "databases/test-data.db.gz").exists())
    self.assertTrue((self.output / "databases/trait-rival/data.db.gz").exists())

  def test_real_gate_runner_records_only_failed_gate_stores(self):
    # Real dispatch/recording, fake gate bodies: no Dark build or container needed.
    repo = Path(__file__).resolve().parent.parent
    work = Path(self.temporary.name)
    for relative in ["scripts/testing/gates", ".circleci/collect_diagnostics.py"]:
      dest = work / relative
      dest.parent.mkdir(parents=True, exist_ok=True)
      shutil.copy(repo / relative, dest)
    guard = work / "scripts/devcontainer/_assert-in-container"
    guard.parent.mkdir(parents=True)
    guard.write_text("# Fixture: no container or real binary is used.\n")
    (work / "scripts/testing/_gates-ui").write_text("""
gate_workbench_scm() { return 7; }
gate_workbench_views() { return 0; }
""")
    for name in ["_gates-store", "_gates-meta"]:
      (work / "scripts/testing" / name).write_text("")
    env = {**os.environ, "CIRCLECI": "true", "CI": "true"}
    env.pop("CI_DIAGNOSTIC_GATE", None)
    for gate, expected in [("workbench-scm", 7), ("workbench-views", 0)]:
      self.database(f"{gate}-test/data.db")
      result = subprocess.run([str(work / "scripts/testing/gates"), gate],
                              cwd=work, env=env, capture_output=True, text=True)
      self.assertEqual(result.returncode, expected, result.stdout + result.stderr)
    diagnostics.failure_databases(self.root)
    self.assertTrue((self.output / "databases/workbench-scm-test/data.db.gz").exists())
    self.assertFalse((self.output / "databases/workbench-views-test/data.db.gz").exists())

  def test_every_registered_gate_has_an_explicit_database_mapping(self):
    runner = Path(__file__).resolve().parent.parent / "scripts/testing/gates"
    names = re.search(r"^GATES=\((.*?)\)", runner.read_text(), re.M | re.S)[1].split()
    for name in names:
      self.assertIn(f"gate-{name}", diagnostics.CHECK_DATABASES)

  def test_first_day_keeps_failed_stores_and_removes_successful_stores(self):
    repo = Path(__file__).resolve().parent.parent
    work = Path(self.temporary.name)
    fake_cli = work / "fake-cli"
    fake_cli.write_text("""#!/usr/bin/env python3
import os, pathlib, sqlite3
root = pathlib.Path(os.environ.get('DARK_CONFIG_RUNDIR',
                                  str(pathlib.Path.home() / '.darklang')))
root.mkdir(parents=True, exist_ok=True)
with sqlite3.connect(root / 'data.db') as db:
  db.execute('CREATE TABLE IF NOT EXISTS evidence (message TEXT)')
if os.environ.get('FIXTURE_PASS') == 'true':
  print('''clean:
draft: 1 item
Created function FirstDay.Hello.greet
Created function FirstDay.Hello.only
hi world
commit abcdef12 -- 1 op
first day
started firstday
not found
would merge
Merged
1
1 conflict
"pending":1
yo x
later x
Deprecated fn
discarded
51
back on the version
2''')
else:
  print('Encountered a Runtime Error: fixture failure')
print('{"id":"abcdef"}')
""")
    fake_cli.chmod(0o755)
    for passing in [True, False]:
      result = subprocess.run([
        "bash", "-c", 'source "$1"; gate_first_day "$2"', "fixture",
        str(repo / "scripts/testing/_gates-store"), str(fake_cli)],
        cwd=work, env={**os.environ, "CIRCLECI": "true",
                       "FIXTURE_PASS": "true" if passing else "false"},
        capture_output=True, text=True)
      self.assertEqual(result.returncode, 0 if passing else 1,
                       result.stdout + result.stderr)
      if passing:
        self.assertEqual(list((self.root / "first-day").iterdir()), [])
    self.record("gate-first-day")
    diagnostics.failure_databases(self.root)
    saved = list((self.output / "databases/first-day").rglob("*.gz"))
    self.assertEqual(len(saved), 2, result.stdout + result.stderr)


if __name__ == "__main__":
  unittest.main()
