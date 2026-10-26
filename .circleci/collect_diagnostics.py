#!/usr/bin/env python3
"""Compress complete CI logs and reports, excluding working databases/backups.

Run from the repo root. --run records a check and its relevant database paths.
--failure-databases snapshots only failed or interrupted checks, in an on_fail
step. Successful checks never contribute databases.
Uses only the Python standard library; fixture tests need no dev container.
"""

import argparse
from contextlib import closing
import gzip
import json
import os
from pathlib import Path
import shutil
import sqlite3
import subprocess
import tarfile
import tempfile
import time


MIB = 1024 * 1024
DATABASE_TIMEOUT = 15
OUTPUT = "ci-diagnostics"
CHECKS = "ci-checks"

# Explicit working stores, never a recursive sweep of databases or backups.
# The shell gates use these paths in _gates-store and _gates-ui.
CHECK_DATABASES = {
  "migrations": ["data.db"],
  "packages": ["data.db"],
  "seed": ["data.db"],
  # With `run-backend-tests --split`, the one-at-a-time tests get their own database
  # (split-sequenced/test-data.db). When the tests fail, save it along with the main one.
  "backend-tests": ["test-data.db", "split-sequenced/test-data.db"],
  "allocation": ["data.db"],
  "gate-workbench-scm": ["workbench-scm-test/data.db",
                         "workbench-scm-test-quiet/data.db"],
  "gate-workbench-views": ["workbench-views-test/data.db"],
  "gate-first-day": ["first-day/home.*/rundir/data.db",
                     "first-day/installed.*/.darklang/data.db"],
  "gate-backups-restore": ["backups-test/data.db"],
  "gate-reload-is-reproducible": ["reload-reproducible/data.db"],
  "gate-trait-choice-survives-rival": ["trait-rival/data.db"],
  "gate-trait-choice-any-batching": ["trait-batching/data.db"],
  "gate-trait-checker-vs-dispatch": ["trait-deprecated/data.db",
                                    "trait-argfit-int-first/data.db",
                                    "trait-argfit-str-first/data.db"],
  "gate-trait-deprecation-is-per-branch": ["trait-branch-dep/data.db"],
  "gate-trait-pin-across-branches": ["trait-pin-branch/data.db"],
  "gate-package-tests": ["package-tests/data.db"],
  "gate-shipped-tree-typechecks": ["shipped-tree-typechecks/data.db"],
  "gate-perf-probes-run": ["data.db"],
  "gate-prompts-unattended": ["prompts-unattended-test/data.db"],
  "gate-gates-are-clean": ["data.db"],
}


def write_json(path, value):
  # An outer timeout may interrupt a check's final update. Leave either the
  # complete running record or the complete finished record, never half JSON.
  temporary = path.with_suffix(path.suffix + ".tmp")
  temporary.write_text(json.dumps(value, indent=2) + "\n")
  temporary.replace(path)


def collect(rundir):
  output = rundir / OUTPUT
  output.mkdir(parents=True, exist_ok=True)
  candidates = []
  for directory, dirs, files in os.walk(rundir, followlinks=False):
    parent = Path(directory)
    dirs[:] = sorted(d for d in dirs
                     if not (parent / d).is_symlink()
                     and parent / d != output)
    for name in sorted(files):
      path = parent / name
      relative = path.relative_to(rundir)
      if path.is_symlink() or not path.is_file():
        continue
      is_report = relative.parts[0] == "test_results" and path.suffix == ".xml"
      if (is_report or path.suffix == ".log"
          or name in {"stderr.txt", "report.json"}
          or relative == Path("build-state.json")
          or (relative.parts[0] == CHECKS and path.suffix == ".json")):
        candidates.append(relative)

  # Stream complete files into one compressed archive. No byte or file-count
  # budget: an error in the middle of a large log is still useful evidence.
  # Replace the archive on recollection, leaving failure database snapshots alone.
  archive = output / "diagnostics.tar.gz"
  temporary = output / "diagnostics.tar.gz.tmp"
  archive.unlink(missing_ok=True)
  (output / "manifest.json").unlink(missing_ok=True)
  entries = []
  try:
    with tarfile.open(temporary, "w:gz", compresslevel=1) as bundle:
      for relative in sorted(candidates):
        source = rundir / relative
        info = bundle.gettarinfo(str(source), arcname=str(relative))
        with source.open("rb") as stream:
          bundle.addfile(info, stream)
        entries.append({"path": str(relative), "source_bytes": info.size})
    temporary.replace(archive)
  finally:
    # A read failure must fail the step, not upload a partial archive as complete.
    temporary.unlink(missing_ok=True)

  summary = {
    "payload_bytes": sum(entry["source_bytes"] for entry in entries),
    "archive_bytes": archive.stat().st_size,
    "file_count": len(entries),
    "files": entries,
  }
  write_json(output / "manifest.json", summary)
  print(f"Diagnostics: {summary['file_count']} complete files, "
        f"{summary['payload_bytes']} bytes before compression, "
        f"{summary['archive_bytes']} bytes to upload. "
        "See manifest.json for individual file sizes.")


def record_check(rundir, name, command):
  if name not in CHECK_DATABASES:
    raise ValueError(f"No diagnostic database mapping for check: {name}")
  records = rundir / CHECKS
  records.mkdir(parents=True, exist_ok=True)
  path = records / f"{name}.json"
  record = {"check": name, "status": "running", "exit_code": None,
            "databases": CHECK_DATABASES[name]}
  # Written before starting: an outer timeout may kill this wrapper too.
  write_json(path, record)
  try:
    rc = subprocess.run(command, check=False).returncode
    rc = rc if rc >= 0 else 128 - rc
  except OSError as error:
    print(f"Cannot start {name}: {error}", flush=True)
    rc = 127
  record.update(status="passed" if rc == 0 else "failed", exit_code=rc)
  write_json(path, record)
  return rc


def snapshot_database(rundir, relative):
  archive = rundir / OUTPUT / "databases" / (str(relative) + ".gz")
  result = {"path": str(relative)}
  try:
    source = (rundir / relative).resolve()
    if not source.is_relative_to(rundir.resolve()) or source.suffix != ".db":
      raise ValueError("database must be inside rundir")
    if not source.is_file():
      raise ValueError("database no longer exists")
    deadline = time.monotonic() + DATABASE_TIMEOUT
    with tempfile.TemporaryDirectory(prefix="ci-db-") as temporary:
      snapshot = Path(temporary) / "snapshot.db"
      # Backup includes committed WAL contents; copying data.db alone does not.
      with closing(sqlite3.connect(source.as_uri() + "?mode=ro", uri=True)) as src:
        def progress(status, remaining, total):
          if time.monotonic() > deadline:
            raise ValueError("database backup exceeded 15 seconds")

        with closing(sqlite3.connect(snapshot)) as dest:
          src.backup(dest, pages=256, progress=progress, sleep=0.05)
      archive.parent.mkdir(parents=True, exist_ok=True)
      with snapshot.open("rb") as src, gzip.open(archive, "wb", compresslevel=1) as dst:
        shutil.copyfileobj(src, dst)
      result.update(status="saved", snapshot_bytes=snapshot.stat().st_size,
                    archive_bytes=archive.stat().st_size)
  except (OSError, ValueError, sqlite3.Error) as error:
    archive.unlink(missing_ok=True)
    result["status"] = f"unavailable: {error}"
  return result


def failure_databases(rundir):
  output = rundir / OUTPUT
  output.mkdir(parents=True, exist_ok=True)
  if (output / "databases").exists():
    shutil.rmtree(output / "databases")
  checks = []
  snapshots = {}
  for path in sorted((rundir / CHECKS).glob("*.json")):
    record = json.loads(path.read_text())
    if record["status"] not in {"failed", "running"}:
      continue
    name = record["check"]
    checks.append(record)
    # Use the declared mapping, not arbitrary paths from a result file.
    for pattern in CHECK_DATABASES[name]:
      matches = list(rundir.glob(pattern))
      if not matches:
        snapshots.setdefault(pattern, {
          "path": pattern, "status": "unavailable: database was not created or was cleaned up",
          "checks": [],
        })["checks"].append(name)
      for source in matches:
        relative = source.relative_to(rundir)
        key = str(relative)
        if key not in snapshots:
          snapshots[key] = snapshot_database(rundir, relative)
          snapshots[key]["checks"] = []
        snapshots[key]["checks"].append(name)
  summary = {
    "failed_or_interrupted_checks": checks,
    "databases": list(snapshots.values()),
    "archive_bytes": sum(item.get("archive_bytes", 0) for item in snapshots.values()),
    "note": "Only available stores are captured. Isolated tests may clean up "
            "their private stores before this step.",
  }
  write_json(output / "databases.json", summary)
  for result in snapshots.values():
    print(f"Failure database {result['path']}: {result['status']} "
          f"({result.get('archive_bytes', 0)} compressed bytes)")
  if not checks:
    print("No failed database-backed checks recorded; no databases uploaded.")


if __name__ == "__main__":
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--rundir", type=Path, default=Path("rundir"))
  modes = parser.add_mutually_exclusive_group()
  modes.add_argument("--failure-databases", action="store_true")
  modes.add_argument("--run", choices=CHECK_DATABASES)
  parser.add_argument("command", nargs=argparse.REMAINDER)
  args = parser.parse_args()
  if args.run:
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
      parser.error("--run requires a command after --")
    raise SystemExit(record_check(args.rundir, args.run, command))
  elif args.failure_databases:
    failure_databases(args.rundir)
  else:
    collect(args.rundir)
