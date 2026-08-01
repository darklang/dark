#!/usr/bin/env python3.12
"""Find the tests you want to run, and the flag that runs them.

Filtering has been a long-standing trap. Three flags do three different things,
`--filter` disagreed with its own help about the separator, `--list-tests` ignores
every filter and prints ten thousand lines, and a filter that matches nothing is
reported as "0 tests run - Success!" with exit code 0. So the failure mode was
believing you had run a suite you hadn't.

This answers "what can I run" and "what runs this", and prints the exact command.

  scripts/run-backend-tests --groups            the tree, with counts
  scripts/run-backend-tests --groups <substr>   just the part you mean
  scripts/run-backend-tests --find <substr>     which tests match, and how to run them
"""

import os
import subprocess
import sys

RUNDIR = os.getenv("DARK_CONFIG_RUNDIR", "rundir")
CACHE = os.path.join(RUNDIR, "test-index.txt")
TESTS_EXE = "backend/Build/out/Tests/Debug/net10.0/Tests"

# Listing costs a full startup of the test binary (~15s, because building the tree
# reads every testfile), and the answer only changes when that binary does, so it's
# cached against the binary's mtime.


def _strip_ansi(line):
  out, i = [], 0
  while i < len(line):
    if line[i] == "\x1b":
      while i < len(line) and line[i] not in "ABCDEFGHJKSTfmnsulh":
        i += 1
      i += 1
    else:
      out.append(line[i])
      i += 1
  return "".join(out)


def _generate():
  env = dict(os.environ, DARK_CONFIG_TELEMETRY_EXPORTER="none")
  proc = subprocess.run([os.path.abspath(TESTS_EXE), "--list-tests"],
                        cwd="backend", env=env, capture_output=True, text=True)
  names = []
  for raw in proc.stdout.splitlines():
    line = _strip_ansi(raw).strip()
    # Everything else on stdout is Expecto's own logging.
    if line.startswith("tests/"):
      names.append(line)
  return names


def load(refresh=False):
  """Every test name, slash-separated. Cached against the test binary's mtime."""
  if not os.path.exists(TESTS_EXE):
    print(f"No test binary at {TESTS_EXE}. Run: scripts/dev/build", file=sys.stderr)
    sys.exit(1)

  fresh = (not refresh and os.path.exists(CACHE)
           and os.path.getmtime(CACHE) > os.path.getmtime(TESTS_EXE))
  if fresh:
    with open(CACHE) as f:
      return [line.rstrip("\n") for line in f if line.strip()]

  print("Listing tests (~15s; cached until the test binary changes)...",
        file=sys.stderr)
  names = _generate()
  if not names:
    print("The test binary listed no tests. Run: scripts/dev/build", file=sys.stderr)
    sys.exit(1)
  os.makedirs(RUNDIR, exist_ok=True)
  with open(CACHE, "w") as f:
    f.write("\n".join(names) + "\n")
  return names


def _group_of(name):
  """The list a case belongs to: everything up to the last slash.

  Case names can contain almost anything, but the separator is a slash now, and
  slashes in a case name are rare enough that the last one is a good boundary.
  """
  return name.rsplit("/", 1)[0] if "/" in name else name


def counts_by_prefix(names):
  """{group path: number of cases under it}, for every level of the tree."""
  counts = {}
  for name in names:
    parts = _group_of(name).split("/")
    for depth in range(1, len(parts) + 1):
      prefix = "/".join(parts[:depth])
      counts[prefix] = counts.get(prefix, 0) + 1
  return counts


def cmd_groups(args):
  substring = args[0] if args else None
  names = load()
  counts = counts_by_prefix(names)

  # Without a search term, two levels is about a screenful; with one, show
  # everything under the match, since you've said what you're after.
  max_depth = 2 if substring is None else 99

  shown = []
  for path in sorted(counts):
    depth = path.count("/")
    if substring is None and depth > max_depth:
      continue
    if substring is not None and substring.lower() not in path.lower():
      continue
    shown.append(path)

  if not shown:
    print(f"No test group matching '{substring}'.")
    print("Try: scripts/run-backend-tests --find " + (substring or ""))
    return 1

  # A match brings its children along, so you can see what's inside it.
  if substring is not None:
    with_children = set(shown)
    for path in sorted(counts):
      if any(path.startswith(m + "/") for m in shown):
        with_children.add(path)
    shown = sorted(with_children)

  print(f"{len(names)} tests in total.\n")
  width = min(66, max(len(p) + 2 * p.count("/") for p in shown))
  for path in shown:
    indent = "  " * path.count("/")
    line = indent + path
    gap = max(2, width - len(line) + 2)
    print(f"{line}{' ' * gap}{counts[path]:>6}")

  print("\nRun one with:")
  print(f"    scripts/run-backend-tests --filter {shown[0]}")
  if substring is None:
    print("\nNarrow this list with: scripts/run-backend-tests --groups <substring>")
  return 0


def cmd_find(args):
  if not args:
    print("usage: --find <substring>", file=sys.stderr)
    return 1
  needle = args[0].lower()
  names = load()

  matches = [n for n in names if needle in n.lower()]
  if not matches:
    print(f"Nothing matches '{args[0]}'.")
    print("Case matching is on the whole slash-separated name; try a shorter term,")
    print("or browse with: scripts/run-backend-tests --groups")
    return 1

  groups = {}
  for name in matches:
    groups.setdefault(_group_of(name), []).append(name)

  print(f"{len(matches)} test(s) in {len(groups)} group(s):\n")
  for group in sorted(groups):
    print(f"  {group}   ({len(groups[group])} matching)")
    for name in sorted(groups[group])[:3]:
      print(f"      {name[len(group) + 1:]}")
    if len(groups[group]) > 3:
      print(f"      ... and {len(groups[group]) - 3} more")

  print("\nRun them with:")
  if len(groups) == 1:
    print(f"    scripts/run-backend-tests --filter {next(iter(groups))}")
  else:
    common = os.path.commonprefix(sorted(groups)).rsplit("/", 1)[0]
    if common:
      print(f"    scripts/run-backend-tests --filter {common}"
            "        # all of them, plus siblings")
  print(f"    scripts/run-backend-tests --filter-test-case {args[0]}"
        "   # just the matching cases")
  return 0


def main():
  args = sys.argv[1:]
  if not args:
    print("usage: _test-explorer.py {groups|find} [substring]", file=sys.stderr)
    return 1
  if args[0] == "groups":
    return cmd_groups(args[1:])
  if args[0] == "find":
    return cmd_find(args[1:])
  print(f"unknown command: {args[0]}", file=sys.stderr)
  return 1


if __name__ == "__main__":
  sys.exit(main())
