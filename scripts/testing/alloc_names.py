"""Type-name histogram from a GC allocation-tick nettrace.

There is no public parser for the nettrace container that ships with the SDK, so this scans the raw file
for the UTF-16 type-name strings the AllocationTick payload carries. The important detail, and the one
that cost me the whole campaign: F# compiler-generated closures and async state machines all have an `@`
in their name (`executeInner@653-6`, `checkFnParam@333-2`). An earlier version of this filter excluded `@`,
which silently dropped every one of them -- and they turned out to be the single largest category.
"""
import re, sys, collections

path = sys.argv[1] if len(sys.argv) > 1 else "rundir/perf-scratch/alloc-now.nettrace"
raw = open(path, "rb").read()

# UTF-16LE runs that look like a managed type name. `@`, `<`, `>` and `-` are all part of names the
# compiler generates, so they belong in the class.
pat = re.compile(
    (r"(?:[A-Za-z0-9_.`+\[\],@<>\-$]\x00){6,}").encode("latin-1"))

counts = collections.Counter()
for m in pat.finditer(raw):
    name = m.group().decode("utf-16-le")
    if "." not in name and "@" not in name and "+" not in name:
        continue
    counts[name] += 1

total = sum(counts.values())
if not total:
    sys.exit("no type names found -- was the trace collected with Microsoft-Windows-DotNETRuntime:0x1:5?")


def bucket(name):
    if "@" in name or "StateMachine" in name or "Ply.TplPrimitives" in name:
        return "closures + async state machines"
    return "other"


print(f"{total} allocation ticks\n")
print("=== by type ===")
for name, n in counts.most_common(28):
    print(f"{100*n/total:6.2f}%  {n:5}  {name}")

groups = collections.Counter()
for name, n in counts.items():
    groups[bucket(name)] += n
print("\n=== grouped ===")
for g, n in groups.most_common():
    print(f"{100*n/total:6.2f}%  {n:5}  {g}")

# Which enclosing method the closures came from, since `executeInner@653-6` and `executeInner@712-9` are
# the same problem in two places.
owners = collections.Counter()
for name, n in counts.items():
    if "@" in name:
        owners[name.split("@")[0].split("+")[-1]] += n
print("\n=== closures by enclosing method ===")
for o, n in owners.most_common(15):
    print(f"{100*n/total:6.2f}%  {n:5}  {o}")
