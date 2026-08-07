# Cumulative allocation. CPython frees by refcount as it goes, so "every byte allocated" is not
# observable by watching the heap. Instead retain each iteration's result so nothing is reclaimed:
# tracemalloc's current-traced figure at the end is then the bytes allocated by the work itself.
# Transient per-iteration objects (the range and map wrappers) are still freed and so are not counted;
# they are a few hundred bytes each, which does not change the order of magnitude.
import tracemalloc, gc
def work(n, keep):
    xs = list(map(lambda x: x + 1, range(1, n + 1)))
    keep.append(xs)
    return len(xs)
def repeat(times, n, acc, keep):
    if times <= 0: return acc
    return repeat(times - 1, n, acc + work(n, keep), keep)
repeat(20, 50, 0, [])
gc.disable()
tracemalloc.start()
keep = []
before, _ = tracemalloc.get_traced_memory()
r = repeat(200, 50, 0, keep)
after, peak = tracemalloc.get_traced_memory()
tracemalloc.stop()
print(f"python cumulative={(after-before)/1024:.1f} KB  per_iter={(after-before)/200/1024:.2f} KB  result={r}")
