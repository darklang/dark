import os, sys


def run(work):
    """Run the work, print `result=<v>`. With CROSSLANG_ALLOC=1, also report allocation.

    Two numbers, because neither alone is what a reader means by "allocation":
      alloc_bytes      the change in live traced memory across the work. CPython frees by
                       refcount as it goes, so for a program that returns an integer this is
                       about zero, however much it churned through.
      alloc_peak_bytes the high-water mark of live traced memory during the work. This is the
                       one worth reading, and it is still "most alive at once", not "total
                       allocated": not the same quantity as Dark's or node's.
    """
    if os.environ.get("CROSSLANG_ALLOC") == "1":
        import tracemalloc, gc
        gc.disable()
        tracemalloc.start()
        before, _ = tracemalloc.get_traced_memory()
        tracemalloc.reset_peak()
        r = work()
        after, peak = tracemalloc.get_traced_memory()
        tracemalloc.stop()
        print("alloc_bytes=%d" % (after - before), file=sys.stderr)
        print("alloc_peak_bytes=%d" % peak, file=sys.stderr)
    else:
        r = work()
    print("result=%s" % (r,))
