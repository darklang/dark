import time
def work(n):
    return len(list(map(lambda x: x + 1, range(1, n + 1))))
def repeat(times, n, acc):
    if times <= 0: return acc
    return repeat(times - 1, n, acc + work(n))
repeat(20, 50, 0)                      # warm
t0 = time.perf_counter(); r = repeat(200, 50, 0); t1 = time.perf_counter()
print(f"python  elapsed_ms={(t1-t0)*1000:.2f}  result={r}")
