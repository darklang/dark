import harness

def work(n):
    return len(list(map(lambda x: x + 1, range(1, n + 1))))

def repeat(times, n, acc):
    return acc if times <= 0 else repeat(times - 1, n, acc + work(n))

harness.run(lambda: repeat(200, 50, 0))
