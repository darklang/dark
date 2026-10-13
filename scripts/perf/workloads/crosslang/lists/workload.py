import harness

def work(n):
    xs = list(range(1, n + 1))
    ys = list(map(lambda x: x + 1, xs))
    zs = list(filter(lambda x: x % 2 == 0, ys))
    return len(zs)

def main():
    acc = 0
    for _ in range(2000):
        acc += work(50)
    return acc

harness.run(main)
