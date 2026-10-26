import harness

def work(n):
    d = {}
    for i in range(1, n + 1):
        d["k" + str(i)] = i
    acc = 0
    for i in range(1, n + 1):
        acc += d["k" + str(i)]
    return acc

def main():
    acc = 0
    for _ in range(1000):
        acc += work(50)
    return acc

harness.run(main)
