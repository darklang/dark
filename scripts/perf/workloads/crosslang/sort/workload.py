import harness

def main():
    xs = []
    s = 1
    for _ in range(50000):
        s = (s * 48271) % 2147483647
        xs.append(s)
    xs.sort()
    return xs[0] + xs[-1] + len(xs)

harness.run(main)
