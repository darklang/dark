import sys, harness
sys.setrecursionlimit(50000)

def depth(n):
    return 0 if n <= 0 else 1 + depth(n - 1)

def main():
    acc = 0
    for _ in range(50):
        acc += depth(5000)
    return acc

harness.run(main)
