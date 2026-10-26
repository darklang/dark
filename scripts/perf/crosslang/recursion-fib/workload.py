import harness

def fib(n):
    return n if n < 2 else fib(n - 1) + fib(n - 2)

harness.run(lambda: fib(24))
