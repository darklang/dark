function fib(n) {
  return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

require("../harness.js").run(() => fib(24));
