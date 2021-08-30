## Coding guide / style guide

# F#

- `ignore` should always use a type signature (this should be enforced by the
  compiler)

- use `print` instead of `Console.WriteLine` or similar, the latter deadlocks

- ensure that `try` do not have a `uply` in the body unless you know what you are
  doing and provide a comment. Typically, the `uply` should be on the outside, which
  causes the compiler to compile the `try` into a version which supports Tasks.
  Otherwise, it won't catch an exception thrown within the `uply`.

- you can only use Tasks (aka `Ply`) once. Using it a second time is undefined.
