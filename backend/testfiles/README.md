# Dark Test Files (testfiles)

Many backend Dark tests are written outside of F# code, so that they may be
written more tersely and not require recompilation between executions. This
directory houses those testfiles and data used within them.

Refer to `docs/unittests.md` for instructions on running tests, and `README`s
within the subdirectories on how to write types of tests.

## Subdirectories

### Tests files

- `execution` files are used to test the Dark language and most of the standard library
- `httpclient` files are used to test the HTTP Client in the standard library
- `httphandler` files are used to test the HTTP handlers hosted by BwdServer

### Accessory/data

- `data` houses static assets useful in tests
- `staticassets` files ... TODO

## Syntax (Parser.fs)

All "Dark code" in tests is parsed by the F# parser, then converted from F# to
Dark - the converseion is within `Parser.fs`. The error messages around
this are not very good, but there are a few things to be careful of:

- be explicit around pipes, wrapping them in parens to make sure the right
  expression is being piped. Otherwise you will typically get type errors.

- by default, function calls are not sent to the errorRail (the opposite of the
  Dark editor). You can send them to the errorRail by calling the function with
  the suffix `_ster`. For example, write `DB.get_v1_ster` instead of
  `DB.get_v1`.

- to get a blank, use the word `blank`

- to produce results that are hard to otherwise create, you can add functions
  to LibTest.fs. For example, `Test.nan_v0` produces a `NaN` float, and
  `Test.typeError` produces a built-in error.

When new langauge constructs are added to Dark, updates are often also required
in `Parser.fs`.
