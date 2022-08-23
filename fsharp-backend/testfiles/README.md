# Dark Test Files (testfiles)

## Subdirectories

### Tests

- `execution` corresponds to
- `httpclient` files are used to test the HTTP Client
- `httphandler` files are used to test the HTTP handlers hosted by BwdServer
- `staticassets` files ...

### Accessory

- `data` houses static assets useful in tests
- `json-suite`

## Syntax (FSharpToExpr.fs)

All "Dark code" in tests is parsed by the F# parser, then converted from F# to
Dark - the converseion is within `FSharpToExpr.fs`. The error messages around
this are not very good, but there are a few things to be careful of:

- all tests must be of the format `x = y` (or `x <> y`, though that's rarely
  used), that is, they must have a single expression on the left, one on the
  right, and an equals sign in between. You can use parens around multiple
  expressions to group them into a single expression, eg `(5 |> toString)`.

- be explicit around pipes, wrapping them in parens to make sure the right
  expression is being piped. Otherwise you will typically get type errors.

- by default, function calls are not sent to the errorRail (the opposite of the
  Dark editor). You can send them to the errorRail by calling the function with
  the suffix `_ster`. For example, write `DB.get_v1_ster` instead of
  `DB.get_v1`.

- to get a blank, use the word `blank`

- to get a partial, use `partial "message" innerExpr`

- to produce results that are hard to otherwise create, you can add functions
  to LibTest.fs. For example, `Test.nan_v0` produces a `NaN` float, and
  `Test.typeError` produces a built-in error.

When new langauge constructs are added to Dark, updates are often also required
in `FSharpToExpr.fs`.