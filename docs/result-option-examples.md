# Results and Options: before and after

Use **Result** when failure needs an explanation: `Ok value` or `Error reason`.
Use **Option** when a value may simply be absent: `Some value` or `None`.

Previously, passing a failure back unchanged meant writing a `match` at each
step. `let!` removes those repeated failure arms. Keep using `match` when you
want to recover, choose a default, or handle different failures differently.

| Operand | What `let! x = operand` does |
| --- | --- |
| `Ok value` | Binds `x` to `value` and continues |
| `Error reason` | Immediately returns `Error reason` from the nearest boundary |
| `Some value` | Binds `x` to `value` and continues |
| `None` | Immediately returns `None` from the nearest boundary |

A boundary is a function or lambda. `let!` handles only the failure side: on
success you still write the constructor yourself, `Ok value` or `Some value`.

The examples are independent alternatives. Short snippets are function bodies
with the inputs described above them. Comments show expected results.

## 1. Handle one Result

**Before**

```dark
let next (input: String) : Stdlib.Result.Result<Int, Stdlib.Int.ParseError> =
  match Stdlib.Int.parse input with
  | Ok n -> Ok (n + 1)
  | Error e -> Error e
```

**After**

```dark
let next (input: String) : Stdlib.Result.Result<Int, Stdlib.Int.ParseError> =
  let! n = Stdlib.Int.parse input
  Ok (n + 1)

// next "4"       -> Ok 5
// next "invalid" -> Error Stdlib.Int.ParseError.BadFormat
```

## 2. Handle one Option

**Before**

```dark
let firstLength (items: List<String>) : Stdlib.Option.Option<Int> =
  match Stdlib.List.head items with
  | Some first -> Some (Stdlib.String.length first)
  | None -> None
```

**After**

```dark
let firstLength (items: List<String>) : Stdlib.Option.Option<Int> =
  let! first = Stdlib.List.head items
  Some (Stdlib.String.length first)

// firstLength [ "abc" ] -> Some 3
// firstLength []        -> None
```

## 3. Run several steps

Given strings `a` and `b`, return their sum or the first parse error.

**Before**

```dark
match Stdlib.Int.parse a with
| Error e -> Error e
| Ok x ->
  match Stdlib.Int.parse b with
  | Error e -> Error e
  | Ok y -> Ok (x + y)
```

**After**

```dark
let! x = Stdlib.Int.parse a
let! y = Stdlib.Int.parse b
Ok (x + y)

// "1", "2" -> Ok 3
// "x", "2" -> Error BadFormat; b is not parsed
// "1", "y" -> Error BadFormat
```

The same pattern works with several Options, ending in `Some`.

## 4. Give an error more context

Given a string `input`, turn its parse error into your own message.

**Before**

```dark
match Stdlib.Int.parse input with
| Ok n -> Ok (n + 1)
| Error _ -> Error "age must be a number"
```

**After**

```dark
let! n =
  (Stdlib.Int.parse input)
  |> Stdlib.Result.mapError (fun _ -> "age must be a number")
Ok (n + 1)
```

Map the error **before** `let!`: once a failure reaches `let!`, the function
returns immediately.

## 5. Convert between Option and Result

Given `maybeName : Option<String>`, supply an error for the missing case.

**Before**

```dark
match maybeName with
| Some name -> Ok (Stdlib.String.length name)
| None -> Error "name missing"
```

**After**

```dark
let! name = Stdlib.Option.toResult maybeName "name missing"
Ok (Stdlib.String.length name)
```

In the other direction, given `parsed : Result<Int, String>`, discard the error.

**Before**

```dark
match parsed with
| Ok n -> Some (n + 1)
| Error _ -> None
```

**After**

```dark
let! n = Stdlib.Result.toOption parsed
Some (n + 1)
```

Conversion is explicit. A Result cannot propagate from an Option-returning
function, or an Option from a Result-returning one.

## 6. Fail a validation deliberately

Given an integer `age`, reject negative values.

**Before**

```dark
if age < 0 then Error "negative age" else Ok age
```

**After, when this is one step in a larger function**

```dark
let! _ = if age < 0 then Error "negative age" else Ok ()
Ok age
```

For an Option, use `None` and `Some ()` instead. `_` discards the success
payload; it does not discard a failure. The original `if` remains simpler
when validation is the whole function.

## 7. Unwrap nested containers, one level at a time

Given `value : Result<Result<Int, String>, String>`:

**Before**

```dark
match value with
| Error e -> Error e
| Ok inner ->
  match inner with
  | Error e -> Error e
  | Ok n -> Ok (n + 1)
```

**After**

```dark
let! inner = value
let! n = inner
Ok (n + 1)

// Ok (Ok 4)          -> Ok 5
// Ok (Error "inner") -> Error "inner"
// Error "outer"      -> Error "outer"
```

For optional record fields, the same pattern applies. Given
`user : Option<User>` where `User.email : Option<String>`:

```dark
// Before
match user with
| None -> None
| Some u ->
  match u.email with
  | None -> None
  | Some email -> Some (Stdlib.String.length email)

// After
let! u = user
let! email = u.email
Some (Stdlib.String.length email)
```

## 8. Destructure the success payload

Given `pair : Option<(Int * Int)>`:

```dark
// Before
match pair with
| Some ((a, b)) -> Some (a + b)
| None -> None

// After
let! (a, b) = pair
Some (a + b)

// Some ((2, 3)) -> Some 5
// None          -> None
```

Ordinary let patterns work: a name, a tuple, `_`, or `()`.
Use `let! () = step` only when the success payload is Unit.

## 9. Handle each callback independently

```dark
// Before
Stdlib.List.map [ Some 1, None, Some 3 ] (fun item ->
  match item with
  | Some n -> Some (n + 1)
  | None -> None)

// After
Stdlib.List.map [ Some 1, None, Some 3 ] (fun item ->
  let! n = item
  Some (n + 1))

// Both produce [ Some 2, None, Some 4 ].
```

The failure exits that callback invocation. It does not exit `List.map` or
the function containing the map. A nested named function behaves the same way.

## 10. Handle a failure locally, then continue

Given a string `input`, this function body returns a plain String.

**Before**

```dark
let computed =
  match Stdlib.Int.parse input with
  | Ok n -> Ok (n * 2)
  | Error e -> Error e
match computed with
| Ok n -> "double: " ++ Stdlib.Int.toString n
| Error _ -> "not a number"
```

**After**

```dark
let double () : Stdlib.Result.Result<Int, Stdlib.Int.ParseError> =
  let! n = Stdlib.Int.parse input
  Ok (n * 2)
match double () with
| Ok n -> "double: " ++ Stdlib.Int.toString n
| Error _ -> "not a number"
```

The nested function is the boundary: a failure leaves `double`, then the outer
function handles its result. Written directly in the outer function, `let!`
would try to return an Error from a function that returns String, which is a
type error.

## 11. Only the chosen branch runs

Given `choose : Bool` and `value : Option<Int>`:

```dark
// Before
if choose then
  match value with
  | Some n -> Some (n + 1)
  | None -> None
else
  Some 12

// After
if choose then
  let! n = value
  Some (n + 1)
else
  Some 12

// false, None -> Some 12
// true, None  -> None
```

Each `let!` operand is evaluated once. A failure skips everything after it
in that function. Effects already performed are not rolled back.

For example, if `read ()` and `save n` are your own functions:

```dark
// Before
match read () with
| Error e -> Error e
| Ok n ->
  let _ = save n
  Ok n

// After
let! n = read ()
let _ = save n
Ok n
```

In both versions, `read` runs once and `save` runs only on success.
Propagation is an ordinary return; it does not catch runtime exceptions.

## 12. Change the success type, keep the error type

Given `value : Result<Int, String>`, return `Result<String, String>`:

```dark
// Before
match value with
| Ok n -> Ok (Stdlib.Int.toString n)
| Error e -> Error e

// After
let! n = value
Ok (Stdlib.Int.toString n)
```

The input and output success types may differ. The error type must agree,
because the same error is returned. Option success types may differ too.
Type aliases do not change these rules.

An empty error payload still keeps any explicit error type:
`Result<Int, List<String>>.Error []` cannot propagate into a function returning
`Result<Int, List<Int>>`. The empty list does not make the types interchangeable.

## 13. Functions, recursion, and partial application still work

Given `maybeFn : Option<Int -> Int>`:

```dark
// Before
match maybeFn with
| Some f -> Some (f 4)
| None -> None

// After
let! f = maybeFn
Some (f 4)
```

A recursive call is an ordinary operand too:

```dark
// Before
match recursive (count - 1) value with
| Some n -> Some (n + 1)
| None -> None

// After: the recursive case of an Option-returning function
let! n = recursive (count - 1) value
Some (n + 1)
```

For a function `combine` taking two Options and returning an Option, partial
application also stays the same:

```dark
let withFirst = combine (Some 2)

// Before
match withFirst maybeSecond with
| Some n -> Some (n + 1)
| None -> None

// After
let! n = withFirst maybeSecond
Some (n + 1)
```

## 14. Syntax and rejected cases

On one line, use `in` to separate a binding from its body:

```dark
let! n = Stdlib.Int.parse "7" in Ok (n * 2)
// Ok 14
```

| Code or situation | Rule |
| --- | --- |
| `let !x = ...` | The `!` must touch `let`: write `let! x = ...` |
| `let! f (x: Int) = ...` | `let!` binds a value; use ordinary `let` to define a function |
| Top-level `let! x = Some 1 in x` | Needs a function or lambda boundary |
| `let! x = Ok 1` in an Option-returning function | Wrong container; convert the Result first |
| `let! x = Some 1` in a Result-returning function | Wrong container; convert the Option first |
| `let! x = 4` | The operand must be an Option or Result |
| A function ending `let! n = value in n` | No automatic success wrapping; return `Ok n` or `Some n` if `n` is plain |
| Propagation in a SQL query lambda | Unsupported by the SQL compiler |

## 15. Current limitations

**Scripts and `eval` skip static type checking.** `let!` still checks its
container at runtime wherever the code shows it: a declared return type, or a
lambda ending in `Some`/`Ok`. A lambda whose final expression hides the
container, such as `wrap n`, may let a wrong-kind failure through. Package
checking rejects the same code. An alias in a declared return type can also
postpone detection until the failure reaches the function's return-type check.

```dark
let wrap (n: Int) : Stdlib.Option.Option<Int> = Some n

// Invalid mixing, but currently returns Error "x" in scripts / eval.
(fun value ->
  let! n = value
  wrap n) (Error "x")
```

**Some inferred record-field chains remain incomplete in the checker.** For
example, a helper using `row.child.value` through an inferred parameter may
run correctly but receive an incomplete verdict. This predates `let!`.

For the precise rules, see [error-propagation.md](error-propagation.md).
