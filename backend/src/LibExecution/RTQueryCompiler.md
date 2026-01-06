# RTQueryCompiler

## The Core Idea

**RTQueryCompiler converts VM instructions into SQL queries.**

When you write a DB query lambda like `(fun p -> p.age > 21L)`, Darklang compiles it to VM instructions (bytecode). RTQueryCompiler then takes those instructions and generates equivalent SQL:

```
Darklang Lambda          VM Instructions              SQL
─────────────────────────────────────────────────────────────────
(fun p -> p.age > 21L)   LoadVal(1, DBRow)            SELECT * FROM table
                     →   GetRecordField(2, 1, "age")  →   WHERE (data->>'age')::bigint > @p1
                         LoadVal(3, 21L)                  -- @p1 = 21
                         Apply(4, ">", [2, 3])
```

Working on instructions means we can compile **any lambda at runtime**, even ones stored in variables or passed as arguments.

## How Instructions Become SQL

The compiler processes each VM instruction and builds up a **symbolic representation** of what the query is doing:

| VM Instruction | What It Means | SQL Output |
|---------------|---------------|------------|
| `LoadVal(reg, DBRow)` | Lambda parameter `p` | (tracked as "the row") |
| `GetRecordField(to, from, "name")` | Field access `p.name` | `(data->>'name')` |
| `LoadVal(reg, DInt64 42)` | Literal value | `@p1` (parameter) |
| `Apply(to, ">", [a, b])` | Comparison | `a > b` |
| `Apply(to, "&&", [a, b])` | Boolean AND | `a AND b` |
| `Apply(to, "Int64.add", [a, b])` | Arithmetic | `(a + b)` |

**Complete Example:**
```darklang
friends (fun p -> p.age > 21L && p.human == true)
```

**VM Instructions:**
```
reg0 = LoadVal(DBRow)                    // p (the lambda parameter)
reg1 = GetRecordField(reg0, "age")       // p.age
reg2 = LoadVal(DInt64 21)                // 21L
reg3 = Apply(">", [reg1, reg2])          // p.age > 21L
reg4 = GetRecordField(reg0, "human")     // p.human
reg5 = LoadVal(DBool true)               // true
reg6 = Apply("==", [reg4, reg5])         // p.human == true
reg7 = Apply("&&", [reg3, reg6])         // (p.age > 21L) && (p.human == true)
```

**SQL Output:**
```sql
SELECT * FROM PersonDB
WHERE ((data->>'age')::bigint > @p1) AND ((data->>'human')::boolean = @p2)
-- Parameters: @p1 = 21, @p2 = true
```

## Symbolic Execution

Rather than actually executing instructions, RTQueryCompiler does **symbolic execution**. Each register holds a `SymbolicValue` representing what it *would* contain:

```fsharp
type SymbolicValue =
  | DBRow                              // The lambda parameter (the row being filtered)
  | DBField of path: List<string>      // Field access: p.name → ["name"]
  | Literal of Dval                    // A concrete value (becomes SQL parameter)
  | Comparison of left * op * right    // p.age > 21
  | BoolAnd of left * right            // expr && expr
  | BoolOr of left * right             // expr || expr
  | BoolNot of inner                   // not expr
  | SqlFnCall of SqlSpec * args        // Function with SQL equivalent
  | Unknown of reason                  // Can't compile to SQL
```

After processing all instructions, the final register contains a symbolic tree. This tree is then converted to SQL.

## Partial Evaluation

Expressions that don't depend on the DB row are evaluated at compile time:

```darklang
let threshold = 60L + 5L
friends (fun p -> p.height > threshold)
```

Here `60L + 5L` doesn't involve `p`, so it's evaluated to `65L` before SQL generation. The SQL just sees `@p1 = 65`.

This also works for function calls:
```darklang
let rossDOB () : DateTime = Stdlib.DateTime.parse "1967-05-12T00:00:00Z"
friends (fun p -> p.dob == rossDOB())
```

The `rossDOB()` call is executed at compile time, and its result becomes a SQL parameter.

## Function Inlining

User-defined functions are inlined up to 3 levels deep:

```darklang
let isAdult (person: Person) : Bool = person.age >= 18L
friends (fun p -> isAdult p)
```

RTQueryCompiler looks up `isAdult`'s body instructions and symbolically executes them, effectively inlining the function. The SQL becomes:
```sql
WHERE (data->>'age')::bigint >= @p1  -- @p1 = 18
```

## SqlSpec

Built-in functions declare their SQL equivalent via `SqlSpec`:

```fsharp
// In function definitions:
sqlSpec = SqlBinOp ">"           // Stdlib.Int64.greaterThan
sqlSpec = SqlBinOp "+"           // Stdlib.Int64.add
sqlSpec = SqlFunction "LOWER"    // Stdlib.String.toLowercase
sqlSpec = NotQueryable           // Can't be used in queries
```

When RTQueryCompiler encounters a function call, it looks up the SqlSpec to determine how to generate SQL.

## Integration with DB.fs

DB query functions use RTQueryCompiler like this:

```fsharp
| exeState, vm, _, [ DDB dbname; DApplicable(AppLambda appLambda) ] ->
  uply {
    let db = exeState.program.dbs[dbname]

    // Compile lambda to SQL
    let! compiled = compileQueryLambda exeState vm appLambda

    // Execute the SQL query
    return! UserDB.executeCompiledQuery exeState vm db compiled.sql compiled.paramValues
  }
```

The `compileQueryLambda` helper:
1. Gets the lambda's instructions from the VM cache (`vm.lambdaInstrCache`)
2. Gets captured closure values from `appLambda.closedRegisters`
3. Calls `RTQueryCompiler.compileLambda` to generate SQL

## Error Handling

Compilation errors are raised as `RuntimeError.SqlCompiler` with a friendly message:

- **Undefined variable**: `"This variable is not defined: y"`
- **Non-bool return**: `"Incorrect type in String \"x\", expected Bool"`
- **Unsupported operation**: `"Nested lambdas not supported in SQL queries"`

## What's Supported

| Feature | Status |
|---------|--------|
| Field access (`p.name`, `p.address.city`) | ✅ |
| Comparisons (`>`, `<`, `==`, `!=`, `>=`, `<=`) | ✅ |
| Boolean ops (`&&`, `\|\|`, `not`) | ✅ |
| Int64/Float arithmetic (`+`, `-`, `*`, `/`) | ✅ |
| String functions (`toLowercase`, `toUppercase`, etc.) | ✅ |
| DateTime comparisons | ✅ |
| Let bindings | ✅ |
| User-defined functions (inlined) | ✅ |
| Closures (captured variables) | ✅ |
| Partial evaluation | ✅ |

## What's Not Supported

| Feature | Reason |
|---------|--------|
| Nested lambdas | SQL doesn't support lambdas |
| List/Dict operations | Complex SQL generation needed |
| Pattern matching | Would need SQL CASE statements |
| String.reverse | SQLite lacks REVERSE function |
| DateTime.year/month/day | Uses Postgres-specific `date_part` |

## Files

| File | Purpose |
|------|---------|
| `LibExecution/RTQueryCompiler.fs` | The compiler implementation |
| `LibExecution/RuntimeTypes.fs` | `SqlCompiler` error type |
| `BuiltinCloudExecution/Libs/DB.fs` | DB functions that use the compiler |
| `LibCloud/SqlCompiler.fs` | Just the error message template |
