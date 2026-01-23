# SQLite DSL Design Document

## Context
The core SQLite builtins are complete and working. This document explores whether a DSL for querying SQLite would add value.

## Existing Query Compiler Pattern
The codebase has `RTQueryCompiler.fs` which compiles Darklang lambdas to SQL for the Datastore. This works by:
1. Analyzing lambda bodies symbolically
2. Compiling field accesses and function calls to SQL
3. Handling parameters as SQL bind variables

## DSL Approaches Considered

### Option 1: Lambda-based Query Compiler (Similar to RTQueryCompiler)
```darklang
// Example usage
Stdlib.Cli.Sqlite.queryWhere "test.db" "users" (fun row ->
  row.age > 25L && row.name == "Alice"
)
```

**Pros:**
- Familiar Darklang syntax
- Type-safe field access
- Could leverage existing RTQueryCompiler patterns

**Cons:**
- Complex implementation (need to analyze lambda IR at runtime)
- Requires runtime compilation infrastructure
- Limited to simple predicates (complex queries still need raw SQL)
- Significant engineering effort for moderate benefit

### Option 2: Query Builder Pattern
```darklang
// Example usage
Stdlib.Cli.SqliteDsl.from "users"
|> Stdlib.Cli.SqliteDsl.where "age > ?" [25L]
|> Stdlib.Cli.SqliteDsl.andWhere "name = ?" ["Alice"]
|> Stdlib.Cli.SqliteDsl.select ["id"; "name"; "email"]
|> Stdlib.Cli.SqliteDsl.execute "test.db"
```

**Pros:**
- Simpler to implement
- Chainable, readable API
- Easy to understand and debug

**Cons:**
- Still string-based (SQL injection risk if misused)
- Not actually safer than raw SQL
- Adds abstraction without major benefit
- Verbose for simple queries

### Option 3: Record-based Query Builder
```darklang
// Example usage
Stdlib.Cli.SqliteDsl.query "test.db"
  { table = "users"
  ; where = Some "age > ?"
  ; params = [25L]
  ; select = Some ["id"; "name"]
  ; orderBy = Some "created_at DESC"
  ; limit = Some 10L }
```

**Pros:**
- Structured, type-safe configuration
- All options in one place
- Easy to compose programmatically

**Cons:**
- Still essentially wrapping SQL strings
- Not more powerful than raw SQL
- Verbose for simple cases

## Recommendation: **No DSL for Now**

### Rationale

1. **Raw SQL is the right abstraction for SQLite**
   - SQLite queries can be arbitrarily complex
   - SQL is already a well-known DSL
   - The builtin functions already provide safe parameterization

2. **Limited benefit vs. complexity**
   - A query builder doesn't eliminate SQL knowledge requirement
   - Complex queries would still need raw SQL escape hatch
   - Implementation effort doesn't justify marginal ergonomic improvement

3. **The current API is already good**
   - `execute`, `query`, `queryOne` cover the core use cases
   - Helper functions (`createTable`, `insert`, `update`, `delete`) provide convenient shortcuts
   - Users maintain full SQL power when needed

4. **Darklang philosophy**
   - Darklang aims for simplicity and directness
   - Raw SQL is more discoverable than a custom DSL
   - No magic—what you write is what runs

### Alternative: SQL Template Functions (Future Enhancement)

If ergonomics become an issue, consider adding template helpers:

```darklang
// Simple parameterized queries
Stdlib.Cli.Sqlite.select "test.db" "users"
  { where = ["age > ?"; "name = ?"]
  ; params = [25L; "Alice"] }
```

This is simpler than a full DSL and still transparent.

## Conclusion

**Skip the DSL**. The current raw SQL approach with helper functions is:
- Simpler to maintain
- More flexible
- Already safe (Result types, proper error handling)
- Doesn't hide complexity
- Sufficient for the spike goals

The core SQLite builtins complete the task successfully.
