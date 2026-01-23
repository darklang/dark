# SQLite Spike - Access & DSL Implementation Plan

## Context
- SQLite embedded via Fumble library (wraps Microsoft.Data.Sqlite v8.0.1)
- Current internal DB access: `/home/dark/app/rundir/data.db`
- Builtins pattern: F# code in `backend/src/BuiltinCli/Libs/` → exposed as `Stdlib.Module.functionName`
- Tests: `.dark` files with test functions returning `TestResult`

## Phase 1: Core SQLite Builtins (Minimal Set)

### 1.1 Create F# Builtin Library
- [ ] Create `/home/dark/app/backend/src/BuiltinCli/Libs/Sqlite.fs`
- [ ] Implement `sqliteOpen` - open connection to .db file path, return connection handle
- [ ] Implement `sqliteClose` - close connection handle
- [ ] Implement `sqliteExecute` - execute SQL (INSERT/UPDATE/DELETE/CREATE), return affected rows count
- [ ] Implement `sqliteQuery` - execute SELECT query, return List of Dict results
- [ ] Implement `sqliteQueryOne` - execute SELECT query, return Option of Dict (first row)
- [ ] Add error handling - wrap results in Result type for all operations
- [ ] Register Sqlite module in `/home/dark/app/backend/src/BuiltinCli/Builtin.fs`

### 1.2 Update Build Configuration
- [ ] Add Sqlite.fs to `/home/dark/app/backend/src/BuiltinCli/BuiltinCli.fsproj`
- [ ] Verify Fumble dependency is available in BuiltinCli project

### 1.3 Create Darklang Wrapper Package
- [ ] Create `/home/dark/app/packages/darklang/stdlib/sqlite.dark`
- [ ] Wrap builtin functions with ergonomic Darklang API
- [ ] Add inline documentation for each function

## Phase 2: Testing

### 2.1 Create Test Database
- [ ] Create test .db file in `/home/dark/app/backend/testfiles/test.db` or use temporary path
- [ ] Write SQL schema setup for test tables

### 2.2 Implement Darklang Tests
- [ ] Create `/home/dark/app/packages/darklang/stdlib/tests/sqlite.dark`
- [ ] Test `sqliteOpen` and `sqliteClose` operations
- [ ] Test `sqliteExecute` - CREATE TABLE, INSERT, UPDATE, DELETE
- [ ] Test `sqliteQuery` - SELECT with multiple rows
- [ ] Test `sqliteQueryOne` - SELECT single row
- [ ] Test error cases - invalid SQL, file not found, etc.
- [ ] Test connection lifecycle and cleanup

## Phase 3: DSL Experiment (Optional/Separate)

### 3.1 Design Query DSL
- [ ] Research: Review existing `/home/dark/app/backend/src/LibExecution/RTQueryCompiler.fs` for patterns
- [ ] Design DSL syntax - consider builder pattern or query combinators
- [ ] Document DSL design decisions in `/home/dark/app/.claude-task/dsl-design.md`

### 3.2 Implement DSL (if viable)
- [ ] Create `/home/dark/app/backend/src/BuiltinCli/Libs/SqliteDsl.fs` (separate from core)
- [ ] Implement DSL → SQL compilation
- [ ] Create Darklang wrapper in `/home/dark/app/packages/darklang/stdlib/sqliteDsl.dark`
- [ ] Add DSL tests to verify query generation

### 3.3 DSL Evaluation
- [ ] Document pros/cons of DSL approach
- [ ] Make recommendation: keep or remove
- [ ] If removing, ensure it's cleanly separated and doesn't affect Phase 1/2

## Phase 4: CLI & VS Code Updates (If Relevant)

### 4.1 CLI Updates
- [ ] Check if CLI commands need updates for SQLite features
- [ ] Update CLI help text if new commands added

### 4.2 VS Code Extension
- [ ] Check if autocomplete/IntelliSense needs updates for new Stdlib.Sqlite module
- [ ] Verify syntax highlighting works for .dark files with SQLite code

## Phase 5: Documentation & Polish

### 5.1 Documentation
- [ ] Add examples to package documentation
- [ ] Document common patterns (connection pooling, transactions if supported)
- [ ] Add migration notes if this changes existing APIs

### 5.2 Final Testing
- [ ] Run full test suite to ensure no regressions
- [ ] Test with real-world use cases (user .db files)
- [ ] Verify error messages are helpful

### 5.3 Cleanup
- [ ] Remove debug/temporary code
- [ ] Ensure consistent naming conventions
- [ ] Final code review

## Design Decisions

### Connection Management
- **Decision**: Use file path strings as connection identifiers, manage connections internally
- **Alternative**: Expose connection handles as opaque types
- **Rationale**: Simpler API, automatic cleanup, matches Darklang patterns

### Type Mapping
- **SQLite → Darklang**:
  - INTEGER → Int64
  - REAL → Float
  - TEXT → String
  - BLOB → List<UInt8>
  - NULL → Option None

### Error Handling
- All functions return `Result<T, String>` for predictable error handling
- Error messages include context (file path, SQL snippet)

### Query Results
- Return `List<Dict<String, T>>` where each Dict is a row
- Column names as Dict keys
- Handle type conversions gracefully

## Out of Scope
- Connection pooling (initial implementation)
- Transaction support beyond single statements
- Prepared statement caching
- Async/streaming large result sets
- Write-ahead log (WAL) configuration (use defaults)

## Success Criteria
1. Can open user .db files from Darklang code
2. Can execute arbitrary SQL (CREATE, INSERT, UPDATE, DELETE, SELECT)
3. Can query and receive results as Darklang data structures
4. Comprehensive tests pass
5. Error handling is robust
6. DSL evaluation complete (keep or remove decision made)
