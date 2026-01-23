# Reflection and CLI Integration Report

## Executive Summary

This document explores how Darklang's enhanced reflection capabilities can improve the CLI experience and potentially integrate with the app/editor. The reflection system now includes three new builtins (`reflectType`, `typeOf`, `typeName`) and a comprehensive package library for type introspection. This report focuses on practical CLI enhancements while keeping F# code changes minimal.

## Current Reflection Capabilities

### New Builtins (F# Code)
1. **`Builtin.reflect`** (existing) - Full Dval meta-representation
2. **`Builtin.reflectType`** (new) - Lightweight ValueType extraction
3. **`Builtin.typeOf`** (new) - Human-readable type names (e.g., "Int64", "List<String>")
4. **`Builtin.typeName`** (new) - FQTypeName for custom types (records/enums)

### Package Library (Darklang Code)
- `Darklang.LanguageTools.Reflection` module with helper functions:
  - Type checking: `isPrimitive`, `isCollection`, `isCustomType`, `isFunction`
  - Type-specific checks: `isList`, `isDict`, `isTuple`, `isRecord`, `isEnum`
  - Data extraction: `getFields`, `getCaseName`, `getListElementType`, `getDictValueType`
  - Utility functions: `getTypeName`, `getValueType`

## CLI Integration Ideas

### 1. Interactive Type Inspection Commands

#### `darklang inspect <expression>`
Evaluate an expression and show detailed type information:

```bash
$ darklang inspect "{ name: 'Alice', age: 30L }"
Type: Record<Person>
Fields:
  - name: String = "Alice"
  - age: Int64 = 30
```

**Implementation Approach:**
- Execute the expression in CLI context
- Use `Builtin.typeOf` for type name
- Use `Reflection.getFields` to extract field information
- Pretty-print the structure
- **F# Impact:** Minimal - just CLI command parsing and output formatting

#### `darklang type <expression>`
Show only the type without evaluating side effects:

```bash
$ darklang type "Stdlib.List.map [1L; 2L; 3L]"
(List<'a> -> ('a -> 'b) -> List<'b>)
Partially applied with: List<Int64>
```

**Implementation Approach:**
- Use type inference from existing type checker
- Combine with reflection for runtime type information
- **F# Impact:** Minimal - leverages existing type checker

### 2. REPL-Style Exploration

#### Enhanced REPL with Type Display
Every evaluation in a REPL could show both value and type:

```
darklang> let x = [1L; 2L; 3L]
x : List<Int64> = [1, 2, 3]

darklang> Builtin.typeOf x
String = "List<Int64>"

darklang> Reflection.isList x
Bool = true
```

**Implementation Approach:**
- After each expression evaluation, call `Builtin.typeOf`
- Display type annotation alongside result
- Provide tab-completion using type information
- **F# Impact:** Minimal - output formatting only

### 3. Runtime Debugging and Introspection

#### Stack Trace Enhancement
When errors occur, use reflection to show actual runtime types:

```
Error: Type mismatch in function call
Expected: String
Received: Int64 (value: 42)

At: myFunction:15
```

**Implementation Approach:**
- Already have runtime type information in error handlers
- Use `Builtin.typeOf` to format error messages
- Minimal changes to existing error reporting
- **F# Impact:** Very minimal - error message formatting

#### Value Inspector
A `darklang debug <file>` command that shows all values and their types:

```bash
$ darklang debug script.dark
Variables in scope:
  - user: Record<User> { name: "Bob", id: 123L }
  - active: Bool = true
  - scores: List<Int64> = [95, 87, 92]
```

**Implementation Approach:**
- Execute script with tracing enabled
- Capture variable bindings from execution state
- Use reflection to format each value
- **F# Impact:** Minimal - uses existing execution tracing

### 4. Schema Inference and Validation

#### `darklang infer-schema <data-file.json>`
Infer Darklang type definitions from data:

```bash
$ darklang infer-schema users.json
Suggested type definition:

type User = {
  name: String
  age: Int64
  email: String
  active: Bool
}
```

**Implementation Approach:**
- Parse JSON into Dvals
- Use reflection to examine structure
- Use `Reflection.getFields` and type helpers
- Generate type definition syntax
- **F# Impact:** Minimal - JSON parsing + reflection usage

#### Type Validation
Validate that data conforms to expected schema:

```bash
$ darklang validate --type User data.json
✓ All records match User type
✗ 3 records have extra field 'phone'
✗ 1 record missing required field 'email'
```

**Implementation Approach:**
- Load type definition
- Parse data file
- Use reflection to compare structure
- **F# Impact:** None - pure Darklang code

### 5. Documentation Generation

#### Auto-generate Type Documentation
Generate markdown docs from type definitions using reflection:

```bash
$ darklang doc --type User
# User

A record type representing a user.

## Fields
- `name`: String - The user's full name
- `age`: Int64 - The user's age in years
- `email`: String - Contact email address
```

**Implementation Approach:**
- Parse type definitions from package files
- Extract field information
- Format as markdown
- **F# Impact:** Minimal - file parsing and markdown generation

### 6. Enhanced Pretty-Printing

#### Smart Value Display
Use reflection for context-aware formatting:

```bash
$ darklang eval script.dark --pretty
Result: Person {
  name: "Alice"
  age: 30
  friends: List<Person> [2 items]
  metadata: Dict<String> [5 entries]
}
```

**Implementation Approach:**
- Use `Reflection.prettyPrint` from demo module
- Recursively format nested structures
- Limit depth for large structures
- **F# Impact:** None - pure Darklang in prettyPrint function

### 7. Performance Profiling by Type

#### `darklang profile <script>`
Profile execution time by value types:

```bash
$ darklang profile expensive-script.dark
Type allocation breakdown:
  - List<Int64>: 45% of allocations
  - Dict<String>: 30% of allocations
  - String: 15% of allocations
  - Other: 10% of allocations
```

**Implementation Approach:**
- Hook into execution tracer
- Track allocations with `Builtin.typeOf`
- Aggregate statistics
- **F# Impact:** Minimal - extends existing tracer

## App/Editor Integration Possibilities

### 1. Hover Type Information
When hovering over a value in the editor:
- Show type using `Builtin.typeOf`
- For custom types, show structure using `Reflection.getFields`
- Display type documentation

### 2. Runtime Value Inspector
While debugging, inspect values at any point:
- Click on a value to see full type structure
- Expand nested records/enums
- Use reflection to navigate complex data

### 3. Type-Aware Auto-completion
Suggest fields and methods based on runtime type:
- Use `Reflection.getFields` for record field completion
- Filter suggestions by actual type

### 4. Data Visualizer
Visualize complex data structures:
- Use reflection to build tree view
- Special rendering for lists, dicts, records
- Interactive exploration of nested structures

### 5. Schema Validator UI
Visual schema validation:
- Upload data file
- Compare against type definition
- Highlight mismatches using reflection

## Implementation Priorities

### High Priority (Immediate Value)
1. **Enhanced error messages** - Better type information in errors
2. **`darklang inspect`** - Quick type inspection command
3. **REPL type display** - Show types in interactive sessions
4. **Pretty-printing** - Reflection-based value formatting

### Medium Priority (Near-term)
1. **Schema inference** - Generate types from data
2. **Type validation** - Validate data against schemas
3. **Documentation generation** - Auto-docs from types
4. **Debug mode** - Enhanced variable inspection

### Lower Priority (Future)
1. **Performance profiling** - Type-based allocation tracking
2. **Editor integration** - Hover types and value inspection
3. **Data visualizer** - Interactive structure explorer
4. **Advanced REPL** - Full type-aware completion

## Technical Considerations

### Performance
- Reflection operations are generally fast
- `Builtin.typeOf` is lightweight (simple pattern matching)
- `Builtin.reflectType` avoids full Dval serialization
- Cache reflection results where possible

### Security/Privacy
- Reflection exposes internal structure
- For CLI tools: Not a concern (user's own code)
- For web/API: May need reflection permissions/limits
- Don't expose reflection in untrusted contexts

### Backward Compatibility
- All reflection features are opt-in
- Existing code unaffected
- Can be introduced gradually

### F# Code Impact
The design deliberately minimizes F# changes:
- Three new builtins (already implemented)
- Rest is pure Darklang package code
- CLI commands use existing execution infrastructure
- Leverages existing type checker and tracer

## Conclusion

Darklang's enhanced reflection capabilities enable significant CLI and editor improvements with minimal F# code changes. The key insight is that most features can be built as:

1. **CLI command parsing** (thin F# layer)
2. **Darklang reflection code** (package functions)
3. **Output formatting** (F# or external tools)

This approach keeps the core language simple while enabling powerful developer tools. The reflection system is production-ready for CLI tools and can be gradually expanded to editor integration.

### Next Steps
1. Validate that new builtins work correctly (tests passing)
2. Implement highest-priority CLI commands (`inspect`, pretty-print)
3. Gather user feedback on which features provide most value
4. Iterate on API design based on real usage

The reflection system is now ready for experimentation and real-world use in CLI tooling.
