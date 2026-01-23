# Task: Enhanced Reflection Capabilities for Darklang

## Overview
Review and expand the minimal reflection system in Darklang. Currently we only have `Builtin.reflect` which converts a Dval to its RuntimeTypes representation. This task will add more reflection capabilities, create supporting package functions, add tests/demos, and explore CLI/app integration possibilities.

## Current State
- **Existing builtin**: `Builtin.reflect` in `backend/src/BuiltinExecution/Libs/Reflection.fs`
  - Takes any value and returns `LanguageTools.RuntimeTypes.Dval` representation
  - Uses `RuntimeTypesToDarkTypes.Dval.toDT` for conversion
- **RuntimeTypes**: Defined in `packages/darklang/languageTools/runtimeTypes.dark`
  - Includes Dval, TypeReference, ValueType, KnownType, FQFnName, etc.
- **Conversion layer**: `backend/src/LibExecution/RuntimeTypesToDarkTypes.fs`
  - Bidirectional conversion between F# RuntimeTypes and Darklang values

## Implementation Tasks

### Phase 1: Research & Design (COMPLETED)
- [x] Review existing reflection implementation
- [x] Study RuntimeTypes structure and conversion layer
- [x] Identify useful reflection operations
- [x] Review CLI execution model

### Phase 2: Add Core Builtins (F# Code - Minimal Impact) ✅ COMPLETED
- [x] Add `Builtin.reflectType` - get the ValueType/KnownType of a value without full Dval structure
  - File: `backend/src/BuiltinExecution/Libs/Reflection.fs`
  - Returns `LanguageTools.RuntimeTypes.ValueType`
  - Useful for type inspection without heavy Dval conversion

- [x] Add `Builtin.typeOf` - simplified type name as string
  - File: `backend/src/BuiltinExecution/Libs/Reflection.fs`
  - Returns human-readable type name (e.g., "Int64", "List<String>", "Dict<Bool>")
  - Lightweight alternative to full reflection

- [x] Add `Builtin.typeName` - get FQTypeName for custom types
  - File: `backend/src/BuiltinExecution/Libs/Reflection.fs`
  - Returns `Option<LanguageTools.RuntimeTypes.FQTypeName>`
  - Returns Some for DRecord/DEnum, None for primitives

### Phase 3: Package Functions (Darklang Code) ✅ COMPLETED
- [x] Create `Darklang.LanguageTools.Reflection` package module
  - File: `packages/darklang/languageTools/reflection.dark`

- [x] Add helper functions in reflection.dark:
  - `isPrimitive: Dval -> Bool` - check if value is a primitive type
  - `isCollection: Dval -> Bool` - check if List/Dict/Tuple
  - `isCustomType: Dval -> Bool` - check if Record/Enum
  - `isFunction: Dval -> Bool` - check if DApplicable
  - `getTypeName: Dval -> String` - wrapper around Builtin.typeOf
  - `getFields: Dval -> Option<Dict<Dval>>` - extract record fields if applicable
  - `getCaseName: Dval -> Option<String>` - extract enum case name if applicable
  - `getListElementType: Dval -> Option<ValueType>` - extract List element type
  - `getDictValueType: Dval -> Option<ValueType>` - extract Dict value type

### Phase 4: Tests & Demos ✅ COMPLETED
- [x] Add F# unit tests for new builtins
  - File: `backend/testfiles/execution/stdlib/reflection.dark`
  - Test reflectType, typeOf, typeName with various value types
  - Test primitives, collections, records, enums, functions

- [x] Create demo/test script in Darklang
  - File: `packages/darklang/languageTools/reflection-demo.dark`
  - Demonstrate reflection on various types
  - Show practical use cases (serialization, validation, debugging)

- [x] Add integration test via CLI
  - File: `backend/testfiles/execution/cli/reflection-test.dark`
  - Small CLI script that uses reflection to inspect values
  - Demonstrates CLI experience with reflection

### Phase 5: CLI Integration Report ✅ COMPLETED
- [x] Create reflection-cli-integration.md report
  - File: `.claude-task/reflection-cli-integration.md`
  - Analyze how reflection could enhance CLI experience
  - Ideas for CLI commands using reflection:
    - `darklang inspect <expr>` - inspect type and value structure
    - `darklang type <expr>` - show type information
    - REPL-style exploration with reflection
    - Type-aware serialization/pretty-printing
    - Runtime debugging and introspection
  - Consider app/editor integration:
    - Type hints and tooltips using reflection
    - Runtime value inspection in debugger
    - Auto-completion based on reflected types
    - Schema inference and validation
  - Discuss tradeoffs and implementation considerations
  - Keep F# code impact minimal - focus on what can be done with existing infrastructure

### Phase 6: Documentation & Cleanup ✅ COMPLETED
- [x] Add docstrings/comments to new builtins
- [x] Ensure new package functions have descriptions
- [x] Update any relevant documentation files
- [x] Verify all tests pass
- [x] Final code review and cleanup

## Success Criteria
1. At least 3 new useful builtins added to Reflection.fs
2. Comprehensive package functions in LanguageTools.Reflection module
3. Working tests/demos that exercise the reflection capabilities
4. Thoughtful CLI integration report with concrete ideas
5. Minimal F# code changes (focused on Reflection.fs and tests)
6. All existing tests continue to pass

## Notes
- Keep F# changes minimal and focused on Reflection.fs
- Most functionality should be in Darklang package code
- Prioritize practical, useful reflection operations
- Consider performance implications of reflection operations
- Think about security/privacy - what should/shouldn't be reflectable?
