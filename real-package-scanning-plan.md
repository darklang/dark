# Real Package Scanning Implementation Plan

## Overview
Transition from mock data to real package scanning for toplevel discovery.

## Phase 1: Package Scanner Foundation

### 1.1 Create Package Scanner Module
```darklang
module Darklang.Cli2.PackageScanner
```

Functions needed:
- `scanAllPackages(): List<Package>`
- `getPackageFunctions(packageName: String): List<Function>`
- `getPackageConstants(packageName: String): List<Constant>`
- `getPackageTypes(packageName: String): List<Type>`

### 1.2 Type Matching
- `isHttpHandler(value: Any): Bool` - Check if value matches HttpHandler type
- `isScript(value: Any): Bool` - Check if value matches Script type
- `isTest(value: Any): Bool` - Check if value matches Test type
- etc. for all toplevel types

## Phase 2: Toplevel Discovery

### 2.1 Value Scanner
```darklang
let scanForToplevelValues(typeName: String): List<ToplevelInstance>
```

Process:
1. Iterate through all packages
2. Check all constants for matching types
3. Build list of matching instances with metadata

### 2.2 Metadata Extraction
Extract from each toplevel instance:
- Name
- Package location
- Type
- Description (if available)
- Creation date
- Dependencies

## Phase 3: Navigation Integration

### 3.1 Enhanced Navigation Commands
- `cd toplevels/<type>` - Navigate into toplevel categories
- `ls toplevels` - List all toplevel categories with counts
- `view <toplevel-instance>` - Show detailed information

### 3.2 Search and Filter
- `search <pattern>` - Search across all toplevels
- `filter <type> <criteria>` - Filter toplevels by criteria

## Phase 4: Caching and Performance

### 4.1 Cache Strategy
- Cache toplevel metadata on first scan
- Refresh cache on package changes
- Store in memory for session

### 4.2 Lazy Loading
- Load toplevel details only when requested
- Paginate large result sets
- Progressive loading for UI responsiveness

## Phase 5: Creation Helpers

### 5.1 Template Generators
```darklang
let createHttpHandler(path: String, method: String): HttpHandler
let createScript(name: String, description: String): Script
let createTest(name: String): Test
```

### 5.2 Interactive Creation
- Wizard-style creation through CLI
- Validation and error handling
- Auto-save to appropriate package

## Implementation Order

1. **Week 1**: Package Scanner Foundation
   - Basic package iteration
   - Type checking infrastructure

2. **Week 2**: Toplevel Discovery
   - Value scanning
   - Metadata extraction

3. **Week 3**: Navigation Integration
   - CLI commands
   - Display formatting

4. **Week 4**: Performance & Polish
   - Caching
   - Creation helpers
   - Testing

## Technical Challenges

### Challenge 1: Type Checking at Runtime
Darklang's type system may not expose enough runtime type information.
**Solution**: Use naming conventions and structural matching.

### Challenge 2: Performance with Large Codebases
Scanning all packages could be slow.
**Solution**: Implement caching and lazy loading from the start.

### Challenge 3: Package Access
Need to access package internals from CLI.
**Solution**: Use PackageManager APIs or create new access functions.

## Success Metrics

- [ ] Can discover all toplevels in < 1 second
- [ ] Navigation feels instant (< 100ms response)
- [ ] Zero false positives in type matching
- [ ] Creation helpers work for all toplevel types
- [ ] Cache invalidation works correctly

## Next Steps

1. Research PackageManager API capabilities
2. Create prototype scanner for one toplevel type
3. Benchmark scanning performance
4. Design cache schema
5. Implement first scanner module