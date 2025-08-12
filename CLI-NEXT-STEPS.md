# CLI Next Steps Plan

## Immediate Priorities

### 1. Fix Integration Test Failures
- **Current Status**: 2 failing tests out of comprehensive test suite
- **Action**: Debug and resolve remaining test failures
- **Impact**: Ensures stability before wider deployment

### 2. Real Data Integration for Canvas Toplevels
- **Current Status**: Canvas commands show realistic but static data
- **Action**: Connect `handlers`, `databases`, `secrets` commands to live canvas data
- **Implementation**: Replace mock data in `toplevels.dark` with actual canvas queries
- **Benefits**: Developers get real-time view of their application structure

### 3. Development Experience for .dark Files
- **Current Status**: CLI can view and navigate packages but lacks file editing workflow
- **Action**: Build comprehensive .dark file development experience
- **Components**:
  - File creation and editing workflows
  - Syntax validation and error reporting
  - Hot reload for development
  - Integration with existing navigation system

## Development Experience Improvements (Month 2)

### 4. Local Execution Infrastructure
- **BWD (Backend Web Development)**: Local backend server execution
- **QW (Queue Worker)**: Local queue processing
- **CC (Cron Controller)**: Local scheduled task execution
- **Benefits**: Full local development without cloud dependencies

### 5. Source Control Integration
- **Git integration**: Native git operations within CLI
- **Sync commands**: Bidirectional sync between local and cloud
- **Version management**: Branch-aware development workflow
- **Conflict resolution**: Intelligent merge strategies for .dark files

### 6. Enhanced Command Coverage
- **Project management**: `new`, `init`, `deploy` commands
- **Testing framework**: `test`, `coverage`, `benchmark` commands
- **Debugging tools**: `trace`, `profile`, `inspect` commands
- **Package management**: `add`, `remove`, `update` package commands

## Technical Architecture Improvements (Month 3)

### 7. Content-Addressable System
- **SHA-256 based**: Complete transition to content-addressable storage
- **Deduplication**: Efficient storage and transmission
- **Integrity**: Cryptographic verification of all content
- **Caching strategy**: Multi-level cache with memory, SQLite, and remote

### 8. Performance Optimizations
- **Startup time**: Further reduce CLI startup time (currently optimized)
- **Memory usage**: Efficient package loading and caching
- **Network efficiency**: Optimized sync protocols
- **Query performance**: Fast package search and navigation

## User Experience Enhancements (Month 4)

### 9. Advanced Navigation
- **Fuzzy search**: Intelligent package and function discovery
- **Bookmarks**: Save frequently accessed locations
- **Recent items**: Quick access to recently viewed functions/types
- **Search history**: Persistent search and navigation history

### 10. Rich Output and Visualization
- **Syntax highlighting**: Enhanced code display with colors
- **Interactive tables**: Sortable, filterable data views
- **Progress indicators**: Real-time feedback for long operations
- **Error contextualization**: Rich error messages with suggestions

## Action Plan Summary

**Week 1-2**: Focus on stability (fix tests, real data integration)
**Week 3-4**: Core development experience (.dark file workflows)
**Month 2**: Local execution infrastructure (BWD, QW, CC)
**Month 3**: Advanced features (source control, enhanced commands)
**Month 4**: Performance and UX polish

## Success Metrics
- **Developer Productivity**: Time to complete common tasks (create, edit, test, deploy)
- **System Reliability**: Zero failing integration tests, stable daily usage
- **Feature Adoption**: Usage patterns for new commands and workflows
- **Performance Benchmarks**: Startup time, command response times, memory usage

## Dependencies and Risks
- **Real data integration** requires coordination with canvas backend systems
- **Local execution** depends on containerization and service orchestration
- **Source control** requires careful handling of merge conflicts in .dark files
- **Performance targets** may require additional optimization iterations

This plan ensures evolves from a functional replacement to a comprehensive development platform that significantly improves the Darklang development experience.