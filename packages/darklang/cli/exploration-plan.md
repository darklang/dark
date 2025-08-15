# Package Exploration System - Architecture Plan

## ✅ Completed
- Basic package navigation (cd, ls, pwd, back)
- Real package data integration
- Tab completion for packages
- Direct children filtering (not all descendants)

## 🚧 Still Needed for Full Interactive Exploration
The full interactive exploration system (like the old CLI's `view` command) requires a major architectural upgrade from line-based to keystroke-based input handling to support real-time navigation, scrolling, and interactive viewing.

## Key Requirements
- **Keystroke-based input**: Use `Stdlib.Cli.Stdin.readKey()` instead of `stdinReadLine()`
- **Real-time navigation**: Arrow keys for immediate response without Enter
- **Interactive scrolling**: Quick scrolling through lists of functions/types/constants
- **Modal interfaces**: Switch between module view and entity definition view
- **Entity selection**: Select items from lists to view detailed definitions
- **Navigation history**: Back navigation with 'b' key
- **Terminal control**: Full screen control with alternate screen mode

## Current State Analysis
The current MVU CLI uses:
- Line-based input with `stdinReadLine()`
- Simple command parsing and execution
- Basic state management with `AppState`
- Registry-based command dispatch

The old CLI uses:
- Keystroke input with `readKey()`
- Complex state model with Page navigation, ViewingState, modal interfaces
- Real-time event processing with immediate screen updates
- Advanced terminal control and syntax highlighting

## Required Architecture Changes

### 1. Input Handling Upgrade
**Current**: `Builtin.stdinReadLine()` → parse → execute → display
**Needed**: `Stdlib.Cli.Stdin.readKey()` → immediate processing → real-time updates

**Implementation**:
- Replace `runInteractiveLoop` with keystroke-based loop similar to old CLI's `runInternalLoop`
- Add `InteractionMode` enum: `NonInteractive | RefreshScreen | Regular`
- Implement message processing system for real-time events

### 2. State Model Extensions
**Current**: Simple `AppState` with basic fields
**Needed**: Rich state model supporting navigation and viewing

**New Types Required**:
```dark
type Page =
  | Root
  | Module of owner: String * submodulePath: List<String>
  | Type of name: LanguageTools.ProgramTypes.PackageType.Name
  | Fn of name: LanguageTools.ProgramTypes.PackageFn.Name
  | Constant of name: LanguageTools.ProgramTypes.PackageConstant.Name

type EntityCategory = Functions | Types | Constants | Submodules

type EntityDefinition =
  { name: String; lines: List<String>; scrollPosition: Int64 }

type ViewingState =
  { moduleContent: LanguageTools.ProgramTypes.Search.SearchResults
    entityName: String
    selectedCategory: EntityCategory
    selectedItemIndex: Int64
    expandedCategories: List<EntityCategory>
    entityDefinition: Option<EntityDefinition>
    entityCache: List<(String * EntityDefinition)>
    viewportScrollPosition: Int64 }

type State =
  { currentPage: Page
    pageHistory: List<Page>
    mainPrompt: String
    cursorPosition: Int64
    commandHistory: List<String>
    historyPosition: Int64
    commandResult: CommandResult
    interactionMode: InteractionMode
    needsFullRedraw: Bool
    isExiting: Bool
    viewingState: Option<ViewingState> }
```

### 3. Message System Expansion
**Current**: Simple command execution
**Needed**: Rich message system for real-time interactions

**New Messages Required**:
```dark
type Msg =
  | KeyPressed of key: Key * modifiers: Modifiers * keyChar: Option<String>
  | NavigateTo of Page
  | ToggleCategory of EntityCategory
  | ShowItemDefinition of EntityCategory * Int64
  | SubmitCommand of String
  | ... (existing command messages)
```

### 4. Commands to Implement

#### Navigation Commands
- `cd <path>` - Change directory/module
- `ls [path]` - List contents of current or specified module
- `pwd` - Show current path
- `tree [path]` - Show hierarchical view

#### Viewing Commands
- `view <module>` - Enter interactive viewing mode for module
- In interactive mode:
  - Arrow keys: Navigate categories and items
  - Enter: Expand category or view entity definition
  - 'b': Go back
  - 'q': Exit viewing mode

### 5. Terminal Control
**Required Features**:
- Alternate screen mode for full-screen interfaces
- Screen clearing and cursor positioning
- Syntax highlighting for code definitions
- Adaptive viewport sizing based on terminal dimensions

## Implementation Phases

### Phase 1: Core Keystroke Infrastructure
1. **Update main CLI loop**:
   - Replace `runInteractiveLoop` with keystroke-based version
   - Add `InteractionMode` support
   - Implement message processing system

2. **Extend state model**:
   - Add `Page` navigation types
   - Add basic `ViewingState` support
   - Update `Types` module with new structures

3. **Basic navigation**:
   - Implement `cd`, `pwd`, `ls` commands
   - Add simple directory navigation
   - Test keystroke input works correctly

### Phase 2: Interactive Viewing Mode
1. **Add viewing commands**:
   - Implement `view` command to enter interactive mode
   - Add modal interface support
   - Implement category navigation with arrow keys

2. **Entity browsing**:
   - Add entity selection and highlighting
   - Implement expand/collapse functionality
   - Add entity definition viewing

3. **Scrolling and navigation**:
   - Add viewport scrolling for long lists
   - Implement page up/down navigation
   - Add back navigation with history

### Phase 3: Advanced Features
1. **Terminal enhancements**:
   - Add full alternate screen mode
   - Implement syntax highlighting
   - Add adaptive viewport sizing

2. **Performance optimizations**:
   - Add entity definition caching
   - Optimize search and rendering
   - Add completion and search features

3. **Tree command**:
   - Add hierarchical module display
   - Implement tree navigation
   - Add filtering and search

## Key Files to Create/Modify

### New Files
- `cli/exploration/` directory
  - `navigation.dark` - Page navigation and path handling
  - `viewing.dark` - Interactive viewing mode logic
  - `terminal.dark` - Terminal control utilities
  - `entities.dark` - Entity lookup and caching

### Modified Files
- `cli/cli.dark` - Update main loop for keystroke input
- `cli/types.dark` - Add new state types
- `cli/registry.dark` - Add exploration commands
- `cli/commands.dark` - Add cd, ls, pwd, view, tree commands

## Success Criteria
1. **Keystroke responsiveness**: Arrow keys work immediately without Enter
2. **Scrolling performance**: Can quickly scroll through 100+ function lists
3. **Modal switching**: Smooth transitions between module and entity views
4. **Navigation memory**: Back button works intuitively
5. **Terminal integration**: Full screen mode works across different terminal sizes

## Risks and Mitigations
1. **Dark language constraints**: May hit limitations with complex state
   - *Mitigation*: Test incrementally, simplify state when needed

2. **Performance with large modules**: Stdlib has 1000+ functions
   - *Mitigation*: Implement viewport pagination and entity caching

3. **Terminal compatibility**: Different terminals may behave differently
   - *Mitigation*: Test on common terminals, add fallback modes

This plan maintains the sophisticated UX of the old CLI while building on the clean MVU architecture we've established.