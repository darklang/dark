# Task Todos: TUI Component Library

## Research Summary

Based on research of external TUI libraries (Charm/Bubbletea, Spectre.Console, Terminal.Gui, Ratatui) and the existing Darklang codebase, we have:

**Existing Components** (in `packages/darklang/cli/experiments/`):
- 17 component modules already built (Button, Card, Label, TextBlock, Divider, Progress, Forms, Dropdown, Message, Navigation, Panel, ListView, Scrollbar, StatusBar, Layout, Modal, Pagination)
- Core infrastructure (Types, Rendering utilities)
- Demo applications and catalog
- CLI abstractions layer
- ~2,000-3,000 lines of existing TUI code

**Key Findings:**
- Components follow functional architecture (Model, create, render pattern)
- No tests exist yet in backend/testfiles/execution
- Components are in `Darklang.Cli.Experiments.UICatalog.*` namespace
- Need migration to `Darklang.CLI.UI` as suggested in task

## Implementation Plan

### Phase 1: Namespace Organization & Migration
- [ ] Review current namespace structure in experiments/
- [ ] Create new `Darklang.CLI.UI` namespace structure in packages/darklang/cli/ui/
- [ ] Design namespace organization (Core, Components, Layout, etc.)
- [ ] Migrate core types and rendering utilities to Darklang.CLI.UI.Core
- [ ] Migrate all 17 component modules to Darklang.CLI.UI.Components
- [ ] Migrate CLI abstractions to appropriate namespace
- [ ] Update all internal references to use new namespaces
- [ ] Verify no broken references remain

### Phase 2: Component Enhancement & Consistency
- [ ] Audit all 17 components for API consistency
- [ ] Ensure all components follow the same create/render pattern
- [ ] Review and standardize component state management
- [ ] Enhance components that are basic (Label, Divider) with missing features
- [ ] Review partially-implemented components (Forms) for completeness
- [ ] Add missing focus management to all focusable components
- [ ] Standardize error handling across components

### Phase 3: Testing Infrastructure
- [ ] Create test structure in backend/testfiles/execution/cli/
- [ ] Write rendering tests for core utilities (box drawing, text padding, truncation)
- [ ] Write tests for basic components (Label, TextBlock, Divider)
- [ ] Write tests for interactive components (Button, Progress)
- [ ] Write tests for form components (TextInput, Checkbox, RadioGroup, Select)
- [ ] Write tests for layout components (Container, Grid, HStack, VStack)
- [ ] Write tests for complex components (Modal, Panel, Dropdown)
- [ ] Write tests for navigation components (Menu, Tabs, Breadcrumbs)

### Phase 4: Documentation & Examples
- [ ] Document core architecture (Component model, rendering, state management)
- [ ] Create usage examples for each component type
- [ ] Document common patterns (layout composition, event handling, focus management)
- [ ] Update or create README for CLI.UI package

### Phase 5: Advanced Features (Time Permitting)
- [ ] Implement theme system (using existing RenderContext.theme field)
- [ ] Add keyboard navigation state machine
- [ ] Enhance event handling system
- [ ] Add animation/transition support
- [ ] Performance optimization for complex UIs

## Testing Strategy

Tests will verify:
1. **Rendering correctness** - Components produce expected string output
2. **Layout calculations** - Dimensions and positioning work correctly
3. **State transitions** - Components update state properly
4. **Edge cases** - Empty content, overflow, boundary conditions
5. **Focus management** - Focusable components handle focus correctly

Test coverage should be practical, not exhaustive. Focus on:
- Core rendering utilities (high value)
- Basic components that others build on
- Complex components with logic (Forms, Modal, etc.)
- Known edge cases or bug-prone areas

## Success Criteria

- [ ] All components migrated to Darklang.CLI.UI namespace
- [ ] Consistent API patterns across all components
- [ ] Test coverage for key components and rendering utilities
- [ ] Documentation of component usage
- [ ] No broken references or build errors
- [ ] Demos/catalog updated to use new namespaces
