# UI Component Inventory - Complete Catalog

This document provides a comprehensive inventory of all 46+ components available in the Darklang CLI UI Component system.

## Basic Components (6 types)
1. **Button** - Interactive button with text, color, and action
2. **Label** - Simple text display with color styling
3. **TextBlock** - Multi-line text block with alignment and color
4. **Divider** - Horizontal line separator with customizable character and color
5. **Progress** - Progress bar with value, range, and color
6. **StatusBar** - Status bar with left/right text alignment

## Form Components (7 types)
1. **TextInput** - Single-line text input with placeholder and max length
2. **Checkbox** - Boolean checkbox with label
3. **RadioGroup** - Single-selection radio button group
4. **Select** - Dropdown selection list
5. **Slider** - Numeric value slider with range and visual feedback
6. **DateField** - Date input field with validation
7. **MultiSelect** - Multi-selection dropdown

## Layout Components (6 types)
1. **Container** - Generic container with optional title and border
2. **Panel** - Content panel with title and styling
3. **Grid** - Grid layout for arranging items in rows and columns
4. **HStack** - Horizontal stack layout with spacing and alignment
5. **VStack** - Vertical stack layout with spacing
6. **FilterPanel** - Specialized panel for filter controls

## Navigation Components (4 types)
1. **Menu** - Dropdown menu with items, shortcuts, and submenus
2. **Tabs** - Tab navigation with active state
3. **Breadcrumb** - Breadcrumb navigation trail
4. **NavBar** - Navigation bar with title and menu items

## Cards and Panels (5 types)
1. **Card** - Content card with title, subtitle, and content
2. **MediaCard** - Card with media display area
3. **Panel** - Interactive panel with items and actions
4. **TabPanel** - Tabbed panel container
5. **FilterPanel** - Specialized filtering panel

## Messages and Alerts (5 types)
1. **Message** - Informational message box with title and content
2. **Toast** - Temporary notification message
3. **Alert** - Alert message with type styling
4. **ContextMenu** - Right-click context menu
5. **Dropdown** - Standard dropdown component

## Pagination and Navigation (2 types)
1. **Pagination** - Page navigation with page numbers and controls
2. **StepNavigation** - Step-by-step navigation with progress indication

## Data Display Components (3 types)
1. **ListView** - List view for displaying items with selection
2. **Scrollbar** - Vertical and horizontal scrollbars
3. **Grid** - Data grid for tabular display

## Modal and Overlay Components (2 types)
1. **Modal** - Modal dialog overlay
2. **ConfirmDialog** - Confirmation dialog with yes/no options

## Advanced Form Components (6 types)
1. **DateField** - Date picker with calendar
2. **Slider** - Range slider with visual feedback
3. **MultiSelect** - Multiple selection dropdown
4. **RadioGroup** - Radio button group
5. **Checkbox** - Boolean checkbox
6. **TextInput** - Enhanced text input

## Total Component Count: 46 unique component types

### Component Categories Summary:
- **Basic**: 6 components
- **Forms**: 7 components  
- **Layout**: 6 components
- **Navigation**: 4 components
- **Cards/Panels**: 5 components
- **Messages/Alerts**: 5 components
- **Pagination**: 2 components
- **Data Display**: 3 components
- **Modals**: 2 components
- **Advanced Forms**: 6 components

### Notes:
- All components support theming through Core.Types.Color
- Components follow Terminal.Gui/Bulma design principles
- Each component has dedicated create, render, and update functions
- Components are modular and can be composed together
- Full keyboard navigation support
- Responsive layout capabilities within terminal constraints