# Installation Command Integration Plan

## Overview
Integrate the existing installation system into the new MVU command registry architecture.

## New Module Structure
- Create `install.dark` module containing 5 commands:
  - `install` - Install Darklang CLI globally
  - `update` - Update CLI to latest version  
  - `uninstall` - Remove CLI installation
  - `status` - Show installation status (portable vs installed)
  - `version` - Display version info (moved from standalone module)

## File Organization
```
/cli/
├── install.dark           # Main install module with 5 commands
├── install/
│   ├── config.dark       # Config management (adapted from _config.dark)  
│   ├── download.dark     # Download/install logic (adapted from _install.dark)
│   ├── system.dark       # System integration (adapted from _main.dark)
│   └── uninstall.dark    # Uninstall logic (adapted from _uninstall.dark)
```

## MVU Integration Strategy

### 1. Preserve Existing Logic
- Keep all complex platform-specific code identical
- Maintain existing Result types and error handling
- Preserve cross-platform compatibility (Windows/Unix)
- Keep all GitHub integration and file operations

### 2. Adapt Command Interface
- Change entry points to match `execute(state, args)` and `help(state)` pattern
- Wrap existing functions in AppState updates
- Maintain existing user interaction flows and prompts

### 3. Registry Integration
Update `registry.dark` to include installation commands:
```dark
let allCommands () : List<CommandHandler> =
  [ CommandHandler { name = "quit"; description = "Exit the CLI"; execute = Quit.execute; help = Quit.help }
    CommandHandler { name = "help"; description = "Show help for commands"; execute = Help.execute; help = Help.help }
    CommandHandler { name = "install"; description = "Install CLI globally"; execute = Install.executeInstall; help = Install.installHelp }
    CommandHandler { name = "update"; description = "Update CLI to latest version"; execute = Install.executeUpdate; help = Install.updateHelp }
    CommandHandler { name = "uninstall"; description = "Remove CLI installation"; execute = Install.executeUninstall; help = Install.uninstallHelp }
    CommandHandler { name = "status"; description = "Show installation status"; execute = Install.executeStatus; help = Install.statusHelp }
    CommandHandler { name = "version"; description = "Display CLI version"; execute = Install.executeVersion; help = Install.versionHelp } ]
```

## Key Benefits

### 1. Minimal Code Changes
- 95% of existing installation code stays identical
- Only adaptation layer changes to match MVU patterns
- All complex logic (GitHub API, file operations, cross-platform support) preserved

### 2. Better Organization
- Installation becomes a cohesive module
- Related commands grouped logically
- Clear separation of concerns

### 3. Composable Help System
- Installation commands automatically appear in main help
- Each command provides its own help alongside implementation
- No central coordination needed for help text

### 4. Consistent UX
- Same MVU patterns as other commands
- Consistent state management
- Integrated error handling

### 5. Easy Extension
- Future package management features fit naturally
- Adding new installation-related commands requires only registry update
- Modular architecture supports complex workflows

## Implementation Steps

1. Move version command into install module
2. Create install directory structure  
3. Adapt existing _*.dark files to new module structure
4. Create main install.dark with 5 command handlers
5. Update registry.dark to include installation commands
6. Test all installation workflows
7. Verify cross-platform compatibility maintained

## Success Criteria
- All existing installation functionality works identically
- Commands integrate seamlessly with help system
- Cross-platform support maintained
- User experience unchanged
- Code organization dramatically improved
- Easy to add future installation-related features