# CLI Refactoring Implementation Plan


### 1.2 Extract Commands Incrementally

**Start with System Commands** (simplest, least coupled):

```dark
// In commands/system.dark
module Darklang =
  module Cli =
    module Commands =
      module System =
        let helpCommand: CommandDetails = {
          name = "help"
          description = "Show help information"
          aliases = []
          arguments = []
          execute = fun state args ->
            // Move existing help logic here
            let commands = getAvailableCommands state
            let helpContent = renderHelpWithCategories commands
            let newState = { state with commandResult = CommandResult.Info helpContent }
            (newState, [])
        }
        
        let versionCommand: CommandDetails = {
          name = "version"
          description = "Show CLI version"
          aliases = ["v"]
          arguments = []
          execute = fun state _ ->
            let versionInfo = getVersionInfo ()
            let newState = { state with commandResult = CommandResult.Info versionInfo }
            (newState, [])
        }
        
        let getCommands (state: State): List<CommandDetails> = [
          helpCommand; versionCommand; statusCommand; quitCommand; modeCommand; clearCommand
        ]
```



### 3.2 Solution: Categorized Help

Add categories to command details:
```dark
type CommandCategory = 
  | System | Navigation | Development | Installation

type CommandDetails = {
  // ... existing fields
  category: CommandCategory
  examples: List<String>
}
```

Then improve help display:
```dark
let renderHelpWithCategories (commands: List<CommandDetails>) : String =
  let systemCommands = commands |> List.filter (fun cmd -> cmd.category == System)
  let navigationCommands = commands |> List.filter (fun cmd -> cmd.category == Navigation)
  // etc.
  
  let result = [
    "Available commands:"
    ""
    "🔧 System Commands"
    renderCommandGroup systemCommands
    ""
    "📁 Navigation Commands" 
    renderCommandGroup navigationCommands
    ""
    "💻 Development Commands"
    renderCommandGroup developmentCommands
    // etc.
  ] |> String.join "\n"
```

**Result**:
```
/> help

Available commands:

🔧 System Commands
   help        Show this help information
   version     Show CLI version (aliases: v)
   status      Show system status
   quit        Exit the CLI
   
📁 Navigation Commands  
   cd <path>   Change directory (example: cd Darklang.Stdlib)
   ls          List current directory contents
   back        Go back to previous location
   view <name> View entity details (example: view String.append)
   
💻 Development Commands
   run <fn>    Run a function (example: run @String.append "hello" " world")
   eval <expr> Evaluate expression (example: eval 1L + 2L)
   scripts     Manage Dark scripts
   
⚙️ Installation Commands (installed mode only)
   update      Update to latest version
   uninstall   Uninstall the CLI
```


### 3.3 Enhanced Help Commands

Add category-specific help:
```dark
// Enhanced help command
let helpCommand: CommandDetails = {
  name = "help"
  // ...
  execute = fun state args ->
    match String.trim args with
    | "" -> renderAllCategoriesHelp state
    | "system" -> renderCategoryHelp state System
    | "navigation" -> renderCategoryHelp state Navigation
    | "development" -> renderCategoryHelp state Development
    | "installation" -> renderCategoryHelp state Installation
    | commandName -> renderSpecificCommandHelp state commandName
}
```

Usage:
```
/> help navigation
📁 Navigation Commands

cd <path>      Change to module/package directory
               Examples: cd Darklang.Stdlib
                        cd Darklang.Stdlib.String
                        
ls             List contents of current directory
               Shows functions, types, constants in current module
               
view <name>    View details of entity
               Examples: view String.append
                        view List.map
                        view HttpClient.Response
                        
back           Return to previous location
               Navigate back through directory history
```

