

### HTTP Handler Space (`http`)

```bash
http                    # List all HTTP handlers
http list              # Same as above  
http create <path>     # Create new HTTP handler
http edit <name>       # Edit existing handler
http test <name>       # Test handler with sample requests
http delete <name>     # Remove handler
http routes            # Show all routes in a tree
http middleware        # Manage middleware
```

### Script Space (`script` or `scripts`)

```bash
scripts                # List all scripts
scripts run <name>     # Execute a script
scripts create <name>  # Create new script
scripts edit <name>    # Edit existing script
scripts delete <name>  # Remove script  
scripts deps <name>    # Show script dependencies
scripts log <name>     # View execution logs
```

### Test Space (`test`)

```bash
test                   # Run all tests
test list             # List available tests  
test run <pattern>    # Run specific tests
test create <name>    # Create new test
test watch           # Run tests in watch mode
test coverage        # Show test coverage
test results         # Show last test results
test failed          # Show only failed tests
```

### Cron/Scheduler Space (`cron`)

```bash
cron                  # List all scheduled tasks
cron list            # Same as above
cron create          # Create new scheduled task
cron edit <name>     # Edit existing task
cron delete <name>   # Remove scheduled task
cron logs <name>     # View task execution logs
cron enable <name>   # Enable task
cron disable <name>  # Disable task
cron next <name>     # Show next execution time
```

### View/UI Space (`views`)

```bash
views                # List all UI components/views
views list          # Same as above
views create <name> # Create new view component
views edit <name>   # Edit existing view
views preview <name> # Preview component in browser
views delete <name> # Remove view component
views templates     # List available templates
views build         # Build all views for production
```

### Documentation Space (`docs`)

```bash
docs                 # List all documentation
docs list           # Same as above  
docs create <name>  # Create new document
docs edit <name>    # Edit existing document
docs build          # Generate static documentation
docs serve          # Start documentation server
docs publish        # Publish docs to hosting
docs search <term>  # Search documentation content
```

## Advanced Features Specification

### Context Awareness
CLI should be context-aware and show different options based on current location:

- **Root Context**: Show all toplevel categories
- **Package Context**: Show package-specific commands
- **Toplevel Context**: Show toplevel-specific operations
- **Development Context**: Show development and testing commands

### Interactive Mode
```bash
darklang cli         # Enter interactive mode
> help              # Context-sensitive help
> cd http           # Enter HTTP handler context
http> list          # List HTTP handlers in current context
http> create api/users  # Create new handler
http> edit api/users    # Edit the handler
http> back          # Go back to root context
>
```

### Batch Operations
```bash
# Batch operations on multiple items
http delete api/users api/posts api/comments
test run unit/ integration/ e2e/
scripts schedule deploy backup cleanup --cron="0 2 * * *"
```

### Pipeline Support
```bash
# Pipe operations
scripts list --status=failed | scripts rerun
test list --pattern="*integration*" | test run
http list --method=GET | http middleware add cors
```

### Configuration Management
```bash
# CLI configuration
config set editor vim
config set test.reporter junit
config set http.default-cors true
config list
config reset
```
