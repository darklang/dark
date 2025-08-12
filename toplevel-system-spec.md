# Darklang Toplevel System Specification

## Overview

The Darklang toplevel system is a powerful organizational framework that automatically recognizes and presents structured data throughout the package system. Instead of traditional file-based organization, toplevels allow developers to create typed values that the system automatically discovers and organizes.

## Core Concept

**Toplevels are types that the system respects in a particular way.** You create pieces of data (values) of specific types, and the system automatically recognizes and organizes them. This provides a magical but optional organizational layer - if you don't like the magic, you can entirely avoid it and work with raw code.

## Global Toplevel Types

These are system-wide recognized types that provide fundamental organizational structures:

### HttpHandler
Web request handlers for building web applications.
```darklang
type HttpHandler = 
  { path: String
    method: String  // GET, POST, PUT, DELETE, etc
    handler: String -> String }
```

### Script  
Executable scripts for automation and development tasks.
```darklang
type Script =
  { name: String
    description: String
    execute: List<String> -> String }
```

### Cron
Scheduled tasks for background jobs and recurring operations.
```darklang
type Cron =
  { name: String
    schedule: String  // cron expression
    task: Unit -> String }
```

### View
UI components and templates for rendering interfaces.
```darklang
type View =
  { name: String
    template: String
    render: Dict<String> -> String }
```

### Test
Test cases for quality assurance and validation.
```darklang
type Test =
  { name: String
    description: String  
    test: Unit -> TestResult }
```

### Markdown
Documentation content for knowledge management.
```darklang
type Markdown =
  { title: String
    content: String
    tags: List<String> }
```

## Organization Toplevels (@Stachu.Extensions)

Personal workflow management structures for advanced organization:

### Module
Grouping related functionality into logical units.
```darklang
type Module =
  { name: String
    description: String
    components: List<String>
    dependencies: List<String> }
```

### Facet
Multi-dimensional categorization system for complex organization.
```darklang
type Facet =
  { name: String
    category: String
    dimensions: List<String>
    values: List<String> }
```

### Project
Project management structure for tracking work.
```darklang
type Project =
  { name: String
    status: ProjectStatus  // Active, OnHold, Completed, Cancelled
    description: String
    tasks: List<String>
    deadline: Option<DateTime> }
```

### Actionable
Task management for tracking actionable items.
```darklang
type Actionable =
  { title: String
    priority: Priority  // Low, Medium, High, Urgent
    context: String
    dueDate: Option<DateTime>
    tags: List<String> }
```

*Note: Additional organization toplevels pending clarification from Stachu*

## How It Works

1. **Creation**: Developers create values of toplevel types anywhere in the package system
2. **Discovery**: The system automatically scans packages for values of recognized types
3. **Organization**: presents these values in structured, navigable interfaces
4. **Access**: Users can browse, search, and interact with toplevels through the CLI

## CLI Integration

The navigation system integrates seamlessly with toplevels:

- `ls` - Shows toplevel categories and instances
- `tree` - Displays hierarchical toplevel structure  
- `view <toplevel>` - Shows detailed information about specific toplevels
- `cd toplevels/<type>` - Navigate into specific toplevel categories

## Benefits

1. **Automatic Organization**: No manual file organization required
2. **Type Safety**: Strong typing ensures consistent structure
3. **Discoverability**: Easy to find and explore related functionality
4. **Flexibility**: Can be ignored if not needed
5. **Extensibility**: Easy to add new toplevel types

## Usage Examples

### Creating an HttpHandler
```darklang
let myApi =
  Darklang.Toplevels.HttpHandler
    { path = "/api/users"
      method = "GET"
      handler = fun request -> "user data response" }
```

### Creating a Script
```darklang
let deployScript =
  Darklang.Toplevels.Script
    { name = "deploy"
      description = "Deploy application to production"
      execute = fun args -> "Deployment completed" }
```

### Creating Documentation
```darklang
let apiDocs =
  Darklang.Toplevels.Markdown
    { title = "API Documentation"
      content = "# API Guide\n\nThis API provides..."
      tags = ["api"; "documentation"; "reference"] }
```

## Future Vision

The toplevel system enables a shift from file-based to content-based organization. Instead of thinking "where did I put that file?", developers think "what type of thing am I looking for?"
The system becomes a semantic layer over the codebase, making it easier to understand, navigate, and maintain large projects.

This aligns with the broader Darklang vision of making programming more accessible and removing accidental complexity from software development.