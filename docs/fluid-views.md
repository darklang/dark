---
This document is somewhat aspirational right now. Most views are not split
cleanly and FluidEditor doesn't exist as a standalone module yet.
---

The final view for a toplevel with a fluid editor is composed of several nested
views. For example, a single toplevel handler could be rendered like this:

                __________
     --------  | doc str  |
    |live val| |__________|
     --------  | some     |  ______
               | code     | |flag-1|
               | here     | |______|
               | and      |  ______
               | there    | |flag-2|
               |          | |______|
               |__________|
               | result   |
               |__________|


We see the main editor box with code in it, capped by the documentation box and
result value on top and bottom. The live value of the current expression floats
to the left and two feature flags float to the right.

Let's try to break down the parts here to undersand how they relate and are
rendered by the various fluid components.

The view that actually renders an expression into tokens and handles input
events for modifying the code is a **FluidEditor**. Each Editor:
  - Displays a subset of the AST for the handler, which may be the whole AST
  - Holds state about it's tokens and caret position (that is, the absolute
    position of the caret within its tokens)
  - Renders the `<div contenteditable=true>` that is used to display fluid
    tokens and registers event handlers to deal with clicks and keyboard input
  - Has its own error rail

The feature flag boxes to the left are each a **FluidPanel**. A panel:
  - Wraps an editor, allowing it to be displayed off to the side of the main
    view or collapsed to an icon
  - Is tied to a specific expression in the AST
  - Can use different tokenization than the main editor (eg, the main editor
    tokenizes the _old code_ part of the feature flag while a panel displaying
    the same flag tokenizes the _condition_ and _new code_ parts

When a handler has no feature flags (which as of 2020-03 is the only thing that
triggers multiple editors within a handler), then the entire handler's AST will
be rendered in a single editor with no panels.

The view that contains everything is **FluidView**, which is responsible for
tying all these components together, as well as rendering the once-per-toplevel
stuff like the live value, documentation, and result value boxes.

     ----------------------
    |       FluidView      |
    |----------------------|
    | live value           |
    | doc string           |
    | result value         |
    |                      |
    |   -------------      |
    |  | FluidEditor |     |
    |  |-------------|     |
    |  | main code   |     |
    |  | error rail  |     |
    |   --------------     |
    |                      |
    |   -----------------  |
    |  |    FluidPanel   | |
    |  |-----------------| |
    |  |  -------------  | |
    |  | | FluidEditor | | |
    |  | |-------------| | |
    |  | | flag-1      | | |
    |  | | error rail  | | |
    |  |  -------------  | |
    |   -----------------  |
    |   -----------------  |
    |  |    FluidPanel   | |
    |  |-----------------| |
    |  |  -------------  | |
    |  | | FluidEditor | | |
    |  | |-------------| | |
    |  | | flag-2      | | |
    |  | | error rail  | | |
    |  |  -------------  | |
    |   -----------------  |
     ----------------------

