# Tasks

A few pieces of feedback I'd like you to create a TODO list for and complete:

## Easy Quick Wins
- [x] delete packages/darklang/cli/exploration-plan.md
- [x] remove the 'h' alias for help and make it 'commands' instead
- [x] remove the "Darklang CLI" text that shows when you go to the site
- [x] instead, in the logo section on the left, print  "darklang.com" just below the logo (with a line of separation)
- [x] while you're at it, remove the words "Available commands:" -- that's pretty obvious
- [x] the 'clear' command should be extracted out of commands.dark into its own clear.dark file
- [x] update text "Launch experiments TUI" to "Try out various WIP CLI experiments"
- [x] retitle "package navigation" to just "packages"
- [x] and code execution to just 'execution'
- [x] delete packages/darklang/cli/experiments/CLI_MVU_PROJECT_STATUS.md
    except, copy any important things from Critical Darklang Syntax Learnings to the root CLAUDE.md, first
- [x] delete packages/darklang/cli/experiments/UI_COMPONENT_INVENTORY.md
- [x] for some reason, the install made the alias for the installed app "darklang" but it should be "dark"
    fix uninstall and other references too
    (the product is Darklang everywhere, but for convenience we make the cli app just 'dark' - easier to type)
- [x] add a "--version" alias for the version command, so `dark --version` works
- also `-v`
- [x] remove --no-icons from tree. always icons.
- [x] getDirectoryContents and isDirectoryAtPath should exist in Stdlib somewhere. if they don't exist already, mput some version of them there, and use those instead (deleting the ones in the Cli fodler)
- [x] for installation/commands.dark, break it down by module a bit. a module for update, a module for install, etc. I find it easier to reason about things that way


## Claude.md updates
- [x] iterate on the root Claude.md file - make it a tad more formal and expansive. reference the root README, and list the major root-level folders. maybe read some of wip.darklang.com's content for inspo as to what should be in the CLAUDE.md
- [x] update the root CLAUDE.md with any syntactical things you've learned when writing Darklang, for future reference
- try not to let this file get too long, though


## Get some context for the harder stuff
- [x] read packages/darklang/cli and packages/darklang/cli_old_backup, for context


## Some keyboard nav related stuff
  - [ ] hitting backspace kinda works, but not fully.
      when I hit backspace, it moves the cursor left, and the _model_ seems to change, but the line is not redrawn
      see cli_old_backup for some things that worked before -- especially _update.dark had some magic around this, it seems
  - [ ] hitting left/right should move my cursor, but it's not
  - [ ] hitting 'home' should move me to the start of the cursor
  - [ ] the keyboard shortcuts (left, right, home, backspace) should only work for the specific Page I'm on
      (if the prompt isn't shown, then the shortcuts don't make sense)
  - [ ] remove parser-combinator-feasibility-analysis.md - it's irrelevant. if possible, make it so it's as if it were never in the git history
  - [ ] delete test-script.dark - unused


## Let's tidy the 'core' folder
- [ ] colors.dark can be in the utils dir
      btw, things in the utils dir don't need a Utils namespace
- [ ] help.dark and quit.dark can be in the root cli dir
- [ ] the rest should be combined into one 'core.dark' file, 'in order' as you see fit.
    analyze, plan the order, then do the work
    I realize it'll be a bit of a long file


## Misc.
- [ ] move findCommonLength and findCommonPrefix to the Completions module - that's the only place they're used
- [ ] and remove the 'location' line of the 'status' command
- [ ] maybe add a splash of color to the 'help' command? think through options, choose your favorite
- [ ] rename updateState to whtaever you see fit

- [ ] I want to generally make this application follow more of the pattern that the Counter List demo shows, with nested MVU stuff


## More Major Refactor of Package Stuff

- [ ] all of this lol

I'd like to do a pretty major refactor of package-specific stuff...

For what it's worth, here's what I consider "package stuff"
- the commands: cd, tree, view, pwd,
- any data supporting those things^ in the root or other state

and here are the problems I have:
- there's some half-baked 'interactive tree stuff'
- there's some half-baked 'interactive view stuff' in the viewing.dark file
- too much state/etc related to packages have leaked from the cli/packages subfolder and into the 'bigger' part of the app
- there's too much code, generally
- the system for navigating with .. and / and ../otherDir -like patterns is repeated/unconsolidated, messy, and incomplete

Here's what I'd like at the end:
- a better model for 'what package thing we're currently "at"'
  - should be one of
    | Root
    | Module of path: List<String>
    | Type of PackageType.Name
    | Fn of PackageFn.Name
    | Constant of PackageConstant.Name
- a better modeling of "what _page_ are we on - are we currently in the Prompt view, or some other view like Settings? (we don't actually have a Setting view, but you might get the point). in any case, the Page should be stored/modeled _separately_ from the current package location. by default, our page is MainPrompt with a default package location of Root
- an updated system to _display_ the current package location visually before the > of a prompt
  Root -> /
  Module path -> /First/Second/Third (module [folder icon])
  Type name -> /module/space/typeName (type [type icon])
  Type name -> /module/space/typeName (fn [fn icon])
  Constant name -> /module/space/typeName (const [const icon])
- better consolidation of the logic that 'navigates' from one place in the package tree to another. for example to handle, from /Darklang/Stdlib/List.filter, to Stdlib with "cd ../.."
  this same logic should be shared for multiple commands: cd, ls, tree

- the commands
  - pwd
    accepts no args
    pretty boring - include the _type_ of node we're at (module, etc)
    include icon

  - ls [path]
    if no arg provided, simply list the things at this level: Submodules, Types, Constants, Fns
    if arg provided, provide the list at the things per the _path_ provided

    the individ items should be comma-separated to ensure the text doesn't take up MANY vertical lines

  - view [path]
    should pretty-print whatever we're looking at
      if it's a fn, pretty print the fn
      if it's a type, pretty print the type
      if it's a constant, pretty print the constant
      if it's a module,
        if --detailed isn't supplied, return the same thing that 'ls' would
          _but_ note that --detailed would pretty print everything in it
        if --detailed is supplied, really pretty print EVERYTHING in that module
      if it's root, same thing as module (just show list of toplevel modules, unless --detailed supplied. gosh I hope no one does that).

    if no path arg provided, view the things at current location
    if path arg provided, ... you know

    later, we'll provide user-configurable means of viewing misc things -- e.g. if a Constant is of a particular Type, we have a special pretty-printer ready _just for that_
      a quick demo of this could be cool - maybe we just do this for one particular constant, to keep things simple? idk.

  - tree [path]
    if path arg provdied...
    if not...

    (this should work basically as it does not, just with all the 'interactive' stuff stripped out)

  - 'cd' [path]
    if arg provided...
    if not, we enter an interactive thing! This is how I wanna be able to interactively navigate from one package thing to another:
    open another Page built just for this thing
    at the end of the experience, either they cancel (i.e. hit 'escape') or simply the path chosen is set as the new 'current' package location


- when you come back from a page other than the Prompt one, your CLI history (visually, in your terminal app) should appear as you left off just where you were with the 'interactive' prompt stuff.

- otherwise, we enter an interactive 'view'/'mode'/'page' in the UI, wherein the tree (relative to 'current') is printed, keyboard is listened to, and you can navgate up/down the tree of stuff, visually.
- when you're "on" a module, you can hit left to go a tree up

- if you hit 'escape' at any time, we just stop the cd/nav process, and exit back to the main prompt stuff
the stuff in path.dark seems a bit in the middle of nowhere or something - there _should_ be a consolidated effort to navigate up/down the tree, and serach for options, but this feels a bit funny

- I'd love for this to be more MVU, with the 'commands' only being relevant on the Main Prompt View, which uses some abstracted Prompt View
you might need to adjust things around 'cd' and 'ls' to work still...
- I hate the uiMode thing that's been introduced - whatever that's used for should instead be derived by the current 'page'
- sticking with MVU intentions, as little of these types should be in the 'core' section as possible - rather, state and such should be managed by each sub-app, where relevant
    namely I'm here referring to the state related to the interactive navigation
- probably start this by removing all "interactive tree" stuff from the codebase
, THEN proceed by adding an interactive way to navigate ethe tree and interact


in any case: think about this for a bit, come to me for feedback, and _then_ get started on this effort.
when in doubt, leave things as they are - we can make things 'more correct' later


## More Misc.
- [ ] the old terminal.dark used to have a calculateWrappedLineCount fn but now we don't - don't we need that for anything?
- [ ] we need to bring back syntax highlighting


## Tidy CLI Experiments
Finally once we've fixed all of that, onto the 'experiments' a bit...

- [ ] let's remove some of the redundant demos; do reasonable research, then _recommend_ consolidation efforts. the goal here is to have _less_ code, by way of removing redundant demos, components, etc.
- [ ] that said, if any component exists but is un-demo'd, we should fix that by adding it where relevant
- [ ] follow through after asking me how I'd like to proceed
