cli.darklang.com
  (or view this in app mode)
  (or SSH into [x])

app.darklang.com
  (or view this in CLI mode)



- commandInfo
    retrieves info about all of this CLI's commands. you can use this to display a custom help screen

- onAbort
    executes the given callback when a [termination signal] is received
    this signals are SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGBREAK


adjacent: look at 'semver' package in npm land, and add something like it in dark


if 'run' fails, will throw an error w/ properties such as
    code: number
    cmd: string (the command we asked to run)
    stdout: string // any info the process wrote to stdout
    stderr: string // any info the process wrote to stderr



really need a way to be able to lista ll available commands


each command needs:
- an Args type injected (<>)
- something to parse things after the first name, and return Result/Option of the Args parsed out
    TODO: avail cool helpers, etc.
- a run() fn that takes (args: Args) along with the current state, or whatever


'available commands' should be gatehred in order of parsing
OR
after being matched, we incldue not only Ok/Success, but also how well they matched
(similar to our http routing stuff, where a wildcard match gets you less points than a direct match)


toolbox.parameters
.string => "MyAwesomePlugin full"
.array => ["MyAwesomePlugin"; "full"]
.first => (Option<String>)
    and .second, .third



toolbox.prompt
.ask 
    {type: 'select'; name: 'shoe'; message: "what shes are you wearing?"; choices: ['clown'; 'other']}
    {type: 'multiselect'; ...}
    {type: 'password'; }
    {type: 'input'; name: "exinput"; message: "what's your middle name?"}
    {type: "autocomplete"; name: "state"; choices: ["PA"; "VT"]; suggest(s: choices) {TODO}}
.confirm "are you ready?" => bool
.separator => "----------"



toolbox.print
.info String -> Unit
.success String -> Unit
.warning String -> Unit
.error String -> Unit
    (probably should be followed by process.exit(0) shortly)
.highlight String -> Unit
.muted String -> Unit
.debug (someObject, "message")


toolbox.print.colors.success(), .error, .info, .warning, .highlight, .info

printHelp

printCommands

.table
    [ ["first name", "last name", "age"]
      ["jack"; "black"; "58"]
      ["frank"; "jacobson"; "32"]
    ]
    {format: "markdown"}



## some filesystem things...
- add `separator` to stdlib
    \ or / depending on OS

- add EOL to stdlib 
    \n on posix
    \r\n on windows

- add homedir to stdlib

- add subdirectories to stdlib
***



## Plugins
a plugin has some optional things:
- commands dir
    { 
        name 
        alias // 
        description // 
        hidden // 
        run }

    `name` and `description` used in printCommands calls
    hidden says whether it should show in help screens





some Elmish notes


type CLIMVU<'State, 'Msg> = 
    { 
        // Model

        
        // Update
        update: 

        //View
        render (state: 'State) (dispatch: 'Msg -> Unit) -> ??? CLI commands or something. effects to stdout, stdin, stderr? idk.
    }


...
let render (state) (dispatch: Msg -> Unit) = 
    let counterDispatch (counterMsg: CounterMsg): Unit = 
        dispatch (Msg.COunterMsg counterMsg)

    ...

    match state.Page with
    | Counter ->
        Html.text...
        divider
        renderCounter state.Counter counterDispatch
    | ...


... or ...


let render (state) (dispatch: Msg -> Unit) = 
    match state.Page with
    | Counter ->
        Html.text...
        divider
        renderCounter state.Counter (fun msg -> dispatch (Msg.CounterMsg msg))
    | ...


... or ...

let render (state) (dispatch: Msg -> Unit) = 
    match state.Page with
    | Counter ->
        Html.text...
        divider
        renderCounter state.Counter (Msg.CounterMsg >> dispatch)
    | ...


... do we need >>? lol surprised it hasn't come up yet


each init should be in its own component module


Program.mkSimple App.init App.update App.render
|> Program.run




parent apps Inspect and Intercept events coming from child programs in order to initialize or trigger more events in the current program and other child programs


CurrentPage

NavigateTo Event/Msg



FSH (F# Shell) pretty cool
- [ ] read and replicate source
- in addition to normal shell features (folder nav, creation, deletion, echo, etc), FSH also supports piping and F# interactive
- to demonstrate what this means, using FSH, you could type a line like
    - `echo hello world |> (fun s -> s.ToUpper()) > result.txt`
- > is a special pipe operator that sends to a file rather than console out
- ->> is also supported to append rather than overwrite
- "Builtins"
    - any shell has a # of commands that you can run, aside from things like external processes or in the case of FSH, code. FSH supports a limited set of these, all with basic but effective functionality. To get a list of these type `help` or `?`. Some common examples are:
    - `cd [path]`
    - `echo [text]`
    - `cat [filename]`
    - `> [filename]`
    - there are almost a dozen further commands like `rm`, `mkdir`, `env`, etc.
    - [ ] REVIEW THOSE^^^
- 

---

- use FxSSH to support a "Wish" like SSH-able thing so you can `ssh cli.darklang.com` or similar
- user CLI apps could be hosted at `ssh darklang.com/stachu/[etc]` or `ssh stachu.builtwithdark.com/[etc]` or `ssh [something]stachu.net`



Read POSIX specs around 1> etc


Messages (Msg) / Events directly do sqlite stuff? or...?

what if 'state' is really thin and is mostly focused around _accessing_ data rather than storing it directly?



-- thoughts from migrations: 
comment out a _lot_ of the sql migration stuff
leave just enough to get local stuff working


remember:
1. good for one, local
2. good for one, dist
3. good for more, dist.

DB.fs but local
CLI(.fs) to run igrations, not ProdExec


Fumble for Sqlite...



- [ ] think about vs code tree, too
    - pull in stuff from taht PR
    - generate it from the PM (right now) and demo it)
    - with raw sqlite access, or a builtin that does the whole thing
- [ ] also FSP stuff



oh so there's tty -- what about pty ?
psuedoterminal? idk, should youtube to research





http
    handlers, server
    client


counter demo
counter list




tree of CLI commands and options and stuff
record separately, but with children parent ids etc




Dark, Unison
CRDTs
event sourcing
fable elmish and elm hot-=reloading


change-driven development


"composable CLI parser"
"commands qare simple objects that provide a name, optional aliases, and a function to run"



is 'dark' just an app that connects you to ssh to something hosted on our servers? 
ssh darklang.com

read wish docs
esp about middleware



Plans for tech stuff
- actually decide what to do, esp about sqlite, pg, what to get built for now, how to do migrations, etc. what are the choices I need to make and what order do I need to do the things in?
    design the ops
    go through those papers that have the rough draft!!!!!
- get build working locally
    - update _all_
- pull draft.dark in
- get it to "Build" (by commenting a lot out)
- get it to run
- fill the screen with a CLI "frame"
- pull in more commands
- review Ocean's branch on FSP stuff
- get sqlite stuff working not just locally, but _when built_ (for Cloud **and** CLI)
- abstract CLI elmish stuff
- counter demo
- counter list
- use it for LSP


- watch/skim that goose CLI video

`dark http client`
`dark http handler` (RouteHandler is another type, and the handler is a bag of those? idk. HandlerRoute or something?)


may need DU composition for things like `let` bindings
interactive stuff

type Page =
    ...
    | CompletingCommand


in DU composition, it is common to match against a tuple of both the current active program and the incoming message to compute the next state


maintain history of location/page
    session forking / graph
    trace graph
    allow users to fork executions _so_ damn seamlessly


how do we do URL matching in dark-classic? do something like that for CLI command route-matching, if possible.
(even if it's "(ab)use sqlite")


- [ ] and -classic sidebar, UI
- [ ] and rM UI
- [ ] and mobile "app" UI



we don't need _all tables_ in both local and server installs
ugh separate schemas? maybe removing postgres was a mistake. prbably. fuck. so we need a separate system for migrations (probably migrondi). yeah it's probably gonna bite me in the ass.


I _do_ need local http handlers and such. because how am I gonna host them otherwise?
so maybe sqlite with _full schema_ is right, and we need a client-server or P2P sync



we can _ship_ system migrations via the PM or something? via re-installs?
look into migrondi



sessions



https://github.com/tforkmann/Fumble/issues/26
https://httl.dev/docs/cli
https://maxmcd.com/posts/http321/
https://practicalalloy.github.io/





respect if tty... (relevant to `--interactive`, etc.) => prbably in UX doc adjacent to this