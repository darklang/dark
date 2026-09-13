/// The builtins the browser has to answer differently from a process on a tty.
///
/// Layered LAST in the CLI's builtin set (`Builtin.combine` keeps the later definition
/// of a name), so everything not listed here is the real implementation from
/// `Builtins.Cli`. Kept deliberately short: the point of the exercise is running the
/// real CLI, not a browser edition of it.
module Darklang.Wasm.Host.BrowserBuiltins

open System
open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts
open LibExecution.Effects

module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module NR = LibExecution.RuntimeTypes.NameResolution
module Stdin = Builtins.Cli.Libs.Stdin

/// Read a whole line off the key queue. Echoes nothing; the prompt that asked is
/// expected to be a TUI region (which is how the CLI's own line reads work).
let private readLine () : Task<string> =
  task {
    let sb = Text.StringBuilder()
    let mutable fin = false
    while not fin do
      let! ev = Browser.nextKey ()
      match ev.paste with
      | Some text ->
        match text.IndexOf '\n' with
        | -1 -> sb.Append text |> ignore<Text.StringBuilder>
        | i ->
          sb.Append(text.Substring(0, i)) |> ignore<Text.StringBuilder>
          fin <- true
      | None ->
        match ev.key.Key with
        | ConsoleKey.Enter -> fin <- true
        | ConsoleKey.Backspace ->
          if sb.Length > 0 then sb.Length <- sb.Length - 1
        | _ ->
          let c = ev.key.KeyChar
          if not (Char.IsControl c) then
            sb.Append c |> ignore<Text.StringBuilder>
    return sb.ToString()
  }

let private fns : List<BuiltInFn> =
  [ { name = fn "stdinReadKey" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        let typeName =
          FQTypeName.fqPackage (PackageRefs.Type.Stdlib.Cli.Stdin.keyRead ())
        TCustomType(NR.ok typeName, [])
      description = "Waits for the next key the page pushes in."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! ev = Browser.nextKey ()
            return Stdin.keyReadToDval ev.key ev.paste 1
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }

    { name = fn "stdinReadLine" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Reads a line from the keys the page pushes in."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! line = readLine ()
            return DString line
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }

    { name = fn "stdinIsInteractive" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TBool
      description = "True: the page's terminal is always a person."
      fn =
        (function
        | _, _, _, [| DUnit |] -> Ply(DBool true)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }

    { name = fn "stdinReadAll" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Nothing is ever piped into a tab."
      fn =
        (function
        | _, _, _, [| DUnit |] -> Ply(DString "")
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdin ]
      deprecated = NotDeprecated }

    { name = fn "cliTerminalSize" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TTuple(TInt, TInt, [])
      description = "The xterm.js terminal's (columns, rows), as last reported by the page."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let (width, height) = Browser.terminalSize ()
          DTuple(Dval.int (bigint width), Dval.int (bigint height), []) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "cliTerminalSessionInfo" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TTuple(TBool, TBool, [ TString ])
      description = "(input is terminal, output is terminal, TERM): always a terminal here."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          DTuple(DBool true, DBool true, [ DString "xterm-256color" ]) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }

    { name = fn "stdoutClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TUnit
      description = "Clears the terminal."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          Browser.writeToTerminal "\u001b[2J\u001b[H"
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated } ]

let builtins () : Builtins = Builtin.make [] fns
