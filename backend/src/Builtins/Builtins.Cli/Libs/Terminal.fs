module Builtins.Cli.Libs.Terminal

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType


/// Restores terminal state if the process exits before Dark can clean up.
///
/// Dark supplies the restoration sequence; the host stores it and invokes it
/// from process lifecycle callbacks.
module TerminalRestoreGuard =
  let private gate = obj ()
  let mutable private restoreSequence : string option = None

  let private snapshot () : string option = lock gate (fun () -> restoreSequence)

  /// Store the fallback restoration sequence after pending output completes.
  ///
  /// Waiting prevents a new fallback from overtaking an earlier terminal
  /// restoration.
  let arm (sequence : string) : unit =
    NonBlockingConsole.wait ()
    lock gate (fun () -> restoreSequence <- Some sequence)

  /// Disarm fallback restoration after pending output completes.
  let disarm () : unit =
    NonBlockingConsole.wait ()
    lock gate (fun () -> restoreSequence <- None)

  /// Write the active restoration sequence, if any.
  ///
  /// The injected writer allows this behavior to be tested without exiting.
  let restoreWith (write : string -> unit) : unit =
    match snapshot () with
    | Some sequence -> write sequence
    | None -> ()

  let private restoreToTerminal () : unit =
    try
      restoreWith (fun sequence ->
        System.Console.Out.Write sequence
        System.Console.Out.Flush())
    with _ ->
      // Cleanup must never replace the failure that caused it.
      ()

  do
    System.AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> restoreToTerminal ())
    System.AppDomain.CurrentDomain.UnhandledException.Add(fun _ ->
      restoreToTerminal ())
    System.Console.CancelKeyPress.Add(fun _ -> restoreToTerminal ())


module DisplayWidth = LibExecution.DisplayWidth


module TerminalCapabilities =
  let isInputTerminal () : bool = not System.Console.IsInputRedirected

  let isOutputTerminal () : bool = not System.Console.IsOutputRedirected

  let terminalName () : string =
    match System.Environment.GetEnvironmentVariable "TERM" with
    | null -> ""
    | value -> value


/// Read one terminal dimension, preferring an explicit environment override.
let private getDimension
  (envVar : string)
  (consoleFn : unit -> int)
  (fallback : int)
  : int64 =
  try
    match System.Environment.GetEnvironmentVariable envVar with
    | null ->
      let value = consoleFn ()
      if value > 0 then int64 value else int64 fallback
    | envValue ->
      match System.Int32.TryParse envValue with
      | true, value when value > 0 -> int64 value
      | _ -> int64 fallback
  with _ ->
    int64 fallback


/// Read the kernel's current terminal window size without a process spawn.
let private tryUnixTerminalSize () : (int64 * int64) option =
  [ 1; 0; 2 ] |> List.tryPick Posix.Libc.tryTerminalWindowSize


/// Return the current terminal size.
///
/// Prefer the Unix terminal API, then fall back to environment values,
/// `System.Console`, or 80×24.
let terminalSize () : int64 * int64 =
  match tryUnixTerminalSize () with
  | Some size -> size
  | None ->
    let width = getDimension "COLUMNS" (fun () -> System.Console.WindowWidth) 80
    let height = getDimension "LINES" (fun () -> System.Console.WindowHeight) 24
    (width, height)


let fns () : List<BuiltInFn> =
  [ { name = fn "cliTerminalSize" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TTuple(TInt, TInt, [])
      description = "Sample terminal width and height together as (columns, rows)"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let (width, height) = terminalSize ()
          DTuple(Dval.int (bigint width), Dval.int (bigint height), []) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalColorEnabled" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TBool
      description =
        "Whether output should carry color: false when NO_COLOR is set, or when "
        + "stdout is redirected"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          // Per no-color.org, only a non-empty value counts: `NO_COLOR=` is how a
          // wrapper re-enables color without having to unset the variable.
          let noColor =
            System.Environment.GetEnvironmentVariable "NO_COLOR"
            |> System.String.IsNullOrEmpty
            |> not
          DBool(not noColor && TerminalCapabilities.isOutputTerminal ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliGetLogDir" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the absolute path to the CLI log directory"
      fn =
        (function
        | _, _, [], [| DUnit |] -> DString(LibConfig.Config.logDir) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalRestoreArm" 0
      typeParams = []
      parameters =
        [ Param.make
            "restoreSequence"
            TString
            "The terminal restoration sequence supplied by the Dark TUI runtime" ]
      returnType = TUnit
      description =
        "Arm emergency terminal restoration for host failure or process termination"
      fn =
        (function
        | _, _, _, [| DString sequence |] ->
          TerminalRestoreGuard.arm sequence
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "cliTerminalRestoreDisarm" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TUnit
      description = "Flush normal terminal cleanup and disarm fallback restoration"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          TerminalRestoreGuard.disarm ()
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "cliTerminalStyledWidth" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "One logical row, possibly carrying SGR styling" ]
      returnType = TInt
      description = "Terminal columns occupied by text that may carry SGR styling"
      fn =
        (function
        | _, _, _, [| DString text |] ->
          text |> DisplayWidth.styledWidth |> bigint |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalClipToWidth" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "One logical row, possibly carrying SGR styling"
          Param.make "maxWidth" TInt "Terminal columns to clip to" ]
      returnType = TString
      description =
        "Clip styled text to a column count, keeping SGR and dropping other escapes and control characters"
      fn =
        (function
        | _, vm, _, [| DString text; DInt maxWidth |] ->
          DString(DisplayWidth.clipToWidth text (intToInt32 vm maxWidth)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalWrapStyled" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "One logical row, possibly carrying SGR styling"
          Param.make "maxWidth" TInt "Terminal columns to wrap at" ]
      returnType = TList TString
      description =
        "Wrap plain or SGR-styled text into terminal-width rows, reapplying active styling after a wrap"
      fn =
        (function
        | _, vm, _, [| DString text; DInt maxWidth |] ->
          DisplayWidth.wrapStyled text (intToInt32 vm maxWidth)
          |> List.map DString
          |> fun rows -> DList(VT.string, rows)
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalPositionAfter" 0
      typeParams = []
      parameters =
        [ Param.make "text" TString "Plain, control-free text"
          Param.make "maxWidth" TInt "Terminal columns per row" ]
      returnType = TTuple(TInt, TInt, [])
      description =
        "Zero-based (row, column) cursor position immediately after plain text at a given width"
      fn =
        (function
        | _, vm, _, [| DString text; DInt maxWidth |] ->
          let (row, column) =
            DisplayWidth.positionAfter text (intToInt32 vm maxWidth)
          DTuple(row |> bigint |> Dval.int, column |> bigint |> Dval.int, []) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "cliTerminalSessionInfo" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TTuple(TBool, TBool, [ TString ])
      description = "Return (input is terminal, output is terminal, TERM value)"
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          DTuple(
            TerminalCapabilities.isInputTerminal () |> DBool,
            TerminalCapabilities.isOutputTerminal () |> DBool,
            [ TerminalCapabilities.terminalName () |> DString ]
          )
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
