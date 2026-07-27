/// Standard libraries for printing and output
/// TODO create equivalent for stderr, and rename these fns...
module Builtins.Cli.Libs.Output

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

module Exe = LibExecution.Execution


let fns () : List<BuiltInFn> =
  [ { name = fn "printLine" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description =
        "Prints the given <param value> to the standard output, followed by a newline."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          print str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "print" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          printInline str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "stdoutClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TUnit
      description = "Clears the standard output."
      fn =
        function
        | _, _, _, [ DUnit ] ->
          if System.OperatingSystem.IsWindows() then
            System.Console.Clear()
          else
            System.Console.Write("\u001b[2J\u001b[H") // ANSI escape for non-Windows
          Ply DUnit
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "stdoutCaptureStart" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TBool
      description =
        "Start capturing standard output into an in-memory buffer instead of printing it. Pair with <fn stdoutCaptureStop>. Used to run a command and show its output in-frame. Returns false if a capture was already open, in which case the existing one is left untouched and this call captured nothing."
      fn =
        (function
        | _, _, _, [ DUnit ] -> DBool(NonBlockingConsole.startCapture ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "stdoutCaptureStop" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TString
      description =
        "Stop capturing standard output and return everything written since <fn stdoutCaptureStart>."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          let captured = NonBlockingConsole.stopCapture ()
          Ply(DString captured)
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated }


    { name = fn "debug" 0
      typeParams = []
      parameters =
        [ Param.make "label" TString "The label to be printed."
          Param.make "value" (TVariable "a") "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output"
      fn =
        (function
        | exeState, _, _, [ DString label; value ] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            print $"DEBUG: {label}: {repr}"
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.Needs.stdout
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
