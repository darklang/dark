/// Standard libraries for printing and output
/// TODO create equivalent for stderr, and rename these fns...
module Builtins.Cli.Libs.Output

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

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
        | _, _, _, [| DString str |] ->
          print str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "print" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output."
      fn =
        (function
        | _, _, _, [| DString str |] ->
          printInline str
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "stdoutClear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TUnit
      description = "Clears the standard output."
      fn =
        function
        | _, _, _, [| DUnit |] ->
          if System.OperatingSystem.IsWindows() then
            System.Console.Clear()
          else
            System.Console.Write("\u001b[2J\u001b[H") // ANSI escape for non-Windows
          Ply DUnit
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "stdoutCapture" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.makeWithArgs
            "f"
            (TFn(NEList.singleton TUnit, TVariable "a"))
            "The computation to capture."
            [] ]
      returnType = TTuple(TVariable "a", TString, [])
      description =
        "Run a computation and return its value and stdout. Restore the enclosing output sink even if the computation raises."
      fn =
        (function
        | state, vm, _, [| DApplicable app |] ->
          uply {
            use capture = NonBlockingConsole.captureOutput ()
            match!
              Exe.executeApplicable
                state
                vm.activeAccess
                app
                (NEList.singleton DUnit)
            with
            | Ok value -> return DTuple(value, DString capture.Output, [])
            | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "debug" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "label" TString "The label to be printed."
          Param.make "value" (TVariable "a") "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the standard output"
      fn =
        (function
        | exeState, _, _, [| DString label; value |] ->
          uply {
            let! repr = Exe.dvalToRepr exeState value
            print $"DEBUG: {label}: {repr}"
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
