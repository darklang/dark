/// Standard libraries for printing and output
/// TODO rename these fns...
module Builtins.Cli.Libs.Output

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts


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
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "printErrorLine" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description =
        "Prints the given <param value> to the standard error, followed by a newline. For "
        + "everything that is not the answer to what was asked: errors, refusals, warnings, "
        + "hints, progress, prompts. Ordered with stdout, so it never overtakes a line printed "
        + "before it."
      fn =
        (function
        | _, _, _, [| DString str |] ->
          NonBlockingConsole.writeErrLine str
          Ply DUnit
        | _ -> incorrectArgs ())
      previewable = Impure
      // Printing, like stdout: output to the person's own terminal, which the default policy
      // allows. A separate effect would put a log line behind a grant.
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
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "printError" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description =
        "Prints the given <param value> to the standard error, with no newline. For a question "
        + "whose answer is typed on the same line."
      fn =
        (function
        | _, _, _, [| DString str |] ->
          NonBlockingConsole.writeErrInline str
          Ply DUnit
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "stdoutCaptureStart" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TBool
      description =
        "Start capturing standard output and standard error into an in-memory buffer instead of printing them. Pair with <fn stdoutCaptureStop>. Used to run a command and show its output in-frame. Returns false if a capture was already open, in which case the existing one is left untouched and this call captured nothing."
      fn =
        (function
        | _, _, _, [| DUnit |] -> DBool(NonBlockingConsole.startCapture ()) |> Ply
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated }


    { name = fn "stdoutCaptureStop" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "A unit" ]
      returnType = TString
      description =
        "Stop capturing and return everything written to either stream since <fn stdoutCaptureStart>, in the order it was written."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          let captured = NonBlockingConsole.stopCapture ()
          Ply(DString(String.normalizeOutside captured))
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.Stdout ]
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
