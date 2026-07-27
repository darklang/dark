/// `printLine`/`print` builtins for the browser REPL. A tab has no stdout, so
/// output collects in a buffer that `Repl.Eval` drains into each result.
module Darklang.Wasm.Output

open Prelude
open LibExecution.RuntimeTypes

module Builtin = LibExecution.Builtin
open Builtin.Shortcuts

let private buffer = System.Text.StringBuilder()

/// Everything printed since the last drain; clears the buffer.
let drain () : string =
  let s = buffer.ToString()
  buffer.Clear() |> ignore<System.Text.StringBuilder>
  s

let private fns : List<BuiltInFn> =
  [ { name = fn "printLine" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description =
        "Prints the given <param value> to the REPL output, followed by a newline."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          buffer.Append(str).Append('\n') |> ignore<System.Text.StringBuilder>
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }

    { name = fn "print" 0
      typeParams = []
      parameters = [ Param.make "value" TString "The value to be printed." ]
      returnType = TUnit
      description = "Prints the given <param value> to the REPL output."
      fn =
        (function
        | _, _, _, [ DString str ] ->
          buffer.Append(str) |> ignore<System.Text.StringBuilder>
          Ply DUnit
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]

let builtins () : Builtins = Builtin.make [] fns
