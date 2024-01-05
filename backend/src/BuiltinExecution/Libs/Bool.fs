module BuiltinExecution.Libs.Bool

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

let fn = fn [ "Bool" ]

let varA = TVariable "a"

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  // TODO: Maybe Expose ENot
  [ { name = fn "not" 0
      typeParams = []
      parameters = [ Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns the inverse of <param b>: {{true}} if <param b> is {{false}} and {{false}} if <param b> is {{true}}"
      fn =
        (function
        | _, _, [ DBool b ] -> Ply(DBool(not b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "not"
      previewable = Pure
      deprecated = NotDeprecated } ]


let contents = (fns, constants)
