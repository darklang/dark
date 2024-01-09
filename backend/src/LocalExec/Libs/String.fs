// CLEANUP: This function is used within the 'Load packages from disk to the canvas DB' process,
// and PACKAGE functions cannot be used at that stage.
// it can be removed once we have a live package manager in place
module LocalExec.Libs.String

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "BuiltIns"; "String" ] "endsWith" 0
      typeParams = []
      parameters =
        [ Param.make "subject" TString "String to test"
          Param.make "suffix" TString "" ]
      returnType = TBool
      description = "Checks if <param subject> ends with <param suffix>"
      fn =
        (function
        | _, _, [ DString subject; DString suffix ] ->
          Ply(DBool(subject.EndsWith(suffix, System.StringComparison.Ordinal)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, constants)
