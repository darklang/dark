module LibExecutionStdLib.LibBool

open System.Threading.Tasks
open FSharp.Control.Tasks
open Prelude
open LibExecution.RuntimeTypes

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"

let fns : List<BuiltInFn> =
  [ { name = fn "Bool" "not" 0
      parameters = [ Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns the inverse of <param b>: {{true}} if <param b> is {{false}} and {{false}} if <param b> is {{true}}"
      fn =
        (function
        | _, [ DBool b ] -> Ply(DBool(not b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlFunction "not"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bool" "and" 0
      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description = "Returns {{true}} if both <param a> and <param b> are {{true}}"
      fn =
        (function
        | _, [ DBool a; DBool b ] -> Ply(DBool(a && b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "AND"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bool" "or" 0
      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns {{true}} if either <param a> is true or <param b> is {{true}}"
      fn =
        (function
        | _, [ DBool a; DBool b ] -> Ply(DBool(a || b))
        | _ -> incorrectArgs ())
      sqlSpec = SqlBinOp "OR"
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bool" "xor" 0
      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns {{true}} if exactly one of <param a> and <param b> is {{true}}. Returns {{false}} if both are {{true}} or neither is {{true}}."
      fn =
        (function
        | _, [ DBool a; DBool b ] -> Ply(DBool(a <> b))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bool" "isNull" 0
      parameters = [ Param.make "check" varA "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param check> parameter is {{null}}"
      fn =
        (function
        | _, [ value ] ->
          Ply(
            match value with
            | DNull -> DBool true
            | _ -> DBool false
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bool" "isError" 0
      parameters = [ Param.make "check" varA "" ]
      returnType = TBool
      description = "Returns {{true}} if the <param check> parameter is an error"
      fn =
        (function
        | _, [ value ] ->
          Ply(
            match value with
            | DError _ -> DBool true
            | _ -> DBool false
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated =
        DeprecatedBecause
          "an old workaround to poor static typing that's no longer needed" } ]
