module LibExecution.LibBool

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.Runtime
open FSharpPlus
open Prelude

let fn = FnDesc.stdFnDesc

let fns : List<BuiltInFn> =
  [ { name = fn "Bool" "not" 0

      parameters = [ Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns the inverse of `b`: true if `b` is false and false if `b` is true"
      fn =
        (function
        | _, [ DBool b ] -> Value(DBool(not b))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Bool" "and" 0
      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description = "Returns true if both a and b are true"
      fn =

        (function
        | _, [ DBool a; DBool b ] -> Value(DBool(a && b))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Bool" "or" 0
      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description = "Returns true if either a is true or b is true"
      fn =

        (function
        | _, [ DBool a; DBool b ] -> Value(DBool(a || b))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Bool" "xor" 0

      parameters = [ Param.make "a" TBool ""; Param.make "b" TBool "" ]
      returnType = TBool
      description =
        "Returns `true` if exactly one of `a` and `b` is `true`. Returns `false` if both are `true` or neither is `true`."
      fn =

        (function
        | _, [ DBool a; DBool b ] -> Value(DBool(a <> b))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Bool" "isNull" 0

      parameters = [ Param.make "check" TAny "" ]
      returnType = TBool
      description = "Returns true if the `check` parameter is null"
      fn =

        (function
        | _, [ value ] ->
            Value
              (match value with
               | DNull -> DBool true
               | _ -> DBool false)
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = fn "Bool" "isError" 0

      parameters = [ Param.make "check" TAny "" ]
      returnType = TBool
      description = "Returns `true` if the `check` parameter is an error"
      fn =

        (function
        | _, [ value ] ->
            Value
              (match value with
               | _ -> DBool true
               | DFakeVal (DError _) -> DBool true)
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = DeprecatedBecause "an old workaround to poor static typing that's no longer needed" } ]
