module LibExecutionStdLib.LibOption

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes

module Interpreter = LibExecution.Interpreter
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"
let optionA = Param.make "option" (TOption varA) ""
let fnAToB = Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ]

let fns : List<BuiltInFn> =
  [ { name = fn "Option" "map" 0
      parameters = [ optionA; fnAToB ]
      returnType = TOption varB
      description =
        "If `option` is `Just value`, returns `Just (f value)` (the lambda `f` is applied to `value` and the result is wrapped in `Just`).
        If `result` is `Nothing`, returns `Nothing`."
      fn =
        (function
        | state, [ DOption o; DFnVal b ] ->
          uply {
            match o with
            | Some dv ->
              let! result =
                Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

              return DOption(Some result)
            | None _ -> return (DOption None)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "Option" "map" 1) }


    { name = fn "Option" "map" 1
      parameters = [ optionA; fnAToB ]
      returnType = TOption varB
      description =
        "If <var option> is {{Just <var value>}}, then return {{Just (f <var value>)}}. The lambda <var f> applied to <var value> and the result is wrapped in {{Just}}. Otherwise if the result is {{Nothing}}, then return {{Nothing}}."
      fn =
        (function
        | state, [ DOption o; DFnVal b ] ->
          uply {
            match o with
            | Some dv ->
              let! result =
                Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

              return Dval.optionJust result
            | None -> return DOption None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "map2" 0
      parameters =
        [ Param.make "option1" (TOption varA) ""
          Param.make "option2" (TOption varB) ""
          Param.makeWithArgs "f" (TFn([ varA; varB ], varC)) "" [ "v1"; "v2" ] ]
      returnType = TOption varC
      description =
        "If both arguments are {{Just}} (<param option1> is {{Just <var v1>}} and <param option2> is {{Just <var v2>}}), then return {{Just (f <var v1> <var v2>)}} -- The lambda <param f> should have two parameters, representing <var v1> and <var v2>. But if either <param option1> or <param option2> are {{Nothing}}, returns {{Nothing}} without applying <param f>."
      fn =
        (function
        | state, [ DOption o1; DOption o2; DFnVal b ] ->
          uply {
            match (o1, o2) with
            | None, _ -> return DOption None
            | _, None -> return DOption None
            | Some dv1, Some dv2 ->
              let! result =
                Interpreter.applyFnVal state (id 0) b [ dv1; dv2 ] NotInPipe NoRail

              return Dval.optionJust result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "andThen" 0
      parameters =
        [ optionA
          Param.makeWithArgs "f" (TFn([ TOption varA ], TOption varB)) "" [ "val" ] ]
      returnType = TOption varB
      description =
        "If <param option> is {{Just <var input>}}, returns {{f <var input>}}. Where the lambda <param f> is applied to <var input> and must return {{Just <var output>}} or {{Nothing}}. Otherwise if <param option> is {{Nothing}}, returns {{Nothing}}."
      fn =
        (function
        | state, [ DOption o; DFnVal b ] ->
          uply {
            match o with
            | Some dv ->
              let! result =
                Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

              match result with
              | DOption result -> return DOption result
              | other ->
                return
                  Errors.throw (Errors.expectedLambdaType "f" (TOption varB) other)
            | None -> return DOption None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "withDefault" 0
      parameters = [ optionA; Param.make "default" varA "" ]
      returnType = varA
      description =
        "If <param option> is {{Just <var value>}}, returns <var value>. Returns <param default> otherwise."
      fn =
        (function
        | _, [ DOption o; default' ] ->
          (match o with
           | Some dv -> Ply dv
           | None -> Ply default')
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
