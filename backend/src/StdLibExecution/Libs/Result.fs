module StdLibExecution.Libs.Result

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors

let varOk = TVariable "ok"
let varErr = TVariable "err"
let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "Result" "map" 1
      typeParams = []
      parameters =
        [ Param.make "result" (TResult(varOk, varErr)) ""
          Param.makeWithArgs "fn" (TFn([ varOk ], varB)) "" [ "val" ] ]
      returnType = TResult(varB, varErr)
      description =
        "If <param result> is {{Ok <var value>}}, returns {{Ok (fn <var value>)}}.
         The lambda <param fn> is applied to <var value> and the result is wrapped in
         {{Ok}}. If <param result> is {{Error <var msg>}}, returns <param result>
         unchanged."
      fn =
        (function
        | state, _, [ DResult r; DFnVal d ] ->
          uply {
            match r with
            | Ok dv ->
              let! result = Interpreter.applyFnVal state d [ dv ]

              return Dval.resultOk result
            | Error _ -> return DResult r
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "mapError" 1
      typeParams = []
      parameters =
        [ Param.make "result" (TResult(varOk, varErr)) ""
          Param.makeWithArgs "fn" (TFn([ varOk ], varB)) "" [ "val" ] ]
      returnType = (TResult(varB, varErr))
      description =
        "If <param result> is {{Error <var msg>}}, returns {{Error (fn <var msg>)}}.
         The lambda <var fn> is applied to <var msg> and the result is wrapped in
         {{Error}}. If <param result> is {{Ok <var value>}}, returns <param result>
         unchanged."
      fn =
        (function
        | state, _, [ DResult r; DFnVal b ] ->
          uply {
            match r with
            | Ok _ -> return DResult r
            | Error err ->
              let! result = Interpreter.applyFnVal state b [ err ]

              return Dval.resultError result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "withDefault" 0
      typeParams = []
      parameters =
        [ Param.make "result" (TResult(varOk, varErr)) ""
          Param.make "default" varB "" ]
      returnType = varB
      description =
        "If <param result> is {{Ok <var value>}}, returns <var value>. Returns <param
         default> otherwise."
      fn =
        (function
        | _, _, [ DResult o; default' ] ->
          match o with
          | Ok dv -> Ply dv
          | Error _ -> Ply default'
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "fromOption" 2
      typeParams = []
      parameters =
        [ Param.make "option" (TOption(varOk)) ""; Param.make "error" varErr "" ]
      returnType = TResult(varOk, varErr)
      description =
        "Turn an option into a result, using <param error> as the error message for Error. Specifically, if <param option> is {{Just <var value>}}, returns {{Ok <var value>}}. Returns {{Error <var error>}} otherwise."
      fn =
        (function
        | _, _, [ DOption o; error ] ->
          match o with
          | Some dv -> Ply(Dval.resultOk dv)
          | None -> Ply(DResult(Error(error)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "toOption" 1
      typeParams = []
      parameters = [ Param.make "result" (TResult(varOk, varErr)) "" ]
      returnType = TOption varB
      description = "Turn a <type Result> into an <type Option>"
      fn =
        (function
        | _, _, [ DResult o ] ->
          match o with
          | Ok dv -> Ply(Dval.optionJust dv)
          | Error _ -> Ply(DOption None)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "map2" 0
      typeParams = []
      parameters =
        [ Param.make "result1" (TResult(varA, varErr)) ""
          Param.make "result2" (TResult(varB, varErr)) ""
          Param.makeWithArgs "fn" (TFn([ varA; varB ], varC)) "" [ "v1"; "v2" ] ]
      returnType = (TResult(varC, varErr))
      description =
        "If both <param result1> is {{Ok <var v1>}} and <param result2> is {{Ok <var
         v2>}}, returns {{Ok (fn <var v1> <var v2>)}} -- the lambda <param fn> is
         applied to <var v1> and <var v2>, and the result is wrapped in {{Ok}}.
         Otherwise, returns the first of <param result1> and <param result2> that is
         an error."
      fn =
        (function
        | state, _, [ DResult r1; DResult r2; DFnVal b ] ->
          uply {
            match (r1, r2) with
            | Error e1, _ -> return DResult(Error e1)
            | Ok _, Error e2 -> return DResult(Error e2)
            | Ok dv1, Ok dv2 ->
              let! result = Interpreter.applyFnVal state b [ dv1; dv2 ]

              return Dval.resultOk result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Result" "andThen" 1
      typeParams = []
      parameters =
        [ Param.make "result" (TResult(varOk, varErr)) ""
          Param.makeWithArgs "fn" (TFn([ varOk ], varB)) "" [ "val" ] ]
      returnType = (TResult(varOk, varErr))
      description =
        "If <param result> is {{Ok <var value>}}, returns {{fn <var value>}}. The
         lambda <param fn> is applied to <var value> and must return {{Error <var
         msg>}} or {{Ok <var newValue>}}. If <param result> is {{Error <var msg>}},
         returns <param result> unchanged."
      fn =
        (function
        | state, _, [ DResult o; DFnVal b ] ->
          uply {
            match o with
            | Ok dv ->
              let! result = Interpreter.applyFnVal state b [ dv ]

              match result with
              | DResult (Ok result) -> return Dval.resultOk result
              | DResult (Error result) -> return Dval.resultError result
              | other ->
                return
                  Exception.raiseCode (
                    Errors.expectedLambdaType "fn" (TResult(varOk, varErr)) other
                  )
            | Error msg -> return DResult(Error msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
