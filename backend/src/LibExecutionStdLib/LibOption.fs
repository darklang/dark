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

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"
let optionA = Param.make "option" (TOption varA) ""
let fnAToB = Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ]

let fns : List<BuiltInFn> =
  [ { name = fn "Option" "map" 1
      typeArgs = []
      parameters = [ optionA; fnAToB ]
      returnType = TOption varB
      description =
        "If <param option> is {{Just <var val>}}, then return {{Just (f <var
         val>)}}. The lambda <fn fn> applied to <var val> and the result is
         wrapped in {{Just}}. Otherwise if the result is {{Nothing}}, then return
         {{Nothing}}."
      fn =
        (function
        | state, _, [ DOption o; DFnVal b ] ->
          uply {
            match o with
            | Some dv ->
              let! result = Interpreter.applyFnVal state b [ dv ]

              return Dval.optionJust result
            | None -> return DOption None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "map2" 0
      typeArgs = []
      parameters =
        [ Param.make "option1" (TOption varA) ""
          Param.make "option2" (TOption varB) ""
          Param.makeWithArgs "fn" (TFn([ varA; varB ], varC)) "" [ "v1"; "v2" ] ]
      returnType = TOption varC
      description =
        "If both arguments are {{Just}} (<param option1> is {{Just <var v1>}} and
         <param option2> is {{Just <var v2>}}), then return {{Just (fn <var v1> <var
         v2>)}}. The lambda <param fn> should have two parameters, representing <var
         v1> and <var v2>. But if either <param option1> or <param option2> are
         {{Nothing}}, returns {{Nothing}} without applying <param fn>."
      fn =
        (function
        | state, _, [ DOption o1; DOption o2; DFnVal b ] ->
          uply {
            match (o1, o2) with
            | None, _ -> return DOption None
            | _, None -> return DOption None
            | Some dv1, Some dv2 ->
              let! result = Interpreter.applyFnVal state b [ dv1; dv2 ]

              return Dval.optionJust result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "andThen" 0
      typeArgs = []
      parameters =
        [ optionA
          Param.makeWithArgs "fn" (TFn([ TOption varA ], TOption varB)) "" [ "val" ] ]
      returnType = TOption varB
      description =
        "If <param option> is {{Just <var input>}}, returns {{fn <var input>}}. Where
         the lambda <param fn> is applied to <var input> and must return {{Just <var
         output>}} or {{Nothing}}. Otherwise if <param option> is {{Nothing}}, returns
         {{Nothing}}."
      fn =
        (function
        | state, _, [ DOption o; DFnVal b ] ->
          uply {
            match o with
            | Some dv ->
              let! result = Interpreter.applyFnVal state b [ dv ]

              match result with
              | DOption result -> return DOption result
              | other ->
                return
                  Exception.raiseCode (
                    Errors.expectedLambdaType "fn" (TOption varB) other
                  )
            | None -> return DOption None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Option" "withDefault" 0
      typeArgs = []
      parameters = [ optionA; Param.make "default" varA "" ]
      returnType = varA
      description =
        "If <param option> is {{Just <var value>}}, returns <var value>. Returns
         <param default> otherwise."
      fn =
        (function
        | _, _, [ DOption o; default' ] ->
          (match o with
           | Some dv -> Ply dv
           | None -> Ply default')
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
