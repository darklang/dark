module BuiltinExecution.Libs.Option

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Errors = LibExecution.Errors

module Interpreter = LibExecution.Interpreter

let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"
let optionA = Param.make "option" (TypeReference.option varA) ""
let fnAToB = Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "val" ]

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fn = fn [ "Option" ]

let fns : List<BuiltInFn> =
  [ { name = fn "map2" 0
      typeParams = []
      parameters =
        [ Param.make "option1" (TypeReference.option varA) ""
          Param.make "option2" (TypeReference.option varB) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varB, varC))
            ""
            [ "v1"; "v2" ] ]
      returnType = TypeReference.option varC
      description =
        "If both arguments are {{Some}} (<param option1> is {{Some <var v1>}} and
         <param option2> is {{Some <var v2>}}), then return {{Some (fn <var v1> <var
         v2>)}}. The lambda <param fn> should have two parameters, representing <var
         v1> and <var v2>. But if either <param option1> or <param option2> are
         {{None}}, returns {{None}} without applying <param fn>."
      fn =
        (function
        | state,
          _,
          [ DEnum(_, _, caseName1, dvs1); DEnum(_, _, caseName2, dvs2); DFnVal b ] ->
          uply {
            match (caseName1, dvs1, caseName2, dvs2) with
            | "None", _, _, _
            | _, _, "None", _ -> return Dval.optionNone
            | "Some", [ dv1 ], "Some", [ dv2 ] ->
              let args = NEList.doubleton dv1 dv2
              let! result = Interpreter.applyFnVal state 0UL b [] args
              return Dval.optionSome result
            | _ ->
              return
                Exception.raiseInternal
                  "Invalid enums"
                  [ "caseName1", caseName1; "caseName2", caseName2 ]
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
