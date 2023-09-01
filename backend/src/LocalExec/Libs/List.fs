// Note: These functions are used within the 'Load packages from disk to the canvas DB' process,
// and PACKAGE functions cannot be used at that stage.
// They can be removed once we have a live package manager in place

module LocalExec.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


module Errors = LibExecution.Errors
module Interpreter = LibExecution.Interpreter

let varA = TVariable "a"

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []


let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "BuiltIns"; "List" ] "iter" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton varA, TUnit))
            ""
            [ "element" ] ]
      returnType = TUnit
      description =
        "Applies the given function <param fn> to each element of the <param list>."
      fn =
        (function
        | state, _, [ DList(_vtTODO, l); DFnVal b ] ->
          uply {
            do!
              l
              |> Ply.List.iterSequentially (fun e ->
                uply {
                  let args = NEList.singleton e
                  match! Interpreter.applyFnVal state 0UL b [] args with
                  | DUnit -> return ()
                  | DError _ as dv -> return Errors.foundFakeDval dv
                  | v ->
                    Exception.raiseCode (Errors.expectedLambdaValue "fn" "unit" v)
                })
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "BuiltIns"; "List" ] "flatten" 0
      typeParams = []
      parameters = [ Param.make "list" (TList(TList varA)) "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of every list directly in <param
         list> (does not recursively flatten nested lists)"
      fn =
        (function
        | _, _, [ DList(_vtTODO, l) ] ->
          let f acc i =
            match i with
            | DList(_vtTODO, l) -> List.append acc l
            | _ -> Exception.raiseCode "Flattening non-lists"

          List.fold f [] l |> Dval.list valueTypeTODO |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }

    ]

let contents = (fns, types, constants)
