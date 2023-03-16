/// This module only exists, temporarily, to demonstrate how to define and use a
/// Standard Library "custom" enum type. Once Option and Result are defined in this
/// way, this module may be deleted. (along with the corresponding test file
/// `maybe.tests`)
module LibExecutionStdLib.LibMaybe

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes

module Interpreter = LibExecution.Interpreter
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName
let tp = PT.FQTypeName.stdlibTypeName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"

/// Use this to _refer_ to the Maybe type
/// (TODO: maybe this should be available more generally in a more accessible place?)
let maybeTypeName = FQTypeName.Stdlib { typ = "Maybe" }

let maybeOf arg = TCustomType(maybeTypeName, [ arg ])

let maybeParamOf name (arg : DType) = Param.make name (maybeOf arg) ""

let fnAToB = Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ]

// TODO: having to assign IDs below is really annoying.
let types : List<PT.BuiltInType> =
  [ { name = tp "Maybe"
      typeArgs = [ "a" ]
      definition =
        PT.CustomType.Enum(
          // Nah
          { id = 1UL; name = "Nah"; fields = [] },

          // Some
          [ { id = 2UL
              name = "Totally"
              fields = [ { id = 3UL; typ = PT.TVariable "a"; label = None } ] } ]
        )
      description = "Represents a value that may or may not be present." } ]

// TODO: ensure type-checking works appropriately for these fns
let fns : List<BuiltInFn> =
  [ { name = fn "Maybe" "map" 0
      parameters = [ maybeParamOf "maybe" varA; fnAToB ]
      returnType = maybeOf varB
      description =
        "If <param maybe> is {{Just <var val>}}, then return {{Just (f <var
         val>)}}. The lambda <fn fn> applied to <var val> and the result is
         wrapped in {{Just}}. Otherwise if the result is {{Nothing}}, then return
         {{Nothing}}."
      fn =
        (function
        | state, [ DConstructor (_typeName, caseName, fields); DFnVal b ] ->
          uply {
            match caseName, fields with
            | "Totally", [ dv ] ->
              let! result = Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe

              // TODO: this used to use `maybeJust` which did some isFake checking,
              // we should see if that's still necessary
              return DConstructor(maybeTypeName, "Totally", [ result ])

            | "Nah", [] -> return DConstructor(maybeTypeName, "Nah", [])

            | _ -> return incorrectArgs ()
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Maybe" "map2" 0
      parameters =
        [ maybeParamOf "maybe1" varA
          maybeParamOf "maybe2" varB
          Param.makeWithArgs "fn" (TFn([ varA; varB ], varC)) "" [ "v1"; "v2" ] ]
      returnType = maybeOf varC
      description =
        "If both arguments are {{Just}} (<param maybe1> is {{Just <var v1>}} and
         <param maybe2> is {{Just <var v2>}}), then return {{Just (fn <var v1> <var
         v2>)}}. The lambda <param fn> should have two parameters, representing <var
         v1> and <var v2>. But if either <param maybe1> or <param maybe2> are
         {{Nothing}}, returns {{Nothing}} without applying <param fn>."
      fn =
        (function
        | state,
          [ DConstructor (_, caseNameA, fieldsA)
            DConstructor (_, caseNameB, fieldsB)
            DFnVal b ] ->
          uply {
            match (caseNameA, fieldsA), (caseNameB, fieldsB) with
            | ("Nah", []), _ -> return DConstructor(maybeTypeName, "Nah", [])

            | _, ("Nah", []) -> return DConstructor(maybeTypeName, "Nah", [])

            | ("Totally", [ dv1 ]), ("Totally", [ dv2 ]) ->
              let! result =
                Interpreter.applyFnVal state (id 0) b [ dv1; dv2 ] NotInPipe

              return DConstructor(maybeTypeName, "Totally", [ result ])

            | _ -> return incorrectArgs ()
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
