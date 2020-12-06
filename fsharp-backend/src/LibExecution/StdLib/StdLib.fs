module LibExecution.StdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr

let any =
  [ { name = FQFnName.stdlibName "" "equals" 0
      description = "Returns true if the two value are equal"
      parameters =
        [ Param.make "a" (TVariable "a") ""; Param.make "b" (TVariable "a") "" ]
      returnType = TBool
      fn =
        (function
        | _, [ a; b ] -> (Value(DBool(a = b))) //FSTODO: use equal_dval
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    { name = FQFnName.stdlibName "" "toString" 0
      description =
        "Returns a string representation of `v`, suitable for displaying to a user. Redacts passwords."
      parameters = [ Param.make "a" (TVariable "a") "" ]
      returnType = TStr
      fn =
        (function
        | _, [ a ] -> a |> DvalRepr.toEnduserReadableTextV0 |> DStr |> Value
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]


let prefixFns : List<BuiltInFn> =
  (LibString.fns
   @ LibList.fns
   @ LibInt.fns
   @ LibBool.fns
   @ LibDict.fns
   @ LibBytes.fns
   @ LibMiddleware.fns
   @ any)

// Add infix functions that are identical except for the name
let infixFns =
  let fns =
    List.choose (fun builtin ->
      let d = builtin.name

      let opName =
        match d.module_, d.function_, d.version with
        | "Int", "add", 0 -> Some "+"
        | "Int", "subtract", 0 -> Some "-"
        | "Int", "greaterThan", 0 -> Some ">"
        | "Int", "greaterThanOrEqualTo", 0 -> Some ">="
        | "Int", "lessThanOrEqualTo", 0 -> Some "<="
        | "Int", "lessThan", 0 -> Some "<"
        | "Int", "power", 0 -> Some "^"
        | "String", "append", 1 -> Some "++"
        | "", "equals", 0 -> Some "=="
        | _ -> None

      Option.map (fun opName ->
        { builtin with name = FQFnName.stdlibName "" opName 0 }) opName) prefixFns

  assert (fns.Length = 8) // make sure we got them all
  fns

let fns = infixFns @ prefixFns

// [ { name = FQFnName.stdlibName "Int" "range" 0
//     parameters =
//       [ param "list" (TList(TVariable("a"))) "The list to be operated on"
//         param "fn" (TFn([ TVariable("a") ], TVariable("b"))) "Function to be called on each member" ]
//     returnType = retVal (TList(TInt)) "List of ints between lowerBound and upperBound"
//     fn =
//       (function
//       | _, [ DInt lower; DInt upper ] ->
//           List.map DInt [ lower .. upper ]
//           |> DList
//           |> Value
//
//       | _ -> Error()) }
//   { name = (FQFnName.stdlibName "HttpClient" "get" 0)
//     parameters = [ param "url" TString "URL to fetch" ]
//     returnType = (retVal TString "Body of response")
//     fn =
//       (function
//       | env, [ DStr url ] ->
//           try
//             Ok
//               (Task
//                 (task {
//                   let! response = FSharp.Data.Http.AsyncRequestString(url)
//                   return DStr(response)
//                  }))
//           with e ->
//             printfn "error in HttpClient::get: %s" (e.ToString())
//             Error()
//       | _ -> Error()) } ]
//
