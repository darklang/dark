module LibExecution.StdLib

open FSharp.Control.Tasks
open Prelude
open Runtime
open Interpreter

let any =
  [ { name = FnDesc.stdFnDesc "" "==" 0
      description = "Equality" // FSTODO
      parameters =
        [ Param.make "a" (TVariable "a") ""; Param.make "b" (TVariable "b") "" ]
      returnType = TBool
      fn =
        (function
        | _, [ a; b ] -> (Value(DBool(a = b)))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]


let prefixFns : List<BuiltInFn> =
  (LibString.fns @ LibList.fns @ LibInt.fns @ LibDict.fns @ LibMiddleware.fns @ any)

// Add infix functions that are identical except for the name
let infixFns =
  let fns =
    List.choose (fun builtin ->
      let d = builtin.name

      let opName =
        match d.module_, d.function_, d.version with
        | "Int", "add", 0 -> Some "+"
        | "Int", "greaterThan", 0 -> Some ">"
        | "String", "append", 1 -> Some "++"
        | _ -> None

      Option.map (fun opName -> { builtin with name = FnDesc.stdFnDesc "" opName 0 })
        opName) prefixFns

  assert (fns.Length = 3) // make sure we got them all
  fns

let fns = infixFns @ prefixFns

// [ { name = FnDesc.stdFnDesc "Int" "range" 0
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
//   { name = (FnDesc.stdFnDesc "HttpClient" "get" 0)
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
