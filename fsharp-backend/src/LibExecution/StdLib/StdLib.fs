module LibExecution.StdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr

let incorrectArgs = LibExecution.Errors.incorrectArgs

let prefixFns : List<BuiltInFn> =
  List.concat [ LibBool.fns
                LibBytes.fns
                LibDate.fns
                LibDict.fns
                LibInt.fns
                LibList.fns
                LibMiddleware.fns
                LibNoModule.fns
                LibResult.fns
                LibString.fns ]

// Map of prefix names to their infix versions
let infixFnMapping =
  // FSTODO: there are 20 infix_names in the ocaml libs
  [ ("Int", "add", 0), ("", "+")
    ("Int", "subtract", 0), ("", "-")
    ("Int", "greaterThan", 0), ("", ">")
    ("Int", "greaterThanOrEqualTo", 0), ("", ">=")
    ("Int", "lessThanOrEqualTo", 0), ("", "<=")
    ("Int", "lessThan", 0), ("", "<")
    ("Int", "power", 0), ("", "^")
    ("Date", "lessThan", 0), ("Date", "<")
    ("Date", "greaterThan", 0), ("Date", ">")
    ("Date", "lessThanOrEqualTo", 0), ("Date", "<=")
    ("Date", "greaterThanOrEqualTo", 0), ("Date", ">=")
    ("String", "append", 1), ("", "++")
    ("", "equals", 0), ("", "==")
    ("", "notEquals", 0), ("", "!=")
    ("Bool", "and", 0), ("", "&&")
    ("Bool", "or", 0), ("", "||") ]
  |> List.map
       (fun ((module_, name, version), (newMod, opName)) ->
         FQFnName.stdlibName module_ name version,
         FQFnName.stdlibName newMod opName 0)
  |> Map

// set of infix names
let infixFnNames =
  infixFnMapping |> Map.toSeq |> Seq.map FSharpPlus.Operators.item1 |> Set

// Is this the name of an infix function?
let isInfixName (name : FQFnName.T) = infixFnNames.Contains name

let infixFns : List<BuiltInFn> =
  let fns =
    List.choose
      (fun (builtin : BuiltInFn) ->
        let opName = infixFnMapping.TryFind builtin.name
        Option.map (fun newName -> { builtin with name = newName }) opName)
      prefixFns

  assertEq "All infixes are parsed" fns.Length infixFnMapping.Count // make sure we got them all
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
