module LibExecution.StdLib.StdLib

open Prelude
open LibExecution.RuntimeTypes

module DvalRepr = LibExecution.DvalRepr

let incorrectArgs = LibExecution.Errors.incorrectArgs

let any =
  [ { name = FQFnName.stdlibName "" "equals" 0
      description = "Returns true if the two value are equal"
      parameters =
        [ Param.make "a" (TVariable "a") ""; Param.make "b" (TVariable "a") "" ]
      returnType = TBool
      fn =
        InProcess
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
        InProcess
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
       @ LibBool.fns @ LibDict.fns @ LibBytes.fns @ LibMiddleware.fns @ any)

// Map of prefix names to their infix versions
let infixFnMapping =
  [ ("Int", "add", 0), "+"
    ("Int", "subtract", 0), "-"
    ("Int", "greaterThan", 0), ">"
    ("Int", "greaterThanOrEqualTo", 0), ">="
    ("Int", "lessThanOrEqualTo", 0), "<="
    ("Int", "lessThan", 0), "<"
    ("Int", "power", 0), "^"
    ("String", "append", 1), "++"
    ("", "equals", 0), "==" ]
  |> List.map
       (fun ((module_, name, version), opName) ->
         FQFnName.stdlibName module_ name version, FQFnName.stdlibName "" opName 0)
  |> Map

// set of infix names
let infixFnNames =
  infixFnMapping |> Map.toSeq |> Seq.map FSharpPlus.Operators.item1 |> Set

// Is this the name of an infix function?
let isInfixName (name : FQFnName.T) = infixFnNames.Contains name

let infixFns =
  let fns =
    List.choose
      (fun builtin ->
        let opName = infixFnMapping.TryFind builtin.name
        Option.map (fun newName -> { builtin with name = newName }) opName)
      prefixFns

  assert (fns.Length = infixFnMapping.Count) // make sure we got them all
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
