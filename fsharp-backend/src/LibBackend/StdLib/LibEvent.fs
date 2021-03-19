module LibBackend.StdLib.LibEvent

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> = []
// [ { name = fn "" "emit" 0
//
//   ; parameters = [Param.make "Data" varA ""; Param.make "Space" TStr ""; Param.make "Name" TStr ""]
//   ; returnType = varA
//   ; description =
//       "Emit event `name` in `space`, passing along `data` as a parameter"
//   ; fn =
//         (function
//         | {canvas_id; account_id; _}, [data; DStr space; DStr name] ->
//             (* See client/src/Entry.ml for the "_" *)
//             let space = Unicode_string.to_string space in
//             let name = Unicode_string.to_string name in
//             Event_queue.enqueue ~canvas_id ~account_id space name "_" data ;
//             data
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "" "emit" "1"
//   ; parameters = [Param.make "event" varA ""; Param.make "Name" TStr ""]
//   ; returnType = varA
//   ; description = "Emit a `event` to the `name` worker"
//   ; fn =
//         (function
//         | {canvas_id; account_id; _}, [data; DStr name] ->
//             (* See client/src/Entry.ml for the "_" *)
//             let name = Unicode_string.to_string name in
//             Event_queue.enqueue ~canvas_id ~account_id "WORKER" name "_" data ;
//             data
//         | _ ->
//             incorrectArgs ())
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = NotDeprecated } ]
//
