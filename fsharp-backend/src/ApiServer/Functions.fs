/// Types used to send functions to the client
///
/// Many of these types exactly match runtime types, but often there are small
/// changes
module ApiServer.Functions

open Prelude
open Tablecloth

module RT = LibExecution.RuntimeTypes

let functionsToString (fns : RT.BuiltInFn list) : string =
  fns
  |> List.map ClientTypes2BackendTypes.UI.Functions.BuiltInFn.toCT
  |> List.sortBy (fun fn -> fn.name)
  |> Json.Vanilla.prettySerialize

let adminFunctions : string =
  LibRealExecution.RealExecution.stdlibFns |> Map.values |> functionsToString

let nonAdminFunctions : string =
  LibRealExecution.RealExecution.stdlibFns
  |> Map.values
  |> List.filter (function
    | { name = { module_ = "DarkInternal" } } -> false
    | _ -> true)
  |> functionsToString

/// Returns a list of all standard library Functions
///
/// Depending on `includeAdminFns` flag, may exclude Dark admin-only fns
let functions (includeAdminFns : bool) : string =
  if includeAdminFns then adminFunctions else nonAdminFunctions
