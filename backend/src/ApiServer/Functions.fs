/// Functions loaded with the Client's UI
module ApiServer.Functions

open Prelude
open Tablecloth

/// Returns a list of all standard library Functions
///
/// Depending on `includeAdminFns` flag, may exclude Dark admin-only fns
let functions (includeAdminFns : bool) : string =
  let fnFilter (fn : LibExecution.RuntimeTypes.BuiltInFn) =
    if includeAdminFns then
      true
    else
      fn.name.module_ <> "DarkInternal" && fn.name.module_ <> "HttpBaseClient"

  LibRealExecution.RealExecution.stdlibFns
  |> Map.values
  |> List.filter fnFilter
  |> List.map ClientTypes2BackendTypes.UI.Functions.BuiltInFn.toCT
  |> List.sortBy (fun fn -> fn.name)
  |> Json.Vanilla.prettySerialize
