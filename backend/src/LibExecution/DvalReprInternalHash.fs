module LibExecution.DvalReprInternalHash

open Prelude

let supportedHashVersions : int list = [ 2 ]

let currentHashVersion : int = 2

let hash (version : int) (arglist : NEList<RuntimeTypes.Dval>) : string =
  match version with
  | 2 -> arglist |> NEList.toList |> DvalReprInternalRoundtrippable.toHashV2
  | _ -> Exception.raiseInternal $"Invalid Dval.hash version" [ "version", version ]
