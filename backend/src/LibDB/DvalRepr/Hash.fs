module LibDB.DvalRepr.Hash

open Prelude

module RT = LibExecution.RuntimeTypes

let supportedHashVersions : int list = [ 2 ]

let currentHashVersion : int = 2

let hash (version : int) (arglist : NEList<RT.Dval>) : string =
  match version with
  | 2 -> arglist |> NEList.toList |> Roundtrippable.toHashV2
  | _ -> Exception.raiseInternal $"Invalid Dval.hash version" [ "version", version ]
