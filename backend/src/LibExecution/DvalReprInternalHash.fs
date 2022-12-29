module LibExecution.DvalReprInternalHash

open Prelude

let supportedHashVersions : int list = [ 0; 1; 2 ]

// CLEANUP switch to 2 once rolled out to clients. Should just work
let currentHashVersion : int = 1

let hash (version : int) (arglist : List<RuntimeTypes.Dval>) : string =
  match version with
  | 0 -> DvalReprInternalDeprecated.toHashV0 arglist
  | 1 -> DvalReprInternalDeprecated.toHashV1 arglist
  | 2 -> DvalReprInternalNew.toHashV2 arglist
  | _ -> Exception.raiseInternal $"Invalid Dval.hash version" [ "version", version ]
