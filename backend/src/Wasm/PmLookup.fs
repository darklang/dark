/// The package-location lookup builtins the pretty-printer uses to render
/// record/enum type names (`pmGetLocationsBy*`). The CLI gets these from
/// Builtins.Matter, which the browser can't link (SQLite/LibCloud); this
/// mirrors that contract over the in-memory PM. The getter reads the live PM
/// so REPL-declared types resolve too.
module Darklang.Wasm.PmLookup

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module NR = LibExecution.RuntimeTypes.NameResolution

let private locationsFn
  (getPM : unit -> PT.PackageManager)
  (name : string)
  (lookup : PT.PackageManager -> PT.Hash -> Ply<List<PT.PackageLocation>>)
  : BuiltInFn =
  { name = fn name 0
    typeParams = []
    parameters =
      [ Param.make
          "branchId"
          TString
          "accepted to match the CLI contract; the browser has no branches, so it is not consulted"
        Param.make "hash" (TCustomType(NR.ok (PT2DT.Hash.typeName ()), [])) "" ]
    returnType = TList(TCustomType(NR.ok (PT2DT.PackageLocation.typeName ()), []))
    description = "Returns all locations of a package item by its hash"
    fn =
      (function
      // The branch is IGNORED here, and this is the one place that's honest rather
      // than a bug: the browser runs against an in-memory PM with no op log, no
      // `op_branches` and no way to be on a branch. The parameter exists so the same
      // Dark pretty-printer code links against both hosts.
      | _, _, _, [ DString _branchId; hashDval ] ->
        uply {
          let hash = PT2DT.Hash.fromDT hashDval
          let! result = lookup (getPM ()) hash
          return
            result
            |> List.map PT2DT.PackageLocation.toDT
            |> Dval.list (KTCustomType((PT2DT.PackageLocation.typeName ()), []))
        }
      | _ -> incorrectArgs ())
    sqlSpec = NotQueryable
    previewable = Impure
    capabilities = LibExecution.Capabilities.noCaps
    deprecated = NotDeprecated }

let builtins (getPM : unit -> PT.PackageManager) : Builtins =
  Builtin.make
    []
    [ locationsFn getPM "pmGetLocationsByType" (fun pm h -> pm.getTypeLocations h)
      locationsFn getPM "pmGetLocationsByValue" (fun pm h -> pm.getValueLocations h)
      locationsFn getPM "pmGetLocationsByFn" (fun pm h -> pm.getFnLocations h) ]
