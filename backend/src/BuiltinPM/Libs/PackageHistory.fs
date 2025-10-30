module BuiltinPM.Libs.PackageHistory

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module DarkDateTime = LibExecution.DarkDateTime
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module PackageIDs = LibExecution.PackageIDs

open Builtin.Shortcuts


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmGetNameHistory: Read-only history lookup, should be public for transparency
let fns : List<BuiltInFn> =
  [ { name = fn "scmGetNameHistory" 0
      typeParams = []
      parameters =
        [ Param.make "owner" TString ""
          Param.make "modules" (TList TString) ""
          Param.make "name" TString ""
          Param.make "itemType" TString "" ] // "fn", "type", or "value"
      returnType = TList(TVariable "historyEntry")
      description =
        "Get the history of what a name has pointed to over time. Returns list of (timestamp, branchId, itemId, opType)"
      fn =
        (function
        | _, _, _, [ DString owner; DList(_vtTODO, modulesDvals); DString name; DString itemType ] ->
          uply {
            let modules =
              modulesDvals
              |> List.map (function
                | DString s -> s
                | _ -> "")

            let! history =
              LibPackageManager.Queries.getNameHistory owner modules name itemType

            let historyEntryTypeName =
              FQTypeName.fqPackage PackageIDs.Type.SCM.Branch.historyEntry

            let historyDvals =
              history
              |> List.map (fun entry ->
                let fields =
                  [ "timestamp", DDateTime(DarkDateTime.fromDateTime entry.timestamp)
                    "branchId",
                    (match entry.branchId with
                     | Some id -> Dval.optionSome KTUuid (DUuid id)
                     | None -> Dval.optionNone KTUuid)
                    "itemId", DUuid entry.itemId
                    "opType", DString entry.opType ]
                  |> Map

                DRecord(historyEntryTypeName, historyEntryTypeName, [], fields))

            let historyEntryVT =
              LibExecution.ValueType.customType historyEntryTypeName []
            return DList(historyEntryVT, historyDvals)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
