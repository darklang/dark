module BuiltinPM.Libs.PackageHistory

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module BinarySerialization = LibBinarySerialization.BinarySerialization
module DarkDateTime = LibExecution.DarkDateTime
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module PackageIDs = LibExecution.PackageIDs

open Builtin.Shortcuts
open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db


type HistoryEntry =
  { timestamp : System.DateTime
    branchId : Option<uuid>
    itemId : uuid
    opType : string }


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

            let modulesStr = String.concat "." modules

            // Query package_ops for all Set*Name ops for this location
            let! history =
              Sql.query
                """
                SELECT id, created_at, branch_id, op_blob
                FROM package_ops
                ORDER BY created_at ASC
                """
              |> Sql.executeAsync (fun read ->
                let opId = read.uuid "id"
                let timestamp = read.dateTime "created_at"
                let branchId = read.uuidOrNone "branch_id"
                let opBlob = read.bytes "op_blob"

                let op = BinarySerialization.PT.PackageOp.deserialize opId opBlob

                // Check if this op sets the name we're looking for
                match op with
                | PT.PackageOp.SetFnName(id, location)
                  when itemType = "fn"
                       && location.owner = owner
                       && String.concat "." location.modules = modulesStr
                       && location.name = name ->
                  Some
                    { timestamp = timestamp
                      branchId = branchId
                      itemId = id
                      opType = "SetFnName" }

                | PT.PackageOp.SetTypeName(id, location)
                  when itemType = "type"
                       && location.owner = owner
                       && String.concat "." location.modules = modulesStr
                       && location.name = name ->
                  Some
                    { timestamp = timestamp
                      branchId = branchId
                      itemId = id
                      opType = "SetTypeName" }

                | PT.PackageOp.SetValueName(id, location)
                  when itemType = "value"
                       && location.owner = owner
                       && String.concat "." location.modules = modulesStr
                       && location.name = name ->
                  Some
                    { timestamp = timestamp
                      branchId = branchId
                      itemId = id
                      opType = "SetValueName" }

                | _ -> None)

            let historyEntryTypeName =
              FQTypeName.fqPackage PackageIDs.Type.SCM.Branch.historyEntry

            let historyDvals =
              history
              |> List.choose (fun x -> x)
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
