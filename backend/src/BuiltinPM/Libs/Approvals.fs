module BuiltinPM.Libs.Approvals

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Approvals = LibPackageManager.Approvals
module DarkDateTime = LibExecution.DarkDateTime
module PackageIDs = LibExecution.PackageIDs
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder

open Builtin.Shortcuts


let approvalStatusTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Approvals.approvalStatus

let approvalRequestStatusTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Approvals.approvalRequestStatus

let approvalRequestTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Approvals.approvalRequest

let requestItemTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Approvals.requestItem

let requestItemWithDetailsTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Approvals.requestItemWithDetails


// Conversion helpers
let approvalStatusToDT (status : Approvals.ApprovalStatus) : Dval =
  match status with
  | Approvals.Pending ->
    DEnum(approvalStatusTypeName, approvalStatusTypeName, [], "Pending", [])
  | Approvals.Approved ->
    DEnum(approvalStatusTypeName, approvalStatusTypeName, [], "Approved", [])
  | Approvals.Rejected reason ->
    DEnum(
      approvalStatusTypeName,
      approvalStatusTypeName,
      [],
      "Rejected",
      [ DString reason ]
    )
  | Approvals.ChangesRequested comment ->
    DEnum(
      approvalStatusTypeName,
      approvalStatusTypeName,
      [],
      "ChangesRequested",
      [ DString comment ]
    )

let dtToApprovalStatus (dval : Dval) : Approvals.ApprovalStatus =
  match dval with
  | DEnum(_, _, [], "Pending", []) -> Approvals.Pending
  | DEnum(_, _, [], "Approved", []) -> Approvals.Approved
  | DEnum(_, _, [], "Rejected", [ DString reason ]) -> Approvals.Rejected reason
  | DEnum(_, _, [], "ChangesRequested", [ DString comment ]) ->
    Approvals.ChangesRequested comment
  | _ -> Exception.raiseInternal "Invalid ApprovalStatus Dval" [ "dval", dval ]

let approvalRequestStatusToDT (status : Approvals.ApprovalRequestStatus) : Dval =
  match status with
  | Approvals.Open ->
    DEnum(
      approvalRequestStatusTypeName,
      approvalRequestStatusTypeName,
      [],
      "Open",
      []
    )
  | Approvals.Closed ->
    DEnum(
      approvalRequestStatusTypeName,
      approvalRequestStatusTypeName,
      [],
      "Closed",
      []
    )

let approvalRequestToDT (request : Approvals.ApprovalRequest) : Dval =
  let fields =
    [ "id", DUuid request.id
      "createdBy", DUuid request.createdBy
      "createdAt", DDateTime(DarkDateTime.fromInstant request.createdAt)
      "status", approvalRequestStatusToDT request.status
      "targetNamespace", DString request.targetNamespace
      "sourceBranchId",
      request.sourceBranchId |> Option.map DUuid |> Dval.option KTUuid
      "title", request.title |> Option.map DString |> Dval.option KTString
      "description",
      request.description |> Option.map DString |> Dval.option KTString ]
    |> Map

  DRecord(approvalRequestTypeName, approvalRequestTypeName, [], fields)

let requestItemToDT (loc : Approvals.RequestItem) : Dval =
  let fields =
    [ "requestId", DUuid loc.requestId
      "locationId", DString loc.locationId
      "status", approvalStatusToDT loc.status
      "reviewedBy", loc.reviewedBy |> Option.map DUuid |> Dval.option KTUuid
      "reviewedAt",
      loc.reviewedAt
      |> Option.map (DarkDateTime.fromInstant >> DDateTime)
      |> Dval.option KTDateTime ]
    |> Map

  DRecord(requestItemTypeName, requestItemTypeName, [], fields)

let requestItemWithDetailsToDT (loc : Approvals.RequestItemWithDetails) : Dval =
  let fields =
    [ ("locationId", DString loc.locationId)
      ("name", DString loc.name)
      ("itemType", DString loc.itemType)
      ("namespace_", DString loc.namespace_)
      ("modules", DString loc.modules)
      ("approvalStatus", approvalStatusToDT loc.approvalStatus)
      ("reviewedBy", loc.reviewedBy |> Option.map DUuid |> Dval.option KTUuid)
      ("reviewedAt",
       loc.reviewedAt
       |> Option.map (DarkDateTime.fromInstant >> DDateTime)
       |> Dval.option KTDateTime) ]
    |> Map

  DRecord(requestItemWithDetailsTypeName, requestItemWithDetailsTypeName, [], fields)


let fns : List<BuiltInFn> =
  [ // Pending Locations Functions
    { name = fn "scmApprovalsListPendingLocationsWithDetails" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" TUuid ""
          Param.make "branchId" (TypeReference.option TUuid) "" ]
      returnType =
        TList(
          TCustomType(
            Ok(
              FQTypeName.fqPackage
                PackageIDs.Type.SCM.Approvals.pendingLocationDetails
            ),
            []
          )
        )
      description =
        "Get all pending locations with full details (namespace, modules, name, type) created by account"
      fn =
        (function
        | _, _, _, [ DUuid accountID; branchId ] ->
          uply {
            let branchId = C2DT.Option.fromDT D.uuid branchId
            let! locations =
              Approvals.listPendingLocationsWithDetails accountID branchId

            let result =
              locations
              |> List.map (fun loc ->
                let fields =
                  [ ("locationId", DString loc.locationId)
                    ("namespace_", DString loc.namespace_)
                    ("modules", DString loc.modules)
                    ("name", DString loc.name)
                    ("itemType", DString loc.itemType)
                    ("createdBy", DUuid loc.createdBy) ]
                  |> Map

                DRecord(
                  FQTypeName.fqPackage
                    PackageIDs.Type.SCM.Approvals.pendingLocationDetails,
                  FQTypeName.fqPackage
                    PackageIDs.Type.SCM.Approvals.pendingLocationDetails,
                  [],
                  fields
                ))

            return
              DList(
                VT.customType
                  (FQTypeName.fqPackage
                    PackageIDs.Type.SCM.Approvals.pendingLocationDetails)
                  [],
                result
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Approval Request Functions
    { name = fn "scmApprovalsCreateApprovalRequest" 0
      typeParams = []
      parameters =
        [ Param.make "accountID" TUuid ""
          Param.make "targetNamespace" TString ""
          Param.make "locationIds" (TList TString) ""
          Param.make "title" (TypeReference.option TString) ""
          Param.make "description" (TypeReference.option TString) ""
          Param.make "sourceBranchId" (TypeReference.option TUuid) "" ]
      returnType = TCustomType(Ok approvalRequestTypeName, [])
      description = "Create a new approval request"
      fn =
        (function
        | _,
          _,
          _,
          [ DUuid accountID
            DString targetNamespace
            DList(_, locationIds)
            title
            description
            sourceBranchId ] ->
          uply {
            let locationIds =
              locationIds
              |> List.map (fun d ->
                match d with
                | DString s -> s
                | _ -> "")
            let title = C2DT.Option.fromDT D.string title
            let description = C2DT.Option.fromDT D.string description
            let sourceBranchId = C2DT.Option.fromDT D.uuid sourceBranchId

            let! request =
              Approvals.createApprovalRequest
                accountID
                targetNamespace
                locationIds
                title
                description
                sourceBranchId

            return approvalRequestToDT request
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsGetApprovalRequest" 0
      typeParams = []
      parameters = [ Param.make "requestId" TUuid "" ]
      returnType = TypeReference.option (TCustomType(Ok approvalRequestTypeName, []))
      description = "Get an approval request by ID"
      fn =
        (function
        | _, _, _, [ DUuid requestId ] ->
          uply {
            let! request = Approvals.getApprovalRequest requestId
            return
              request
              |> Option.map approvalRequestToDT
              |> Dval.option (KTCustomType(approvalRequestTypeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsGetApprovalRequestLocations" 0
      typeParams = []
      parameters = [ Param.make "requestId" TUuid "" ]
      returnType = TList(TCustomType(Ok requestItemTypeName, []))
      description = "Get all location approvals for a request"
      fn =
        (function
        | _, _, _, [ DUuid requestId ] ->
          uply {
            let! locations = Approvals.getRequestItems requestId
            let vt = VT.customType requestItemTypeName []
            return DList(vt, locations |> List.map requestItemToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsGetApprovalRequestLocationsWithDetails" 0
      typeParams = []
      parameters = [ Param.make "requestId" TUuid "" ]
      returnType = TList(TCustomType(Ok requestItemWithDetailsTypeName, []))
      description =
        "Get all location approvals for a request with full location details (name, itemType, etc)"
      fn =
        (function
        | _, _, _, [ DUuid requestId ] ->
          uply {
            let! locations = Approvals.getRequestItemsWithDetails requestId
            let vt = VT.customType requestItemWithDetailsTypeName []
            return DList(vt, locations |> List.map requestItemWithDetailsToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsListIncomingRequests" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "" ]
      returnType = TList(TCustomType(Ok approvalRequestTypeName, []))
      description = "List incoming requests for namespaces the account can review"
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! requests = Approvals.listIncomingRequests accountID
            let vt = VT.customType approvalRequestTypeName []
            return DList(vt, requests |> List.map approvalRequestToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsListOutgoingRequests" 0
      typeParams = []
      parameters = [ Param.make "accountID" TUuid "" ]
      returnType = TList(TCustomType(Ok approvalRequestTypeName, []))
      description = "List requests created by account"
      fn =
        (function
        | _, _, _, [ DUuid accountID ] ->
          uply {
            let! requests = Approvals.listOutgoingRequests accountID
            let vt = VT.customType approvalRequestTypeName []
            return DList(vt, requests |> List.map approvalRequestToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // Review Functions
    { name = fn "scmApprovalsSetLocationStatus" 0
      typeParams = []
      parameters =
        [ Param.make "requestId" TUuid ""
          Param.make "locationId" TString ""
          Param.make "status" (TCustomType(Ok approvalStatusTypeName, [])) ""
          Param.make "reviewerId" TUuid "" ]
      returnType = TUnit
      description = "Set the status of a location in a request"
      fn =
        (function
        | _, _, _, [ DUuid requestId; DString locationId; status; DUuid reviewerId ] ->
          uply {
            let status = dtToApprovalStatus status
            do! Approvals.setLocationStatus requestId locationId status reviewerId
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsWithdrawRequest" 0
      typeParams = []
      parameters =
        [ Param.make "requestId" TUuid ""; Param.make "submitterId" TUuid "" ]
      returnType = TBool
      description = "Withdraw an approval request"
      fn =
        (function
        | _, _, _, [ DUuid requestId; DUuid submitterId ] ->
          uply {
            let! result = Approvals.withdrawRequest requestId submitterId
            return DBool result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmApprovalsUpdateRequestStatusIfComplete" 0
      typeParams = []
      parameters = [ Param.make "requestId" TUuid "" ]
      returnType = TUnit
      description = "Update request status to closed if all locations decided"
      fn =
        (function
        | _, _, _, [ DUuid requestId ] ->
          uply {
            do! Approvals.updateRequestStatusIfComplete requestId
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
