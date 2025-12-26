/// Database layer for the approvals system
/// See approvals-plan.md for full design
module LibPackageManager.Approvals

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Fumble
open LibDB.Db


// =============================================================================
// Types
// =============================================================================

type ApprovalStatus =
  | Pending
  | Approved
  | Rejected of reason : string
  | ChangesRequested of comment : string

type ApprovalRequestStatus =
  | Open
  | Closed

type ApprovalRequest =
  { id : uuid
    createdBy : uuid
    createdAt : NodaTime.Instant
    status : ApprovalRequestStatus
    targetNamespace : string
    sourceBranchId : Option<uuid>
    title : Option<string>
    description : Option<string> }

type RequestItem =
  { requestId : uuid
    locationId : string
    status : ApprovalStatus
    reviewedBy : Option<uuid>
    reviewedAt : Option<NodaTime.Instant> }


// =============================================================================
// Helpers
// =============================================================================

let private parseApprovalStatus
  (status : string)
  (message : Option<string>)
  : ApprovalStatus =
  match status with
  | "pending" -> Pending
  | "approved" -> Approved
  | "rejected" -> Rejected(message |> Option.defaultValue "")
  | "changes_requested" -> ChangesRequested(message |> Option.defaultValue "")
  | _ -> Exception.raiseInternal "Invalid approval status in DB" [ "status", status ]

let private approvalStatusToString
  (status : ApprovalStatus)
  : string * Option<string> =
  match status with
  | Pending -> "pending", None
  | Approved -> "approved", None
  | Rejected reason -> "rejected", Some reason
  | ChangesRequested comment -> "changes_requested", Some comment

let private parseRequestStatus (status : string) : ApprovalRequestStatus =
  match status with
  | "open" -> Open
  | "closed" -> Closed
  | _ -> Exception.raiseInternal "Invalid request status in DB" [ "status", status ]


// =============================================================================
// Pending Locations Functions
// =============================================================================

/// Full details for a pending location
type PendingLocationDetails =
  { locationId : string
    namespace_ : string
    modules : string
    name : string
    itemType : string
    createdBy : uuid }

/// Get all pending locations with full details created by account
/// Excludes locations that are already part of an open approval request
let listPendingLocationsWithDetails
  (accountID : uuid)
  (branchID : Option<uuid>)
  : Task<List<PendingLocationDetails>> =
  task {
    return!
      """
      SELECT l.location_id, l.owner, l.modules, l.name, l.item_type, l.created_by
      FROM locations l
      WHERE l.created_by = @account_id
        AND l.approval_status = 'pending'
        AND l.deprecated_at IS NULL
        AND (l.branch_id IS NULL OR l.branch_id = @branch_id)
        AND NOT EXISTS (
          SELECT 1 FROM request_items ri
          INNER JOIN approval_requests ar ON ri.request_id = ar.id
          WHERE ri.location_id = l.location_id
            AND ar.status = 'open'
        )
      ORDER BY l.owner, l.created_at DESC
      """
      |> Sql.query
      |> Sql.parameters
        [ "account_id", Sql.uuid accountID
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeAsync (fun read ->
        { locationId = read.string "location_id"
          namespace_ = read.string "owner"
          modules = read.string "modules"
          name = read.string "name"
          itemType = read.string "item_type"
          createdBy = read.uuid "created_by" })
  }

// =============================================================================
// Approval Request Functions
// =============================================================================

/// Create a new approval request
/// Creates a RequestNamingApproval op that syncs across instances
let createApprovalRequest
  (accountID : uuid)
  (targetNamespace : string)
  (locationIds : List<string>)
  (title : Option<string>)
  (description : Option<string>)
  (sourceBranchId : Option<uuid>)
  : Task<ApprovalRequest> =
  task {
    let requestId = System.Guid.NewGuid()
    let now = NodaTime.Instant.now ()

    // Create the op - playback will handle DB inserts
    let op =
      LibExecution.ProgramTypes.PackageOp.RequestNamingApproval(
        requestId,
        accountID,
        targetNamespace,
        locationIds,
        title,
        description,
        sourceBranchId
      )

    let! _ = Inserts.insertAndApplyOps None sourceBranchId (Some accountID) [ op ]

    return
      { id = requestId
        createdBy = accountID
        createdAt = now
        status = Open
        targetNamespace = targetNamespace
        sourceBranchId = sourceBranchId
        title = title
        description = description }
  }

/// Get an approval request by ID
let getApprovalRequest (requestId : uuid) : Task<Option<ApprovalRequest>> =
  task {
    return!
      """
      SELECT id, created_by, created_at, status, target_namespace, source_branch_id, title, description
      FROM approval_requests
      WHERE id = @id
      """
      |> Sql.query
      |> Sql.parameters [ "id", Sql.uuid requestId ]
      |> Sql.executeRowOptionAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuid "created_by"
          createdAt = read.instant "created_at"
          status = parseRequestStatus (read.string "status")
          targetNamespace = read.string "target_namespace"
          sourceBranchId = read.uuidOrNone "source_branch_id"
          title = read.stringOrNone "title"
          description = read.stringOrNone "description" })
  }

/// Get all items for a request
let getRequestItems (requestId : uuid) : Task<List<RequestItem>> =
  task {
    return!
      """
      SELECT request_id, location_id, status, status_message, reviewed_by, reviewed_at
      FROM request_items
      WHERE request_id = @request_id
      ORDER BY location_id
      """
      |> Sql.query
      |> Sql.parameters [ "request_id", Sql.uuid requestId ]
      |> Sql.executeAsync (fun read ->
        { requestId = read.uuid "request_id"
          locationId = read.string "location_id"
          status =
            parseApprovalStatus
              (read.string "status")
              (read.stringOrNone "status_message")
          reviewedBy = read.uuidOrNone "reviewed_by"
          reviewedAt = read.instantOrNone "reviewed_at" })
  }

/// Request item with full details from locations table
type RequestItemWithDetails =
  { locationId : string
    name : string
    itemType : string
    namespace_ : string
    modules : string
    approvalStatus : ApprovalStatus
    reviewedBy : Option<uuid>
    reviewedAt : Option<NodaTime.Instant> }

/// Get all items for a request with full location details
let getRequestItemsWithDetails
  (requestId : uuid)
  : Task<List<RequestItemWithDetails>> =
  task {
    return!
      """
      SELECT ri.location_id, ri.status, ri.status_message, ri.reviewed_by, ri.reviewed_at,
             l.name, l.item_type, l.owner, l.modules
      FROM request_items ri
      INNER JOIN locations l ON ri.location_id = l.location_id
      WHERE ri.request_id = @request_id
      ORDER BY l.name
      """
      |> Sql.query
      |> Sql.parameters [ "request_id", Sql.uuid requestId ]
      |> Sql.executeAsync (fun read ->
        { locationId = read.string "location_id"
          name = read.string "name"
          itemType = read.string "item_type"
          namespace_ = read.string "owner"
          modules = read.string "modules"
          approvalStatus =
            parseApprovalStatus
              (read.string "status")
              (read.stringOrNone "status_message")
          reviewedBy = read.uuidOrNone "reviewed_by"
          reviewedAt = read.instantOrNone "reviewed_at" })
  }

/// List incoming requests for namespaces the account can review
let listIncomingRequests (accountID : uuid) : Task<List<ApprovalRequest>> =
  task {
    return!
      """
      SELECT DISTINCT ar.id, ar.created_by, ar.created_at, ar.status, ar.target_namespace, ar.source_branch_id, ar.title, ar.description
      FROM approval_requests ar
      JOIN namespace_access na ON ar.target_namespace = na.namespace
      WHERE na.account_id = @account_id
        AND ar.status = 'open'
        AND ar.created_by != @account_id
      ORDER BY ar.created_at DESC
      """
      |> Sql.query
      |> Sql.parameters [ "account_id", Sql.uuid accountID ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuid "created_by"
          createdAt = read.instant "created_at"
          status = parseRequestStatus (read.string "status")
          targetNamespace = read.string "target_namespace"
          sourceBranchId = read.uuidOrNone "source_branch_id"
          title = read.stringOrNone "title"
          description = read.stringOrNone "description" })
  }

/// List open requests created by account (excludes closed/completed requests)
let listOutgoingRequests (accountID : uuid) : Task<List<ApprovalRequest>> =
  task {
    return!
      """
      SELECT id, created_by, created_at, status, target_namespace, source_branch_id, title, description
      FROM approval_requests
      WHERE created_by = @account_id
        AND status = 'open'
      ORDER BY created_at DESC
      """
      |> Sql.query
      |> Sql.parameters [ "account_id", Sql.uuid accountID ]
      |> Sql.executeAsync (fun read ->
        { id = read.uuid "id"
          createdBy = read.uuid "created_by"
          createdAt = read.instant "created_at"
          status = parseRequestStatus (read.string "status")
          targetNamespace = read.string "target_namespace"
          sourceBranchId = read.uuidOrNone "source_branch_id"
          title = read.stringOrNone "title"
          description = read.stringOrNone "description" })
  }


// =============================================================================
// Location Review Functions
// =============================================================================

/// Set the status of a location, optionally within an approval request
/// When requestId is Some, updates the request_items table
/// Creates PackageOps for statuses that need to sync across instances
let setLocationStatus
  (requestId : Option<uuid>)
  (locationId : string)
  (status : ApprovalStatus)
  (reviewerId : uuid)
  : Task<bool> =
  task {
    // Update request_items table if this is part of an approval request
    match requestId with
    | Some reqId ->
      match status with
      | ChangesRequested _ -> ()
      | _ ->
        let (statusStr, message) = approvalStatusToString status
        let now = NodaTime.Instant.now ()
        do!
          """
          UPDATE request_items
          SET status = @status,
              status_message = @message,
              reviewed_by = @reviewer,
              reviewed_at = @reviewed_at
          WHERE request_id = @request_id
            AND location_id = @location_id
          """
          |> Sql.query
          |> Sql.parameters
            [ "request_id", Sql.uuid reqId
              "location_id", Sql.string locationId
              "status", Sql.string statusStr
              "message",
              (match message with
               | Some m -> Sql.string m
               | None -> Sql.dbnull)
              "reviewer", Sql.uuid reviewerId
              "reviewed_at", Sql.instant now ]
          |> Sql.executeNonQueryAsync
          |> Task.map (fun _ -> ())
    | None -> ()

    // Create PackageOps for statuses that need to sync across instances
    match status with
    | Approved
    | Rejected _ ->
      // Look up item_id and branch_id from location_id
      let! locationInfo =
        """
        SELECT item_id, branch_id
        FROM locations
        WHERE location_id = @location_id
          AND deprecated_at IS NULL
        """
        |> Sql.query
        |> Sql.parameters [ "location_id", Sql.string locationId ]
        |> Sql.executeRowOptionAsync (fun read ->
          (read.uuid "item_id", read.uuidOrNone "branch_id"))

      match locationInfo, status with
      | Some(itemId, branchId), Approved ->
        let op =
          LibExecution.ProgramTypes.PackageOp.ApproveItem(
            itemId,
            branchId,
            reviewerId
          )
        let! _ = Inserts.insertAndApplyOps None branchId (Some reviewerId) [ op ]
        return true
      | Some(itemId, branchId), Rejected reason ->
        let op =
          LibExecution.ProgramTypes.PackageOp.RejectItem(
            itemId,
            branchId,
            reviewerId,
            reason
          )
        let! _ = Inserts.insertAndApplyOps None branchId (Some reviewerId) [ op ]
        return true
      | Some _, (Pending | ChangesRequested _) -> return false
      | None, _ -> return false

    | ChangesRequested comment ->
      // ChangesRequested requires a request
      match requestId with
      | Some reqId ->
        let op =
          LibExecution.ProgramTypes.PackageOp.RequestChanges(
            reqId,
            locationId,
            reviewerId,
            comment
          )
        let! _ = Inserts.insertAndApplyOps None None (Some reviewerId) [ op ]
        return true
      | None -> return false

    | Pending ->
      // No PackageOp for pending status - it's the initial state
      return true
  }

/// Withdraw (cancel) an approval request
/// Creates a WithdrawApprovalRequest op that syncs across instances
let withdrawRequest (requestId : uuid) (submitterId : uuid) : Task<bool> =
  task {
    // Verify the submitter owns the request
    let! request = getApprovalRequest requestId

    match request with
    | Some r when r.createdBy = submitterId ->
      // Create the op - playback will handle DB delete
      let op =
        LibExecution.ProgramTypes.PackageOp.WithdrawApprovalRequest(
          requestId,
          submitterId
        )
      let! _ =
        Inserts.insertAndApplyOps None r.sourceBranchId (Some submitterId) [ op ]
      return true
    | _ -> return false
  }

/// Update request status if all items are decided
/// If all approved, delete the request (items are now visible in package tree)
/// If any rejected, close the request (keep for history)
let updateRequestStatusIfComplete (requestId : uuid) : Task<unit> =
  task {
    // Check if any items are still pending or changes_requested
    let! pendingCount =
      """
      SELECT COUNT(*) as count
      FROM request_items
      WHERE request_id = @request_id
        AND status IN ('pending', 'changes_requested')
      """
      |> Sql.query
      |> Sql.parameters [ "request_id", Sql.uuid requestId ]
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    if pendingCount = 0L then
      // Check if all items are approved (vs some rejected)
      let! rejectedCount =
        """
        SELECT COUNT(*) as count
        FROM request_items
        WHERE request_id = @request_id
          AND status = 'rejected'
        """
        |> Sql.query
        |> Sql.parameters [ "request_id", Sql.uuid requestId ]
        |> Sql.executeRowAsync (fun read -> read.int64 "count")

      if rejectedCount = 0L then
        // All approved - delete the request (cascade deletes request_items)
        do!
          """
          DELETE FROM approval_requests
          WHERE id = @id
          """
          |> Sql.query
          |> Sql.parameters [ "id", Sql.uuid requestId ]
          |> Sql.executeNonQueryAsync
          |> Task.map (fun _ -> ())
      else
        // Some rejected - close the request but keep for history
        do!
          """
          UPDATE approval_requests
          SET status = 'closed'
          WHERE id = @id
          """
          |> Sql.query
          |> Sql.parameters [ "id", Sql.uuid requestId ]
          |> Sql.executeNonQueryAsync
          |> Task.map (fun _ -> ())
  }


/// Check if an account has access to a namespace (owner or reviewer)
let hasAccessToNamespace (accountId : uuid) (namespace_ : string) : Task<bool> =
  task {
    let! count =
      """
      SELECT COUNT(*) as count
      FROM namespace_access
      WHERE account_id = @account_id
        AND namespace = @namespace
      """
      |> Sql.query
      |> Sql.parameters
        [ "account_id", Sql.uuid accountId; "namespace", Sql.string namespace_ ]
      |> Sql.executeRowAsync (fun read -> read.int64 "count")

    return count > 0L
  }


/// Approve a single location directly (for own-namespace items)
let approveLocationDirectly (locationId : string) (reviewerId : uuid) : Task<bool> =
  setLocationStatus None locationId Approved reviewerId
