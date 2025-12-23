/// Applies PackageOps to the DB projection tables.
/// These tables (package_types, package_values, package_functions, locations) are projections
/// of the source-of-truth package_ops table.
module LibPackageManager.PackageOpPlayback


open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open Microsoft.Data.Sqlite
open Fumble
open LibDB.Db

module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module BS = LibBinarySerialization.BinarySerialization


/// Apply a single AddType op to the package_types table
let private applyAddType (typ : PT.PackageType.PackageType) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageType.serialize typ.id typ
    let rtDef = typ |> PT2RT.PackageType.toRT |> BS.RT.PackageType.serialize typ.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_types (id, pt_def, rt_def)
        VALUES (@id, @pt_def, @rt_def)
        """
      |> Sql.parameters
        [ "id", Sql.uuid typ.id
          "pt_def", Sql.bytes ptDef
          "rt_def", Sql.bytes rtDef ]
      |> Sql.executeStatementAsync
  }

/// Apply a single AddValue op to the package_values table
let private applyAddValue (value : PT.PackageValue.PackageValue) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageValue.serialize value.id value
    let rtDval =
      value |> PT2RT.PackageValue.toRT |> BS.RT.PackageValue.serialize value.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_values (id, pt_def, rt_dval)
        VALUES (@id, @pt_def, @rt_dval)
        """
      |> Sql.parameters
        [ "id", Sql.uuid value.id
          "pt_def", Sql.bytes ptDef
          "rt_dval", Sql.bytes rtDval ]
      |> Sql.executeStatementAsync
  }

/// Apply a single AddFn op to the package_functions table
let private applyAddFn (fn : PT.PackageFn.PackageFn) : Task<unit> =
  task {
    let ptDef = BS.PT.PackageFn.serialize fn.id fn
    let rtInstrs = fn |> PT2RT.PackageFn.toRT |> BS.RT.PackageFn.serialize fn.id

    do!
      Sql.query
        """
        INSERT OR REPLACE INTO package_functions (id, pt_def, rt_instrs)
        VALUES (@id, @pt_def, @rt_instrs)
        """
      |> Sql.parameters
        [ "id", Sql.uuid fn.id
          "pt_def", Sql.bytes ptDef
          "rt_instrs", Sql.bytes rtInstrs ]
      |> Sql.executeStatementAsync
  }

/// Apply a Set*Name op to the locations table
let private applySetName
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (createdBy : Option<uuid>)
  (itemId : uuid)
  (location : PT.PackageLocation)
  (itemType : string)
  : Task<unit> =
  task {
    let modulesStr = String.concat "." location.modules

    // First, deprecate any existing location for this item in this branch
    // (handles moves: old location gets deprecated, new location created)
    do!
      Sql.query
        """
        UPDATE locations
        SET deprecated_at = datetime('now')
        WHERE item_id = @item_id
          AND deprecated_at IS NULL
          AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid itemId
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull) ]
      |> Sql.executeStatementAsync

    // Determine approval status based on source:
    // - instanceID = Some _ (from sync): always start pending, let ApproveItem ops set status
    // - instanceID = None, createdBy = None (disk-load): auto-approve (built-in packages)
    // - instanceID = None, createdBy = Some creator (local editor):
    //     - creator == owner: auto-approve (own namespace)
    //     - creator != owner: pending (needs approval from namespace owner)
    let! approvalStatus =
      task {
        match instanceID with
        | Some _ ->
          // From sync - always start pending
          // ApproveItem ops in the synced op stream will set the correct status
          return "pending"
        | None ->
          // Local or disk-loaded
          match createdBy with
          | None ->
            // Disk-load: no known creator, auto-approve as built-in packages
            return "approved"
          | Some creatorId ->
            // Local editor with known creator - look up if they own the namespace
            let! creatorName = Accounts.getName creatorId
            match creatorName with
            | Some name when location.owner = name -> return "approved"
            | _ -> return "pending"
      }

    // Insert new location entry with unique location_id
    let locationId = System.Guid.NewGuid()
    do!
      Sql.query
        """
        INSERT INTO locations (location_id, item_id, branch_id, owner, modules, name, item_type, created_by, approval_status)
        VALUES (@location_id, @item_id, @branch_id, @owner, @modules, @name, @item_type, @created_by, @approval_status)
        """
      |> Sql.parameters
        [ "location_id", Sql.uuid locationId
          "item_id", Sql.uuid itemId
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "owner", Sql.string location.owner
          "modules", Sql.string modulesStr
          "name", Sql.string location.name
          "item_type", Sql.string itemType
          "created_by",
          (match createdBy with
           | Some id -> Sql.uuid id
           | None -> Sql.string "")
          "approval_status", Sql.string approvalStatus ]
      |> Sql.executeStatementAsync
  }


/// Apply an ApproveItem op - update the approval_status of the active location
let private applyApproveItem
  (branchID : Option<PT.BranchID>)
  (itemId : uuid)
  (reviewerId : uuid)
  : Task<unit> =
  task {
    let now = NodaTime.Instant.now ()
    do!
      Sql.query
        """
        UPDATE locations
        SET approval_status = 'approved',
            reviewed_by = @reviewer_id,
            reviewed_at = @reviewed_at
        WHERE item_id = @item_id
          AND deprecated_at IS NULL
          AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid itemId
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "reviewer_id", Sql.uuid reviewerId
          "reviewed_at", Sql.instant now ]
      |> Sql.executeStatementAsync
  }


/// Apply a RejectItem op - update the approval_status of the active location
/// Note: reason is stored in request_items.status_message, not in locations
let private applyRejectItem
  (branchID : Option<PT.BranchID>)
  (itemId : uuid)
  (reviewerId : uuid)
  (_reason : string)
  : Task<unit> =
  task {
    let now = NodaTime.Instant.now ()
    do!
      Sql.query
        """
        UPDATE locations
        SET approval_status = 'rejected',
            reviewed_by = @reviewer_id,
            reviewed_at = @reviewed_at
        WHERE item_id = @item_id
          AND deprecated_at IS NULL
          AND (branch_id = @branch_id OR (branch_id IS NULL AND @branch_id IS NULL))
        """
      |> Sql.parameters
        [ "item_id", Sql.uuid itemId
          "branch_id",
          (match branchID with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "reviewer_id", Sql.uuid reviewerId
          "reviewed_at", Sql.instant now ]
      |> Sql.executeStatementAsync
  }


/// Apply a RequestNamingApproval op - create a review request with associated items
let private applyRequestNamingApproval
  (requestId : uuid)
  (createdBy : uuid)
  (targetNamespace : string)
  (locationIds : List<string>)
  (title : Option<string>)
  (description : Option<string>)
  (sourceBranchId : Option<uuid>)
  : Task<unit> =
  task {
    let now = NodaTime.Instant.now ()

    // Insert the approval request
    do!
      Sql.query
        """
        INSERT INTO approval_requests (id, created_by, created_at, status, target_namespace, source_branch_id, title, description)
        VALUES (@id, @created_by, @created_at, 'open', @target_namespace, @source_branch_id, @title, @description)
        """
      |> Sql.parameters
        [ "id", Sql.uuid requestId
          "created_by", Sql.uuid createdBy
          "created_at", Sql.instant now
          "target_namespace", Sql.string targetNamespace
          "source_branch_id",
          (match sourceBranchId with
           | Some id -> Sql.uuid id
           | None -> Sql.dbnull)
          "title",
          (match title with
           | Some t -> Sql.string t
           | None -> Sql.dbnull)
          "description",
          (match description with
           | Some d -> Sql.string d
           | None -> Sql.dbnull) ]
      |> Sql.executeStatementAsync

    // Insert request_items for each location
    for locationId in locationIds do
      do!
        Sql.query
          """
          INSERT INTO request_items (request_id, location_id, status)
          VALUES (@request_id, @location_id, 'pending')
          """
        |> Sql.parameters
          [ "request_id", Sql.uuid requestId; "location_id", Sql.string locationId ]
        |> Sql.executeStatementAsync
  }


/// Apply a WithdrawApprovalRequest op - delete the review request and its items
let private applyWithdrawApprovalRequest
  (requestId : uuid)
  (_withdrawnBy : uuid)
  : Task<unit> =
  task {
    // Delete the request (cascade will delete request_items)
    do!
      Sql.query
        """
        DELETE FROM approval_requests
        WHERE id = @id
        """
      |> Sql.parameters [ "id", Sql.uuid requestId ]
      |> Sql.executeStatementAsync
  }


/// Apply a RequestChanges op - update request_items status to changes_requested
let private applyRequestChanges
  (requestId : uuid)
  (locationId : string)
  (reviewerId : uuid)
  (comment : string)
  : Task<unit> =
  task {
    let now = NodaTime.Instant.now ()
    do!
      Sql.query
        """
        UPDATE request_items
        SET status = 'changes_requested',
            status_message = @comment,
            reviewed_by = @reviewer_id,
            reviewed_at = @reviewed_at
        WHERE request_id = @request_id
          AND location_id = @location_id
        """
      |> Sql.parameters
        [ "request_id", Sql.uuid requestId
          "location_id", Sql.string locationId
          "comment", Sql.string comment
          "reviewer_id", Sql.uuid reviewerId
          "reviewed_at", Sql.instant now ]
      |> Sql.executeStatementAsync
  }


/// Apply a single PackageOp to the projection tables
let applyOp
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (createdBy : Option<uuid>)
  (op : PT.PackageOp)
  : Task<unit> =
  task {
    match op with
    | PT.PackageOp.AddType typ -> do! applyAddType typ
    | PT.PackageOp.AddValue value -> do! applyAddValue value
    | PT.PackageOp.AddFn fn -> do! applyAddFn fn
    | PT.PackageOp.SetTypeName(id, loc) ->
      do! applySetName instanceID branchID createdBy id loc "type"
    | PT.PackageOp.SetValueName(id, loc) ->
      do! applySetName instanceID branchID createdBy id loc "value"
    | PT.PackageOp.SetFnName(id, loc) ->
      do! applySetName instanceID branchID createdBy id loc "fn"
    | PT.PackageOp.ApproveItem(itemId, opBranchId, reviewerId) ->
      // Use branchId from op if present, otherwise use the batch's branchID
      let effectiveBranchId = opBranchId |> Option.orElse branchID
      do! applyApproveItem effectiveBranchId itemId reviewerId
    | PT.PackageOp.RejectItem(itemId, opBranchId, reviewerId, reason) ->
      let effectiveBranchId = opBranchId |> Option.orElse branchID
      do! applyRejectItem effectiveBranchId itemId reviewerId reason
    | PT.PackageOp.RequestNamingApproval(requestId,
                                         opCreatedBy,
                                         targetNamespace,
                                         locationIds,
                                         title,
                                         description,
                                         sourceBranchId) ->
      do!
        applyRequestNamingApproval
          requestId
          opCreatedBy
          targetNamespace
          locationIds
          title
          description
          sourceBranchId
    | PT.PackageOp.WithdrawApprovalRequest(requestId, withdrawnBy) ->
      do! applyWithdrawApprovalRequest requestId withdrawnBy
    | PT.PackageOp.RequestChanges(requestId, locationId, reviewerId, comment) ->
      do! applyRequestChanges requestId locationId reviewerId comment
  }


/// Apply a list of PackageOps to the projection tables
let applyOps
  (instanceID : Option<PT.InstanceID>)
  (branchID : Option<PT.BranchID>)
  (createdBy : Option<uuid>)
  (ops : List<PT.PackageOp>)
  : Task<unit> =
  task {
    for op in ops do
      do! applyOp instanceID branchID createdBy op
  }
