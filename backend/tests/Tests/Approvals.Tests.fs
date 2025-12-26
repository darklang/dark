module Tests.Approvals

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Fumble

open Prelude
open TestUtils.TestUtils
open LibDB.Db

module PT = LibExecution.ProgramTypes
module Inserts = LibPackageManager.Inserts
module Branches = LibPackageManager.Branches
module Approvals = LibPackageManager.Approvals
module Accounts = LibPackageManager.Accounts
module BinarySerialization = LibBinarySerialization.BinarySerialization


// =============================================================================
// Test Helpers
// =============================================================================

// Named parameters for insertAndApplyOps clarity
let noInstance : Option<PT.InstanceID> = None
let noBranch : Option<PT.BranchID> = None


/// Get ops inserted after a given rowid, optionally filtered by branch
let getOpsAfter
  (afterRowid : int64)
  (branchId : Option<System.Guid>)
  : Task<List<PT.PackageOp>> =
  task {
    let sql, parameters =
      match branchId with
      | Some bid ->
        "SELECT id, op_blob FROM package_ops WHERE rowid > @after AND branch_id = @branch_id ORDER BY rowid",
        [ "after", Sql.int64 afterRowid; "branch_id", Sql.uuid bid ]
      | None ->
        "SELECT id, op_blob FROM package_ops WHERE rowid > @after AND branch_id IS NULL ORDER BY rowid",
        [ "after", Sql.int64 afterRowid ]
    return!
      Sql.query sql
      |> Sql.parameters parameters
      |> Sql.executeAsync (fun read ->
        let id = read.uuid "id"
        let blob = read.bytes "op_blob"
        BinarySerialization.PT.PackageOp.deserialize id blob)
  }

/// Get current max rowid
let getMaxRowid () : Task<int64> =
  // Get the current max rowid in package_ops (to find ops inserted after this)
  // If no rows, return 0
  Sql.query "SELECT COALESCE(MAX(rowid), 0) as v FROM package_ops"
  |> Sql.executeRowAsync (fun read -> read.int64 "v")

/// Set up a test namespace with an owner
/// Returns the account UUID for use in other operations
let setupNamespace (namespace_ : string) (ownerName : string) : Task<System.Guid> =
  task {
    // Create or get the account first (this ensures it exists in accounts_v0)
    let! ownerId = Accounts.getOrCreate ownerName
    do!
      Sql.query
        """
        INSERT OR REPLACE INTO namespace_access (namespace, account_id, role, granted_by)
        VALUES (@namespace, @owner_id, 'owner', @owner_id)
        """
      |> Sql.parameters
        [ "namespace", Sql.string namespace_; "owner_id", Sql.uuid ownerId ]
      |> Sql.executeStatementAsync
    return ownerId
  }


/// Create a simple test type and return ops for it
let createTestTypeOps (owner : string) (name : string) : PT.PackageOp list =
  let typeId = System.Guid.NewGuid()
  let typ : PT.PackageType.PackageType =
    { id = typeId
      description = "Test type"
      deprecated = PT.NotDeprecated
      declaration =
        { typeParams = []; definition = PT.TypeDeclaration.Alias PT.TInt64 } }
  let location : PT.PackageLocation = { owner = owner; modules = []; name = name }
  [ PT.PackageOp.AddType typ; PT.PackageOp.SetTypeName(typeId, location) ]


/// Get the approval status of a location
let getApprovalStatus (owner : string) (name : string) : Task<Option<string>> =
  task {
    return!
      Sql.query
        """
        SELECT approval_status FROM locations
        WHERE owner = @owner AND modules = '' AND name = @name
          AND deprecated_at IS NULL
        """
      |> Sql.parameters [ "owner", Sql.string owner; "name", Sql.string name ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "approval_status")
  }


/// Get the name from a SetTypeName op location, if present
let getTypeNameFromOp (op : PT.PackageOp) : Option<string> =
  match op with
  | PT.PackageOp.SetTypeName(_, loc) -> Some loc.name
  | _ -> None


/// Map op to its type name for assertions
let opToTypeName (op : PT.PackageOp) : string =
  match op with
  | PT.PackageOp.AddType _ -> "AddType"
  | PT.PackageOp.AddFn _ -> "AddFn"
  | PT.PackageOp.AddValue _ -> "AddValue"
  | PT.PackageOp.SetTypeName _ -> "SetTypeName"
  | PT.PackageOp.SetFnName _ -> "SetFnName"
  | PT.PackageOp.SetValueName _ -> "SetValueName"
  | PT.PackageOp.ApproveItem _ -> "ApproveItem"
  | PT.PackageOp.RejectItem _ -> "RejectItem"
  | PT.PackageOp.RequestNamingApproval _ -> "RequestNamingApproval"
  | PT.PackageOp.WithdrawApprovalRequest _ -> "WithdrawApprovalRequest"
  | PT.PackageOp.RequestChanges _ -> "RequestChanges"


/// Get location_id for a type by owner and name
let getLocationId (owner : string) (name : string) : Task<Option<string>> =
  task {
    return!
      Sql.query
        """
        SELECT location_id FROM locations
        WHERE owner = @owner AND modules = '' AND name = @name
          AND deprecated_at IS NULL
        """
      |> Sql.parameters [ "owner", Sql.string owner; "name", Sql.string name ]
      |> Sql.executeRowOptionAsync (fun read -> read.string "location_id")
  }


// =============================================================================
// Tests
// =============================================================================

let testAddItemToOwnNamespaceMain =
  testTask "add item to own namespace (no branch) - auto-approves" {
    let ownerName = "TestAlice"
    let! ownerId = setupNamespace ownerName ownerName

    // Get current max rowid, to find ops after this
    let! maxRowidBefore = getMaxRowid ()

    // Add a type to own namespace
    let ops = createTestTypeOps ownerName "MyType"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some ownerId) ops

    // Check ops: AddType + SetTypeName + ApproveItem
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "AddType"; "SetTypeName"; "ApproveItem" ]
      "Own namespace (no branch) should auto-approve: AddType + SetTypeName + ApproveItem"

    // Final state should be approved
    let! status = getApprovalStatus ownerName "MyType"
    Expect.equal status (Some "approved") "Status should be approved"
  }


let testAddItemToOwnNamespaceBranch =
  testTask "add item to own namespace (branch) - auto-approves" {
    let ownerName = "TestJosh"
    let! ownerId = setupNamespace ownerName ownerName

    // Create a branch
    let! (branch : Branches.Branch) = Branches.create ownerId "test-branch"

    let! maxRowidBefore = getMaxRowid ()

    // Add a type to own namespace in branch
    let ops = createTestTypeOps ownerName "MyBranchType"
    let! _ = Inserts.insertAndApplyOps noInstance (Some branch.id) (Some ownerId) ops

    // Check ops in branch: AddType + SetTypeName + ApproveItem
    let! branchOps = getOpsAfter maxRowidBefore (Some branch.id)
    let branchOpTypes = branchOps |> List.map opToTypeName
    Expect.equal
      branchOpTypes
      [ "AddType"; "SetTypeName"; "ApproveItem" ]
      "Own namespace (branch) should auto-approve: AddType + SetTypeName + ApproveItem"

    // Check our type was NOT added to main
    let! mainOps = getOpsAfter maxRowidBefore noBranch
    let mainTypeNames = mainOps |> List.choose getTypeNameFromOp
    Expect.isFalse
      (List.contains "MyBranchType" mainTypeNames)
      "Branch ops should not appear in main"

    // Final state should be approved
    let! status = getApprovalStatus ownerName "MyBranchType"
    Expect.equal status (Some "approved") "Status should be approved"
  }


let testAddItemToOthersNamespaceMain =
  testTask "add item to others' namespace (main) - pending" {
    let namespaceOwnerName = "TestCharlie"
    let contributorName = "TestDave"
    let! _ = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    let! maxRowidBefore = getMaxRowid ()

    // Dave adds a type to Charlie's namespace
    let ops = createTestTypeOps namespaceOwnerName "ContributedType"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    // Check ops: AddType + SetTypeName only (no ApproveItem) (main only)
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "AddType"; "SetTypeName" ]
      "Others' namespace (main) should NOT auto-approve: AddType + SetTypeName only"

    // Final state should be pending
    let! status = getApprovalStatus namespaceOwnerName "ContributedType"
    Expect.equal status (Some "pending") "Status should be pending"
  }


let testAddItemToOthersNamespaceBranch =
  testTask "add item to others' namespace (branch) - pending" {
    let namespaceOwnerName = "TestEve"
    let contributorName = "TestFrank"
    let! _ = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Create a branch (owned by contributor)
    let! (branch : Branches.Branch) = Branches.create contributorId "contrib-branch"

    let! maxRowidBefore = getMaxRowid ()

    // Frank adds a type to Eve's namespace in branch
    let ops = createTestTypeOps namespaceOwnerName "BranchContributedType"
    let! _ =
      Inserts.insertAndApplyOps noInstance (Some branch.id) (Some contributorId) ops

    // Check ops in branch: AddType + SetTypeName only (no ApproveItem)
    let! branchOps = getOpsAfter maxRowidBefore (Some branch.id)
    let branchOpTypes = branchOps |> List.map opToTypeName
    Expect.equal
      branchOpTypes
      [ "AddType"; "SetTypeName" ]
      "Others' namespace (branch) should NOT auto-approve: AddType + SetTypeName only"

    // Check our type was NOT added to main
    let! mainOps = getOpsAfter maxRowidBefore noBranch
    let mainTypeNames = mainOps |> List.choose getTypeNameFromOp
    Expect.isFalse
      (List.contains "BranchContributedType" mainTypeNames)
      "Branch ops should not appear in main"

    // Final state should be pending
    let! status = getApprovalStatus namespaceOwnerName "BranchContributedType"
    Expect.equal status (Some "pending") "Status should be pending"
  }


// =============================================================================
// Approval Workflow Tests
// =============================================================================

let testCreateApprovalRequestSingleItem =
  testTask "create approval request (single item) - RequestNamingApproval op" {
    let namespaceOwnerName = "TestGrace"
    let contributorName = "TestHenry"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Henry adds a type to Grace's namespace (pending)
    let ops = createTestTypeOps namespaceOwnerName "RequestedType"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    // Get the location_id
    let! locationIdOpt = getLocationId namespaceOwnerName "RequestedType"
    let locationId =
      match locationIdOpt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location not found" []

    let! maxRowidBefore = getMaxRowid ()

    // Henry creates an approval request
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId ]
        (Some "Add RequestedType")
        None
        noBranch

    // Check ops: RequestNamingApproval
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "RequestNamingApproval" ]
      "Creating approval request should generate RequestNamingApproval op"

    // Verify request was created
    Expect.equal
      request.createdBy
      contributorId
      "Request should be created by contributor"
    Expect.equal
      request.targetNamespace
      namespaceOwnerName
      "Request should target namespace owner"

    // Verify Grace can see the incoming request
    let! (incomingRequests : List<Approvals.ApprovalRequest>) =
      Approvals.listIncomingRequests ownerId
    Expect.equal
      (List.length incomingRequests)
      1
      "Owner should have 1 incoming request"
    Expect.equal incomingRequests[0].id request.id "Request ID should match"
  }


let testCreateApprovalRequestMultipleItems =
  testTask
    "create approval request (multiple items) - single RequestNamingApproval op" {
    let namespaceOwnerName = "TestIvy"
    let contributorName = "TestJack"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Jack adds two types to Ivy's namespace (pending)
    let ops1 = createTestTypeOps namespaceOwnerName "MultiType1"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops1
    let ops2 = createTestTypeOps namespaceOwnerName "MultiType2"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops2

    // Get the location_ids
    let! locationId1Opt = getLocationId namespaceOwnerName "MultiType1"
    let! locationId2Opt = getLocationId namespaceOwnerName "MultiType2"
    let locationId1 =
      match locationId1Opt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location 1 not found" []
    let locationId2 =
      match locationId2Opt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location 2 not found" []

    let! maxRowidBefore = getMaxRowid ()

    // Jack creates an approval request with both items
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId1; locationId2 ]
        (Some "Add MultiType1 and MultiType2")
        None
        noBranch

    // Check ops: single RequestNamingApproval (contains multiple items)
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "RequestNamingApproval" ]
      "Creating multi-item request should generate single RequestNamingApproval op"

    // Verify Ivy can see the incoming request with both items
    let! (incomingRequests : List<Approvals.ApprovalRequest>) =
      Approvals.listIncomingRequests ownerId
    Expect.equal
      (List.length incomingRequests)
      1
      "Owner should have 1 incoming request (not 2)"
    Expect.equal incomingRequests[0].id request.id "Request ID should match"

    // Verify the request contains both items
    let! (items : List<Approvals.RequestItem>) = Approvals.getRequestItems request.id
    Expect.equal (List.length items) 2 "Request should contain 2 items"
  }


let testApproveRequest =
  testTask "approve request - ApproveItem op per item" {
    let namespaceOwnerName = "TestKate"
    let contributorName = "TestLiam"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Liam adds a type to Kate's namespace (pending)
    let ops = createTestTypeOps namespaceOwnerName "ApproveMe"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    let! locationIdOpt = getLocationId namespaceOwnerName "ApproveMe"
    let locationId =
      match locationIdOpt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location not found" []

    // Liam creates an approval request
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId ]
        (Some "Please approve")
        None
        noBranch

    let! maxRowidBefore = getMaxRowid ()

    // Kate approves the item
    do!
      Approvals.setLocationStatus
        (Some request.id)
        locationId
        Approvals.Approved
        ownerId

    // Check ops: ApproveItem
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal opTypes [ "ApproveItem" ] "Approving should generate ApproveItem op"

    // Status should be approved
    let! status = getApprovalStatus namespaceOwnerName "ApproveMe"
    Expect.equal status (Some "approved") "Status should be approved after approval"
  }


let testRejectRequest =
  testTask "reject request (with reason) - RejectItem op" {
    let namespaceOwnerName = "TestMia"
    let contributorName = "TestNoah"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Noah adds a type to Mia's namespace (pending)
    let ops = createTestTypeOps namespaceOwnerName "RejectMe"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    let! locationIdOpt = getLocationId namespaceOwnerName "RejectMe"
    let locationId =
      match locationIdOpt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location not found" []

    // Noah creates an approval request
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId ]
        (Some "Please approve")
        None
        noBranch

    let! maxRowidBefore = getMaxRowid ()

    // Mia rejects the item with a reason
    do!
      Approvals.setLocationStatus
        (Some request.id)
        locationId
        (Approvals.Rejected "Does not follow naming conventions")
        ownerId

    // Check ops: RejectItem
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal opTypes [ "RejectItem" ] "Rejecting should generate RejectItem op"

    // Status should be rejected
    let! status = getApprovalStatus namespaceOwnerName "RejectMe"
    Expect.equal status (Some "rejected") "Status should be rejected after rejection"
  }


let testRequestChanges =
  testTask "request changes (with feedback) - RequestChanges op" {
    let namespaceOwnerName = "TestOlivia"
    let contributorName = "TestMarcus"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Marcus adds a type to Olivia's namespace (pending)
    let ops = createTestTypeOps namespaceOwnerName "NeedsChanges"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    let! locationIdOpt = getLocationId namespaceOwnerName "NeedsChanges"
    let locationId =
      match locationIdOpt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location not found" []

    // Marcus creates an approval request
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId ]
        (Some "Please approve")
        None
        noBranch

    let! maxRowidBefore = getMaxRowid ()

    // Olivia requests changes
    do!
      Approvals.setLocationStatus
        (Some request.id)
        locationId
        (Approvals.ChangesRequested "Please add documentation")
        ownerId

    // Check ops: RequestChanges
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "RequestChanges" ]
      "Requesting changes should generate RequestChanges op"

    // Status should still be pending (not approved or rejected)
    let! status = getApprovalStatus namespaceOwnerName "NeedsChanges"
    Expect.equal
      status
      (Some "pending")
      "Status should remain pending after changes requested"
  }


let testWithdrawRequest =
  testTask "withdraw request - WithdrawApprovalRequest op" {
    let namespaceOwnerName = "TestQuinn"
    let contributorName = "TestRyan"
    let! ownerId = setupNamespace namespaceOwnerName namespaceOwnerName
    let! contributorId = Accounts.getOrCreate contributorName

    // Ryan adds a type to Quinn's namespace (pending)
    let ops = createTestTypeOps namespaceOwnerName "WithdrawMe"
    let! _ = Inserts.insertAndApplyOps noInstance noBranch (Some contributorId) ops

    let! locationIdOpt = getLocationId namespaceOwnerName "WithdrawMe"
    let locationId =
      match locationIdOpt with
      | Some id -> id
      | None -> Exception.raiseInternal "Location not found" []

    // Ryan creates an approval request
    let! (request : Approvals.ApprovalRequest) =
      Approvals.createApprovalRequest
        contributorId
        namespaceOwnerName
        [ locationId ]
        (Some "Please approve")
        None
        noBranch

    let! maxRowidBefore = getMaxRowid ()

    // Ryan withdraws the request
    let! withdraw = Approvals.withdrawRequest request.id contributorId

    // Check ops: WithdrawApprovalRequest
    let! newOps = getOpsAfter maxRowidBefore noBranch
    let opTypes = newOps |> List.map opToTypeName
    Expect.equal
      opTypes
      [ "WithdrawApprovalRequest" ]
      "Withdrawing should generate WithdrawApprovalRequest op"

    Expect.isTrue withdraw "Withdraw should succeed for request creator"

    // Item should still be pending (not deleted)
    let! status = getApprovalStatus namespaceOwnerName "WithdrawMe"
    Expect.equal
      status
      (Some "pending")
      "Item should remain pending after withdrawal"

    // Quinn should no longer see the withdrawn request
    let! (incomingRequests : List<Approvals.ApprovalRequest>) =
      Approvals.listIncomingRequests ownerId
    Expect.equal
      (List.length incomingRequests)
      0
      "Owner should have no incoming requests after withdrawal"
  }


let tests =
  testSequencedGroup
    "approvals"
    (testList
      "approvals"
      [ testAddItemToOwnNamespaceMain
        testAddItemToOwnNamespaceBranch
        testAddItemToOthersNamespaceMain
        testAddItemToOthersNamespaceBranch
        testCreateApprovalRequestSingleItem
        testCreateApprovalRequestMultipleItems
        testApproveRequest
        testRejectRequest
        testRequestChanges
        testWithdrawRequest ])
