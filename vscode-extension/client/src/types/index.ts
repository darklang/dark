// Core data structures
export interface PackageNode {
  id: string;
  label: string;
  type: "owner" | "module" | "function" | "type" | "value";
  children?: PackageNode[];
  collapsibleState: number; // 0 = collapsed, 1 = expanded
  contextValue?: string;
  packagePath?: string;
}

export enum BranchNodeType {
  PendingOp = "pending-op",
  OwnerGroup = "owner-group",
  BranchItem = "branch-item",
  BranchAction = "branch-action",
  InstanceItem = "instance-item",
  InstanceRoot = "instance-root",
  BranchRoot = "branch-root",
  ChangesRoot = "changes-root",
  SeeMore = "see-more",
  // Approval system node types
  ApprovalsRoot = "approvals-root",
  IncomingRequestsRoot = "incoming-requests-root",
  OutgoingRequestsRoot = "outgoing-requests-root",
  PendingLocationsRoot = "pending-locations-root",
  ApprovalRequest = "approval-request",
  PendingLocation = "pending-location",
  NamespaceGroup = "namespace-group",
  // Todos/notifications node types
  TodosRoot = "todos-root",
  TodoItem = "todo-item",
  // Dependency Changes sections
  BreakingChangesSection = "breaking-changes-section",
  AvailableUpdatesSection = "available-updates-section",
  AppliedUpdatesSection = "applied-updates-section",
  AvailableUpdateItem = "available-update-item",
  AppliedUpdateItem = "applied-update-item",
  ChangedItemGroup = "changed-item-group",
  // Pending changes (user's uncommitted edits)
  PendingChangesRoot = "pending-changes-root",
  PendingChangeItem = "pending-change-item"
}

export interface BranchNode {
  id: string;
  label: string;
  type: BranchNodeType;
  contextValue: string;
  children?: BranchNode[];
  instanceData?: {
    url?: string;
    path?: string;
    dbName?: string;
    status?: "connected" | "disconnected" | "syncing";
    packageCount?: number;
    branchCount?: number;
  };
  branchData?: {
    branchId: string;
    branchName: string;
    isCurrent?: boolean;
    isMain?: boolean;
    status?: "up-to-date" | "ahead" | "behind";
    opsCount?: number;
    lastSynced?: Date;
  };
  // Approval system data
  approvalData?: {
    requestId?: string;
    locationId?: string;
    namespace?: string;
    status?: "pending" | "approved" | "rejected" | "changes_requested";
    createdBy?: string;
    title?: string;
    description?: string;
    itemType?: "type" | "fn" | "value";
    itemName?: string;
  };
  // Todo/notification data
  todoData?: {
    todoId?: string;
    editId?: string;
    oldItemId?: string;
    newItemId?: string;
    dependentItemId?: string;
    isBreaking?: boolean;
    itemName?: string;
    itemType?: "type" | "fn" | "value";
    namespace?: string;
    modules?: string[];
    changedBy?: string;
    createdAt?: string;
    // For available/applied updates
    changeType?: "breaking" | "compatible";
    appliedAt?: string;
    isChecked?: boolean; // For checkbox state in available updates
    changedItemName?: string; // Name of the item that was changed (parent)
    // Counts for grouped updates
    fnCount?: number;
    typeCount?: number;
    valueCount?: number;
    todoIds?: string[]; // All todo IDs in this group (for batch operations)
  };
  // Pending change data (user's uncommitted edits)
  pendingChangeData?: {
    itemId?: string;
    itemType?: string;
    owner?: string;
    modules?: string;
    name?: string;
    createdAt?: string;
    locationId?: string;
  };
}


export interface InstanceNode {
  id: string;
  label: string;
  type: "current" | "remote" | "local" | "category" | "packages" | "branches" | "patches";
  contextValue: string;
  children?: InstanceNode[];
  instanceData?: {
    url?: string;
    path?: string;
    dbName?: string;
    status?: "connected" | "disconnected" | "syncing";
    packageCount?: number;
    branchCount?: number;
  };
}