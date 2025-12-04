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
  SeeMore = "see-more"
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