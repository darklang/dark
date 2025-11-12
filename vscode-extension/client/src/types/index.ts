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

export interface BranchNode {
  id: string;
  label: string;
  type:
    "current"
    | "recent"
    | "shared"
    | "actions"
    | "operation"
    | "conflict"
    | "section"
    | "instance-root"
    | "branch-root"
    | "changes-root"
    | "instance-item"
    | "packages"
    | "branches"
    | "category"
    | "pending-op";
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