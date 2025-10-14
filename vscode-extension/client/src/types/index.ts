import * as vscode from "vscode";

// Core data structures
export interface PackageNode {
  id: string;
  label: string;
  type: "owner" | "module" | "function" | "type" | "constant";
  children?: PackageNode[];
  collapsibleState: number; // 0 = collapsed, 1 = expanded
  contextValue?: string;
  packagePath?: string;
}

export interface PatchNode {
  id: string;
  label: string;
  type: "current" | "draft" | "incoming" | "applied" | "sync-status";
  author?: string;
  intent?: string;
  contextValue: string;
  children?: PatchNode[];
  status?: string;
  operations?: number;
  conflicts?: number;
  tests?: number;
}

export interface BranchNode {
  id: string;
  label: string;
  type: "current" | "recent" | "shared" | "actions" | "patch" | "operation" | "conflict" | "section"
    | "instance-root" | "branch-root" | "patches-root" | "instance-item" | "packages" | "branches" | "category"
    | "patch-namespace" | "patch-module" | "patch-change";
  contextValue: string;
  children?: BranchNode[];
  patchData?: {
    status?: "draft" | "ready" | "applied" | "conflicts" | "merged";
    operations?: number;
    conflicts?: number;
    tests?: number;
    intent?: string;
    author?: string;
    isFocused?: boolean;
    referenceCount?: number;
    isMergedUpstream?: boolean;
  };
  instanceData?: {
    url?: string;
    path?: string;
    status?: "connected" | "disconnected" | "syncing";
    packageCount?: number;
    branchCount?: number;
    patchCount?: number;
  };
  changeData?: {
    changeType?: "add" | "modify" | "delete" | "rename";
    targetType?: "function" | "type" | "constant";
    fullPath?: string;
  };
  changeCount?: number;  // For modules to show "(n)" badge
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
    status?: "connected" | "disconnected" | "syncing";
    packageCount?: number;
    branchCount?: number;
    patchCount?: number;
  };
}

// URL Pattern types
export type UrlMode = 'package' | 'edit' | 'draft' | 'patch' | 'history' | 'compare' | 'branch' | 'config' | 'instance';

export interface ParsedUrl {
  mode: UrlMode;
  target?: string;
  view?: string;
  context?: string;
  params?: { [key: string]: string };
}

// Content Provider interfaces
export interface ContentProvider {
  getContent(parsedUrl: ParsedUrl): string;
}

// Status bar data
export interface StatusBarData {
  branch: {
    name: string;
    active: boolean;
  };
  patch: {
    current: string;
    changes: number;
  };
  conflicts: {
    count: number;
    hasUnresolved: boolean;
  };
  sync: {
    incoming: number;
    outgoing: number;
  };
}

// Tree view item states
export type TreeItemState = "normal" | "modified" | "new" | "conflict" | "resolved";

// Command context
export interface CommandContext {
  extensionUri: vscode.Uri;
  statusBarManager: any; // TODO: Type this properly
}

// Demo data interfaces
export interface DemoScenario {
  name: string;
  description: string;
  execute: () => void;
}

export interface ValidationError {
  field: string;
  message: string;
  severity: "error" | "warning";
}

// Patch operations
export interface PatchOperation {
  type: "create" | "modify" | "delete";
  target: string;
  description: string;
  content?: string;
  diff?: string;
}

// User and authentication
export interface User {
  id: string;
  name: string;
  email: string;
  avatar?: string;
}

export interface Branch {
  id: string;
  name: string;
  intent: string;
  author: User;
  createdAt: Date;
  isActive: boolean;
}

export interface Patch {
  id: string;
  title: string;
  intent: string;
  author: User;
  operations: PatchOperation[];
  status: "draft" | "ready" | "applied" | "conflicts";
  createdAt: Date;
  updatedAt: Date;
}

// Package information
export interface PackageInfo {
  name: string;
  namespace: string;
  version: string;
  description?: string;
  functions: FunctionInfo[];
  types: TypeInfo[];
  constants: ConstantInfo[];
}

export interface FunctionInfo {
  name: string;
  signature: string;
  description?: string;
  parameters: ParameterInfo[];
  returnType: string;
  examples?: string[];
}

export interface TypeInfo {
  name: string;
  definition: string;
  description?: string;
  variants?: string[];
}

export interface ConstantInfo {
  name: string;
  type: string;
  value: string;
  description?: string;
}

export interface ParameterInfo {
  name: string;
  type: string;
  description?: string;
  optional?: boolean;
}