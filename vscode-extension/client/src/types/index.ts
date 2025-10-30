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

export interface BranchNode {
  id: string;
  label: string;
  type: "current" | "recent" | "shared" | "actions" | "operation" | "conflict" | "section"
    | "instance-root" | "branch-root" | "changes-root" | "instance-item" | "packages" | "branches" | "category"
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
    dbName?: string;
    status?: "connected" | "disconnected" | "syncing";
    packageCount?: number;
    branchCount?: number;
  };
}

// URL Pattern types
export type UrlMode = 'package' | 'edit' | 'draft' | 'history' | 'compare' | 'branch' | 'config' | 'instance';

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