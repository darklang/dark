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
