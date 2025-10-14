import { BranchNode } from "../types";

/**
 * Represents a single change in a patch
 */
export interface PatchChange {
  fullPath: string;  // e.g., "MyApp.Database.ConnectionPool"
  changeType: "add" | "modify" | "delete" | "rename";
  targetType: "function" | "type" | "constant";
}

/**
 * Builds a package-centric tree structure from a flat list of changes
 * Returns an array of namespace nodes (no container wrapper)
 */
export function buildPackageTree(patchId: string, changes: PatchChange[]): BranchNode[] {
  // Group changes by namespace -> module -> item
  const namespaces = new Map<string, Map<string, PatchChange[]>>();

  for (const change of changes) {
    const parts = change.fullPath.split(".");
    if (parts.length < 2) continue;  // Invalid path

    const namespace = parts[0];
    const item = parts[parts.length - 1];
    const module = parts.slice(1, -1).join(".");

    if (!namespaces.has(namespace)) {
      namespaces.set(namespace, new Map());
    }
    const modules = namespaces.get(namespace)!;

    if (!modules.has(module)) {
      modules.set(module, []);
    }
    modules.get(module)!.push(change);
  }

  // Build namespace nodes
  const namespaceNodes: BranchNode[] = [];
  for (const [namespace, modules] of namespaces) {
    const moduleNodes: BranchNode[] = [];

    for (const [module, moduleChanges] of modules) {
      const changeNodes: BranchNode[] = moduleChanges.map((change, idx) => ({
        id: `${patchId}-change-${namespace}-${module}-${idx}`,
        label: change.fullPath.split(".").pop()!,
        type: "patch-change",
        contextValue: `patch-change-${change.changeType}`,
        changeData: {
          changeType: change.changeType,
          targetType: change.targetType,
          fullPath: change.fullPath
        }
      }));

      const moduleLabel = moduleChanges.length > 1
        ? `${module} (${moduleChanges.length})`
        : module;

      const fullModulePath = `${namespace}.${module}`;

      moduleNodes.push({
        id: `${patchId}-module-${namespace}-${module}`,
        label: moduleLabel,
        type: "patch-module",
        contextValue: "patch-module",
        changeCount: moduleChanges.length,
        changeData: {
          fullPath: fullModulePath  // Store full path for opening package view
        },
        children: changeNodes
      });
    }

    namespaceNodes.push({
      id: `${patchId}-namespace-${namespace}`,
      label: namespace,
      type: "patch-namespace",
      contextValue: "patch-namespace",
      children: moduleNodes
    });
  }

  // Return namespace nodes directly (no wrapper)
  return namespaceNodes;
}

/**
 * Quick helper to create a PatchChange from a simple description
 */
export function change(
  fullPath: string,
  changeType: "add" | "modify" | "delete" | "rename" = "modify",
  targetType: "function" | "type" | "constant" = "function"
): PatchChange {
  return { fullPath, changeType, targetType };
}
