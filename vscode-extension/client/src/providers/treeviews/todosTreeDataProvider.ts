import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode, BranchNodeType } from "../../types";
import { AccountService } from "../../services/accountService";

// Response types from LSP
interface TodoResponse {
  todoId: string;
  editId: string;
  oldItemId: string;
  newItemId: string;
  dependentItemId: string;
  isBreaking: boolean;
  itemName: string;
  changedItemName: string;
  itemType: string;
  namespace: string;
  modules: string[];
  changedBy: string;
  createdAt: string;
}

// Grouped response for available/applied updates
// Now returns one item per changed function with counts of affected dependents
interface GroupedUpdateResponse {
  oldItemId: string;
  newItemId: string;
  changeType: string;
  changedItemName: string;
  fnCount: number;
  typeCount: number;
  valueCount: number;
  todoIds: string[];
  createdAt: string;
  appliedAt?: string; // Only for applied updates
}

export class TodosTreeDataProvider
  implements vscode.TreeDataProvider<BranchNode>
{
  private _onDidChangeTreeData: vscode.EventEmitter<
    BranchNode | undefined | null | void
  > = new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<
    BranchNode | undefined | null | void
  > = this._onDidChangeTreeData.event;

  // Track checked state for available update groups (oldItemId -> checked)
  private checkedGroups: Set<string> = new Set();
  // Track all available group IDs (oldItemId) to clean up checked set
  private availableGroupIds: Set<string> = new Set();
  // Map from oldItemId to list of todoIds for batch operations
  private groupTodoIds: Map<string, string[]> = new Map();

  constructor(private client: LanguageClient) {}

  refresh(): void {
    this._onDidChangeTreeData.fire(undefined);
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
  }

  setCurrentAccount(_accountId: string): void {
    // Account is managed centrally by AccountService
    // Clear selection state when account changes
    this.checkedGroups.clear();
    this.availableGroupIds.clear();
    this.groupTodoIds.clear();
    this.refresh();
  }

  /** Handle checkbox state changes from the tree view */
  handleCheckboxChange(
    items: ReadonlyArray<[BranchNode, vscode.TreeItemCheckboxState]>,
  ): void {
    for (const [node, state] of items) {
      // Handle "Available Updates" section header - select/deselect all
      if (node.type === BranchNodeType.AvailableUpdatesSection) {
        if (state === vscode.TreeItemCheckboxState.Checked) {
          // Select all groups
          for (const id of this.availableGroupIds) {
            this.checkedGroups.add(id);
          }
        } else {
          // Deselect all
          this.checkedGroups.clear();
        }
        this.refresh();
        return;
      }

      // Handle group items (ChangedItemGroup with available updates)
      if (node.type === BranchNodeType.ChangedItemGroup) {
        const groupId = node.todoData?.oldItemId;
        if (!groupId) continue;

        if (state === vscode.TreeItemCheckboxState.Checked) {
          this.checkedGroups.add(groupId);
        } else {
          this.checkedGroups.delete(groupId);
        }
      }
    }

    // Refresh to update parent checkbox state
    this.refresh();
  }

  // Get list of all todo IDs from checked groups for applying
  getCheckedUpdates(): string[] {
    const allTodoIds: string[] = [];
    for (const groupId of this.checkedGroups) {
      const todoIds = this.groupTodoIds.get(groupId);
      if (todoIds) {
        allTodoIds.push(...todoIds);
      }
    }
    return allTodoIds;
  }

  // Select all available updates
  selectAllUpdates(): void {
    this.checkedGroups = new Set(this.availableGroupIds);
    this.refresh();
  }

  // Deselect all updates
  deselectAllUpdates(): void {
    this.checkedGroups.clear();
    this.refresh();
  }

  getTreeItem(element: BranchNode): vscode.TreeItem {
    let collapsibleState: vscode.TreeItemCollapsibleState;

    // Root and section nodes are expanded
    // ChangedItemGroup for breaking changes has children, others don't
    if (
      element.type === BranchNodeType.TodosRoot ||
      element.type === BranchNodeType.BreakingChangesSection ||
      element.type === BranchNodeType.AvailableUpdatesSection ||
      element.type === BranchNodeType.AppliedUpdatesSection
    ) {
      collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else if (
      element.type === BranchNodeType.ChangedItemGroup &&
      element.children &&
      element.children.length > 0
    ) {
      // Breaking changes group has children (individual dependents)
      collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else {
      collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    const item = new vscode.TreeItem(element.label, collapsibleState);
    item.contextValue = element.contextValue;

    // Root node
    if (element.type === BranchNodeType.TodosRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "bell",
        new vscode.ThemeColor("charts.blue"),
      );
      item.tooltip = "Dependency changes affecting your code";
      return item;
    }

    // Breaking Changes section header
    if (element.type === BranchNodeType.BreakingChangesSection) {
      item.iconPath = new vscode.ThemeIcon(
        "warning",
        new vscode.ThemeColor("charts.orange"),
      );
      item.tooltip = "Breaking changes that require manual fixes";
      return item;
    }

    // Available Updates section header (with checkbox to select/deselect all)
    if (element.type === BranchNodeType.AvailableUpdatesSection) {
      item.iconPath = new vscode.ThemeIcon(
        "cloud-download",
        new vscode.ThemeColor("charts.blue"),
      );
      item.tooltip =
        "Compatible updates that can be automatically applied. Check items to include in update.";

      // Show checkbox state based on selection (all, some, none)
      const checkedCount = this.checkedGroups.size;
      const totalCount = this.availableGroupIds.size;

      if (totalCount > 0) {
        if (checkedCount === totalCount) {
          item.checkboxState = vscode.TreeItemCheckboxState.Checked;
        } else if (checkedCount > 0) {
          // Partial selection - show as unchecked (VSCode doesn't have indeterminate)
          item.checkboxState = vscode.TreeItemCheckboxState.Unchecked;
        } else {
          item.checkboxState = vscode.TreeItemCheckboxState.Unchecked;
        }
      }

      return item;
    }

    // Applied Updates section header
    if (element.type === BranchNodeType.AppliedUpdatesSection) {
      item.iconPath = new vscode.ThemeIcon(
        "check-all",
        new vscode.ThemeColor("charts.green"),
      );
      item.tooltip = "Recently applied updates that can be reverted";
      return item;
    }

    // Changed item group (now a leaf node showing counts)
    if (element.type === BranchNodeType.ChangedItemGroup) {
      const data = element.todoData;
      const fnCount = data?.fnCount ?? 0;
      const typeCount = data?.typeCount ?? 0;
      const valueCount = data?.valueCount ?? 0;
      const totalCount = fnCount + typeCount + valueCount;

      // Build counts description
      const counts: string[] = [];
      if (fnCount > 0) counts.push(`${fnCount} fn${fnCount !== 1 ? "s" : ""}`);
      if (typeCount > 0) counts.push(`${typeCount} type${typeCount !== 1 ? "s" : ""}`);
      if (valueCount > 0) counts.push(`${valueCount} value${valueCount !== 1 ? "s" : ""}`);
      const countsStr = counts.join(", ");

      item.iconPath = new vscode.ThemeIcon(
        "symbol-function",
        new vscode.ThemeColor("charts.purple"),
      );
      item.tooltip = `${element.label} was updated\nAffects: ${countsStr}`;
      item.description = countsStr || `${totalCount} item${totalCount !== 1 ? "s" : ""}`;

      // For available updates section, show checkbox
      if (element.contextValue === "available-update-group") {
        const groupId = data?.oldItemId;
        const isChecked = groupId ? this.checkedGroups.has(groupId) : false;
        item.checkboxState = isChecked
          ? vscode.TreeItemCheckboxState.Checked
          : vscode.TreeItemCheckboxState.Unchecked;
      }

      return item;
    }

    // Breaking change todo item - clicking opens the definition
    if (element.type === BranchNodeType.TodoItem) {
      const data = element.todoData;
      item.iconPath = new vscode.ThemeIcon(
        "warning",
        new vscode.ThemeColor("charts.orange"),
      );
      item.tooltip = `Breaking change: ${data?.itemName}\nRequires manual update`;
      item.description = "breaking";

      // Click to open the definition
      item.command = {
        command: "darklang.todos.viewItem",
        title: "View Item",
        arguments: [element],
      };
      return item;
    }

    // Available update item (with checkbox)
    if (element.type === BranchNodeType.AvailableUpdateItem) {
      const data = element.todoData;
      const isChecked = data?.todoId
        ? this.checkedUpdates.has(data.todoId)
        : false;

      item.iconPath = new vscode.ThemeIcon(
        "arrow-circle-up",
        new vscode.ThemeColor("charts.blue"),
      );
      item.tooltip = `Compatible update available for: ${data?.itemName}\nCheck to include in update`;
      item.description = isChecked ? "selected" : "";

      // Checkbox state
      item.checkboxState = isChecked
        ? vscode.TreeItemCheckboxState.Checked
        : vscode.TreeItemCheckboxState.Unchecked;

      // Click to open the definition
      item.command = {
        command: "darklang.todos.viewItem",
        title: "View Item",
        arguments: [element],
      };
      return item;
    }

    // Applied update item - clicking opens the definition
    if (element.type === BranchNodeType.AppliedUpdateItem) {
      const data = element.todoData;
      item.iconPath = new vscode.ThemeIcon(
        "check",
        new vscode.ThemeColor("charts.green"),
      );
      item.tooltip = `Applied: ${data?.itemName}\nApplied at: ${data?.appliedAt || "unknown"}`;
      item.description = "applied";

      // Click to open the definition
      item.command = {
        command: "darklang.todos.viewItem",
        title: "View Item",
        arguments: [element],
      };
      return item;
    }

    return item;
  }

  async getChildren(element?: BranchNode): Promise<BranchNode[]> {
    if (!element) {
      // Root level: show todos root
      return this.getRootNodes();
    }

    // Handle todos root - show sections
    if (element.type === BranchNodeType.TodosRoot) {
      return await this.getSections();
    }

    // Handle Breaking Changes section
    if (element.type === BranchNodeType.BreakingChangesSection) {
      return await this.getBreakingTodos();
    }

    // Handle Available Updates section
    if (element.type === BranchNodeType.AvailableUpdatesSection) {
      return await this.getAvailableUpdates();
    }

    // Handle Applied Updates section
    if (element.type === BranchNodeType.AppliedUpdatesSection) {
      return await this.getAppliedUpdates();
    }

    // Handle ChangedItemGroup - return its children
    if (element.type === BranchNodeType.ChangedItemGroup) {
      return element.children ?? [];
    }

    return [];
  }

  private getRootNodes(): BranchNode[] {
    return [
      {
        id: "todos-root",
        label: "Dependency Changes",
        type: BranchNodeType.TodosRoot,
        contextValue: "todos-root",
        children: [],
      },
    ];
  }

  private async getSections(): Promise<BranchNode[]> {
    const sections: BranchNode[] = [];

    // Breaking Changes section
    sections.push({
      id: "breaking-changes-section",
      label: "Breaking Changes",
      type: BranchNodeType.BreakingChangesSection,
      contextValue: "breaking-changes-section",
      children: [],
    });

    // Available Updates section
    sections.push({
      id: "available-updates-section",
      label: "Available Updates",
      type: BranchNodeType.AvailableUpdatesSection,
      contextValue: "available-updates-section",
      children: [],
      todoData: {
        todoId: "section-available-updates", // Dummy ID for checkbox handling
      },
    });

    // Applied Updates section
    sections.push({
      id: "applied-updates-section",
      label: "Applied Updates",
      type: BranchNodeType.AppliedUpdatesSection,
      contextValue: "applied-updates-section",
      children: [],
    });

    return sections;
  }

  private async getBreakingTodos(): Promise<BranchNode[]> {
    try {
      const todos = await this.client.sendRequest<TodoResponse[]>(
        "dark/listPendingBreakingTodos",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!todos || todos.length === 0) {
        return [
          {
            id: "no-breaking-todos",
            label: "No breaking changes",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      // Group todos by oldItemId (the changed item)
      const groupedByChangedItem = new Map<
        string,
        { changedItemName: string; todos: TodoResponse[] }
      >();

      for (const todo of todos) {
        const key = todo.oldItemId;
        if (!groupedByChangedItem.has(key)) {
          groupedByChangedItem.set(key, {
            changedItemName: todo.changedItemName || todo.oldItemId,
            todos: [],
          });
        }
        groupedByChangedItem.get(key)!.todos.push(todo);
      }

      // Create group nodes with children
      const groupNodes: BranchNode[] = [];
      for (const [oldItemId, group] of groupedByChangedItem) {
        const children: BranchNode[] = group.todos.map(todo => ({
          id: `todo-${todo.todoId}`,
          label: todo.itemName,
          type: BranchNodeType.TodoItem,
          contextValue: "todo-breaking",
          todoData: {
            todoId: todo.todoId,
            editId: todo.editId,
            oldItemId: todo.oldItemId,
            newItemId: todo.newItemId,
            dependentItemId: todo.dependentItemId,
            isBreaking: true,
            itemName: todo.itemName,
            changedItemName: todo.changedItemName,
            itemType: todo.itemType as "type" | "fn" | "value",
            namespace: todo.namespace,
            modules: todo.modules,
            changedBy: todo.changedBy,
            createdAt: todo.createdAt,
            changeType: "breaking",
          },
        }));

        groupNodes.push({
          id: `breaking-changed-item-${oldItemId}`,
          label: group.changedItemName,
          type: BranchNodeType.ChangedItemGroup,
          contextValue: "changed-item-group",
          children,
        });
      }

      return groupNodes;
    } catch (error) {
      console.error("Failed to get breaking todos:", error);
      return [
        {
          id: "error-breaking-todos",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }

  private async getAvailableUpdates(): Promise<BranchNode[]> {
    try {
      // Response is now pre-grouped by changed item with counts
      const groups = await this.client.sendRequest<GroupedUpdateResponse[]>(
        "dark/listAvailableUpdates",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!groups || groups.length === 0) {
        this.availableGroupIds.clear();
        this.checkedGroups.clear();
        this.groupTodoIds.clear();
        return [
          {
            id: "no-available-updates",
            label: "No available updates",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      // Track available group IDs (by oldItemId)
      const newGroupIds = new Set(groups.map(g => g.oldItemId));

      // Add new groups as checked by default
      for (const group of groups) {
        if (
          !this.availableGroupIds.has(group.oldItemId) &&
          !this.checkedGroups.has(group.oldItemId)
        ) {
          // New group - add to checked set (default to checked)
          this.checkedGroups.add(group.oldItemId);
        }
        // Store todoIds mapping for batch operations
        this.groupTodoIds.set(group.oldItemId, group.todoIds);
      }

      // Clean up: remove checked IDs that are no longer available
      for (const id of this.checkedGroups) {
        if (!newGroupIds.has(id)) {
          this.checkedGroups.delete(id);
        }
      }

      this.availableGroupIds = newGroupIds;

      // Create group nodes (no children - groups are leaf nodes now showing counts)
      const groupNodes: BranchNode[] = groups.map(group => ({
        id: `changed-item-${group.oldItemId}`,
        label: group.changedItemName,
        type: BranchNodeType.ChangedItemGroup,
        contextValue: "available-update-group",
        todoData: {
          oldItemId: group.oldItemId,
          newItemId: group.newItemId,
          changedItemName: group.changedItemName,
          fnCount: group.fnCount,
          typeCount: group.typeCount,
          valueCount: group.valueCount,
          todoIds: group.todoIds,
          createdAt: group.createdAt,
          changeType: "compatible",
        },
      }));

      return groupNodes;
    } catch (error) {
      console.error("Failed to get available updates:", error);
      return [
        {
          id: "error-available-updates",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }

  private async getAppliedUpdates(): Promise<BranchNode[]> {
    try {
      // Response is now pre-grouped by changed item with counts
      const groups = await this.client.sendRequest<GroupedUpdateResponse[]>(
        "dark/listAppliedUpdates",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!groups || groups.length === 0) {
        return [
          {
            id: "no-applied-updates",
            label: "No applied updates",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      // Create group nodes (no children - groups are leaf nodes now showing counts)
      const groupNodes: BranchNode[] = groups.map(group => ({
        id: `applied-changed-item-${group.oldItemId}`,
        label: group.changedItemName,
        type: BranchNodeType.ChangedItemGroup,
        contextValue: "applied-update-group",
        todoData: {
          oldItemId: group.oldItemId,
          newItemId: group.newItemId,
          changedItemName: group.changedItemName,
          fnCount: group.fnCount,
          typeCount: group.typeCount,
          valueCount: group.valueCount,
          todoIds: group.todoIds,
          createdAt: group.createdAt,
          appliedAt: group.appliedAt,
          changeType: "compatible",
        },
      }));

      return groupNodes;
    } catch (error) {
      console.error("Failed to get applied updates:", error);
      return [
        {
          id: "error-applied-updates",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }
}
