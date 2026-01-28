import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode, BranchNodeType } from "../../types";
import { AccountService } from "../../services/accountService";

// Response type from LSP
interface PendingChangeResponse {
  itemId: string;
  itemType: string;
  owner: string;
  modules: string;
  name: string;
  createdAt: string;
  locationId: string;
}

export class PendingChangesTreeDataProvider
  implements vscode.TreeDataProvider<BranchNode>
{
  private _onDidChangeTreeData: vscode.EventEmitter<
    BranchNode | undefined | null | void
  > = new vscode.EventEmitter<BranchNode | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<
    BranchNode | undefined | null | void
  > = this._onDidChangeTreeData.event;

  constructor(private client: LanguageClient) {}

  refresh(): void {
    this._onDidChangeTreeData.fire(undefined);
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
  }

  setCurrentAccount(_accountId: string): void {
    // Account is managed centrally by AccountService
    // Just refresh to pick up the new account
    this.refresh();
  }

  getTreeItem(element: BranchNode): vscode.TreeItem {
    let collapsibleState: vscode.TreeItemCollapsibleState;

    // Root node is expanded
    if (element.type === BranchNodeType.PendingChangesRoot) {
      collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else {
      collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    const item = new vscode.TreeItem(element.label, collapsibleState);
    item.contextValue = element.contextValue;

    // Pending changes root
    if (element.type === BranchNodeType.PendingChangesRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "edit",
        new vscode.ThemeColor("charts.yellow"),
      );
      item.tooltip = "Your pending changes (not yet approved)";
      return item;
    }

    // Pending change item
    if (element.type === BranchNodeType.PendingChangeItem) {
      const data = element.pendingChangeData;

      // Icon based on item type
      const iconName = data?.itemType === "fn" ? "symbol-function"
        : data?.itemType === "type" ? "symbol-class"
        : "symbol-constant";

      item.iconPath = new vscode.ThemeIcon(
        iconName,
        new vscode.ThemeColor("charts.yellow"),
      );

      // Build full path for tooltip
      const fullPath = data?.modules
        ? `${data.owner}.${data.modules}.${data.name}`
        : `${data?.owner}.${data?.name}`;
      item.tooltip = `Pending: ${fullPath}\nType: ${data?.itemType}\nCreated: ${data?.createdAt}`;

      // Description shows item type
      item.description = data?.itemType;

      return item;
    }

    return item;
  }

  async getChildren(element?: BranchNode): Promise<BranchNode[]> {
    if (!element) {
      // Root level: show pending changes root
      return this.getRootNodes();
    }

    // Handle pending changes root
    if (element.type === BranchNodeType.PendingChangesRoot) {
      return await this.getPendingChanges();
    }

    return [];
  }

  private getRootNodes(): BranchNode[] {
    return [
      {
        id: "pending-changes-root",
        label: "Pending Changes",
        type: BranchNodeType.PendingChangesRoot,
        contextValue: "pending-changes-root",
        children: [],
      },
    ];
  }

  private async getPendingChanges(): Promise<BranchNode[]> {
    try {
      const pendingChanges = await this.client.sendRequest<PendingChangeResponse[]>(
        "dark/listPendingChanges",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!pendingChanges || pendingChanges.length === 0) {
        return [
          {
            id: "no-pending-changes",
            label: "No pending changes",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      return pendingChanges.map(change => ({
        id: `pending-change-${change.locationId}`,
        label: change.name,
        type: BranchNodeType.PendingChangeItem,
        contextValue: "pending-change-item",
        pendingChangeData: {
          itemId: change.itemId,
          itemType: change.itemType,
          owner: change.owner,
          modules: change.modules,
          name: change.name,
          createdAt: change.createdAt,
          locationId: change.locationId,
        },
      }));
    } catch (error) {
      console.error("Failed to get pending changes:", error);
      return [
        {
          id: "error-pending-changes",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }
}
