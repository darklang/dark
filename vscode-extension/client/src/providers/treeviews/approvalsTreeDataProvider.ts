import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchNode, BranchNodeType } from "../../types";
import { AccountService } from "../../services/accountService";

// Response types from LSP
interface PendingLocationResponse {
  locationId: string;
  namespace: string;
  modules: string[];
  name: string;
  itemType: string;
}

interface ApprovalRequestResponse {
  id: string;
  createdBy: string;
  targetNamespace: string;
  title: string | null;
  description: string | null;
  status: string;
  locationCount: number;
}

export class ApprovalsTreeDataProvider
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

    // Root nodes and namespace groups are expanded
    if (
      element.type === BranchNodeType.IncomingRequestsRoot ||
      element.type === BranchNodeType.OutgoingRequestsRoot ||
      element.type === BranchNodeType.PendingLocationsRoot ||
      element.type === BranchNodeType.NamespaceGroup
    ) {
      collapsibleState = vscode.TreeItemCollapsibleState.Expanded;
    } else if (element.children && element.children.length > 0) {
      collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
    } else {
      collapsibleState = vscode.TreeItemCollapsibleState.None;
    }

    const item = new vscode.TreeItem(element.label, collapsibleState);
    item.contextValue = element.contextValue;

    // Incoming requests root
    if (element.type === BranchNodeType.IncomingRequestsRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "inbox",
        new vscode.ThemeColor("charts.blue"),
      );
      item.tooltip = "Approval requests from others for your namespaces";
      return item;
    }

    // Outgoing requests root
    if (element.type === BranchNodeType.OutgoingRequestsRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "send",
        new vscode.ThemeColor("charts.purple"),
      );
      item.tooltip = "Your approval requests to other namespace owners";
      return item;
    }

    // Pending locations root
    if (element.type === BranchNodeType.PendingLocationsRoot) {
      item.iconPath = new vscode.ThemeIcon(
        "clock",
        new vscode.ThemeColor("charts.yellow"),
      );
      item.tooltip = "Your changes waiting for approval";
      return item;
    }

    // Namespace group
    if (element.type === BranchNodeType.NamespaceGroup) {
      item.iconPath = new vscode.ThemeIcon("folder");
      item.tooltip = `Namespace: ${element.approvalData?.namespace}`;
      const count = element.children?.length || 0;
      item.description = `${count} item${count !== 1 ? "s" : ""}`;
      return item;
    }

    // Approval request
    if (element.type === BranchNodeType.ApprovalRequest) {
      const data = element.approvalData;
      if (data?.status === "pending") {
        item.iconPath = new vscode.ThemeIcon(
          "git-pull-request",
          new vscode.ThemeColor("charts.yellow"),
        );
      } else if (data?.status === "approved") {
        item.iconPath = new vscode.ThemeIcon(
          "check",
          new vscode.ThemeColor("charts.green"),
        );
      } else if (data?.status === "rejected") {
        item.iconPath = new vscode.ThemeIcon(
          "x",
          new vscode.ThemeColor("charts.red"),
        );
      } else {
        item.iconPath = new vscode.ThemeIcon("git-pull-request");
      }
      item.tooltip = data?.description || `Request from ${data?.createdBy}`;
      item.description = data?.createdBy;

      // Make expandable to show locations
      item.collapsibleState = vscode.TreeItemCollapsibleState.Collapsed;
      return item;
    }

    // Pending location
    if (element.type === BranchNodeType.PendingLocation) {
      const data = element.approvalData;

      // Icon based on item type
      if (data?.itemType === "fn") {
        item.iconPath = new vscode.ThemeIcon("symbol-function");
      } else if (data?.itemType === "type") {
        item.iconPath = new vscode.ThemeIcon("symbol-class");
      } else if (data?.itemType === "value") {
        item.iconPath = new vscode.ThemeIcon("symbol-constant");
      } else {
        item.iconPath = new vscode.ThemeIcon("file");
      }

      item.tooltip = `${data?.namespace}.${data?.itemName} (${data?.itemType})`;
      item.description = data?.status;

      return item;
    }

    return item;
  }

  async getChildren(element?: BranchNode): Promise<BranchNode[]> {
    if (!element) {
      // Root level: show three sections
      return this.getRootNodes();
    }

    // Handle incoming requests root
    if (element.type === BranchNodeType.IncomingRequestsRoot) {
      return await this.getIncomingRequests();
    }

    // Handle outgoing requests root
    if (element.type === BranchNodeType.OutgoingRequestsRoot) {
      return await this.getOutgoingRequests();
    }

    // Handle pending locations root
    if (element.type === BranchNodeType.PendingLocationsRoot) {
      return await this.getPendingLocations();
    }

    // Handle namespace group - return children
    if (element.type === BranchNodeType.NamespaceGroup) {
      return element.children || [];
    }

    // Handle approval request - fetch locations
    if (
      element.type === BranchNodeType.ApprovalRequest &&
      element.approvalData?.requestId
    ) {
      return await this.getRequestLocations(element.approvalData.requestId);
    }

    return element.children || [];
  }

  private getRootNodes(): BranchNode[] {
    return [
      {
        id: "incoming-requests",
        label: "Incoming Requests",
        type: BranchNodeType.IncomingRequestsRoot,
        contextValue: "incoming-requests-root",
        children: [],
      },
      {
        id: "outgoing-requests",
        label: "Outgoing Requests",
        type: BranchNodeType.OutgoingRequestsRoot,
        contextValue: "outgoing-requests-root",
        children: [],
      },
      {
        id: "pending-locations",
        label: "My Pending Changes",
        type: BranchNodeType.PendingLocationsRoot,
        contextValue: "pending-locations-root",
        children: [],
      },
    ];
  }

  private async getIncomingRequests(): Promise<BranchNode[]> {
    try {
      // Get formal approval requests (not raw pending locations)
      const requests = await this.client.sendRequest<ApprovalRequestResponse[]>(
        "dark/listIncomingApprovalRequests",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!requests || requests.length === 0) {
        return [
          {
            id: "no-incoming",
            label: "No incoming approval requests",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      return requests.map(req => ({
        id: `incoming-${req.id}`,
        label: req.title || `Request for ${req.targetNamespace}`,
        type: BranchNodeType.ApprovalRequest,
        contextValue: "approval-request-incoming",
        approvalData: {
          requestId: req.id,
          namespace: req.targetNamespace,
          status: req.status as
            | "pending"
            | "approved"
            | "rejected"
            | "changes_requested",
          createdBy: req.createdBy,
          title: req.title || undefined,
          description: req.description || undefined,
        },
      }));
    } catch (error) {
      console.error("Failed to get incoming requests:", error);
      return [
        {
          id: "error-incoming",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }

  private async getOutgoingRequests(): Promise<BranchNode[]> {
    try {
      const requests = await this.client.sendRequest<ApprovalRequestResponse[]>(
        "dark/listOutgoingApprovalRequests",
        { accountID: AccountService.getCurrentAccountId() },
      );

      if (!requests || requests.length === 0) {
        return [
          {
            id: "no-outgoing",
            label: "No outgoing requests",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      return requests.map(req => ({
        id: `outgoing-${req.id}`,
        label: req.title || `Request for ${req.targetNamespace}`,
        type: BranchNodeType.ApprovalRequest,
        contextValue: "approval-request-outgoing",
        approvalData: {
          requestId: req.id,
          namespace: req.targetNamespace,
          status: req.status as
            | "pending"
            | "approved"
            | "rejected"
            | "changes_requested",
          createdBy: req.createdBy,
          title: req.title || undefined,
          description: req.description || undefined,
        },
      }));
    } catch (error) {
      console.error("Failed to get outgoing requests:", error);
      return [
        {
          id: "error-outgoing",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }

  private async getPendingLocations(): Promise<BranchNode[]> {
    try {
      const locations = await this.client.sendRequest<
        PendingLocationResponse[]
      >("dark/listPendingLocations", {
        accountID: AccountService.getCurrentAccountId(),
      });

      if (!locations || locations.length === 0) {
        return [
          {
            id: "no-pending",
            label: "No pending changes",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      // Group by namespace
      const byNamespace = new Map<string, PendingLocationResponse[]>();
      for (const loc of locations) {
        const ns = loc.namespace;
        if (!byNamespace.has(ns)) {
          byNamespace.set(ns, []);
        }
        byNamespace.get(ns)!.push(loc);
      }

      // Create namespace group nodes
      const groups: BranchNode[] = [];
      for (const [namespace, locs] of byNamespace.entries()) {
        const children: BranchNode[] = locs.map(loc => ({
          id: `pending-loc-${loc.locationId}`,
          label: loc.name,
          type: BranchNodeType.PendingLocation,
          contextValue: "pending-location",
          approvalData: {
            locationId: loc.locationId,
            namespace: loc.namespace,
            itemType: loc.itemType as "type" | "fn" | "value",
            itemName: loc.name,
            status: "pending",
          },
        }));

        groups.push({
          id: `ns-group-${namespace}`,
          label: namespace,
          type: BranchNodeType.NamespaceGroup,
          contextValue: "namespace-group",
          children,
          approvalData: { namespace },
        });
      }

      return groups;
    } catch (error) {
      console.error("Failed to get pending locations:", error);
      return [
        {
          id: "error-pending",
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }

  private async getRequestLocations(requestId: string): Promise<BranchNode[]> {
    try {
      const details = await this.client.sendRequest<{
        id: string;
        createdBy: string;
        targetNamespace: string;
        title: string | null;
        description: string | null;
        status: string;
        locations: Array<{
          locationId: string;
          name: string;
          itemType: string;
          status: string;
        }>;
      }>("dark/getApprovalRequestDetails", { requestId });

      if (!details || !details.locations || details.locations.length === 0) {
        return [
          {
            id: `no-locations-${requestId}`,
            label: "No locations",
            type: BranchNodeType.SeeMore,
            contextValue: "empty-message",
          },
        ];
      }

      return details.locations.map(loc => ({
        id: `request-loc-${requestId}-${loc.locationId}`,
        label: loc.name,
        type: BranchNodeType.PendingLocation,
        contextValue: "request-location",
        approvalData: {
          locationId: loc.locationId,
          requestId: requestId,
          namespace: details.targetNamespace,
          itemType: loc.itemType as "type" | "fn" | "value",
          itemName: loc.name,
          status: loc.status as
            | "pending"
            | "approved"
            | "rejected"
            | "changes_requested",
        },
      }));
    } catch (error) {
      console.error("Failed to get request locations:", error);
      return [
        {
          id: `error-locations-${requestId}`,
          label: "Failed to load",
          type: BranchNodeType.SeeMore,
          contextValue: "error-message",
        },
      ];
    }
  }
}
