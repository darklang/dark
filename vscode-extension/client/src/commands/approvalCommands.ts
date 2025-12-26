import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { ApprovalsTreeDataProvider } from "../providers/treeviews/approvalsTreeDataProvider";
import { PackagesTreeDataProvider } from "../providers/treeviews/packagesTreeDataProvider";
import { HomepagePanel } from "../panels/homepage/homepagePanel";
import {
  ApprovalRequestPanel,
  ApprovalItem,
} from "../panels/approvalRequestPanel";
import { BranchNode } from "../types";
import { PinnedItemsService } from "../services/pinnedItemsService";
import { BranchStateManager } from "../data/branchStateManager";
import { AccountService } from "../services/accountService";

export class ApprovalCommands {
  private client: LanguageClient | undefined;
  private approvalsProvider: ApprovalsTreeDataProvider | undefined;
  private packagesProvider: PackagesTreeDataProvider | undefined;
  private accountChangeDisposable: vscode.Disposable | undefined;

  setClient(client: LanguageClient): void {
    this.client = client;
    // Send the default account to the LSP server on initialization
    this.syncAccountToLsp();

    // Subscribe to account changes
    this.accountChangeDisposable = AccountService.onDidChange(accountID => {
      this.syncAccountToLsp();
      this.approvalsProvider?.setCurrentAccount(accountID);
    });
  }

  /** Sync the current account ID to the LSP server */
  private async syncAccountToLsp(): Promise<void> {
    if (!this.client) return;
    const accountID = AccountService.getCurrentAccountId();
    try {
      await this.client.sendRequest("dark/setCurrentAccount", {
        accountID,
      });
    } catch (error) {
      console.error("[ApprovalCommands] Failed to sync account to LSP:", error);
    }
  }

  setApprovalsProvider(provider: ApprovalsTreeDataProvider): void {
    this.approvalsProvider = provider;
  }

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  /** Get the current account ID */
  getCurrentAccountId(): string {
    return AccountService.getCurrentAccountId();
  }

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.approvals.refresh", () =>
        this.refreshApprovals(),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.viewRequest",
        (node: BranchNode) => this.viewRequest(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.approve",
        (node: BranchNode) => this.approveRequest(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.reject",
        (node: BranchNode) => this.rejectRequest(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.requestChanges",
        (node: BranchNode) => this.requestChanges(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.withdraw",
        (node: BranchNode) => this.withdrawRequest(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.createRequest",
        (node?: BranchNode) => this.createRequest(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.quickSubmit",
        (node: BranchNode) => this.quickSubmit(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.quickApprove",
        (node: BranchNode) => this.quickApprove(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.quickReject",
        (node: BranchNode) => this.quickReject(node),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.approveSelected",
        () => this.approveSelected(),
      ),
      vscode.commands.registerCommand("darklang.approvals.rejectSelected", () =>
        this.rejectSelected(),
      ),
      vscode.commands.registerCommand("darklang.approvals.switchAccount", () =>
        this.switchAccount(),
      ),
      vscode.commands.registerCommand(
        "darklang.approvals.setAccount",
        (accountID: string) => this.setAccount(accountID),
      ),
    ];
  }

  private refreshApprovals(): void {
    this.approvalsProvider?.refresh();
  }

  private async viewRequest(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.requestId) {
      vscode.window.showErrorMessage("No request ID found");
      return;
    }

    try {
      // Get request details from LSP
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
      }>("dark/getApprovalRequestDetails", { requestId: data.requestId });

      // Show info in a quick pick or output channel
      const items = details.locations.map(loc => ({
        label: `${loc.itemType}: ${loc.name}`,
        description: loc.status,
        locationId: loc.locationId,
      }));

      const title = details.title || `Request for ${details.targetNamespace}`;
      const description = details.description || `From: ${details.createdBy}`;

      vscode.window.showQuickPick(items, {
        title: `${title} - ${description}`,
        placeHolder: `Status: ${details.status} | ${items.length} location(s)`,
      });
    } catch (error) {
      console.error("Failed to get request details:", error);
      vscode.window.showErrorMessage("Failed to load request details");
    }
  }

  private async approveRequest(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;

    if (!data?.requestId) {
      vscode.window.showErrorMessage("No request ID found");
      return;
    }

    const confirm = await vscode.window.showWarningMessage(
      `Approve all locations in this request?`,
      { modal: true },
      "Approve",
    );

    if (confirm !== "Approve") {
      return;
    }

    try {
      await this.client.sendRequest("dark/approveRequest", {
        requestId: data.requestId,
        reviewerId: AccountService.getCurrentAccountId(),
      });

      vscode.window.showInformationMessage("Request approved successfully");
      this.approvalsProvider?.refresh();
      this.packagesProvider?.refresh();
    } catch (error) {
      console.error("Failed to approve request:", error);
      vscode.window.showErrorMessage("Failed to approve request");
    }
  }

  private async rejectRequest(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;

    if (!data?.requestId) {
      vscode.window.showErrorMessage("No request ID found");
      return;
    }

    const reason = await vscode.window.showInputBox({
      prompt: "Enter rejection reason",
      placeHolder: "Reason for rejecting...",
    });

    if (reason === undefined) {
      return; // Cancelled
    }

    try {
      await this.client.sendRequest("dark/rejectRequest", {
        requestId: data.requestId,
        reviewerId: AccountService.getCurrentAccountId(),
        reason: reason || "No reason provided",
      });

      vscode.window.showInformationMessage("Request rejected");
      this.approvalsProvider?.refresh();
    } catch (error) {
      console.error("Failed to reject request:", error);
      vscode.window.showErrorMessage("Failed to reject request");
    }
  }

  private async requestChanges(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.requestId) {
      vscode.window.showErrorMessage("No request ID found");
      return;
    }

    const comment = await vscode.window.showInputBox({
      prompt: "What changes are needed?",
      placeHolder: "Describe the requested changes...",
    });

    if (!comment) {
      return; // Cancelled or empty
    }

    try {
      await this.client.sendRequest("dark/requestChangesForRequest", {
        requestId: data.requestId,
        reviewerId: AccountService.getCurrentAccountId(),
        comment,
      });

      vscode.window.showInformationMessage("Changes requested");
      this.approvalsProvider?.refresh();
    } catch (error) {
      console.error("Failed to request changes:", error);
      vscode.window.showErrorMessage("Failed to request changes");
    }
  }

  private async withdrawRequest(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.requestId) {
      vscode.window.showErrorMessage("No request ID found");
      return;
    }

    const confirm = await vscode.window.showWarningMessage(
      "Withdraw this approval request?",
      { modal: true },
      "Withdraw",
    );

    if (confirm !== "Withdraw") {
      return;
    }

    try {
      await this.client.sendRequest("dark/withdrawApprovalRequest", {
        requestId: data.requestId,
        submitterId: AccountService.getCurrentAccountId(),
      });

      vscode.window.showInformationMessage("Request withdrawn");
      this.approvalsProvider?.refresh();
    } catch (error) {
      console.error("Failed to withdraw request:", error);
      vscode.window.showErrorMessage("Failed to withdraw request");
    }
  }

  /** Quick submit a single pending item for approval (no prompts) */
  private async quickSubmit(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.locationId || !data?.namespace) {
      vscode.window.showErrorMessage("No location data found");
      return;
    }

    try {
      // Check if user has access to this namespace
      const accessCheck = (await this.client.sendRequest(
        "dark/checkNamespaceAccess",
        {
          accountID: AccountService.getCurrentAccountId(),
          namespace: data.namespace,
        },
      )) as { hasAccess: boolean };

      if (accessCheck.hasAccess) {
        // User has access - directly approve
        await this.client.sendRequest("dark/approveLocations", {
          locationIds: [data.locationId],
          reviewerId: AccountService.getCurrentAccountId(),
        });

        vscode.window.showInformationMessage(
          `Approved ${data.itemName || "item"}`,
        );
      } else {
        // User doesn't have access - create approval request
        await this.client.sendRequest("dark/createApprovalRequest", {
          accountID: AccountService.getCurrentAccountId(),
          targetNamespace: data.namespace,
          locationIds: [data.locationId],
          title: `${data.itemName || "Change"} in ${data.namespace}`,
          description: null,
        });

        vscode.window.showInformationMessage(
          `Submitted ${data.itemName || "item"} for approval`,
        );
      }
      this.approvalsProvider?.clearSelection();
      this.approvalsProvider?.refresh();
    } catch (error) {
      console.error("Failed to submit for approval:", error);
      vscode.window.showErrorMessage("Failed to submit for approval");
    }
  }

  /** Quick approve a single location (no prompts) */
  private async quickApprove(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.locationId || !data?.requestId) {
      vscode.window.showErrorMessage("No location data found");
      return;
    }

    try {
      await this.client.sendRequest("dark/approveLocations", {
        requestId: data.requestId,
        locationIds: [data.locationId],
        reviewerId: AccountService.getCurrentAccountId(),
      });

      vscode.window.showInformationMessage(
        `Approved ${data.itemName || "item"}`,
      );
      this.approvalsProvider?.clearReviewSelection();
    } catch (error) {
      console.error("Failed to approve:", error);
      vscode.window.showErrorMessage("Failed to approve item");
    }
  }

  /** Quick reject a single location */
  private async quickReject(node: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const data = node.approvalData;
    if (!data?.locationId || !data?.requestId) {
      vscode.window.showErrorMessage("No location data found");
      return;
    }

    const reason = await vscode.window.showInputBox({
      prompt: `Why are you rejecting ${data.itemName || "this item"}?`,
      placeHolder: "Reason for rejection...",
    });

    if (reason === undefined) {
      return; // Cancelled
    }

    try {
      await this.client.sendRequest("dark/rejectLocations", {
        requestId: data.requestId,
        locationIds: [data.locationId],
        reviewerId: AccountService.getCurrentAccountId(),
        reason: reason || "No reason provided",
      });

      vscode.window.showInformationMessage(
        `Rejected ${data.itemName || "item"}`,
      );
      this.approvalsProvider?.clearReviewSelection();
    } catch (error) {
      console.error("Failed to reject:", error);
      vscode.window.showErrorMessage("Failed to reject item");
    }
  }

  /** Approve all selected review items */
  private async approveSelected(): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const byRequest = this.approvalsProvider?.getSelectedReviewItemsByRequest();
    if (!byRequest || byRequest.size === 0) {
      vscode.window.showInformationMessage("No items selected to approve");
      return;
    }

    const totalCount = Array.from(byRequest.values()).reduce(
      (sum, ids) => sum + ids.length,
      0,
    );

    const confirm = await vscode.window.showWarningMessage(
      `Approve ${totalCount} selected item(s)?`,
      { modal: true },
      "Approve",
    );

    if (confirm !== "Approve") {
      return;
    }

    try {
      // Make a request for each approval request
      for (const [requestId, locationIds] of byRequest) {
        await this.client.sendRequest("dark/approveLocations", {
          requestId,
          locationIds,
          reviewerId: AccountService.getCurrentAccountId(),
        });
      }

      vscode.window.showInformationMessage(`Approved ${totalCount} item(s)`);
      this.approvalsProvider?.clearReviewSelection();
    } catch (error) {
      console.error("Failed to approve selected:", error);
      vscode.window.showErrorMessage("Failed to approve selected items");
    }
  }

  /** Reject all selected review items */
  private async rejectSelected(): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    const byRequest = this.approvalsProvider?.getSelectedReviewItemsByRequest();
    if (!byRequest || byRequest.size === 0) {
      vscode.window.showInformationMessage("No items selected to reject");
      return;
    }

    const totalCount = Array.from(byRequest.values()).reduce(
      (sum, ids) => sum + ids.length,
      0,
    );

    const reason = await vscode.window.showInputBox({
      prompt: `Why are you rejecting ${totalCount} item(s)?`,
      placeHolder: "Reason for rejection...",
    });

    if (reason === undefined) {
      return; // Cancelled
    }

    try {
      // Make a request for each approval request
      for (const [requestId, locationIds] of byRequest) {
        await this.client.sendRequest("dark/rejectLocations", {
          requestId,
          locationIds,
          reviewerId: AccountService.getCurrentAccountId(),
          reason: reason || "No reason provided",
        });
      }

      vscode.window.showInformationMessage(`Rejected ${totalCount} item(s)`);
      this.approvalsProvider?.clearReviewSelection();
    } catch (error) {
      console.error("Failed to reject selected:", error);
      vscode.window.showErrorMessage("Failed to reject selected items");
    }
  }

  private async createRequest(node?: BranchNode): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    // If called from a namespace group, create request for that namespace
    if (
      node?.contextValue === "namespace-group" &&
      node.approvalData?.namespace
    ) {
      const namespace = node.approvalData.namespace;
      await this.createRequestForNamespace(namespace);
      return;
    }

    // Check if there are pre-selected items from checkboxes
    const preSelectedIds = this.approvalsProvider?.getSelectedLocationIds();
    const hasPreSelected = preSelectedIds && preSelectedIds.length > 0;

    try {
      const locations = await this.fetchPendingLocations();

      if (locations.length === 0) {
        vscode.window.showInformationMessage(
          "No pending changes to submit for approval",
        );
        return;
      }

      // Group ALL locations by namespace
      const allByNamespace = new Map<
        string,
        Array<{
          locationId: string;
          modules: string[];
          name: string;
          itemType: string;
        }>
      >();
      for (const loc of locations) {
        if (!allByNamespace.has(loc.namespace)) {
          allByNamespace.set(loc.namespace, []);
        }
        allByNamespace.get(loc.namespace)!.push(loc);
      }

      if (hasPreSelected) {
        // Filter to only selected items
        const selectedSet = new Set(preSelectedIds);
        const selectedLocations = locations.filter(l =>
          selectedSet.has(l.locationId),
        );

        if (selectedLocations.length > 0) {
          // Group selected by namespace
          const selectedByNamespace = new Map<
            string,
            Array<{
              locationId: string;
              modules: string[];
              name: string;
              itemType: string;
            }>
          >();
          for (const loc of selectedLocations) {
            if (!selectedByNamespace.has(loc.namespace)) {
              selectedByNamespace.set(loc.namespace, []);
            }
            selectedByNamespace.get(loc.namespace)!.push(loc);
          }

          // Process each namespace
          for (const [namespace, selectedItems] of selectedByNamespace) {
            // Check if user has access to this namespace
            const accessCheck = (await this.client.sendRequest(
              "dark/checkNamespaceAccess",
              {
                accountID: AccountService.getCurrentAccountId(),
                namespace,
              },
            )) as { hasAccess: boolean };

            if (accessCheck.hasAccess) {
              // User has access - directly approve
              await this.client.sendRequest("dark/approveLocations", {
                locationIds: selectedItems.map(i => i.locationId),
                reviewerId: AccountService.getCurrentAccountId(),
              });

              vscode.window.showInformationMessage(
                `Approved ${selectedItems.length} item(s)`,
              );
            } else {
              // User doesn't have access - open approval tab
              const allItems = allByNamespace.get(namespace) || [];
              this.openApprovalTab(namespace, allItems, selectedItems);
            }
          }

          this.approvalsProvider?.clearSelection();
          this.approvalsProvider?.refresh();
          return;
        }
      }

      // No pre-selected items, let user pick a namespace then open tab
      const namespaces = Array.from(allByNamespace.keys());

      const selectedNamespace = await vscode.window.showQuickPick(namespaces, {
        placeHolder: "Select namespace to create request for",
      });

      if (!selectedNamespace) {
        return;
      }

      const locationsForNs = allByNamespace.get(selectedNamespace) || [];

      // Check if user has access to this namespace
      const accessCheck = (await this.client.sendRequest(
        "dark/checkNamespaceAccess",
        {
          accountID: AccountService.getCurrentAccountId(),
          namespace: selectedNamespace,
        },
      )) as { hasAccess: boolean };

      if (accessCheck.hasAccess) {
        // User has access - directly approve all items
        await this.client.sendRequest("dark/approveLocations", {
          locationIds: locationsForNs.map(i => i.locationId),
          reviewerId: AccountService.getCurrentAccountId(),
        });

        vscode.window.showInformationMessage(
          `Approved ${locationsForNs.length} item(s)`,
        );
        this.approvalsProvider?.refresh();
      } else {
        // User doesn't have access - open approval tab
        this.openApprovalTab(selectedNamespace, locationsForNs, locationsForNs);
      }
    } catch (error) {
      console.error("Failed to create request:", error);
      vscode.window.showErrorMessage("Failed to create approval request");
    }
  }

  /** Create approval request for a specific namespace */
  private async createRequestForNamespace(namespace: string): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    try {
      const locations = await this.fetchPendingLocations();
      const locationsForNs = locations.filter(l => l.namespace === namespace);

      if (locationsForNs.length === 0) {
        vscode.window.showInformationMessage(
          `No pending changes in ${namespace}`,
        );
        return;
      }

      // Use selected items if any, otherwise all items in namespace
      const selectedIds = new Set(
        this.approvalsProvider?.getSelectedLocationIds() || [],
      );
      const selectedInNs = locationsForNs.filter(l =>
        selectedIds.has(l.locationId),
      );
      const initialItems =
        selectedInNs.length > 0 ? selectedInNs : locationsForNs;

      // Check if user has access to this namespace
      const accessCheck = (await this.client.sendRequest(
        "dark/checkNamespaceAccess",
        {
          accountID: AccountService.getCurrentAccountId(),
          namespace,
        },
      )) as { hasAccess: boolean };

      if (accessCheck.hasAccess) {
        // User has access - directly approve all items
        await this.client.sendRequest("dark/approveLocations", {
          locationIds: initialItems.map(i => i.locationId),
          reviewerId: AccountService.getCurrentAccountId(),
        });

        vscode.window.showInformationMessage(
          `Approved ${initialItems.length} item(s)`,
        );
        this.approvalsProvider?.clearSelection();
        this.approvalsProvider?.refresh();
      } else {
        // User doesn't have access - open the approval request panel
        this.openApprovalTab(namespace, locationsForNs, initialItems);
        this.approvalsProvider?.clearSelection();
      }
    } catch (error) {
      console.error("Failed to create request:", error);
      vscode.window.showErrorMessage("Failed to create approval request");
    }
  }

  /** Fetch pending locations for the current account */
  private async fetchPendingLocations(): Promise<
    Array<{
      locationId: string;
      namespace: string;
      modules: string[];
      name: string;
      itemType: string;
    }>
  > {
    if (!this.client) return [];
    const locations = await this.client.sendRequest<
      Array<{
        locationId: string;
        namespace: string;
        modules: string[];
        name: string;
        itemType: string;
      }>
    >("dark/listPendingLocations", {
      accountID: AccountService.getCurrentAccountId(),
    });
    return locations || [];
  }

  private async switchAccount(): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    try {
      // Fetch accounts from LSP
      const accounts = await this.client.sendRequest<string[]>(
        "dark/listAccounts",
      );
      if (!accounts || accounts.length === 0) {
        vscode.window.showErrorMessage("No accounts available");
        return;
      }

      const currentAccountId = AccountService.getCurrentAccountId();

      const selected = await vscode.window.showQuickPick(accounts, {
        placeHolder: `Current: ${currentAccountId}. Select account to switch to:`,
      });

      if (selected && selected !== currentAccountId) {
        // Update centralized account service - this triggers LSP sync via subscription
        AccountService.setCurrentAccount(selected);

        // Update pinned items for the new account
        await PinnedItemsService.setCurrentAccount(selected);

        // Clear current branch and refresh branch list for new account
        const branchManager = BranchStateManager.getInstance();
        await branchManager.clearCurrentBranch();
        await branchManager.fetchBranches();

        // Also update the homepage panel if it's open
        await HomepagePanel.currentPanel?.setCurrentAccount(selected);

        vscode.window.showInformationMessage(
          `Switched to account: ${selected}`,
        );
      }
    } catch (error) {
      console.error("Failed to switch account:", error);
      vscode.window.showErrorMessage("Failed to switch account");
    }
  }

  /** Set account directly (called from homepage dropdown) */
  private async setAccount(accountID: string): Promise<void> {
    if (!accountID) return;

    // Update centralized account service - this triggers LSP sync via subscription
    AccountService.setCurrentAccount(accountID);

    // Update pinned items for the new account
    await PinnedItemsService.setCurrentAccount(accountID);

    // Clear current branch and refresh branch list for new account
    const branchManager = BranchStateManager.getInstance();
    await branchManager.clearCurrentBranch();
    await branchManager.fetchBranches();
  }

  /** Open a tab for creating an approval request */
  private openApprovalTab(
    namespace: string,
    allItems: ApprovalItem[],
    initiallySelected: ApprovalItem[],
  ): void {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    ApprovalRequestPanel.create(
      this.client,
      this.approvalsProvider ?? null,
      namespace,
      allItems,
      initiallySelected,
    );
  }
}
