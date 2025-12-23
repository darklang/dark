import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { ApprovalsTreeDataProvider } from "../providers/treeviews/approvalsTreeDataProvider";
import { PackagesTreeDataProvider } from "../providers/treeviews/packagesTreeDataProvider";
import { HomepagePanel } from "../panels/homepage/homepagePanel";
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
      vscode.commands.registerCommand("darklang.approvals.createRequest", () =>
        this.createRequest(),
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

  private async createRequest(): Promise<void> {
    if (!this.client) {
      vscode.window.showErrorMessage("LSP client not ready");
      return;
    }

    // Get pending locations for this user
    try {
      const locations = await this.client.sendRequest<
        Array<{
          locationId: string;
          namespace: string;
          name: string;
          itemType: string;
        }>
      >("dark/listPendingLocations", {
        accountID: AccountService.getCurrentAccountId(),
      });

      if (!locations || locations.length === 0) {
        vscode.window.showInformationMessage(
          "No pending changes to submit for approval",
        );
        return;
      }

      // Group by namespace for selection
      const namespaces = [...new Set(locations.map(l => l.namespace))];

      const selectedNamespace = await vscode.window.showQuickPick(namespaces, {
        placeHolder: "Select namespace to create request for",
      });

      if (!selectedNamespace) {
        return;
      }

      const locationsForNs = locations.filter(
        l => l.namespace === selectedNamespace,
      );

      // Let user select which locations to include
      const locationItems = locationsForNs.map(loc => ({
        label: loc.name,
        description: loc.itemType,
        picked: true, // Pre-select all by default
        locationId: loc.locationId,
      }));

      const selectedLocations = await vscode.window.showQuickPick(
        locationItems,
        {
          placeHolder: "Select items to include in this request",
          canPickMany: true,
        },
      );

      if (!selectedLocations || selectedLocations.length === 0) {
        return;
      }

      const locationIds = selectedLocations.map(l => l.locationId);

      const title = await vscode.window.showInputBox({
        prompt: "Enter a title for this approval request",
        placeHolder: `Changes to ${selectedNamespace}`,
      });

      if (title === undefined) {
        return;
      }

      const description = await vscode.window.showInputBox({
        prompt: "Enter a description (optional)",
        placeHolder: "Description of changes...",
      });

      await this.client.sendRequest("dark/createApprovalRequest", {
        accountID: AccountService.getCurrentAccountId(),
        targetNamespace: selectedNamespace,
        locationIds,
        title: title || `Changes to ${selectedNamespace}`,
        description: description || null,
      });

      vscode.window.showInformationMessage(
        `Approval request created for ${locationIds.length} item(s) in ${selectedNamespace}`,
      );
      this.approvalsProvider?.refresh();
    } catch (error) {
      console.error("Failed to create request:", error);
      vscode.window.showErrorMessage("Failed to create approval request");
    }
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
}
