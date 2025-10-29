import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export interface Branch {
  id: string;
  name: string;
  createdAt: string;
  mergedAt: string | null;
}

function getBranchState(branch: Branch): "active" | "merged" {
  return branch.mergedAt ? "merged" : "active";
}

export class BranchStateManager {
  private static _instance: BranchStateManager;
  private _currentBranchId: string | null = null;
  private _onBranchChanged = new vscode.EventEmitter<string | null>();
  readonly onBranchChanged = this._onBranchChanged.event;
  private _client: LanguageClient | null = null;
  private _branches: Branch[] = [];

  static getInstance(): BranchStateManager {
    if (!BranchStateManager._instance) {
      BranchStateManager._instance = new BranchStateManager();
    }
    return BranchStateManager._instance;
  }

  setClient(client: LanguageClient): void {
    this._client = client;
    this.fetchBranches();
  }

  async fetchBranches(): Promise<void> {
    if (!this._client) {
      console.log("Cannot fetch branches: LSP client not set");
      return;
    }

    try {
      console.log("Fetching branches from LSP server...");
      const response = await this._client.sendRequest<Branch[]>(
        "darklang/getBranches",
      );
      this._branches = response;
      console.log(`Fetched ${this._branches.length} branches:`, this._branches);

      // Don't automatically set a branch - user can choose one if they want
      // This allows working across all branches by default
    } catch (error) {
      console.error("Error fetching branches:", error);
      this._branches = [];
    }
  }

  getBranches(): Branch[] {
    return this._branches;
  }

  getCurrentBranch(): Branch | null {
    if (!this._currentBranchId) return null;
    return this._branches.find(b => b.id === this._currentBranchId) || null;
  }

  async setCurrentBranchById(branchId: string): Promise<void> {
    this._currentBranchId = branchId;
    console.log(`Branch changed to: ${branchId}`);

    // Notify the LSP server about the branch switch
    if (this._client) {
      try {
        await this._client.sendRequest("darklang/switchBranch", {
          branchId: branchId,
        });
        console.log(`LSP server updated to branch: ${branchId}`);
      } catch (error) {
        console.error("Error updating LSP server branch context:", error);
      }
    }

    this._onBranchChanged.fire(branchId);
  }

  getCurrentBranchId(): string | null {
    return this._currentBranchId;
  }

  getCurrentBranchName(): string {
    const currentBranch = this.getCurrentBranch();
    return currentBranch?.name || "No Branch";
  }

  async clearCurrentBranch(): Promise<void> {
    this._currentBranchId = null;
    console.log("Branch cleared");

    // Notify the LSP server about clearing the branch
    if (this._client) {
      try {
        await this._client.sendRequest("darklang/clearBranch", {});
        console.log("LSP server cleared branch context");
      } catch (error) {
        console.error("Error clearing LSP server branch context:", error);
      }
    }

    this._onBranchChanged.fire(null);
  }

  async createBranch(name: string): Promise<Branch | null> {
    if (!this._client) {
      console.error("Cannot create branch: LSP client not set");
      return null;
    }

    try {
      console.log(`Creating branch: ${name}`);
      const response = await this._client.sendRequest<Branch>(
        "darklang/createBranch",
        { name },
      );

      // Refresh the branch list to include the new branch
      await this.fetchBranches();

      // Set the new branch as current
      this._currentBranchId = response.id;
      this._onBranchChanged.fire(response.id);

      return response;
    } catch (error) {
      console.error("Error creating branch:", error);
      return null;
    }
  }
}
