import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { StatusBarManager } from "../ui/statusbar/statusBarManager";
import { WorkspaceTreeDataProvider } from "../providers/treeviews/workspaceTreeDataProvider";
import { PackagesTreeDataProvider } from "../providers/treeviews/packagesTreeDataProvider";

export class InstanceCommands {
  private client: LanguageClient | null = null;
  private packagesProvider: PackagesTreeDataProvider | null = null;

  constructor(
    private statusBarManager: StatusBarManager,
    private workspaceProvider: WorkspaceTreeDataProvider
  ) {}

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  setClient(client: LanguageClient): void {
    this.client = client;
  }

  register(): vscode.Disposable[] {
    return [
      // Switch to instance (triggered by clicking instance in workspace tree)
      vscode.commands.registerCommand("darklang.instance.switch", (instanceId) => {
        console.log('🔄 Switching to instance:', instanceId);
        // TODO: Actually switch instance state
        this.workspaceProvider.refresh();
        vscode.window.showInformationMessage(`Switched to instance: ${instanceId}`);
      }),

      // Sync with instance
      vscode.commands.registerCommand("darklang.instance.sync", async (treeItem) => {
        if (!this.client) {
          vscode.window.showErrorMessage("LSP client not ready");
          return;
        }

        // Extract instance info from tree item
        const instanceId = treeItem.id || "unknown";
        const instanceUrl = treeItem.instanceData?.url;

        if (!instanceUrl) {
          vscode.window.showErrorMessage("Instance URL not available");
          return;
        }

        try {
          vscode.window.showInformationMessage(`Syncing with ${instanceId}...`);

          const response = await this.client.sendRequest<{
            success: boolean;
            message: string;
          }>("darklang/sync", {
            instanceId,
            remoteUrl: instanceUrl
          });

          if (response.success) {
            vscode.window.showInformationMessage(response.message);
            // Refresh both trees to show synced changes
            this.workspaceProvider.refresh();
            if (this.packagesProvider) {
              this.packagesProvider.refresh();
            }
          } else {
            vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
          }
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      }),
    ];
  }
}