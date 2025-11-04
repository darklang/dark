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
      // Sync with instance
      vscode.commands.registerCommand("darklang.instance.sync", async (treeItem) => {
        if (!this.client) {
          vscode.window.showErrorMessage("LSP client not ready");
          return;
        }

        // Extract instance info from tree item
        const instanceID = treeItem.id || "unknown";
        const instanceUrl = treeItem.instanceData?.url;

        if (!instanceUrl) {
          vscode.window.showErrorMessage("Instance URL not available");
          return;
        }

        try {
          await vscode.window.withProgress(
            {
              location: vscode.ProgressLocation.Notification,
              title: `Syncing with ${instanceID}...`,
              cancellable: false
            },
            async () => {
              const syncPromise = this.client.sendRequest<{
                success: boolean;
                message: string;
              }>("darklang/sync", { instanceID });

              const timeoutPromise = new Promise<never>((_, reject) =>
                setTimeout(() => reject(new Error("Sync request timed out")), 10000)
              );

              const response = await Promise.race([syncPromise, timeoutPromise]);

              if (response.success) {
                // Show success message
                vscode.window.showInformationMessage(`✓ ${response.message}`);
                // Refresh both trees to show synced changes
                this.workspaceProvider.refresh();
                if (this.packagesProvider) {
                  this.packagesProvider.refresh();
                }
              } else {
                vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
              }
            }
          );
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      }),
    ];
  }
}