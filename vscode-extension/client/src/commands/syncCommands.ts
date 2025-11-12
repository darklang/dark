import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export class SyncCommands {
  constructor(private client: LanguageClient) {}

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.sync.execute", async () => {

        // Ask for instance name
        const instanceName = await vscode.window.showInputBox({
          prompt: "Enter instance name to sync with",
          placeHolder: "e.g., local, origin, production",
          value: "local"
        });

        if (!instanceName) return;

        try {
          vscode.window.showInformationMessage(`Syncing with ${instanceName}...`);

          const response = await this.client.sendRequest<{
            success: boolean;
            message: string;
          }>("dark/sync", { instanceID: instanceName });

          if (response.success) {
            vscode.window.showInformationMessage(response.message);
          } else {
            vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
          }
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      }),

      vscode.commands.registerCommand("darklang.sync.quickSync", async () => {
        // Quick sync with default local instance
        const instanceName = "local";

        try {
          vscode.window.showInformationMessage(`Syncing with ${instanceName}...`);

          const response = await this.client.sendRequest<{
            success: boolean;
            message: string;
          }>("dark/sync", { instanceID: instanceName });

          if (response.success) {
            vscode.window.showInformationMessage(response.message);
          } else {
            vscode.window.showErrorMessage(`Sync failed: ${response.message}`);
          }
        } catch (error) {
          const errorMessage = error instanceof Error ? error.message : String(error);
          vscode.window.showErrorMessage(`Sync error: ${errorMessage}`);
        }
      })
    ];
  }
}
