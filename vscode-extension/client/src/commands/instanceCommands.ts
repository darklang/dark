import * as vscode from "vscode";
import { StatusBarManager } from "../ui/statusbar/statusBarManager";
import { WorkspaceTreeDataProvider } from "../providers/treeviews/workspaceTreeDataProvider";

export class InstanceCommands {
  constructor(
    private statusBarManager: StatusBarManager,
    private workspaceProvider: WorkspaceTreeDataProvider
  ) {}

  register(): vscode.Disposable[] {
    return [
      // Switch to instance (triggered by clicking instance in workspace tree)
      vscode.commands.registerCommand("darklang.instance.switch", (instanceId) => {
        console.log('🔄 Switching to instance:', instanceId);
        // TODO: Actually switch instance state
        this.workspaceProvider.refresh();
        vscode.window.showInformationMessage(`Switched to instance: ${instanceId}`);
      }),
    ];
  }
}