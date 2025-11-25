import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { StatusBarManager } from "../ui/statusbar/statusBarManager";
import { WorkspaceTreeDataProvider } from "../providers/treeviews/workspaceTreeDataProvider";
import { PackagesTreeDataProvider } from "../providers/treeviews/packagesTreeDataProvider";

export class InstanceCommands {
  private packagesProvider: PackagesTreeDataProvider | null = null;

  constructor(
    private client: LanguageClient,
    private statusBarManager: StatusBarManager,
    private workspaceProvider: WorkspaceTreeDataProvider
  ) {}

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  register(): vscode.Disposable[] {
    return [];
  }
}