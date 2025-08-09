import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

const COLLAPSIBLE_STATE = {
  NONE: 0,
  COLLAPSED: 1,
  EXPANDED: 2,
} as const;

interface TreeItemResponse {
  id: string;
  label: string;
  collapsibleState: number;
  contextValue?: string;
}

export class Node extends vscode.TreeItem {
  constructor(
    public readonly id: string,
    public readonly label: string,
    public readonly type: "file" | "directory",
    public readonly collapsibleState: vscode.TreeItemCollapsibleState,
    public readonly packagePath?: string,
  ) {
    super(label, collapsibleState);
    this.tooltip = this.label;

    // Add command to open the definition when clicked
    if (this.type === "file" && this.packagePath) {
      this.command = {
        command: "darklang.openPackageDefinition",
        title: "Open Definition",
        arguments: [this.packagePath],
      };
    } else if (this.type === "file" && this.id.startsWith("script/")) {
      this.command = {
        command: "darklang.openScript",
        title: "Open Script",
        arguments: [this.id],
      };
    }
  }
}

export class ServerBackedTreeDataProvider
  implements vscode.TreeDataProvider<Node>
{
  private _client: LanguageClient;

  constructor(client: LanguageClient) {
    this._client = client;
  }

  getTreeItem(node: Node): vscode.TreeItem {
    return node;
  }

  private mapResponseToNode(item: TreeItemResponse): Node {
    const type =
      item.collapsibleState === COLLAPSIBLE_STATE.NONE ? "file" : "directory";
    const collapsibleState =
      item.collapsibleState === COLLAPSIBLE_STATE.NONE
        ? vscode.TreeItemCollapsibleState.None
        : item.collapsibleState === COLLAPSIBLE_STATE.COLLAPSED
        ? vscode.TreeItemCollapsibleState.Collapsed
        : vscode.TreeItemCollapsibleState.Expanded;

    return new Node(
      item.id,
      item.label,
      type,
      collapsibleState,
      item.contextValue,
    );
  }

  async getChildren(node?: Node): Promise<Node[]> {
    try {
      const items = node
        ? await this._client.sendRequest<TreeItemResponse[]>(
            "darklang/getChildNodes",
            { nodeId: node.id },
          )
        : await this._client.sendRequest<TreeItemResponse[]>(
            "darklang/getRootNodes",
          );

      return items.map(item => this.mapResponseToNode(item));
    } catch (error) {
      console.error(`Failed to get tree nodes: ${error}`);
      vscode.window.showErrorMessage(
        `Failed to load tree view: ${
          error instanceof Error ? error.message : "Unknown error"
        }`,
      );
      return [];
    }
  }
}
