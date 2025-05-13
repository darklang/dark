import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";

export class Node extends vscode.TreeItem {
  constructor(
    public readonly id: string,
    public readonly label: string,
    public readonly type: "file" | "directory",
    public readonly collapsibleState: vscode.TreeItemCollapsibleState,
  ) {
    super(label, collapsibleState);
    this.tooltip = `${this.label} (${this.type})`;
    this.description = this.type;
  }

  // You might want to include contextValue if you're implementing context menus
  contextValue = "myTreeNode";
}

export class ServerBackedTreeDataProvider
  implements vscode.TreeDataProvider<Node>
{
  private _client: LanguageClient;

  constructor(client: LanguageClient) {
    this._client = client;
  }

  /** Maps from our internal 'Node' to `TreeItem`
   *
   * At time of writing, our 'Node' _is_ just a TreeItem, so maybe this is silly.
   */
  getTreeItem(node: Node): vscode.TreeItem {
    return {
      id: node.id,
      label: node.label,
      collapsibleState: node.collapsibleState,
    };
  }

  async getChildren(node?: Node): Promise<Node[]> {
    if (!node) {
      return this._client.sendRequest("darklang/getRootNodes");
    } else {
      return this._client.sendRequest("darklang/getChildNodes", {
        nodeId: node.id,
      });
    }
  }
}
