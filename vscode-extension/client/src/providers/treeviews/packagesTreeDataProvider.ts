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
    contextValue?: string,
  ) {
    super(label, collapsibleState);
    this.tooltip = this.label;

    // Set the contextValue if provided
    if (contextValue) {
      this.contextValue = contextValue;
    }

    // Add command to open the definition when clicked
    if (this.type === "file" && this.packagePath) {
      this.command = {
        command: "darklang.openPackageDefinition",
        title: "Open Definition",
        arguments: [this.packagePath],
      };
    }

    // Set icons based on the type of node
    this.setIcon();
  }

  private setIcon(): void {
    // For packages (root level items) - detect by ID not containing a dot
    // Root level IDs are single words like "Darklang", "Stachu", "Scripts"
    // Child IDs contain dots like "Darklang.Stdlib", "Darklang.Stdlib.Int64"
    const isRootLevel = this.type === "directory" && !this.id.includes(".");

    if (isRootLevel) {
      this.iconPath = new vscode.ThemeIcon(
        "package",
        new vscode.ThemeColor("charts.orange"),
      );
    }
    // For modules (collapsible directories inside packages)
    else if (this.type === "directory") {
      this.iconPath = new vscode.ThemeIcon(
        "symbol-structure",
        new vscode.ThemeColor("charts.lines"),
      );
    }
    // For entities - check the contextValue
    else if (this.contextValue) {
      if (this.contextValue.startsWith("fn:")) {
        this.iconPath = new vscode.ThemeIcon("symbol-function");
      } else if (this.contextValue.startsWith("type:")) {
        this.iconPath = new vscode.ThemeIcon(
          "symbol-type-parameter",
          new vscode.ThemeColor("charts.blue"),
        );
      } else if (this.contextValue.startsWith("value:") || this.contextValue.startsWith("const:")) {
        this.iconPath = new vscode.ThemeIcon(
          "symbol-constant",
          new vscode.ThemeColor("charts.orange"),
        );
      }
    }
  }
}

export class PackagesTreeDataProvider implements vscode.TreeDataProvider<Node> {
  private _client: LanguageClient;
  private _onDidChangeTreeData: vscode.EventEmitter<
    Node | undefined | null | void
  > = new vscode.EventEmitter<Node | undefined | null | void>();
  readonly onDidChangeTreeData: vscode.Event<Node | undefined | null | void> =
    this._onDidChangeTreeData.event;
  private _isServerReady: boolean = false;
  private _rootNodesCache: Node[] | null = null;

  constructor(client: LanguageClient) {
    this._client = client;

    // Wait for server to be ready, then refresh the tree
    this._client.onReady().then(() => {
      this._isServerReady = true;
      this._onDidChangeTreeData.fire();
    });
  }

  refresh(): void {
    this._rootNodesCache = null;
    this._onDidChangeTreeData.fire();
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
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

    // Extract the actual package path from the prefixed contextValue
    let packagePath = item.contextValue;
    let contextValue = item.contextValue;

    // If contextValue has a type prefix, extract the actual path
    if (item.contextValue) {
      const prefixMatch = item.contextValue.match(
        /^(fn:|type:|const:|value:)(.+)$/,
      );
      if (prefixMatch) {
        contextValue = item.contextValue; // Keep the full value with prefix for icon detection
        packagePath = prefixMatch[2]; // Extract the path without prefix for the command
      }
    }

    const node = new Node(
      item.id,
      item.label,
      type,
      collapsibleState,
      packagePath,
      contextValue,
    );

    // Set contextValue for modules (directories) to enable context menu
    // Only set if there's no existing contextValue (which would be a packagePath for entities)
    if (type === "directory" && !item.contextValue) {
      node.contextValue = "module";
      node.tooltip = `${item.label} - Right-click to open full module`;
    }

    return node;
  }

  async getChildren(node?: Node): Promise<Node[]> {
    // Wait for server to be ready before fetching any nodes
    if (!this._isServerReady) {
      return [];
    }

    // If requesting root nodes
    if (!node) {
      // Use cache if available
      if (this._rootNodesCache) {
        return this._rootNodesCache;
      }

      // Fetch root nodes from LSP
      try {
        const items = await this._client.sendRequest<TreeItemResponse[]>(
          "dark/getRootNodes",
          {}
        );

        this._rootNodesCache = items.map(item => this.mapResponseToNode(item));
        return this._rootNodesCache;
      } catch (error) {
        console.error(`Failed to get root nodes: ${error}`);
        return [];
      }
    }

    try {
      const items = await this._client.sendRequest<TreeItemResponse[]>(
        "dark/getChildNodes",
        { nodeId: node.id },
      );

      const nodes = items.map(item => this.mapResponseToNode(item));

      return nodes;
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
