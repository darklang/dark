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
    // For packages (root level items)
    if (
      this.id === "Darklang" ||
      this.id === "Stachu" ||
      this.id === "Scripts"
    ) {
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
      } else if (this.contextValue.startsWith("value:")) {
        this.iconPath = new vscode.ThemeIcon(
          "symbol-constant",
          new vscode.ThemeColor("charts.orange"),
        );
      } else if (this.contextValue.startsWith("script/")) {
        this.iconPath = new vscode.ThemeIcon(
          "file-code",
          new vscode.ThemeColor("charts.purple"),
        );
      }
    }
  }
}

export class ServerBackedTreeDataProvider
  implements vscode.TreeDataProvider<Node>
{
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
      this._onDidChangeTreeData.fire();
    });
  }

  refresh(): void {
    this._rootNodesCache = null;
    this._onDidChangeTreeData.fire();
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
      // For scripts, keep the full path including "script/"
      else if (item.contextValue.startsWith("script/")) {
        contextValue = item.contextValue;
        packagePath = item.contextValue; // Keep the full path for scripts
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
    // If requesting root nodes
    if (!node) {
      // Return static root nodes immediately, without waiting for server
      if (!this._rootNodesCache) {
        this._rootNodesCache = [
          new Node(
            "Darklang",
            "Darklang",
            "directory",
            vscode.TreeItemCollapsibleState.Collapsed,
          ),
          new Node(
            "Stachu",
            "Stachu",
            "directory",
            vscode.TreeItemCollapsibleState.Collapsed,
          ),
          new Node(
            "Scripts",
            "Scripts",
            "directory",
            vscode.TreeItemCollapsibleState.Collapsed,
          ),
        ];
      }
      return this._rootNodesCache;
    }

    try {
      const items = await this._client.sendRequest<TreeItemResponse[]>(
        "darklang/getChildNodes",
        { nodeId: node.id },
      );

      const nodes = items.map(item => this.mapResponseToNode(item));

      // For each module node, check if it has any non-module children
      // Don't open context menu for empty modules
      for (const childNode of nodes) {
        if (
          childNode.type === "directory" &&
          childNode.contextValue === "module"
        ) {
          const children = await this._client.sendRequest<TreeItemResponse[]>(
            "darklang/getChildNodes",
            { nodeId: childNode.id },
          );

          // Check if any children are actual definitions (not submodules)
          const hasDefinitions = children.some(
            c =>
              c.contextValue &&
              (c.contextValue.startsWith("fn:") ||
                c.contextValue.startsWith("type:") ||
                c.contextValue.startsWith("value:")),
          );

          if (!hasDefinitions) {
            // Remove context menu and tooltip for empty modules
            childNode.contextValue = undefined;
            childNode.tooltip = childNode.label;
          }
        }
      }

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
