import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { PinnedItemsService } from "../../services/pinnedItemsService";

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

// Special IDs for section nodes
const PINNED_SECTION_ID = "__pinned_section__";
const ALL_SECTION_ID = "__all_section__";

export class Node extends vscode.TreeItem {
  public isPinned: boolean = false;
  public isSection: boolean = false;
  public sectionType?: "pinned" | "all";
  public itemKind?: "function" | "type" | "value" | "module" | "owner";

  constructor(
    public readonly id: string,
    public readonly label: string,
    public readonly type: "file" | "directory" | "section",
    public readonly collapsibleState: vscode.TreeItemCollapsibleState,
    public readonly packagePath?: string,
    contextValue?: string,
    isPinned: boolean = false,
  ) {
    super(label, collapsibleState);
    this.tooltip = this.label;
    this.isPinned = isPinned;

    // Check if this is a section node
    if (id === PINNED_SECTION_ID) {
      this.isSection = true;
      this.sectionType = "pinned";
      this.contextValue = "section";
    } else if (id === ALL_SECTION_ID) {
      this.isSection = true;
      this.sectionType = "all";
      this.contextValue = "section";
    }

    // Set the contextValue if provided (and not a section)
    if (contextValue && !this.isSection) {
      this.contextValue = contextValue;
    }

    // Extract itemKind from contextValue prefix
    if (contextValue) {
      if (contextValue.startsWith("fn:")) {
        this.itemKind = "function";
      } else if (contextValue.startsWith("type:")) {
        this.itemKind = "type";
      } else if (contextValue.startsWith("value:") || contextValue.startsWith("const:")) {
        this.itemKind = "value";
      }
    }

    // Add command to open the definition when clicked
    if (this.type === "file" && this.packagePath) {
      this.command = {
        command: "darklang.openPackageDefinition",
        title: "Open Definition",
        arguments: [this.packagePath, this.itemKind],
      };
    }

    // Set icons based on the type of node
    this.setIcon();
  }

  private setIcon(): void {
    // Section nodes don't have icons
    if (this.isSection) {
      return;
    }

    // For packages (root level items) - detect by ID not containing a dot
    // Root level IDs are single words like "Darklang", "Stachu", "Scripts"
    // Child IDs contain dots like "Darklang.Stdlib", "Darklang.Stdlib.Int64"
    const isRootLevel = this.type === "directory" && !this.id.includes(".");

    if (isRootLevel) {
      this.setIconFromKind("owner");
    } else if (this.type === "directory") {
      this.setIconFromKind("module");
    } else if (this.itemKind) {
      this.setIconFromKind(this.itemKind);
    }
  }

  /** Set icon based on kind */
  setIconFromKind(kind: string): void {
    const normalizedKind = kind.toLowerCase();
    if (normalizedKind === "function") {
      this.iconPath = new vscode.ThemeIcon("symbol-function");
    } else if (normalizedKind === "type") {
      this.iconPath = new vscode.ThemeIcon(
        "symbol-type-parameter",
        new vscode.ThemeColor("charts.blue"),
      );
    } else if (normalizedKind === "constant" || normalizedKind === "value") {
      this.iconPath = new vscode.ThemeIcon(
        "symbol-constant",
        new vscode.ThemeColor("charts.orange"),
      );
    } else if (normalizedKind === "module") {
      this.iconPath = new vscode.ThemeIcon(
        "symbol-structure",
        new vscode.ThemeColor("charts.lines"),
      );
    } else if (normalizedKind === "package" || normalizedKind === "owner") {
      this.iconPath = new vscode.ThemeIcon(
        "package",
        new vscode.ThemeColor("charts.orange"),
      );
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
  private _pinnedServiceDisposable: vscode.Disposable;
  private _parentMap: Map<string, Node> = new Map();
  private _nodeMap: Map<string, Node> = new Map();

  constructor(client: LanguageClient) {
    this._client = client;

    // Subscribe to pinned items changes
    this._pinnedServiceDisposable = PinnedItemsService.onDidChange(() => {
      this._rootNodesCache = null;
      this._onDidChangeTreeData.fire();
    });

    // Wait for server to be ready, then refresh the tree
    this._client.onReady().then(() => {
      this._isServerReady = true;
      // Load pinned items before refreshing
      PinnedItemsService.load().then(() => {
        this._onDidChangeTreeData.fire();
      });
    });
  }

  /** Check if an item is pinned */
  isItemPinned(id: string): boolean {
    return PinnedItemsService.isPinned(id);
  }

  refresh(): void {
    this._rootNodesCache = null;
    // Reload pinned items when refreshing
    PinnedItemsService.refresh().then(() => {
      this._onDidChangeTreeData.fire();
    });
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
    this._pinnedServiceDisposable.dispose();
  }

  getTreeItem(node: Node): vscode.TreeItem {
    return node;
  }

  getParent(node: Node): Node | undefined {
    return this._parentMap.get(node.id);
  }

  /** Get a node by its ID */
  getNodeById(id: string): Node | undefined {
    return this._nodeMap.get(id);
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

    // Check if this item is pinned
    const isPinned = PinnedItemsService.isPinned(item.id);

    const node = new Node(
      item.id,
      item.label,
      type,
      collapsibleState,
      packagePath,
      contextValue,
      isPinned,
    );

    // Set contextValue for all items to enable context menu and when clause matching
    const isRootLevel = !item.id.includes(".");
    if (type === "directory") {
      if (isRootLevel) {
        // Root level items (owners like "Darklang", "Stachu")
        node.contextValue = "owner";
      } else if (!item.contextValue) {
        // Modules (directories inside packages)
        node.contextValue = "module";
        node.tooltip = `${item.label} - Right-click to open full module`;
      }
    } else if (!node.contextValue) {
      // Entities without a contextValue (shouldn't happen but just in case)
      node.contextValue = "entity";
    }

    // Append pinned state to contextValue for when clause matching
    if (isPinned) {
      node.contextValue = node.contextValue
        ? `${node.contextValue}:pinned`
        : "pinned";
    }

    return node;
  }

  async getChildren(node?: Node): Promise<Node[]> {
    // Wait for server to be ready before fetching any nodes
    if (!this._isServerReady) {
      return [];
    }

    // If requesting root nodes, return the section nodes
    if (!node) {
      // Use cache if available
      if (this._rootNodesCache) {
        return this._rootNodesCache;
      }

      // Clear maps on root refresh
      this._parentMap.clear();
      this._nodeMap.clear();

      // Create section nodes
      const pinnedSection = new Node(
        PINNED_SECTION_ID,
        "Pinned",
        "section",
        vscode.TreeItemCollapsibleState.Expanded,
      );

      const allSection = new Node(
        ALL_SECTION_ID,
        "All",
        "section",
        vscode.TreeItemCollapsibleState.Expanded,
      );

      // Store section nodes
      this._nodeMap.set(PINNED_SECTION_ID, pinnedSection);
      this._nodeMap.set(ALL_SECTION_ID, allSection);

      this._rootNodesCache = [pinnedSection, allSection];
      return this._rootNodesCache;
    }

    // Handle Pinned section - return flat list of pinned items
    if (node.id === PINNED_SECTION_ID) {
      const pinnedItems = PinnedItemsService.getAll();
      return pinnedItems.map(item => {
        // Normalize kind to lowercase
        const kind = item.kind.toLowerCase();
        const isModule = kind === "module" || kind === "package";
        const collapsibleState = isModule
          ? vscode.TreeItemCollapsibleState.Collapsed
          : vscode.TreeItemCollapsibleState.None;

        const pinnedNode = new Node(
          `pinned:${item.treeId}`,
          item.name,
          isModule ? "directory" : "file",
          collapsibleState,
          item.treeId, // Use treeId as packagePath for navigation
          undefined,
          true, // isPinned
        );

        // Set the icon based on the kind
        pinnedNode.setIconFromKind(item.kind);

        // Set contextValue for pinned items
        if (kind === "function") {
          pinnedNode.contextValue = "fn:" + item.treeId + ":pinned";
        } else if (kind === "type") {
          pinnedNode.contextValue = "type:" + item.treeId + ":pinned";
        } else if (kind === "constant" || kind === "value") {
          pinnedNode.contextValue = "value:" + item.treeId + ":pinned";
        } else if (kind === "module" || kind === "package") {
          pinnedNode.contextValue = "module:pinned";
        }

        // Add command to open the definition when clicked (for non-modules)
        if (!isModule) {
          // Map kind to item type for recent tracking
          let itemType: "function" | "type" | "value" | "package" | undefined;
          if (kind === "function") {
            itemType = "function";
          } else if (kind === "type") {
            itemType = "type";
          } else if (kind === "constant" || kind === "value") {
            itemType = "value";
          }

          pinnedNode.command = {
            command: "darklang.openPackageDefinition",
            title: "Open Definition",
            arguments: [item.treeId, itemType],
          };
        }

        return pinnedNode;
      });
    }

    // Handle All section - return the actual root nodes from LSP
    if (node.id === ALL_SECTION_ID) {
      try {
        const items = await this._client.sendRequest<TreeItemResponse[]>(
          "dark/getRootNodes",
          {},
        );
        const nodes = items.map(item => this.mapResponseToNode(item));
        // Store parent references for reveal functionality
        for (const childNode of nodes) {
          this._nodeMap.set(childNode.id, childNode);
          this._parentMap.set(childNode.id, node);
        }
        return nodes;
      } catch (error) {
        console.error(`Failed to get root nodes: ${error}`);
        return [];
      }
    }

    // Handle children of pinned module items
    if (node.id.startsWith("pinned:")) {
      const actualId = node.id.substring("pinned:".length);
      try {
        const items = await this._client.sendRequest<TreeItemResponse[]>(
          "dark/getChildNodes",
          { nodeId: actualId },
        );
        return items.map(item => this.mapResponseToNode(item));
      } catch (error) {
        console.error(`Failed to get children of pinned module: ${error}`);
        return [];
      }
    }

    // Handle regular child nodes
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
