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
      } else if (this.contextValue.startsWith("value:")) {
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
  private _searchQuery: string = "";
  private _allNodesCache: Node[] | null = null;
  private _allNodesCachePromise: Promise<Node[]> | null = null;
  private _searchResults: Node[] | null = null;
  private _searchMatchingNodes: Node[] | null = null;

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
    this._allNodesCache = null;
    this._allNodesCachePromise = null;
    this._onDidChangeTreeData.fire();
  }

  dispose(): void {
    this._onDidChangeTreeData.dispose();
  }

  setSearchQuery(query: string): void {
    this._searchQuery = query.toLowerCase();
    this._searchResults = null; // Clear previous search results
    this._searchMatchingNodes = null; // Clear previous matching nodes
    // Don't clear caches - just fire refresh to re-filter
    this._onDidChangeTreeData.fire();
  }

  getSearchQuery(): string {
    return this._searchQuery;
  }

  clearSearch(): void {
    this.setSearchQuery("");
  }

  private matchesSearch(node: Node): boolean {
    if (!this._searchQuery) {
      return true;
    }

    // Search in both ID and label (case-insensitive)
    const searchLower = this._searchQuery;
    const idLower = node.id.toLowerCase();
    const labelLower = node.label.toLowerCase();

    return idLower.includes(searchLower) || labelLower.includes(searchLower);
  }

  private async _performSearch(): Promise<void> {
    try {
      const allNodes = await this.getAllNodes();
      const matchingNodes = allNodes.filter(n => this.matchesSearch(n));

      // Store matching nodes for later use
      this._searchMatchingNodes = matchingNodes;

      // Group by module/package and store results
      this._searchResults = this._groupSearchResults(matchingNodes);
    } catch (error) {
      console.error(`Failed to perform search: ${error}`);
      this._searchMatchingNodes = [];
      this._searchResults = [];
    }
  }

  private async getAllNodes(): Promise<Node[]> {
    // Return cached nodes if available
    if (this._allNodesCache) {
      return this._allNodesCache;
    }

    // If a request is already in progress, return that promise
    if (this._allNodesCachePromise) {
      return this._allNodesCachePromise;
    }

    // Start fetching all nodes
    this._allNodesCachePromise = this._fetchAllNodes();
    this._allNodesCache = await this._allNodesCachePromise;
    this._allNodesCachePromise = null;

    return this._allNodesCache;
  }

  private async _fetchAllNodes(): Promise<Node[]> {
    const allNodes: Node[] = [];

    try {
      // Get root nodes
      const rootItems = await this._client.sendRequest<TreeItemResponse[]>(
        "dark/getRootNodes",
        {},
      );

      const rootNodes = rootItems.map(item => this.mapResponseToNode(item));

      // Recursively fetch all descendants
      for (const rootNode of rootNodes) {
        allNodes.push(rootNode);
        if (rootNode.type === "directory") {
          const descendants = await this._getAllDescendantsRecursive(rootNode);
          allNodes.push(...descendants);
        }
      }
    } catch (error) {
      console.error(`Failed to fetch all nodes: ${error}`);
    }

    return allNodes;
  }

  private async _getAllDescendantsRecursive(node: Node): Promise<Node[]> {
    const descendants: Node[] = [];

    try {
      const items = await this._client.sendRequest<TreeItemResponse[]>(
        "dark/getChildNodes",
        { nodeId: node.id },
      );

      const childNodes = items.map(item => this.mapResponseToNode(item));

      for (const child of childNodes) {
        descendants.push(child);
        if (child.type === "directory") {
          const subDescendants = await this._getAllDescendantsRecursive(child);
          descendants.push(...subDescendants);
        }
      }
    } catch (error) {
      console.error(`Failed to get descendants for ${node.id}: ${error}`);
    }

    return descendants;
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

    // Set contextValue for directories to enable context menu
    const isRootLevel = !item.id.includes(".");
    if (type === "directory" && !item.contextValue) {
      if (isRootLevel) {
        node.contextValue = "package-owner";
        node.tooltip = `${item.label} - Right-click to create new module`;
      } else {
        node.contextValue = "module";
        node.tooltip = `${item.label} - Right-click to open full module`;
      }
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
      // If no search query, return normal root nodes
      if (!this._searchQuery) {
        if (this._rootNodesCache) {
          return this._rootNodesCache;
        }

        try {
          const items = await this._client.sendRequest<TreeItemResponse[]>(
            "dark/getRootNodes",
            {},
          );

          this._rootNodesCache = items.map(item => this.mapResponseToNode(item));
          return this._rootNodesCache;
        } catch (error) {
          console.error(`Failed to get root nodes: ${error}`);
          return [];
        }
      }

      // If searching, show header with loading state
      // If we have cached search results, show them
      if (this._searchResults !== null) {
        // Create search header node
        const searchHeader = new Node(
          "search-header",
          `Searching for "${this._searchQuery}"`,
          "directory",
          vscode.TreeItemCollapsibleState.None
        );
        searchHeader.iconPath = new vscode.ThemeIcon("search");
        searchHeader.contextValue = "search-header";
        searchHeader.command = undefined; // Remove click action

        return [searchHeader, ...this._searchResults];
      }

      // Otherwise, show loading indicator and start fetching results in background
      const loadingHeader = new Node(
        "search-loading",
        `Searching for "${this._searchQuery}"...`,
        "directory",
        vscode.TreeItemCollapsibleState.None
      );
      loadingHeader.iconPath = new vscode.ThemeIcon("loading~spin");
      loadingHeader.contextValue = "search-loading";
      loadingHeader.command = undefined; // Remove click action

      // Start fetching results in background
      this._performSearch().then(() => {
        // Fire refresh to show results after search completes
        this._onDidChangeTreeData.fire();
      });

      // Return loading header immediately while search runs
      return [loadingHeader];
    }

    // If this is a search result group node (synthetic node we created)
    if (node.id.startsWith("search-group:")) {
      const groupId = node.id.replace("search-group:", "");

      // Use cached matching nodes if available
      if (this._searchMatchingNodes) {
        return this._searchMatchingNodes.filter(n => {
          // Extract module path from node ID
          const modulePath = this._getModulePath(n.id);
          return modulePath === groupId;
        });
      }

      // Fallback: fetch and filter (shouldn't normally happen)
      const allNodes = await this.getAllNodes();
      const matchingNodes = allNodes.filter(n => this.matchesSearch(n));

      // Return nodes that belong to this group
      return matchingNodes.filter(n => {
        // Extract module path from node ID
        const modulePath = this._getModulePath(n.id);
        return modulePath === groupId;
      });
    }

    // If we have a search query active, return empty for normal nodes
    // (we already showed everything grouped at root level)
    if (this._searchQuery) {
      return [];
    }

    // Normal tree navigation (no search)
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

  private _getModulePath(nodeId: string): string {
    // Extract the module path from a node ID
    // E.g., "Darklang.Stdlib.Int64.add" -> "Darklang.Stdlib.Int64"
    // E.g., "Darklang.Stdlib" -> "Darklang.Stdlib"
    const parts = nodeId.split(".");
    if (parts.length <= 2) {
      // Root level or top-level module
      return parts[0];
    }
    // Return all but the last part (which is the function/type name)
    return parts.slice(0, -1).join(".");
  }

  private _groupSearchResults(nodes: Node[]): Node[] {
    // Group nodes by their module path
    const groups = new Map<string, Node[]>();

    for (const node of nodes) {
      // Skip root-level packages (they're just containers)
      if (node.type === "directory" && !node.id.includes(".")) {
        continue;
      }

      const modulePath = this._getModulePath(node.id);

      if (!groups.has(modulePath)) {
        groups.set(modulePath, []);
      }
      groups.get(modulePath)!.push(node);
    }

    // Create group nodes
    const groupNodes: Node[] = [];

    for (const [modulePath, nodesInGroup] of groups.entries()) {
      // Create a synthetic group node
      const groupNode = new Node(
        `search-group:${modulePath}`,
        `${modulePath} (${nodesInGroup.length})`,
        "directory",
        vscode.TreeItemCollapsibleState.Expanded,
      );
      groupNode.iconPath = new vscode.ThemeIcon(
        "symbol-structure",
        new vscode.ThemeColor("charts.blue"),
      );
      groupNodes.push(groupNode);
    }

    // Sort groups alphabetically
    return groupNodes.sort((a, b) => a.label.localeCompare(b.label));
  }
}
