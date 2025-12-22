import * as vscode from "vscode";
import * as crypto from "crypto";
import { httpRequest, DARK_EDITOR_HOST, DARK_EDITOR_PORT } from "../utils/http";
import { AccountService } from "./accountService";

export interface PinnedItem {
  itemId: string;
  accountID: string;
  kind: string;
  name: string;
  owner: string;
  modules: string;
  treeId: string; // Full path for tree view (e.g., "Darklang.Stdlib.List.map")
}

/** Data needed to pin an item */
export interface PinData {
  treeId: string;
  name: string;
  kind: string;
}

type PinnedItemsListener = () => void;

/** Singleton service for managing pinned items */
class PinnedItemsServiceImpl {
  private _pinnedItems: Map<string, PinnedItem> = new Map(); // treeId -> PinnedItem
  private _listeners: Set<PinnedItemsListener> = new Set();
  private _isLoaded: boolean = false;

  /** Subscribe to pinned items changes */
  onDidChange(listener: PinnedItemsListener): vscode.Disposable {
    this._listeners.add(listener);
    return { dispose: () => this._listeners.delete(listener) };
  }

  private _notifyListeners(): void {
    this._listeners.forEach(listener => listener());
  }

  /** Set the current account and reload pinned items */
  async setCurrentAccount(accountID: string): Promise<void> {
    if (AccountService.getCurrentAccountId() !== accountID) {
      AccountService.setCurrentAccount(accountID);
      await this.load();
    }
  }

  /** Get the current account ID */
  getCurrentAccountId(): string {
    return AccountService.getCurrentAccountId();
  }

  /** Compute owner and modules from a treeId */
  private _parseTreeId(treeId: string): { owner: string; modules: string } {
    const parts = treeId.split(".");
    const owner = parts[0] || "Darklang";
    const modules = parts.slice(0, -1).join(".");
    return { owner, modules };
  }

  /** Load pinned items from the server */
  async load(): Promise<void> {
    const currentAccountId = AccountService.getCurrentAccountId();
    const url = `/pinned?accountID=${encodeURIComponent(currentAccountId)}`;
    try {
      const response = await httpRequest({
        hostname: DARK_EDITOR_HOST,
        port: DARK_EDITOR_PORT,
        path: url,
        method: "GET",
      });

      if (response.statusCode === 200) {
        const items = JSON.parse(response.data);
        this._pinnedItems.clear();

        for (const item of items) {
          // Server now stores treeId directly, but handle legacy data that doesn't have it
          let treeId = item.treeId;
          if (!treeId) {
            // Legacy reconstruction for old data
            if (!item.modules && item.owner === item.name) {
              treeId = item.owner;
            } else {
              const parts = [item.owner];
              if (item.modules) {
                const moduleParts = item.modules.split(".");
                if (moduleParts[0] === item.owner) {
                  parts.push(...moduleParts.slice(1));
                } else {
                  parts.push(...moduleParts);
                }
              }
              parts.push(item.name);
              treeId = parts.join(".");
            }
          }

          this._pinnedItems.set(treeId, {
            itemId: item.itemId,
            accountID: item.accountID || currentAccountId,
            kind: item.kind,
            name: item.name,
            owner: item.owner,
            modules: item.modules || "",
            treeId,
          });
        }

        this._isLoaded = true;
        this._notifyListeners();
      }
    } catch (error) {
      console.error("Failed to load pinned items:", error);
    }
  }

  /** Refresh pinned items from server */
  async refresh(): Promise<void> {
    await this.load();
  }

  /** Check if an item is pinned by its treeId */
  isPinned(treeId: string): boolean {
    return this._pinnedItems.has(treeId);
  }

  /** Get the server itemId for a pinned node */
  getItemId(treeId: string): string | undefined {
    return this._pinnedItems.get(treeId)?.itemId;
  }

  /** Get all pinned items */
  getAll(): PinnedItem[] {
    return Array.from(this._pinnedItems.values());
  }

  /** Whether items have been loaded at least once */
  get isLoaded(): boolean {
    return this._isLoaded;
  }

  /** Pin an item - handles HTTP call and optimistic update */
  async pin(data: PinData): Promise<boolean> {
    const { treeId, name, kind } = data;
    const { owner, modules } = this._parseTreeId(treeId);
    const itemId = crypto.randomUUID();
    const accountID = AccountService.getCurrentAccountId();

    const item: PinnedItem = {
      itemId,
      accountID,
      treeId,
      name,
      kind,
      owner,
      modules,
    };

    // Optimistic update
    this._pinnedItems.set(treeId, item);
    this._notifyListeners();

    try {
      const body = JSON.stringify({
        itemId,
        accountID,
        treeId,
        name,
        kind,
        owner,
        modules,
      });
      const response = await httpRequest(
        {
          hostname: DARK_EDITOR_HOST,
          port: DARK_EDITOR_PORT,
          path: "/pin",
          method: "POST",
          headers: { "Content-Type": "application/json" },
        },
        body,
      );

      if (response.statusCode !== 200) {
        // Revert on failure
        this._pinnedItems.delete(treeId);
        this._notifyListeners();
        vscode.window.showErrorMessage(`Failed to pin: ${response.data}`);
        return false;
      }
      return true;
    } catch (error) {
      // Revert on failure
      this._pinnedItems.delete(treeId);
      this._notifyListeners();
      console.error("Failed to pin item:", error);
      vscode.window.showErrorMessage(
        `Failed to pin: ${
          error instanceof Error ? error.message : "Connection failed"
        }`,
      );
      return false;
    }
  }

  /** Unpin an item - handles HTTP call and optimistic update */
  async unpin(treeId: string): Promise<boolean> {
    const item = this._pinnedItems.get(treeId);
    if (!item) {
      return false;
    }

    // Optimistic update
    this._pinnedItems.delete(treeId);
    this._notifyListeners();

    try {
      const body = JSON.stringify({
        itemId: item.itemId,
        accountID: item.accountID,
      });
      const response = await httpRequest(
        {
          hostname: DARK_EDITOR_HOST,
          port: DARK_EDITOR_PORT,
          path: "/pin",
          method: "DELETE",
          headers: { "Content-Type": "application/json" },
        },
        body,
      );

      if (response.statusCode !== 200) {
        // Revert on failure
        this._pinnedItems.set(treeId, item);
        this._notifyListeners();
        vscode.window.showErrorMessage(`Failed to unpin: ${response.data}`);
        return false;
      }
      return true;
    } catch (error) {
      // Revert on failure
      this._pinnedItems.set(treeId, item);
      this._notifyListeners();
      console.error("Failed to unpin item:", error);
      vscode.window.showErrorMessage(
        `Failed to unpin: ${
          error instanceof Error ? error.message : "Connection failed"
        }`,
      );
      return false;
    }
  }
}

export const PinnedItemsService = new PinnedItemsServiceImpl();
