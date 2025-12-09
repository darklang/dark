import * as vscode from "vscode";

export interface RecentItem {
  id: string;
  title: string;
  type: "branch" | "app" | "module" | "function" | "type" | "value";
  meta: string[];
  accessedAt: number; // timestamp
}

const STORAGE_KEY = "darklang.recentItems";
const MAX_RECENT_ITEMS = 10;

type RecentItemsListener = () => void;

/** Singleton service for managing recently accessed items */
class RecentItemsServiceImpl {
  private _context: vscode.ExtensionContext | null = null;
  private _listeners: Set<RecentItemsListener> = new Set();

  /** Initialize with extension context */
  initialize(context: vscode.ExtensionContext): void {
    this._context = context;
  }

  /** Subscribe to recent items changes */
  onDidChange(listener: RecentItemsListener): vscode.Disposable {
    this._listeners.add(listener);
    return { dispose: () => this._listeners.delete(listener) };
  }

  private _notifyListeners(): void {
    this._listeners.forEach(listener => listener());
  }

  /** Get all recent items */
  getAll(): RecentItem[] {
    if (!this._context) {
      return [];
    }
    const items = this._context.globalState.get<RecentItem[]>(STORAGE_KEY, []);
    // Sort by most recent first and deduplicate by id (keep most recent)
    const sorted = items.sort((a, b) => b.accessedAt - a.accessedAt);
    const seen = new Set<string>();
    return sorted.filter(item => {
      if (seen.has(item.id)) {
        return false;
      }
      seen.add(item.id);
      return true;
    });
  }

  /** Track a recently accessed item */
  async trackItem(item: Omit<RecentItem, "accessedAt">): Promise<void> {
    if (!this._context) {
      return;
    }

    const recent = this._context.globalState.get<RecentItem[]>(STORAGE_KEY, []);

    // Remove existing entry with same id
    const filtered = recent.filter(r => r.id !== item.id);

    // Add new item at front with current timestamp
    const newItem: RecentItem = {
      ...item,
      accessedAt: Date.now(),
    };

    // Limit to max items
    const updated = [newItem, ...filtered].slice(0, MAX_RECENT_ITEMS);

    await this._context.globalState.update(STORAGE_KEY, updated);
    this._notifyListeners();
  }

  /** Clear all recent items */
  clear(): void {
    if (!this._context) {
      return;
    }
    this._context.globalState.update(STORAGE_KEY, []);
    this._notifyListeners();
  }
}

export const RecentItemsService = new RecentItemsServiceImpl();
