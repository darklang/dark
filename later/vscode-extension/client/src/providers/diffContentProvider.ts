import * as vscode from "vscode";

/**
 * Virtual document provider for diff content.
 * Uses URIs like: darklang-diff:/before/{id} and darklang-diff:/after/{id}
 */
export class DiffContentProvider implements vscode.TextDocumentContentProvider {
  public static readonly scheme = "darklang-diff";
  private static instance: DiffContentProvider;

  private _content: Map<string, string> = new Map();
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this._onDidChange.event;

  static getInstance(): DiffContentProvider {
    if (!DiffContentProvider.instance) {
      DiffContentProvider.instance = new DiffContentProvider();
    }
    return DiffContentProvider.instance;
  }

  /** Store content for a diff and return the URIs */
  setDiffContent(id: string, before: string, after: string): { beforeUri: vscode.Uri; afterUri: vscode.Uri } {
    this._content.set(`before/${id}`, before);
    this._content.set(`after/${id}`, after);

    const beforeUri = vscode.Uri.parse(`${DiffContentProvider.scheme}:before/${id}.dark`);
    const afterUri = vscode.Uri.parse(`${DiffContentProvider.scheme}:after/${id}.dark`);

    return { beforeUri, afterUri };
  }

  /** Clear content for a diff (optional cleanup) */
  clearDiffContent(id: string): void {
    this._content.delete(`before/${id}`);
    this._content.delete(`after/${id}`);
  }

  provideTextDocumentContent(uri: vscode.Uri): string {
    // URI path is like "before/{id}.dark" or "after/{id}.dark"
    const path = uri.path.replace(/\.dark$/, "");
    return this._content.get(path) || "";
  }
}
