import * as vscode from 'vscode';
import { UrlMetadataSystem } from './urlMetadataSystem';

// CLEANUP:
// - refresh() method is never called - either use it or remove it
// - dispose() is defined but provider is not added to context.subscriptions in extension.ts
// - UrlMetadata.title field is generated but never used by this provider
// - UrlMetadata.contentProvider field is set but never read

export class DarklangFileDecorationProvider implements vscode.FileDecorationProvider {
  private readonly _onDidChangeFileDecorations = new vscode.EventEmitter<vscode.Uri | vscode.Uri[] | undefined>();
  readonly onDidChangeFileDecorations = this._onDidChangeFileDecorations.event;

  provideFileDecoration(uri: vscode.Uri): vscode.FileDecoration | undefined {
    if (uri.scheme !== 'dark') {
      return undefined;
    }

    const metadata = UrlMetadataSystem.getMetadata(uri);
    if (!metadata) {
      return undefined;
    }

    return {
      badge: metadata.badge,
      tooltip: metadata.tooltip,
      color: metadata.themeColor
    };
  }


  public refresh(): void {
    this._onDidChangeFileDecorations.fire(undefined);
  }

  public dispose(): void {
    this._onDidChangeFileDecorations.dispose();
  }
}