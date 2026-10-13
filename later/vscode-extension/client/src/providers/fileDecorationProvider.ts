import * as vscode from 'vscode';
import { UrlMetadataSystem } from './urlMetadataSystem';

// CLEANUP:
// - UrlMetadata.title field is generated but never used by this provider

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

  public dispose(): void {
    this._onDidChangeFileDecorations.dispose();
  }
}