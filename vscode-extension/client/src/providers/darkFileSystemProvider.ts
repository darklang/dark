import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { WorkspaceTreeDataProvider } from "./treeviews/workspaceTreeDataProvider";
import { PackagesTreeDataProvider } from "./treeviews/packagesTreeDataProvider";

/**
 * FileSystemProvider for darkfs:// URIs
 * Enables editing of package modules with automatic parsing and op generation on save
 */
export class DarkFileSystemProvider implements vscode.FileSystemProvider {
  private _emitter = new vscode.EventEmitter<vscode.FileChangeEvent[]>();
  readonly onDidChangeFile: vscode.Event<vscode.FileChangeEvent[]> = this._emitter.event;

  private workspaceProvider: WorkspaceTreeDataProvider | null = null;
  private packagesProvider: PackagesTreeDataProvider | null = null;

  // Cache of file contents to detect changes
  private contentCache = new Map<string, Uint8Array>();

  constructor(private client: LanguageClient) {
    console.log('DarkFileSystemProvider initialized');
  }

  setWorkspaceProvider(provider: WorkspaceTreeDataProvider): void {
    this.workspaceProvider = provider;
  }

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  /**
   * Refresh all open darkfs:// files by clearing cache and firing change events
   * This should be called when switching branches to show updated content
   */
  async refreshAllOpenFiles(): Promise<void> {
    console.log('Refreshing all open darkfs:// files');

    // Get all open text documents
    const openDarkfsFiles = vscode.workspace.textDocuments.filter(
      doc => doc.uri.scheme === 'darkfs'
    );

    for (const doc of openDarkfsFiles) {
      try {
        // Clear cache for this file
        this.contentCache.delete(doc.uri.toString());

        // Fire change event to trigger reload
        this._emitter.fire([{
          type: vscode.FileChangeType.Changed,
          uri: doc.uri
        }]);
      } catch (err) {
        console.error(`Failed to refresh ${doc.uri.toString()}:`, err);
      }
    }
  }

  watch(_uri: vscode.Uri, _options: { recursive: boolean; excludes: readonly string[]; }): vscode.Disposable {
    // Watching is not required for our use case
    return new vscode.Disposable(() => {});
  }

  stat(_uri: vscode.Uri): vscode.FileStat {
    return {
      type: vscode.FileType.File,
      ctime: Date.now(),
      mtime: Date.now(),
      size: 0,
    };
  }

  readDirectory(_uri: vscode.Uri): [string, vscode.FileType][] {
    throw vscode.FileSystemError.NoPermissions('Directory reading not supported');
  }

  createDirectory(_uri: vscode.Uri): void {
    throw vscode.FileSystemError.NoPermissions('Directory creation not supported');
  }

  async readFile(uri: vscode.Uri): Promise<Uint8Array> {
    try {
      const response = await this.client.sendRequest<{ content: string }>(
        'fileSystem/read',
        { uri: uri.toString() }
      );

      const content = new Uint8Array(Buffer.from(response.content, 'utf-8'));
      this.contentCache.set(uri.toString(), content);
      return content;
    } catch (error) {
      console.error(`Failed to read ${uri.toString()}:`, error);
      throw vscode.FileSystemError.FileNotFound(uri);
    }
  }

  async writeFile(
    uri: vscode.Uri,
    content: Uint8Array,
    options: { create: boolean; overwrite: boolean; }
  ): Promise<void> {
    // Content must be base64 encoded per LSP spec
    const contentStr = Buffer.from(content).toString('utf-8');

    try {
      // Parse the content and create ops for new declarations
      const response = await this.client.sendRequest<{
        success: boolean;
        ops?: any[];
        updatedContent?: string;
        errors?: string[];
      }>(
        'fileSystem/write',
        {
          uri: uri.toString(),
          content: contentStr,
          options: {
            create: options.create,
            overwrite: options.overwrite
          }
        }
      );

      if (response.success) {
        if (response.ops && response.ops.length > 0 && this.workspaceProvider) {
          // Add ops to workspace tree view
          this.workspaceProvider.addPendingOps(response.ops);
          vscode.window.showInformationMessage(
            `✓ Parsed successfully. Created ${response.ops.length} operation(s).`
          );

          // If LSP returned updated content with IDs, use it immediately
          if (response.updatedContent) {
            const updatedContentBuffer = Buffer.from(response.updatedContent, 'utf-8');
            this.contentCache.set(uri.toString(), new Uint8Array(updatedContentBuffer));

            // Fire change event to update the editor
            this._emitter.fire([{
              type: vscode.FileChangeType.Changed,
              uri
            }]);
          }

          // Refresh packages tree to show new declarations
          if (this.packagesProvider) {
            this.packagesProvider.refresh();
          }
        } else {
          // No ops created - just update cache with what was written
          this.contentCache.set(uri.toString(), content);
          vscode.window.showInformationMessage('✓ Saved (no changes detected)');
        }

        // Fire change event
        this._emitter.fire([{
          type: vscode.FileChangeType.Changed,
          uri
        }]);
      } else{
        // Show parse errors
        const errors = response.errors || ['Unknown error'];
        vscode.window.showErrorMessage(
          `Parse failed: ${errors.join(', ')}`
        );
      }
    } catch (error) {
      console.error(`Failed to write ${uri.toString()}:`, error);
      vscode.window.showErrorMessage(
        `Failed to save: ${error instanceof Error ? error.message : 'Unknown error'}`
      );
      throw vscode.FileSystemError.Unavailable('Write failed');
    }
  }

  delete(_uri: vscode.Uri, _options: { recursive: boolean; }): void {
    throw vscode.FileSystemError.NoPermissions('Deletion not supported');
  }

  rename(_oldUri: vscode.Uri, _newUri: vscode.Uri, _options: { overwrite: boolean; }): void {
    throw vscode.FileSystemError.NoPermissions('Renaming not supported');
  }
}
