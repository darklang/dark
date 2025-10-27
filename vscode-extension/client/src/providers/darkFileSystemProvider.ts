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

  private client: LanguageClient | null = null;
  private workspaceProvider: WorkspaceTreeDataProvider | null = null;
  private packagesProvider: PackagesTreeDataProvider | null = null;

  // Cache of file contents to detect changes
  private contentCache = new Map<string, Uint8Array>();

  constructor() {
    console.log('DarkFileSystemProvider initialized');
  }

  setClient(client: LanguageClient): void {
    this.client = client;
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
    console.log('🔄 Refreshing all open darkfs:// files...');

    // Get all open text documents
    const openDarkfsFiles = vscode.workspace.textDocuments.filter(
      doc => doc.uri.scheme === 'darkfs'
    );

    console.log(`Found ${openDarkfsFiles.length} open darkfs:// files`);

    for (const doc of openDarkfsFiles) {
      try {
        const uri = doc.uri;
        console.log(`Refreshing ${uri.toString()}`);

        // Clear cache for this file
        this.contentCache.delete(uri.toString());

        // Fire change event to trigger reload
        this._emitter.fire([{
          type: vscode.FileChangeType.Changed,
          uri
        }]);
      } catch (err) {
        console.error(`Failed to refresh ${doc.uri.toString()}:`, err);
      }
    }

    console.log('✓ All open darkfs:// files refreshed');
  }

  watch(uri: vscode.Uri, options: { recursive: boolean; excludes: readonly string[]; }): vscode.Disposable {
    // Watching is not required for our use case
    return new vscode.Disposable(() => {});
  }

  stat(uri: vscode.Uri): vscode.FileStat {
    return {
      type: vscode.FileType.File,
      ctime: Date.now(),
      mtime: Date.now(),
      size: 0,
    };
  }

  readDirectory(uri: vscode.Uri): [string, vscode.FileType][] {
    throw vscode.FileSystemError.NoPermissions('Directory reading not supported');
  }

  createDirectory(uri: vscode.Uri): void {
    throw vscode.FileSystemError.NoPermissions('Directory creation not supported');
  }

  async readFile(uri: vscode.Uri): Promise<Uint8Array> {
    if (!this.client) {
      throw vscode.FileSystemError.Unavailable('LSP client not ready');
    }

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
    if (!this.client) {
      throw vscode.FileSystemError.Unavailable('LSP client not ready');
    }

    // Content must be base64 encoded per LSP spec
    const contentStr = Buffer.from(content).toString('utf-8');

    try {
      // Parse the content and create ops for new declarations
      const response = await this.client.sendRequest<{
        success: boolean;
        ops?: any[];
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
        // Update cache
        this.contentCache.set(uri.toString(), content);

        // Fire change event
        this._emitter.fire([{
          type: vscode.FileChangeType.Changed,
          uri
        }]);

        // Store ops in workspace tree view
        console.log('Save response:', JSON.stringify(response));
        console.log('Ops count:', response.ops?.length);
        console.log('Workspace provider:', this.workspaceProvider);

        if (response.ops && response.ops.length > 0 && this.workspaceProvider) {
          console.log('Adding ops to workspace provider:', response.ops);
          this.workspaceProvider.addPendingOps(response.ops);
          vscode.window.showInformationMessage(
            `✓ Parsed successfully. Created ${response.ops.length} operation(s).`
          );

          // Refresh the file content and tree views after ops are applied
          // Wait a bit for ops to be fully applied to the database
          setTimeout(async () => {
            try {
              console.log('🔄 Starting refresh after ops applied...');

              // Clear cache so next read fetches fresh data
              this.contentCache.delete(uri.toString());
              console.log('✓ Cache cleared');

              // Re-read from database
              const freshContent = await this.readFile(uri);
              console.log('✓ Read fresh content from DB, length:', freshContent.length);
              console.log('Fresh content preview:', Buffer.from(freshContent).toString('utf-8').substring(0, 200));

              // Fire change event to update the editor
              this._emitter.fire([{
                type: vscode.FileChangeType.Changed,
                uri
              }]);
              console.log('✓ Fired file change event');

              // Refresh the packages tree to show the new type
              if (this.packagesProvider) {
                this.packagesProvider.refresh();
                console.log('✓ Packages tree refreshed');
              }

              console.log('✓ File content refreshed from database');
            } catch (err) {
              console.error('❌ Failed to refresh file content:', err);
            }
          }, 1000); // Wait 1 second for DB ops to complete
        } else {
          vscode.window.showInformationMessage('✓ Saved (no changes detected)');
        }
      } else {
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

  delete(uri: vscode.Uri, options: { recursive: boolean; }): void {
    throw vscode.FileSystemError.NoPermissions('Deletion not supported');
  }

  rename(oldUri: vscode.Uri, newUri: vscode.Uri, options: { overwrite: boolean; }): void {
    throw vscode.FileSystemError.NoPermissions('Renaming not supported');
  }
}
