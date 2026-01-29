import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { PackagesTreeDataProvider } from "./treeviews/packagesTreeDataProvider";

/**
 * FileSystemProvider for darkfs:// URIs
 * Enables editing of package modules with automatic parsing and op generation on save
 */
export class DarkFileSystemProvider implements vscode.FileSystemProvider {
  private _emitter = new vscode.EventEmitter<vscode.FileChangeEvent[]>();
  readonly onDidChangeFile: vscode.Event<vscode.FileChangeEvent[]> = this._emitter.event;

  private packagesProvider: PackagesTreeDataProvider | null = null;

  // Cache of file contents to detect changes
  private contentCache = new Map<string, Uint8Array>();

  constructor(private client: LanguageClient) {
    console.log('DarkFileSystemProvider initialized');
  }

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  /**
   * Refresh all open darkfs:// files by clearing cache and firing change events
   */
  async refreshAllOpenFiles(): Promise<void> {
    const openDarkfsFiles = vscode.workspace.textDocuments.filter(
      doc => doc.uri.scheme === 'darkfs'
    );

    for (const doc of openDarkfsFiles) {
      try {
        this.contentCache.delete(doc.uri.toString());
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
    const contentStr = Buffer.from(content).toString('utf-8');

    try {
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
        if (response.ops && response.ops.length > 0) {
          vscode.window.showInformationMessage(
            `Parsed successfully. Created ${response.ops.length} operation(s).`
          );

          if (response.updatedContent) {
            const updatedContentBuffer = Buffer.from(response.updatedContent, 'utf-8');
            this.contentCache.set(uri.toString(), new Uint8Array(updatedContentBuffer));

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
          this.contentCache.set(uri.toString(), content);
          vscode.window.showInformationMessage('Saved (no changes detected)');
        }

        this._emitter.fire([{
          type: vscode.FileChangeType.Changed,
          uri
        }]);
      } else {
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
