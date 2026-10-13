import * as vscode from "vscode";
import { UrlPatternRouter, ParsedUrl } from "./urlPatternRouter";
import { PackageContentProvider } from "./content/packageContentProvider";
import { LanguageClient } from "vscode-languageclient/node";

export class DarkContentProvider implements vscode.TextDocumentContentProvider {
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this._onDidChange.event;

  constructor(private client: LanguageClient) {
    console.log(
      "DarkContentProvider initialized with support for all URL patterns",
    );
  }

  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    try {
      // TODO: Support AST view via URL query parameter (e.g., ?view=ast)
      return await this.renderSourceFor(uri);
    } catch (error) {
      console.error("Error providing content:", error);
      return this.getErrorContent(uri.toString(), `Error: ${error}`);
    }
  }

  /** Render the source view for a URI */
  private async renderSourceFor(uri: vscode.Uri): Promise<string> {
    const url = uri.toString();
    const parsedUrl = UrlPatternRouter.parseUrl(url);

    if (!parsedUrl) {
      return this.getErrorContent(url, "Invalid URL format");
    }

    return await this.getContentForParsedUrl(parsedUrl);
  }

  /** Route parsed URL to appropriate content provider */
  private async getContentForParsedUrl(parsedUrl: ParsedUrl): Promise<string> {
    switch (parsedUrl.mode) {
      case "package":
        return await PackageContentProvider.getContentAsync(parsedUrl);

      default:
        return this.getErrorContent(
          `dark:///${parsedUrl.mode}/${parsedUrl.context || ""}/${
            parsedUrl.target || ""
          }`,
          `Unsupported URL mode: ${parsedUrl.mode}`,
        );
    }
  }

  public refresh(uri?: vscode.Uri): void {
    if (uri) {
      this._onDidChange.fire(uri);
    } else {
      // Refresh all dark:// documents
      vscode.workspace.textDocuments
        .filter(doc => doc.uri.scheme === "dark")
        .forEach(doc => this._onDidChange.fire(doc.uri));
    }
  }

  private getErrorContent(url: string, error: string): string {
    return `# Error Loading Darklang Content

**URL:** ${url}
**Error:** ${error}

Please check your URL format and try again.`;
  }
}
