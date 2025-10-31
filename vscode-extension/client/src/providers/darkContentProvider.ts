import * as vscode from "vscode";
import { UrlPatternRouter, ParsedUrl } from "./urlPatternRouter";
import { PackageContentProvider } from "./content/packageContentProvider";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchStateManager } from "../data/branchStateManager";

export class DarkContentProvider implements vscode.TextDocumentContentProvider {
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this._onDidChange.event;
  private client: LanguageClient | null = null;

  constructor() {
    console.log(
      "DarkContentProvider initialized with support for all URL patterns",
    );
  }

  setClient(client: LanguageClient): void {
    this.client = client;
    PackageContentProvider.setClient(client);
  }

  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    try {
      const url = uri.toString();
      console.log(`Providing content for: ${url}`);

      // TODO: Support AST view via URL query parameter (e.g., ?view=ast)
      // For now, always render source view
      return await this.renderSourceFor(uri);
    } catch (error) {
      console.error("Error providing content:", error);
      return this.getErrorContent(uri.toString(), `Error: ${error}`);
    }
  }

  /**
   * Render the source view for a URI
   */
  private async renderSourceFor(uri: vscode.Uri): Promise<string> {
    const url = uri.toString();
    const parsedUrl = UrlPatternRouter.parseUrl(url);

    if (!parsedUrl) {
      return this.getErrorContent(url, "Invalid URL format");
    }

    return await this.getContentForParsedUrl(parsedUrl);
  }

  /**
   * Route parsed URL to appropriate content provider
   */
  private async getContentForParsedUrl(parsedUrl: ParsedUrl): Promise<string> {
    switch (parsedUrl.mode) {
      case "package":
        return await PackageContentProvider.getContentAsync(parsedUrl);

      case "branch":
        return this.getBranchContent(parsedUrl);

      case "instance":
        // TODO: Implement instance content provider
        return `# Instance View

Instance content is not yet implemented.`;

      default:
        return this.getErrorContent(
          `dark:///${parsedUrl.mode}/${parsedUrl.context || ""}/${
            parsedUrl.target || ""
          }`,
          `Unsupported URL mode: ${parsedUrl.mode}`,
        );
    }
  }

  private getBranchContent(parsedUrl: ParsedUrl): string {
    const { context: branchId } = parsedUrl;

    if (!branchId) {
      return this.getBranchListContent();
    }

    return this.getBranchOverviewContent(branchId);
  }

  private getBranchOverviewContent(branchId: string): string {
    const branchManager = BranchStateManager.getInstance();
    const branch = branchManager.getBranches().find(b => b.id === branchId);

    if (!branch) {
      return `# Branch Not Found

Branch with ID \`${branchId}\` was not found.
`;
    }

    const formatDate = (dateStr: string) => {
      try {
        return new Date(dateStr).toLocaleString();
      } catch {
        return dateStr;
      }
    };

    const state = branch.mergedAt ? "merged" : "active";

    return `# Branch: ${branch.name}

## Branch Information

| Field | Value |
|-------|-------|
| **ID** | \`${branch.id}\` |
| **Name** | ${branch.name} |
| **State** | ${state} |
| **Created At** | ${formatDate(branch.createdAt)} |
| **Merged At** | ${
      branch.mergedAt ? formatDate(branch.mergedAt) : "(not merged)"
    } |

## Branch Actions

- [🎯 Switch to Branch](command:darklang.branch.switch?${branchId})`;
  }

  private getBranchListContent(): string {
    // TODO: Implement branch list view
    // Should fetch actual branches from BranchStateManager and display them
    return `# Branches Overview

Branch list view is not yet implemented.

To view a specific branch, use the format: \`dark:///branch/branch-id\`

## Quick Actions

- [🆕 Create New Branch](command:darklang.branch.new)
- [📋 Manage All Branches](command:darklang.branches.manageAll)`;
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
