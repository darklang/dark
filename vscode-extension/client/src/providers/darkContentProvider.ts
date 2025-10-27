import * as vscode from "vscode";
import { UrlPatternRouter, ParsedUrl } from "./urlPatternRouter";
import { PackageContentProvider } from "./content/packageContentProvider";
import { LanguageClient } from "vscode-languageclient/node";
import { BranchStateManager } from "../data/branchStateManager";

export class DarkContentProvider implements vscode.TextDocumentContentProvider {
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this._onDidChange.event;
  private client: LanguageClient | null = null;

  // Injected by activate() to get view mode for a given URI
  getModeForUri?: (uri: vscode.Uri) => "source" | "ast";

  constructor() {
    console.log(
      "DarkContentProvider initialized with support for all URL patterns",
    );
  }

  setClient(client: LanguageClient): void {
    this.client = client;
    PackageContentProvider.setClient(client);
  }

  /**
   * Refresh a specific URI by firing the onDidChange event
   */
  bump(uri: vscode.Uri): void {
    this._onDidChange.fire(uri);
  }

  async provideTextDocumentContent(uri: vscode.Uri): Promise<string> {
    try {
      const url = uri.toString();
      console.log(`Providing content for: ${url}`);

      // Check view mode and delegate accordingly
      const mode = this.getModeForUri ? this.getModeForUri(uri) : "source";

      if (mode === "ast") {
        return await this.renderAstFor(uri);
      } else {
        return await this.renderSourceFor(uri);
      }
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
   * Render the AST view for a URI
   */
  private async renderAstFor(uri: vscode.Uri): Promise<string> {
    const url = uri.toString();
    const parsedUrl = UrlPatternRouter.parseUrl(url);

    if (!parsedUrl) {
      return this.getErrorContent(
        url,
        "Invalid URL format - cannot render AST",
      );
    }

    // For now, we'll request AST rendering from the LSP
    // TODO: implement proper LSP request for AST
    if (!this.client) {
      return this.getErrorContent(
        url,
        "LSP client not ready - cannot render AST",
      );
    }

    try {
      // Request AST from LSP server
      const result = await this.client.sendRequest<{ ast: string }>(
        "darklang/ast",
        {
          uri: url,
          parsedUrl: parsedUrl,
        },
      );

      if (result && result.ast) {
        return result.ast;
      }

      // Fallback: render a placeholder AST view
      return this.getPlaceholderAstContent(parsedUrl);
    } catch (error) {
      console.error("Error fetching AST from LSP:", error);
      // Fallback to placeholder
      return this.getPlaceholderAstContent(parsedUrl);
    }
  }

  /**
   * Generate a placeholder AST view (used when LSP doesn't support AST yet)
   */
  private getPlaceholderAstContent(parsedUrl: ParsedUrl): string {
    return `# AST View (Preview)

**Mode:** ${parsedUrl.mode}
**Context:** ${parsedUrl.context || "N/A"}
**Target:** ${parsedUrl.target || "N/A"}
**View:** ${parsedUrl.view || "N/A"}

---

## Abstract Syntax Tree

\`\`\`
AST rendering is currently in development.
The LSP server will provide structured AST data here.

Parsed URL structure:
${JSON.stringify(parsedUrl, null, 2)}
\`\`\`

---

*Switch back to Source mode to see the rendered content.*
`;
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
        return "TODO: fake content (instance)";

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
    const { context: branchId, view: action } = parsedUrl;

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

    return `# Branch: ${branch.title}

## Branch Information

| Field | Value |
|-------|-------|
| **ID** | \`${branch.id}\` |
| **Title** | ${branch.title} |
| **State** | ${branch.state} |
| **Created By** | ${branch.createdBy || "(none)"} |
| **Created At** | ${formatDate(branch.createdAt)} |
| **Last Active** | ${formatDate(branch.lastActiveAt)} |
| **Merged At** | ${
      branch.mergedAt ? formatDate(branch.mergedAt) : "(not merged)"
    } |

## Branch Contents

### Branch State
- **Modified Items:** 5
- **New Items:** 2
- **Pending Changes:** 3 operations
- **Test Status:** 95% passing

### Collaboration
- **Owner:** stachu
- **Shared With:** alice, bob
- **Access Level:** Read/Write

## Branch Actions

- [🎯 Switch to Branch](command:darklang.branch.switch?${branchId})
- [📤 Export Branch](dark://branch/${branchId}/export)
- [🔄 Transfer Branch](dark://branch/${branchId}/transfer)
- [⏸️ Suspend Branch](command:darklang.branch.suspend?${branchId})
- [🛑 End Branch](command:darklang.branch.end?${branchId})

## Recent Activity

- 16:45 - Modified MyApp.User.validate
- 16:30 - Created ValidationError type
- 16:15 - Updated MyApp.User.create
- 15:45 - Added validation tests

[📊 View Branch Statistics](dark://branch/${branchId}/stats)
[🗂️ Browse Branch Files](dark://branch/${branchId}/files)`;
  }

  private getBranchListContent(): string {
    return `# Branchs Overview

## Active Branchs

### Current Branch
- **feature-auth** (Active) - Authentication features development

### Available Branchs
- **team-branch-alpha** (Shared) - Team collaboration branch
- **performance-optimizations** (Suspended) - Performance improvements
- **ui-redesign** (Draft) - User interface updates

## Quick Actions

- [🆕 Create New Branch](command:darklang.branch.new)
- [📥 Import Branch](command:darklang.branch.import)
- [🔄 Sync Branchs](command:darklang.branch.sync)

## Branch Management

### Recent Activity
- 16:45 - Switched to feature-auth
- 15:30 - Created performance-optimizations
- 14:20 - Shared team-branch-alpha

### Branch Templates
- [🔐 Authentication Template](command:darklang.branch.template.auth)
- [🎨 UI Development Template](command:darklang.branch.template.ui)
- [⚡ Performance Template](command:darklang.branch.template.perf)

[📊 Branch Statistics](dark://branch/stats)
[⚙️ Branch Settings](dark://config/branchs)`;
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
