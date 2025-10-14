import * as vscode from "vscode";
import { UrlPatternRouter, ParsedUrl } from "./urlPatternRouter";
import { PackageContentProvider } from "./content/packageContentProvider";

export class ComprehensiveDarkContentProvider implements vscode.TextDocumentContentProvider {
  private _onDidChange = new vscode.EventEmitter<vscode.Uri>();
  readonly onDidChange = this._onDidChange.event;

  constructor() {
    console.log('ComprehensiveDarkContentProvider initialized with support for all URL patterns');
  }

  provideTextDocumentContent(uri: vscode.Uri): string {
    try {
      const url = uri.toString();
      console.log(`Providing content for: ${url}`);

      const parsedUrl = UrlPatternRouter.parseUrl(url);

      if (!parsedUrl) {
        return this.getErrorContent(url, "Invalid URL format");
      }

      return this.getContentForParsedUrl(parsedUrl);

    } catch (error) {
      console.error('Error providing content:', error);
      return this.getErrorContent(uri.toString(), `Error: ${error}`);
    }
  }

  /**
   * Route parsed URL to appropriate content provider
   */
  private getContentForParsedUrl(parsedUrl: ParsedUrl): string {
    switch (parsedUrl.mode) {
      case 'package':
        return PackageContentProvider.getContent(parsedUrl);

      case 'branch':
        return this.getBranchContent(parsedUrl);

      case 'instance':
        return "TODO: fake content (instance)"

      default:
        return this.getErrorContent(
          `dark:///${parsedUrl.mode}/${parsedUrl.context || ''}/${parsedUrl.target || ''}`,
          `Unsupported URL mode: ${parsedUrl.mode}`
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
    return `# Branch: ${branchId}

## Branch Information

| Field | Value |
|-------|-------|
| **ID** | ${branchId} |
| **Name** | ${branchId === 'feature-auth' ? 'Authentication Features' : branchId} |
| **Status** | ${branchId === 'feature-auth' ? 'Active' : 'Available'} |
| **Created** | 2024-01-15 10:00:00 |
| **Last Activity** | 2024-01-15 16:45:00 |

## Branch Contents

### Active Patches
- [abc123](dark://patch/abc123) - Add user validation (Draft)
- [def456](dark://patch/def456) - Fix email validation (Ready)

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