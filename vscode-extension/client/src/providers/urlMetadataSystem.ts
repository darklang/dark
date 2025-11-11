import * as vscode from 'vscode';
import { UrlPatternRouter, ParsedUrl } from './urlPatternRouter';

// CLEANUP:
// - UrlMetadata.title field is generated but never used
// - getInstanceBadge() method (line 91) just returns hardcoded emoji - could be inlined
// - Hardcoded emoji strings could be extracted to constants

export interface UrlMetadata {
  title: string;
  badge: string;
  tooltip: string;
  themeColor: vscode.ThemeColor;
}

/**
 * Central system that determines all URL behavior based on the URL pattern itself
 */
export class UrlMetadataSystem {
  static getMetadata(uri: vscode.Uri): UrlMetadata | null {
    const parsed = UrlPatternRouter.parseUrl(uri.toString());
    if (!parsed) return null;

    return this.createMetadata(parsed, uri);
  }

  private static createMetadata(parsed: ParsedUrl, uri: vscode.Uri): UrlMetadata {
    switch (parsed.mode) {
      case 'branch':
        return {
          title: this.getBranchTitle(parsed),
          badge: '🏢',
          tooltip: `Branch: ${parsed.context || 'Unknown'}`,
          themeColor: new vscode.ThemeColor('charts.blue'),
        };

      case 'package':
        return {
          title: this.getPackageTitle(parsed),
          badge: parsed.view === "module" ? "🗂️" : "📦",
          tooltip:
            parsed.view === "module"
              ? `Module: ${parsed.target || "Unknown"}`
              : `Package: ${parsed.target || "Unknown"}`,
          themeColor: new vscode.ThemeColor("charts.purple"),
        };

      case 'instance':
        return {
          title: this.getInstanceTitle(parsed),
          badge: this.getInstanceBadge(parsed),
          tooltip: `Instance: ${parsed.context} (${parsed.view})`,
          themeColor: new vscode.ThemeColor('charts.orange'),
        };

      default:
        return {
          title: 'Darklang Document',
          badge: '🌑',
          tooltip: 'Darklang Document',
          themeColor: new vscode.ThemeColor('charts.foreground'),
        };
    }
  }

  // Title generation methods
  private static getBranchTitle(parsed: ParsedUrl): string {
    const branchName = parsed.context || 'unknown';
    return `Branch: ${branchName}`;
  }

  private static getPackageTitle(parsed: ParsedUrl): string {
    const target = parsed.target || 'unknown';

    if (parsed.view === 'module') {
      // For module view, show just the module path
      return target;
    } else {
      // For regular package items (functions, types, etc.), show just the last part
      const lastPart = target.split('.').pop() || target;
      return `${lastPart}`;
    }
  }

  private static getInstanceTitle(parsed: ParsedUrl): string {
    const context = parsed.context || 'unknown';

    return `Instance: ${context}`;
  }

  private static getInstanceBadge(_parsed: ParsedUrl): string {
    return '🖥️';
  }
}