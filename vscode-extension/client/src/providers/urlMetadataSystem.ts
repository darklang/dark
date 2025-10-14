import * as vscode from 'vscode';
import { UrlPatternRouter, ParsedUrl } from './urlPatternRouter';

export interface UrlMetadata {
  title: string;
  badge: string;
  tooltip: string;
  themeColor: vscode.ThemeColor;
  contentProvider: string; // Which provider handles this URL type
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
          contentProvider: 'branch'
        };

      case 'package':
        return {
          title: this.getPackageTitle(parsed),
          badge: '📦',
          tooltip: `Package: ${parsed.target || 'Unknown'}`,
          themeColor: new vscode.ThemeColor('charts.purple'),
          contentProvider: 'package'
        };

      case 'instance':
        return {
          title: this.getInstanceTitle(parsed),
          badge: this.getInstanceBadge(parsed),
          tooltip: `Instance: ${parsed.context} (${parsed.view})`,
          themeColor: new vscode.ThemeColor('charts.orange'),
          contentProvider: 'instance'
        };

      default:
        return {
          title: 'Darklang Document',
          badge: '🌑',
          tooltip: 'Darklang Document',
          themeColor: new vscode.ThemeColor('charts.foreground'),
          contentProvider: 'default'
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
    const lastPart = target.split('.').pop() || target;
    return `Package: ${lastPart}`;
  }

  private static getInstanceTitle(parsed: ParsedUrl): string {
    const context = parsed.context || 'unknown';

    return `Instance: ${context}`;
  }

  private static getInstanceBadge(parsed: ParsedUrl): string {
    return '🖥️';
  }

  /**
   * Create a clean URL without redundant extensions
   */
  static createUrl(mode: string, context?: string, view?: string, target?: string, queryParams?: Record<string, string>): string {
    let url = `dark:///${mode}`;

    if (context) url += `/${context}`;
    if (view && view !== 'overview') url += `/${view}`;
    if (target) url += `/${target}`;

    if (queryParams && Object.keys(queryParams).length > 0) {
      const params = new URLSearchParams(queryParams);
      url += `?${params.toString()}`;
    }

    return url;
  }
}