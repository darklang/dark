import { ParsedUrl } from "../urlPatternRouter";
import { LanguageClient } from "vscode-languageclient/node";

/**
 * Content provider for package browsing URLs
 * Handles: dark://package/Name.Space.item[?view=type]
 */
export class PackageContentProvider {
  private static client: LanguageClient;

  static setClient(client: LanguageClient): void {
    this.client = client;
  }

  static async getContentAsync(parsedUrl: ParsedUrl): Promise<string> {
    const { target, view } = parsedUrl;

    console.log(
      `PackageContentProvider.getContentAsync: target="${target}", view="${view}"`,
    );

    if (!target) {
      return this.getPackageListContent();
    }

    // TODO: Add AST view support (should integrate with LSP's darklang/ast request)
    return await this.getSourceView(target);
  }

  static getContent(parsedUrl: ParsedUrl): string {
    const { target } = parsedUrl;
    if (!target) {
      return this.getPackageListContent();
    }
    return this.getLoadingContent(target);
  }

  private static async getSourceView(target: string): Promise<string> {
    try {
      const packagePath = target;
      const uri = `darkfs:/${packagePath}.dark`;

      const response = await this.client.sendRequest<{ content: string }>(
        "fileSystem/read",
        { uri },
      );

      return response.content;
    } catch (error) {
      console.error(`Failed to fetch package content for ${target}:`, error);
      return this.getGenericPackageContent(target);
    }
  }

  private static getLoadingContent(target: string): string {
    return `# ${target}

Loading package definition...`;
  }

  private static getGenericPackageContent(target: string): string {
    // TODO: Implement fallback content when LSP client is unavailable or fetch fails?
    return `# ${target}

Package content could not be loaded.`;
  }

  private static getPackageListContent(): string {
    // TODO: Implement package browser/explorer page
    // Should show:
    // - List of all available packages organized by namespace
    // - Search functionality
    // - Package categories (Stdlib, Darklang, User packages, etc.)
    // - Quick links to commonly used packages
    return `# Darklang Packages

Package browser is not yet implemented.`;
  }
}
