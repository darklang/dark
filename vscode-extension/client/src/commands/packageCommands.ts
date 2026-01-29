import * as vscode from "vscode";
import * as crypto from "crypto";
import { PackagesTreeDataProvider } from "../providers/treeviews/packagesTreeDataProvider";
import { DiffContentProvider } from "../providers/diffContentProvider";
import { RecentItemsService } from "../services/recentItemsService";
import { PinnedItemsService } from "../services/pinnedItemsService";

export class PackageCommands {
  private packagesProvider: PackagesTreeDataProvider | null = null;
  private packagesView: vscode.TreeView<any> | null = null;

  setPackagesProvider(provider: PackagesTreeDataProvider): void {
    this.packagesProvider = provider;
  }

  setPackagesView(view: vscode.TreeView<any>): void {
    this.packagesView = view;
  }

  private async openDarkFile(packagePath: string, itemType?: "function" | "type" | "value" | "module"): Promise<void> {
    try {
      const virtualUri = vscode.Uri.parse(`darkfs:/${packagePath}.dark`);
      const doc = await vscode.workspace.openTextDocument(virtualUri);
      await vscode.languages.setTextDocumentLanguage(doc, 'darklang');
      await vscode.window.showTextDocument(doc, { preview: false, preserveFocus: false });

      // Track as recent item
      const parts = packagePath.split(".");
      const name = parts[parts.length - 1];
      const owner = parts[0] || "Darklang";
      const modules = parts.slice(1, -1).join(".");

      await RecentItemsService.trackItem({
        id: packagePath,
        title: name,
        type: itemType || "module",
        meta: [itemType || "module", modules || owner],
      });
    } catch (error) {
      console.error(`Failed to open ${packagePath}:`, error);
      vscode.window.showErrorMessage(`Failed to open ${packagePath}: ${error instanceof Error ? error.message : "Unknown error"}`);
    }
  }

  private async revealInTree(treeId: string): Promise<void> {
    if (!this.packagesView || !this.packagesProvider) {
      return;
    }

    try {
      // First, try to get the node from the cache
      let targetNode = this.packagesProvider.getNodeById(treeId);

      // If not in cache, we need to expand the "All" section first to populate the cache
      if (!targetNode) {
        const allSection = this.packagesProvider.getNodeById("__all_section__");
        if (allSection) {
          // Expand the All section to load children
          await this.packagesProvider.getChildren(allSection);
          targetNode = this.packagesProvider.getNodeById(treeId);
        }
      }

      if (targetNode) {
        // Reveal the node and expand it
        await this.packagesView.reveal(targetNode, { select: true, focus: true, expand: true });
      }
    } catch (error) {
      console.error("Failed to reveal in tree:", error);
    }
  }

  /** Extract kind from node's contextValue */
  private getKindFromNode(node: any): string {
    const baseContextValue = node.contextValue?.replace(/:pinned$/, "");
    if (baseContextValue?.startsWith("fn:")) return "function";
    if (baseContextValue?.startsWith("type:")) return "type";
    if (baseContextValue?.startsWith("value:") || baseContextValue?.startsWith("const:")) return "value";
    return "module";
  }

  private async pinItem(node: any): Promise<void> {
    if (!node) return;

    const treeId = node.id.startsWith("pinned:") ? node.id.substring("pinned:".length) : node.id;
    const parts = treeId.split(".");
    const name = node.label || parts[parts.length - 1];
    const kind = this.getKindFromNode(node);

    await PinnedItemsService.pin({ treeId, name, kind });
  }

  private async unpinItem(node: any): Promise<void> {
    if (!node) return;

    const treeId = node.id.startsWith("pinned:") ? node.id.substring("pinned:".length) : node.id;
    await PinnedItemsService.unpin(treeId);
  }

  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.lookUpToplevel", async () => {
        try {
          const input = await vscode.window.showInputBox({
            prompt: "Enter the package element name",
            placeHolder: "e.g. Darklang.Stdlib.Option.Option",
          });

          if (!input) return;
          const virtualUri = vscode.Uri.parse(`dark:///package/${input}`);
          const doc = await vscode.workspace.openTextDocument(virtualUri);

          await vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false,
          });
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to read package: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.openPackageDefinition", async (packagePath: string, itemType?: "function" | "type" | "value" | "module") => {
        await this.openDarkFile(packagePath, itemType);
      }),

      vscode.commands.registerCommand("darklang.openFullModule", async (node: any) => {
        await this.openDarkFile(node?.id || "");
      }),

      vscode.commands.registerCommand("darklang.packages.pin", (node: any) => this.pinItem(node)),
      vscode.commands.registerCommand("darklang.packages.unpin", (node: any) => this.unpinItem(node)),
      vscode.commands.registerCommand("darklang.packages.refresh", () => {
        if (this.packagesProvider) {
          this.packagesProvider.refresh();
        }
      }),

      vscode.commands.registerCommand("darklang.revealInPackagesTree", async (treeId: string) => {
        await this.revealInTree(treeId);
      }),

      vscode.commands.registerCommand("darklang.showOpDiff", async (opData: any, location: any) => {
        try {
          // Parse the op to extract the definition
          const parsed = typeof opData === 'string' ? JSON.parse(opData) : opData;

          let itemName = location?.name || "Unknown";
          let itemType = "";

          // Determine item type
          if (parsed.SetTypeName) {
            itemType = "type";
          } else if (parsed.SetFnName) {
            itemType = "function";
          } else if (parsed.SetValueName) {
            itemType = "value";
          }

          // Read the actual module file to get the full definition
          const modulePath = [location.owner, ...location.modules].join('.');
          const moduleUri = vscode.Uri.parse(`darkfs:/${modulePath}.dark`);

          let definition = "";
          try {
            const doc = await vscode.workspace.openTextDocument(moduleUri);
            const fullContent = doc.getText();

            // Extract the specific definition for this item
            // Look for patterns like "type ItemName =", "let itemName", or "val itemName"
            const lines = fullContent.split('\n');
            let startIdx = -1;
            let endIdx = -1;
            let indentLevel = -1;

            // Find the start of the definition
            for (let i = 0; i < lines.length; i++) {
              const line = lines[i];
              const trimmed = line.trim();

              if (itemType === "type" && trimmed.startsWith(`type ${itemName}`)) {
                startIdx = i;
                indentLevel = line.length - line.trimStart().length;
                break;
              } else if (itemType === "function" && trimmed.startsWith(`let ${itemName}`)) {
                startIdx = i;
                indentLevel = line.length - line.trimStart().length;
                break;
              } else if (itemType === "value" && trimmed.startsWith(`val ${itemName}`)) {
                startIdx = i;
                indentLevel = line.length - line.trimStart().length;
                break;
              }
            }

            // Extract the definition (until we hit another top-level declaration or end of module)
            if (startIdx >= 0) {
              endIdx = startIdx;
              for (let i = startIdx + 1; i < lines.length; i++) {
                const line = lines[i];
                const trimmed = line.trim();
                const currentIndent = line.length - line.trimStart().length;

                // Stop if we hit another top-level declaration at the same indent level
                if (trimmed && currentIndent <= indentLevel &&
                    (trimmed.startsWith('type ') || trimmed.startsWith('let ') ||
                     trimmed.startsWith('val ') || trimmed.startsWith('module '))) {
                  break;
                }
                endIdx = i;
              }

              definition = lines.slice(startIdx, endIdx + 1).join('\n');
            }

            // If we couldn't extract, show a helpful message
            if (!definition) {
              const fullPath = [location.owner, ...location.modules, location.name].filter((p: string) => p).join('.');
              definition = `// Could not extract definition for ${itemType}: ${fullPath}\n\n// Full module content:\n${fullContent}`;
            }
          } catch (err) {
            console.error(`Failed to read module file:`, err);
            const fullPath = [location.owner, ...location.modules, location.name].filter((p: string) => p).join('.');
            definition = `// Error reading module file for ${itemType}: ${fullPath}\n\n${err}`;
          }

          // Use virtual document provider for diff view
          const diffId = crypto.randomUUID();
          const diffProvider = DiffContentProvider.getInstance();
          const { beforeUri, afterUri } = diffProvider.setDiffContent(diffId, "", definition);

          await vscode.commands.executeCommand(
            'vscode.diff',
            beforeUri,
            afterUri,
            `${itemName} (Added)`,
            { preview: false }
          );

        } catch (error) {
          console.error(`Failed to show op diff:`, error);
          vscode.window.showErrorMessage(`Failed to show diff: ${error instanceof Error ? error.message : "Unknown error"}`);
        }
      })
    ];
  }
}