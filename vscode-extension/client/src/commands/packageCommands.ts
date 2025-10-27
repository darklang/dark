import * as vscode from "vscode";
import * as os from "os";
import * as path from "path";
import * as fs from "fs";

export class PackageCommands {
  register(): vscode.Disposable[] {
    return [
      vscode.commands.registerCommand("darklang.lookUpToplevel", async () => {
        try {
          const input = await vscode.window.showInputBox({
            prompt: "Enter the package element name",
            placeHolder: "e.g. Darklang.Stdlib.Option.Option",
          });

          if (!input) return;
          const packageName = input.split('.').pop() || input;
          const virtualUri = vscode.Uri.parse(`dark://package/${input.replace(/\./g, '/')}/${packageName}.darklang`);
          const doc = await vscode.workspace.openTextDocument(virtualUri);

          await vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false,
          });
        } catch (error) {
          vscode.window.showErrorMessage(`Failed to read package: ${error}`);
        }
      }),

      vscode.commands.registerCommand("darklang.openPackageDefinition", async (packagePath: string) => {
        try {
          const virtualUri = vscode.Uri.parse(`darkfs:/${packagePath}.dark`);
          const doc = await vscode.workspace.openTextDocument(virtualUri);

          // Set the language to darklang for syntax highlighting
          await vscode.languages.setTextDocumentLanguage(doc, 'darklang');

          await vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false,
          });
        } catch (error) {
          console.error(`Failed to open package definition for ${packagePath}:`, error);
          vscode.window.showErrorMessage(`Failed to open ${packagePath}: ${error instanceof Error ? error.message : "Unknown error"}`);
        }
      }),

      vscode.commands.registerCommand("darklang.openFullModule", async (node: any) => {
        try {
          const modulePath = node?.id || "";
          const virtualUri = vscode.Uri.parse(`darkfs:/${modulePath}.dark`);
          const doc = await vscode.workspace.openTextDocument(virtualUri);

          await vscode.languages.setTextDocumentLanguage(doc, 'darklang');

          await vscode.window.showTextDocument(doc, {
            preview: false,
            preserveFocus: false,
          });
        } catch (error) {
          console.error(`Failed to open full module:`, error);
          vscode.window.showErrorMessage(`Failed to open module: ${error instanceof Error ? error.message : "Unknown error"}`);
        }
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

          // Create temporary files for diff view
          const tmpDir = os.tmpdir();
          const beforePath = path.join(tmpDir, `before-${itemName}.dark`);
          const afterPath = path.join(tmpDir, `after-${itemName}.dark`);

          // Write empty before file
          fs.writeFileSync(beforePath, "");

          // Write after file with the actual definition
          fs.writeFileSync(afterPath, definition);

          // Open diff view
          const beforeUri = vscode.Uri.file(beforePath);
          const afterUri = vscode.Uri.file(afterPath);

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