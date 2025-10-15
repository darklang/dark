import * as vscode from "vscode";

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
          const virtualUri = vscode.Uri.parse(`dark:///package/${packagePath}`);
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
          const virtualUri = vscode.Uri.parse(`dark:///package/module/${modulePath}`);
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
      })
    ];
  }
}