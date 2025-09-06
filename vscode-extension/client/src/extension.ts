import { workspace, ExtensionContext, commands, window, Uri } from "vscode";
import * as os from "os";
import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Trace,
  TransportKind,
} from "vscode-languageclient/node";

import { ServerBackedTreeDataProvider } from "./ServerBackedTreeDataProvider";
import { DarkFS } from "./fileSystemProvider";

let client: LanguageClient;

interface LSPFileResponse {
  content: string;
}

export function activate(context: ExtensionContext) {
  const isDebugMode = () => process.env.VSCODE_DEBUG_MODE === "true";

  const sharedServerOptions = {
    options: { cwd: "/home/dark/app" },
    command: "bash",
    args: [
      isDebugMode ? "./scripts/run-cli" : "darklang",
      "run",
      "@Darklang.LanguageTools.LspServer.runServerCli",
      "()", // 'parses' to () - TODO clean this up once we switch over to new parser
    ],
    transport: TransportKind.stdio,
  };
  const serverOptions: ServerOptions = {
    run: sharedServerOptions,
    debug: sharedServerOptions,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [
      { scheme: "file", language: "darklang" },
      { scheme: "darkfs", language: "darklang" },
    ],
    synchronize: {
      fileEvents: workspace.createFileSystemWatcher("**/*.dark"),
    },

    // in the window that has the extension loaded, go to the Output tab,
    // and select this option in the dropdown to find corresponding logs
    traceOutputChannel: vscode.window.createOutputChannel(
      "Darklang LSP - Client",
    ),

    // without this, VS Code will try to restart our extension 5 times,
    // which can be really annoying while debugging
    connectionOptions: {
      cancellationStrategy: null,
      maxRestartCount: 0,
    },
  };

  // start the LSP client -- note: this will also launch the server
  client = new LanguageClient(
    "darklangLsp",
    "Darklang LSP - Server",
    serverOptions,
    clientOptions,
  );

  client.registerFeature(new SemanticTokensFeature(client));
  client.trace = Trace.Verbose;

  // Create tree view immediately with static root nodes
  const treeDataProvider = new ServerBackedTreeDataProvider(client);
  let view = vscode.window.createTreeView(`darklangTreeView`, {
    treeDataProvider: treeDataProvider,
    showCollapseAll: true,
  });
  context.subscriptions.push(view);

  // Register refresh command
  context.subscriptions.push(
    commands.registerCommand('darklang.refreshTreeView', () => {
      treeDataProvider.refresh();
    })
  );

  client.start();

  let disposable = commands.registerCommand("darklang.runScript", async () => {
    const editor = window.activeTextEditor;
    const uri = editor.document.uri;
    let scriptPath: string;

    if (uri.scheme === "darkfs") {
      // For virtual files, get the temp file path
      const content = editor.document.getText();
      const contentBytes = new TextEncoder().encode(content);

      // Write/update the file content which will create a temp file
      await provider.writeFile(uri, contentBytes, {
        create: true,
        overwrite: true,
      });

      // Get the temp file path
      const tempPath = DarkFS.getTempFilePath(uri);
      if (!tempPath) {
        window.showErrorMessage(
          "Failed to create temporary file for script execution",
        );
        return;
      }
      scriptPath = tempPath;
    } else {
      // For regular files, use the actual path
      scriptPath = uri.fsPath;
    }

    let terminal = window.terminals.find(t => t.name === "darklang-terminal");

    if (!terminal) {
      terminal = window.createTerminal(`darklang-terminal`);
    }

    terminal.show(true);

    if (isDebugMode()) {
      let scriptCommand = `cd /home/dark/app && { TIMEFORMAT=$'Script executed in: %3lR'; time ./scripts/run-cli "${scriptPath}" --skip-self-update; }`;

      terminal.sendText(scriptCommand, true);
    } else {
      let scriptCommand = `cd ${os.homedir()} && { TIMEFORMAT=$'Script executed in: %3lR'; time darklang "${scriptPath}" --skip-self-update; }`;
      terminal.sendText(scriptCommand, true);
    }
  });

  let lookUpToplevelCommand = commands.registerCommand(
    "darklang.lookUpToplevel",
    async () => {
      try {
        const input = await window.showInputBox({
          prompt: "Enter the package element name",
          placeHolder: "e.g. Darklang.Stdlib.Option.Option",
        });

        if (!input) return;
        const virtualUri = Uri.parse(`darkfs:/${input}.dark`);
        const doc = await workspace.openTextDocument(virtualUri);

        await window.showTextDocument(doc, {
          preview: false, // open in a new tab instead of preview mode
          preserveFocus: false, // give focus to the new tab
        });
      } catch (error) {
        window.showErrorMessage(`Failed to read remote file: ${error}`);
      }
    },
  );

  let openPackageDefinitionCommand = commands.registerCommand(
    "darklang.openPackageDefinition",
    async (packagePath: string) => {
      try {
        const virtualUri = Uri.parse(`darkfs:/${packagePath}.dark`);
        const doc = await workspace.openTextDocument(virtualUri);

        await window.showTextDocument(doc, {
          preview: false,
          preserveFocus: false,
        });
      } catch (error) {
        console.error(
          `Failed to open package definition for ${packagePath}:`,
          error,
        );
        window.showErrorMessage(
          `Failed to open ${packagePath}: ${
            error instanceof Error ? error.message : "Unknown error"
          }`,
        );
      }
    },
  );

  let openFullModuleCommand = commands.registerCommand(
    "darklang.openFullModule",
    async (node: any) => {
      try {
        // Get the module path from the node
        const modulePath = node?.id || "";
        const virtualUri = Uri.parse(`darkfs:/module/${modulePath}.dark`);
        const doc = await workspace.openTextDocument(virtualUri);

        await window.showTextDocument(doc, {
          preview: false,
          preserveFocus: false,
        });
      } catch (error) {
        console.error(`Failed to open full module:`, error);
        window.showErrorMessage(
          `Failed to open module: ${
            error instanceof Error ? error.message : "Unknown error"
          }`,
        );
      }
    },
  );

  const provider = new DarkFS(client);
  const registration = workspace.registerFileSystemProvider(
    "darkfs",
    provider,
    {
      isCaseSensitive: true,
      isReadonly: false,
    },
  );

  let initWorkspace = commands.registerCommand("darklang.init", async () => {
    try {
      // Create the sample folder URI
      const folderUri = Uri.parse("darkfs:/DarkFS-Sample");
      const fileUri = Uri.parse("darkfs:/DarkFS-Sample/edit.dark");

      try {
        await provider.createDirectory(folderUri);
      } catch (error) {
        console.log("Error creating directory:", error);
      }

      const content = new TextEncoder().encode("");

      // Write the file
      await provider.writeFile(fileUri, content, {
        create: true,
        overwrite: true,
      });

      // Open the file in the editor
      const document = await workspace.openTextDocument(fileUri);
      await window.showTextDocument(document);

      window.showInformationMessage("edit file created successfully!");
    } catch (error) {
      window.showErrorMessage(`Failed to create file: ${error}`);
    }
  });

  context.subscriptions.push(initWorkspace);
  context.subscriptions.push(registration);
  context.subscriptions.push(lookUpToplevelCommand);
  context.subscriptions.push(openPackageDefinitionCommand);
  context.subscriptions.push(openFullModuleCommand);
  context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
