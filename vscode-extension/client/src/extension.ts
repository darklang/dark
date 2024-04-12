import { workspace, ExtensionContext, commands, window } from "vscode";
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

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const isDebugMode = () => process.env.VSCODE_DEBUG_MODE === "true";

  const sharedServerOptions = {
    options: { cwd: "/home/dark/app" },
    command: "bash",
    args: [
      isDebugMode ? "./scripts/run-cli" : "darklang",
      "--skip-self-update",
      "@PACKAGE.Darklang.LanguageTools.LspServer.runServerCli",
      "null", // 'parses' to () - TODO clean this up once we switch over to new parser
    ],
    transport: TransportKind.stdio,
  };
  const serverOptions: ServerOptions = {
    run: sharedServerOptions,
    debug: sharedServerOptions,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "darklang" }],
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

  client
    .onReady()
    .then(() => {
      // TODO: only when initialized...
      //client.onNotification("initialized", () => {
      let view = vscode.window.createTreeView(`darklangTreeView`, {
        treeDataProvider: new ServerBackedTreeDataProvider(client),
      });
      context.subscriptions.push(view);
      //});
    })
    .catch(e => {
      console.error(e);
    });

  client.start();

  let disposable = commands.registerCommand("darklang.runScript", () => {
    const editor = window.activeTextEditor;
    if (!editor) {
      window.showWarningMessage(
        "No active editor! Please open a file to run the script.",
      );
      return;
    }

    const filePath = editor.document.uri.fsPath;

    let terminal = window.terminals.find(t => t.name === "darklang-terminal");

    if (!terminal) {
      terminal = window.createTerminal(`darklang-terminal`);
    }

    terminal.show(true);

    if (isDebugMode()) {
      let changeDirCommand = `cd /home/dark/app`;
      let scriptCommand = `./scripts/run-cli  "${filePath}" --skip-self-update`;
      terminal.sendText(changeDirCommand, true);
      terminal.sendText(scriptCommand, true);
    } else {
      let changeDirCommand = `cd ${os.homedir()}`;
      let scriptCommand = `darklang "${filePath}" --skip-self-update`;
      terminal.sendText(changeDirCommand, true);
      terminal.sendText(scriptCommand, true);
    }
  });

  context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
