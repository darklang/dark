import { workspace, ExtensionContext } from "vscode";

import * as vscode from "vscode";
import { SemanticTokensFeature } from "vscode-languageclient/lib/common/semanticTokens";
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  Trace,
  TransportKind,
} from "vscode-languageclient/node";

let client: LanguageClient;

export function activate(context: ExtensionContext) {
  const sharedServerOptions = {
    options: { cwd: "/home/dark/app" },
    command: "bash",
    args: [
      "./scripts/run-cli",
      "./user-code/darklang/scripts/language-server.dark",
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

  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
