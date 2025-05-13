import {
  workspace,
  ExtensionContext,
  commands,
  window,
  Uri,
  FileSystemProvider,
  FileType,
  EventEmitter,
} from "vscode";
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

interface LSPFileResponse {
  content: string;
}

class SimpleRemoteFileProvider implements FileSystemProvider {
  private _emitter = new EventEmitter<vscode.FileChangeEvent[]>();
  readonly onDidChangeFile = this._emitter.event;

  static fileContents = new Map<string, Uint8Array>();

  async readFile(uri: Uri): Promise<Uint8Array> {
    try {
      // Check the cache first; if the content is available, return it directly without making a request to the LSP server
      const cachedContent = SimpleRemoteFileProvider.fileContents.get(
        uri.toString(),
      );
      if (cachedContent) {
        return cachedContent;
      }

      // Forward the request to LSP and get the response
      const response = await client.sendRequest<LSPFileResponse>(
        "fileSystem/read",
        {
          uri: uri.toString(),
        },
      );

      if (!response || typeof response.content !== "string") {
        throw new Error("Invalid response from LSP server");
      }

      // Convert response content to Uint8Array
      const content = new TextEncoder().encode(response.content);
      // Cache the content of the remote file for future reads
      SimpleRemoteFileProvider.fileContents.set(uri.toString(), content);
      return content;
    } catch (error) {
      console.error("LSP read request failed:", error);
      throw vscode.FileSystemError.FileNotFound(uri);
    }
  }

  async writeFile(
    uri: Uri,
    content: Uint8Array,
    options: { create: boolean; overwrite: boolean },
  ): Promise<void> {
    try {
      // Convert the Uint8Array content to a string
      const contentString = new TextDecoder().decode(content);

      // Send write request to LSP server
      await client.sendRequest("fileSystem/write", {
        uri: uri.toString(),
        content: contentString,
        options: {
          create: options.create,
          overwrite: options.overwrite,
        },
      });

      // Update the file content with the new content
      SimpleRemoteFileProvider.fileContents.set(uri.toString(), content);
    } catch (error) {
      console.error("LSP write request failed:", error);

      if (!options.create) {
        throw vscode.FileSystemError.FileNotFound(uri);
      }
      if (!options.overwrite) {
        throw vscode.FileSystemError.FileExists(uri);
      }

      throw vscode.FileSystemError.Unavailable(uri);
    }
  }
  watch(): vscode.Disposable {
    return new vscode.Disposable(() => {});
  }
  stat(): vscode.FileStat {
    return { type: FileType.File, ctime: 0, mtime: 0, size: 0 };
  }
  readDirectory(): [string, FileType][] {
    return [];
  }
  createDirectory(): void {}

  delete(): void {}
  rename(): void {}
}

export function activate(context: ExtensionContext) {
  const isDebugMode = () => process.env.VSCODE_DEBUG_MODE === "true";

  const sharedServerOptions = {
    options: { cwd: "/home/dark/app" },
    command: "bash",
    args: [
      isDebugMode ? "./scripts/run-cli" : "darklang",
      "--skip-self-update",
      "@PACKAGE.Darklang.LanguageTools.LspServer.runServerCli",
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
      { scheme: "darklang", language: "darklang" },
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
      let scriptCommand = `cd /home/dark/app && { TIMEFORMAT=$'Script executed in: %3lR'; time ./scripts/run-cli "${filePath}" --skip-self-update; }`;

      terminal.sendText(scriptCommand, true);
    } else {
      let scriptCommand = `cd ${os.homedir()} && { TIMEFORMAT=$'Script executed in: %3lR'; time darklang "${filePath}" --skip-self-update; }`;
      terminal.sendText(scriptCommand, true);
    }
  });

  let testRemoteFileCommand = commands.registerCommand(
    "darklang.testRemoteFile",
    async () => {
      try {
        const input = await window.showInputBox({
          prompt: "Enter the GitHub raw URL",
          placeHolder: "e.g. type/Darklang/Stdlib/Option/Option",
        });

        if (!input) return;
        const virtualUri = Uri.parse(`darklang://${input}.dark`);
        const doc = await workspace.openTextDocument(virtualUri);

        try {
          await window.showTextDocument(doc, {
            preview: false, // open in a new tab instead of preview mode
            preserveFocus: false, // give focus to the new tab
          });
        } catch (error) {
          console.error("Error showing document:", error);
          throw error;
        }
      } catch (error) {
        window.showErrorMessage(`Failed to read remote file: ${error}`);
      }
    },
  );

  const provider = new SimpleRemoteFileProvider();
  const registration = workspace.registerFileSystemProvider(
    "darklang",
    provider,
    {
      isCaseSensitive: true,
    },
  );

  context.subscriptions.push(registration);
  context.subscriptions.push(testRemoteFileCommand);
  context.subscriptions.push(disposable);
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
