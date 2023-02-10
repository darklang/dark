import * as vscode from "vscode";
import axios from "axios";
import { CanvasElementsTreeProvider } from "./CanvasElementsTree";
import { spawn } from "node:child_process";
import * as fs from "fs";
import { DarkHandlerPanel } from "./DarkHandlerPanel";
import { getWebviewOptions } from "./utils";
import { viewTypes } from "./ViewTypes";

type Repl = {
  id: string;
  name: string;
  code: string;
  result: string;
};
type GlobalState = {
  mainRepl: Repl | undefined;
};

const globalState: GlobalState = {
  mainRepl: undefined,
};

let panel: vscode.WebviewPanel | undefined = undefined;

export async function activate(context: vscode.ExtensionContext) {
  // fetch config vals
  // const config = vscode.workspace.getConfiguration('darklang');
  // vscode.window.showInformationMessage(`Fetched some config vals: ${config.get("chatGptKey")}`);

  var statusBarItem = vscode.window.createStatusBarItem(
    vscode.StatusBarAlignment.Right,
    100,
  );
  statusBarItem.text = "todo: remove some darkness";
  statusBarItem.command = "darklang-vscode-editor.codeGenPrompt";
  statusBarItem.show();

  // Set up fakey tree view of canvas elements
  const canvasExplorerId = "tree-canvas-explorer";
  // todo: what's the difference between these two?
  // vscode.window.registerTreeDataProvider(
  // 	canvasExplorerId,
  // 	new CanvasElementsTreeProvider()
  // );
  vscode.window.createTreeView(canvasExplorerId, {
    treeDataProvider: new CanvasElementsTreeProvider(),
  });

  // Fetch, save, and start the interpreter
  const fetchedInterpreterScript = await axios.get(
    "https://stachu-scratch.builtwithdark.com/interpreter.sh",
  );
  const interpreterScriptName = "./interpreter.sh";
  fs.writeFileSync(interpreterScriptName, fetchedInterpreterScript.data);
  fs.chmodSync(interpreterScriptName, "755"); // executable
  const ls = spawn(interpreterScriptName);
  ls.stdout.on("data", data => {
    vscode.window.showInformationMessage(`eval response: ${data}`);
  });
  ls.stderr.on("data", data => {
    vscode.window.showInformationMessage(`interpreter error: ${data}`);
  });
  ls.on("close", _code => {});

  // register command
  let codeGenPromptCommand = vscode.commands.registerCommand(
    "darklang-vscode-editor.codeGenPrompt",
    async () => {
      const test = await vscode.window.showInputBox({
        prompt: "Enter your code-gen prompt",
      });
      ls.send(test || "");
    },
  );
  context.subscriptions.push(codeGenPromptCommand);

  globalState.mainRepl = { id: "123", name: "", code: "1+2", result: "" };

  // show handler panel
  context.subscriptions.push(
    vscode.commands.registerCommand("darklang.showHandlerPanel", () => {
      DarkHandlerPanel.createOrShow(context.extensionUri);
    }),
  );

  if (vscode.window.registerWebviewPanelSerializer) {
    // Make sure we register a serializer in activation event
    vscode.window.registerWebviewPanelSerializer(viewTypes.darkHandlerPanel, {
      async deserializeWebviewPanel(
        webviewPanel: vscode.WebviewPanel,
        state: any,
      ) {
        console.log(`Got state: ${state}`);
        // Reset the webview options so we use latest uri for `localResourceRoots`.
        webviewPanel.webview.options = getWebviewOptions(context.extensionUri);
        DarkHandlerPanel.revive(webviewPanel, context.extensionUri);
      },
    });
  }

  vscode.window.showInformationMessage(
    "Darklang extension has been activated!",
  );
}

export function deactivate() {
  if (panel) {
    panel.dispose();
  }
}
