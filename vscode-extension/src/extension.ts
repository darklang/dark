import * as vscode from "vscode";
import { CanvasElementsTreeProvider } from "./CanvasElementsTree";
import { DarkHandlerPanel } from "./DarkHandlerPanel";
import { getWebviewOptions } from "./utils";
import { viewTypes } from "./ViewTypes";
import * as Executor from "./Executor";

type Repl = {
  id: string;
  name: string;
  code: string;
};
type GlobalState = {
  mainRepl: Repl | undefined;
};

const globalState: GlobalState = {
  mainRepl: undefined,
};

export async function activate(context: vscode.ExtensionContext) {
  // fetch config vals
  // const extensionConfig = vscode.workspace.getConfiguration('darklang');
  // const chatGptKey = extensionConfig.get("chatGptKey");

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

  // faking this until Paul tidies something up
  let latestExecutorHash = "526c07b8f"; //await Executor.latestExecutorHash();

  // TODO: don't download if we already have latest on disk
  let executorPath = await Executor.downloadExecutor(latestExecutorHash);
  let executorHttpServerPort = "3275";
  await Executor.startExecutorHttpServer(executorPath, executorHttpServerPort);

  // const z = await Executor.evalSomeCodeAgainstHttpServer(
  //   executorHttpServerPort,
  //   "1+2",
  // );
  // vscode.window.showInformationMessage(`eval response: ${z}`);

  // register command
  let codeGenPromptCommand = vscode.commands.registerCommand(
    "darklang-vscode-editor.evalCode",
    async () => {
      const test = await vscode.window.showInputBox({
        prompt: "Enter your Dark code to evaluate",
      });
      // const response = await Executor.evalSomeCodeAgainstHttpServer(
      //   executorHttpServerPort,
      //   test || "",
      // );
      // vscode.window.showInformationMessage(`eval response: ${response}`);
    },
  );
  context.subscriptions.push(codeGenPromptCommand);

  globalState.mainRepl = { id: "123", name: "", code: "1+2" };

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

export async function deactivate() {
  await Executor.stopExecutor();
}
