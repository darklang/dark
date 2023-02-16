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
  statusBarItem.text = "dl: created tree view";

  // faking this until Paul tidies something up
  let latestExecutorHash = await Executor.latestExecutorHash();
  statusBarItem.text = "dl: latest executor is " + latestExecutorHash;

  // LightTODO: it's possible for us to end in a state where the spawned dark-executor
  // process hasn't been properly cleaned up. You may have to manually kill this running process.
  // I'm not sure why the spawned process isn't always cleaned up properly - thought
  // the deactivate() fn would clear that up. If you're trying to work on something else
  // and are hitting issues here, comment out the next few (~3) lines of code.
  //

  vscode.workspace.fs.createDirectory(context.globalStorageUri);

  let executorUri = await Executor.downloadExecutor(
    context.globalStorageUri,
    latestExecutorHash,
  );
  statusBarItem.text = "dl: downloaded executor";
  let executorHttpServerPort = "3275";
  console.log(
    `executorPath: ${executorUri}`,
    `executorHttpServerPort: ${executorHttpServerPort}`,
  );
  await Executor.startExecutorHttpServer(executorUri, executorHttpServerPort);

  // LightTODO: for some reason, the HTTP request here fails with
  // request to http://localhost:3275/api/v0/execute-text failed,
  // reason: connect ECONNREFUSED 127.0.0.1:3275.
  // I'm not sure why.
  //
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
