import * as vscode from 'vscode';
import { CanvasElementsTreeProvider } from './CanvasElementsTree';

let globalDarkState = {
	canvasName: "dark-editor",
	responseFromInitialLoad: null, // for now
	chatGptKey: "SUPER SECRET - how do vs code devs do this?",

	references: {
		theStatusBar: null
	}
};

export function activate(context: vscode.ExtensionContext) {
	console.log('Congratulations, your extension "darklang-vscode-editor" is now active!');

	// The command has been defined in the package.json file
	// The commandId parameter must match the command field in package.json
	let helloWorldCommand = vscode.commands.registerCommand('darklang-vscode-editor.helloWorld', async () => {
		const answer = await vscode.window.showInformationMessage('Hello World from darklang-vscode-editor!', "A", "b");

		console.log("answer", answer);
	});

	vscode.window.showInformationMessage('Hello, Darklang user!');

	var statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
	statusBarItem.text = "todo: remove some darkness";
	statusBarItem.show();


	context.subscriptions.push(helloWorldCommand);


	const canvasExplorerId = 'tree-canvas-explorer';
	// todo: what's the difference between these two?
	// vscode.window.registerTreeDataProvider(
	// 	canvasExplorerId,
	// 	new CanvasElementsTreeProvider()
	// );
	vscode.window.createTreeView(canvasExplorerId, {
		treeDataProvider: new CanvasElementsTreeProvider()
	});
}

export function deactivate() { }
