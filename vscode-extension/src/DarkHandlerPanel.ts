import * as vscode from "vscode";
import { getNonce, getWebviewOptions } from "./utils";
import { viewTypes } from "./ViewTypes";

// This is a trimmed-down version of the official 'sample' for a WebView.
// https://github.com/microsoft/vscode-extension-samples/tree/main/webview-sample
//
// Ideally, we'd manage state centrally and not manage the panel/state as this is currently doing.
// (whether that state is managed in Dark or in JS, not sure)
// A few things make this a bit awkward. Some notes:
// - panels disappear once they're not visible.
//   so, if you have 2 tabs in a panel, one of them being this webview,
//   and you switch between the tabs back and forth, the webview is totally reset when you
//   come back to it. So, in-progress user-input etc is lost.
// - this^ is handled in extensions by
//   - registering a WebviewPanelSerializer in extension.ts
//   - using `vscode.getState()` and `vscode.setState({ count: currentCount })`
//     in the corresponding `main.js`.
//   - some other bits that I don't fully understand, related to
// - I still think it's possible to centrally wrangle the state, but I've tried a
//   few times and have failed, so will leave the next attempt for another day/person.
//
// Some other notes/learnings about working with Webviews in VS Code extensions:
// - This demonstrates how they recommend to host and reference css/js resources
//   I suppose we'll get away from this system in time, but that's unclear to me
//   and I think trying to write and hook this up from scratch would be rather painful
//   so let's leave in what's here for now, even if unused.
// - The HTML set in a Webview is expected to have a few security meaures in place:
//   - Some combination of `nonce` and a `Content-Security-Policy` <meta> tag
//     (I don't fully understand what's going on here)
//   - When creating the Webview, set the `options` of the webview to have a
//     property `localResourceRoots` set to only allow loading content from
//     the `media` folder in our extension's directory/workspace.
//     (see extension.ts, along with utils.ts/getWebviewOptions())
// - I haven't yet figured out how to have multiple instances of a panel safely,
//   and to manage the state of such. I imagine it'll be best to figure out state-
//   management first, and then follow up by allowing 2 of the same Webview type
//   after that.

export class DarkHandlerPanel {
  public static currentPanel: DarkHandlerPanel | undefined;

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, show it.
    if (DarkHandlerPanel.currentPanel) {
      DarkHandlerPanel.currentPanel._panel.reveal(column);
      return;
    }

    // Otherwise, create a new panel.
    const panel = vscode.window.createWebviewPanel(
      viewTypes.darkHandlerPanel,
      "Dark Handler",
      column || vscode.ViewColumn.One,
      getWebviewOptions(extensionUri),
    );

    DarkHandlerPanel.currentPanel = new DarkHandlerPanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    DarkHandlerPanel.currentPanel = new DarkHandlerPanel(panel, extensionUri);
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    // Set the webview's initial html content
    this._update();

    // This happens when the user closes the panel or when the panel is closed programmatically
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Update the content based on view changes
    this._panel.onDidChangeViewState(
      e => {
        if (this._panel.visible) {
          this._update();
        }
      },
      null,
      this._disposables,
    );

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      message => {
        switch (message.command) {
          case "alert":
            vscode.window.showErrorMessage(message.text);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  // this isn't used right now, but shows off how
  // messages can be passed to the webview
  public demoPostToWebview() {
    // Send a message to the webview webview.
    // You can send any JSON serializable data.
    this._panel.webview.postMessage({ command: "refactor" });
  }

  public dispose() {
    DarkHandlerPanel.currentPanel = undefined;

    // Clean up our resources
    this._panel.dispose();

    while (this._disposables.length) {
      const x = this._disposables.pop();
      if (x) {
        x.dispose();
      }
    }
  }

  private _update() {
    const webview = this._panel.webview;

    this._panel.title = "Dark Handler";
    this._panel.webview.html = this._getHtmlForWebview(webview);
  }

  private _getHtmlForWebview(webview: vscode.Webview) {
    // Local path to main script run in the webview
    const scriptPathOnDisk = vscode.Uri.joinPath(
      this._extensionUri,
      "media",
      "main.js",
    );

    // And the uri we use to load this script in the webview
    const scriptUri = webview.asWebviewUri(scriptPathOnDisk);

    // Local path to css styles
    const styleResetPath = vscode.Uri.joinPath(
      this._extensionUri,
      "media",
      "reset.css",
    );
    const stylesPathMainPath = vscode.Uri.joinPath(
      this._extensionUri,
      "media",
      "vscode.css",
    );

    // Uri to load styles into webview
    const stylesResetUri = webview.asWebviewUri(styleResetPath);
    const stylesMainUri = webview.asWebviewUri(stylesPathMainPath);

    // Use a nonce to only allow specific scripts to be run
    const nonce = getNonce();

    return `<!DOCTYPE html>
			<html lang="en">
			<head>
				<meta charset="UTF-8">
				<meta http-equiv="Content-Security-Policy"
				      content="default-src 'none'; style-src ${webview.cspSource}; img-src ${webview.cspSource} https:; script-src 'nonce-${nonce}';">
				<meta name="viewport" content="width=device-width, initial-scale=1.0">

				<link href="${stylesResetUri}" rel="stylesheet">
				<link href="${stylesMainUri}" rel="stylesheet">

				<title>Dark Handler</title>
			</head>
			<body>
				<h1 id="lines-of-code-counter">0</h1>
				<script nonce="${nonce}" src="${scriptUri}"></script>
			</body>
			</html>`;
  }
}
