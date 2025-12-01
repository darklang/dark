import * as vscode from "vscode";
import * as http from "http";
import * as https from "https";

export class HttpHandlerPanel {
  public static currentPanel: HttpHandlerPanel | undefined;

  public static readonly viewType = "darklang.httpHandler";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    // If we already have a panel, show it
    if (HttpHandlerPanel.currentPanel) {
      HttpHandlerPanel.currentPanel._panel.reveal(column);
      return;
    }

    // Otherwise, create a new panel
    const panel = vscode.window.createWebviewPanel(
      HttpHandlerPanel.viewType,
      "New HTTP Handler",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [extensionUri],
      },
    );

    HttpHandlerPanel.currentPanel = new HttpHandlerPanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    HttpHandlerPanel.currentPanel = new HttpHandlerPanel(panel, extensionUri);
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    // Set the webview's initial html content
    this._update();

    // Listen for when the panel is disposed
    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    // Handle messages from the webview
    this._panel.webview.onDidReceiveMessage(
      async message => {
        switch (message.command) {
          case "executeHandler":
            await this._executeHandler(
              message.method,
              message.route,
              message.expression,
            );
            return;
          case "saveHandler":
            await this._saveHandler(
              message.method,
              message.route,
              message.expression,
            );
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  private async _executeHandler(
    method: string,
    route: string,
    _expression: string,
  ) {
    // Build the URL for testing the handler
    // The Darklang canvas server runs on port 11001 with hostname pattern
    const baseUrl = "http://dark-packages.dlio.localhost:11001";
    const testUrl = new URL(route, baseUrl);

    const options: http.RequestOptions = {
      hostname: testUrl.hostname,
      port: testUrl.port || 11001,
      path: testUrl.pathname + testUrl.search,
      method: method,
      headers: {
        "Content-Type": "application/json",
      },
    };

    const httpModule = testUrl.protocol === "https:" ? https : http;

    const req = httpModule.request(options, res => {
      let data = "";

      res.on("data", chunk => {
        data += chunk;
      });

      res.on("end", () => {
        let responseBody: string;
        try {
          // Try to parse and pretty-print JSON
          const json = JSON.parse(data);
          responseBody = JSON.stringify(json, null, 2);
        } catch {
          responseBody = data;
        }

        // Collect headers
        const headers: Record<string, string> = {};
        for (const [key, value] of Object.entries(res.headers)) {
          if (typeof value === "string") {
            headers[key] = value;
          } else if (Array.isArray(value)) {
            headers[key] = value.join(", ");
          }
        }

        // Send response back to webview
        this._panel.webview.postMessage({
          command: "showResponse",
          status: res.statusCode || 0,
          statusText: res.statusMessage || "",
          headers: headers,
          body: responseBody,
        });
      });
    });

    req.on("error", error => {
      this._panel.webview.postMessage({
        command: "showError",
        error: error.message,
      });
    });

    req.end();
  }

  private async _saveHandler(
    method: string,
    route: string,
    expression: string,
  ) {
    // TODO: Implement saving the handler to the canvas
    // This would write to a .dark file with the handler definition
    vscode.window.showInformationMessage(
      `Handler ${method} ${route} saved! (implementation pending)`,
    );
  }

  public dispose() {
    HttpHandlerPanel.currentPanel = undefined;

    this._panel.dispose();

    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }

  private _update() {
    const webview = this._panel.webview;
    this._panel.webview.html = this._getHtmlForWebview(webview);
  }

  private _getHtmlForWebview(webview: vscode.Webview) {
    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>New HTTP Handler</title>
    <style>
        * {
            box-sizing: border-box;
            margin: 0;
            padding: 0;
        }

        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 20px;
        }

        .container {
            max-width: 1200px;
            margin: 0 auto;
        }

        .header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 24px;
            padding-bottom: 16px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .header h1 {
            font-size: 20px;
            font-weight: 500;
            display: flex;
            align-items: center;
            gap: 10px;
        }

        .header-icon {
            width: 24px;
            height: 24px;
            color: #8B5CF6;
        }

        .handler-form {
            display: flex;
            flex-direction: column;
            gap: 20px;
        }

        .form-row {
            display: flex;
            gap: 12px;
            align-items: flex-start;
        }

        .form-group {
            display: flex;
            flex-direction: column;
            gap: 6px;
        }

        .form-group.method {
            flex: 0 0 120px;
        }

        .form-group.route {
            flex: 1;
        }

        label {
            font-size: 12px;
            font-weight: 500;
            color: var(--vscode-descriptionForeground);
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        select, input, textarea {
            background-color: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            color: var(--vscode-input-foreground);
            border-radius: 4px;
            padding: 10px 12px;
            font-family: var(--vscode-editor-font-family);
            font-size: 14px;
        }

        select:focus, input:focus, textarea:focus {
            outline: none;
            border-color: var(--vscode-focusBorder);
        }

        select {
            cursor: pointer;
        }

        .expression-group {
            flex: 1;
        }

        .expression-editor {
            min-height: 200px;
            resize: vertical;
            font-family: var(--vscode-editor-font-family);
            line-height: 1.5;
            tab-size: 2;
        }

        .actions {
            display: flex;
            gap: 12px;
            margin-top: 8px;
        }

        .btn {
            padding: 10px 20px;
            border-radius: 4px;
            border: none;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            display: flex;
            align-items: center;
            gap: 8px;
            transition: all 0.15s;
        }

        .btn-primary {
            background-color: #8B5CF6;
            color: white;
        }

        .btn-primary:hover {
            background-color: #7C3AED;
        }

        .btn-secondary {
            background-color: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
            border: 1px solid var(--vscode-button-border);
        }

        .btn-secondary:hover {
            background-color: var(--vscode-button-secondaryHoverBackground);
        }

        .response-section {
            margin-top: 32px;
            border-top: 1px solid var(--vscode-panel-border);
            padding-top: 24px;
        }

        .response-header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            margin-bottom: 16px;
        }

        .response-header h2 {
            font-size: 16px;
            font-weight: 500;
        }

        .status-badge {
            padding: 4px 12px;
            border-radius: 12px;
            font-size: 12px;
            font-weight: 600;
        }

        .status-badge.success {
            background-color: rgba(34, 197, 94, 0.2);
            color: #22c55e;
        }

        .status-badge.error {
            background-color: rgba(239, 68, 68, 0.2);
            color: #ef4444;
        }

        .status-badge.redirect {
            background-color: rgba(234, 179, 8, 0.2);
            color: #eab308;
        }

        .response-content {
            background-color: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 6px;
            overflow: hidden;
        }

        .response-tabs {
            display: flex;
            border-bottom: 1px solid var(--vscode-panel-border);
        }

        .response-tab {
            padding: 10px 20px;
            cursor: pointer;
            font-size: 13px;
            color: var(--vscode-descriptionForeground);
            border-bottom: 2px solid transparent;
            transition: all 0.15s;
        }

        .response-tab:hover {
            color: var(--vscode-foreground);
        }

        .response-tab.active {
            color: var(--vscode-foreground);
            border-bottom-color: #8B5CF6;
        }

        .response-body {
            padding: 16px;
            font-family: var(--vscode-editor-font-family);
            font-size: 13px;
            line-height: 1.5;
            white-space: pre-wrap;
            word-break: break-word;
            max-height: 400px;
            overflow-y: auto;
        }

        .response-placeholder {
            color: var(--vscode-descriptionForeground);
            font-style: italic;
            padding: 40px;
            text-align: center;
        }

        .error-message {
            background-color: rgba(239, 68, 68, 0.1);
            border: 1px solid rgba(239, 68, 68, 0.3);
            color: #ef4444;
            padding: 16px;
            border-radius: 6px;
            margin-top: 16px;
        }

        .hidden {
            display: none;
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>
                <svg class="header-icon" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M22 12h-4l-3 9L9 3l-3 9H2"/>
                </svg>
                HTTP Handler
            </h1>
        </div>

        <div class="handler-form">
            <div class="form-row">
                <div class="form-group method">
                    <label for="method">Method</label>
                    <select id="method">
                        <option value="GET" selected>GET</option>
                        <option value="POST">POST</option>
                        <option value="PUT">PUT</option>
                        <option value="PATCH">PATCH</option>
                        <option value="DELETE">DELETE</option>
                    </select>
                </div>
                <div class="form-group route">
                    <label for="route">Route</label>
                    <input type="text" id="route" placeholder="/api/example/:id" value="/stats" />
                </div>
            </div>

            <div class="form-group expression-group">
                <label for="expression">Expression</label>
                <textarea id="expression" class="expression-editor" placeholder="Enter your Darklang expression here...">let stats = Builtin.pmGetStats ()

$"Package stats:
- types: {stats.types |> Builtin.int64ToString}
- fns: {stats.fns |> Builtin.int64ToString}
- values: {stats.values |> Builtin.int64ToString}"</textarea>
            </div>

            <div class="actions">
                <button class="btn btn-primary" id="executeBtn">
                    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <polygon points="5 3 19 12 5 21 5 3"/>
                    </svg>
                    Execute
                </button>
                <button class="btn btn-secondary" id="saveBtn">
                    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                        <path d="M19 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11l5 5v11a2 2 0 0 1-2 2z"/>
                        <polyline points="17 21 17 13 7 13 7 21"/>
                        <polyline points="7 3 7 8 15 8"/>
                    </svg>
                    Save Handler
                </button>
            </div>
        </div>

        <div class="response-section" id="responseSection">
            <div class="response-header">
                <h2>Response</h2>
                <span class="status-badge hidden" id="statusBadge"></span>
            </div>
            <div class="response-content">
                <div class="response-tabs">
                    <div class="response-tab active" data-tab="body">Body</div>
                    <div class="response-tab" data-tab="headers">Headers</div>
                </div>
                <div class="response-body" id="responseBody">
                    <div class="response-placeholder">Execute the handler to see the response</div>
                </div>
            </div>
            <div class="error-message hidden" id="errorMessage"></div>
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        const methodSelect = document.getElementById('method');
        const routeInput = document.getElementById('route');
        const expressionTextarea = document.getElementById('expression');
        const executeBtn = document.getElementById('executeBtn');
        const saveBtn = document.getElementById('saveBtn');
        const statusBadge = document.getElementById('statusBadge');
        const responseBody = document.getElementById('responseBody');
        const errorMessage = document.getElementById('errorMessage');

        let currentResponse = null;
        let currentTab = 'body';

        // Execute button handler
        executeBtn.addEventListener('click', () => {
            vscode.postMessage({
                command: 'executeHandler',
                method: methodSelect.value,
                route: routeInput.value,
                expression: expressionTextarea.value
            });

            // Show loading state
            responseBody.innerHTML = '<div class="response-placeholder">Executing...</div>';
            statusBadge.classList.add('hidden');
            errorMessage.classList.add('hidden');
        });

        // Save button handler
        saveBtn.addEventListener('click', () => {
            vscode.postMessage({
                command: 'saveHandler',
                method: methodSelect.value,
                route: routeInput.value,
                expression: expressionTextarea.value
            });
        });

        // Tab switching
        document.querySelectorAll('.response-tab').forEach(tab => {
            tab.addEventListener('click', () => {
                document.querySelectorAll('.response-tab').forEach(t => t.classList.remove('active'));
                tab.classList.add('active');
                currentTab = tab.getAttribute('data-tab');
                updateResponseDisplay();
            });
        });

        // Handle messages from extension
        window.addEventListener('message', event => {
            const message = event.data;
            switch (message.command) {
                case 'showResponse':
                    currentResponse = message;
                    errorMessage.classList.add('hidden');

                    // Update status badge
                    statusBadge.textContent = message.status + ' ' + message.statusText;
                    statusBadge.classList.remove('hidden', 'success', 'error', 'redirect');
                    if (message.status >= 200 && message.status < 300) {
                        statusBadge.classList.add('success');
                    } else if (message.status >= 300 && message.status < 400) {
                        statusBadge.classList.add('redirect');
                    } else {
                        statusBadge.classList.add('error');
                    }

                    updateResponseDisplay();
                    break;

                case 'showError':
                    currentResponse = null;
                    statusBadge.classList.add('hidden');
                    responseBody.innerHTML = '<div class="response-placeholder">Request failed</div>';
                    errorMessage.textContent = message.error;
                    errorMessage.classList.remove('hidden');
                    break;
            }
        });

        function updateResponseDisplay() {
            if (!currentResponse) return;

            if (currentTab === 'body') {
                responseBody.textContent = currentResponse.body || '(empty response)';
            } else if (currentTab === 'headers') {
                const headers = currentResponse.headers || {};
                const headerLines = Object.entries(headers)
                    .map(([key, value]) => key + ': ' + value)
                    .join('\\n');
                responseBody.textContent = headerLines || '(no headers)';
            }
        }

        // Handle keyboard shortcut for execute
        document.addEventListener('keydown', (e) => {
            if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
                executeBtn.click();
            }
        });
    </script>
</body>
</html>`;
  }
}
