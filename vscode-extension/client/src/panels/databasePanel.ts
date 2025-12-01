import * as vscode from "vscode";
import * as http from "http";

export class DatabasePanel {
  public static currentPanel: DatabasePanel | undefined;

  public static readonly viewType = "darklang.database";

  private readonly _panel: vscode.WebviewPanel;
  private readonly _extensionUri: vscode.Uri;
  private _disposables: vscode.Disposable[] = [];

  public static createOrShow(extensionUri: vscode.Uri) {
    const column = vscode.window.activeTextEditor
      ? vscode.window.activeTextEditor.viewColumn
      : undefined;

    if (DatabasePanel.currentPanel) {
      DatabasePanel.currentPanel._panel.reveal(column);
      return;
    }

    const panel = vscode.window.createWebviewPanel(
      DatabasePanel.viewType,
      "New Database",
      column || vscode.ViewColumn.One,
      {
        enableScripts: true,
        localResourceRoots: [extensionUri],
      },
    );

    DatabasePanel.currentPanel = new DatabasePanel(panel, extensionUri);
  }

  public static revive(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    DatabasePanel.currentPanel = new DatabasePanel(panel, extensionUri);
  }

  private constructor(panel: vscode.WebviewPanel, extensionUri: vscode.Uri) {
    this._panel = panel;
    this._extensionUri = extensionUri;

    this._update();

    this._panel.onDidDispose(() => this.dispose(), null, this._disposables);

    this._panel.webview.onDidReceiveMessage(
      async message => {
        switch (message.command) {
          case "createDB":
            await this._createDB(message.dbName, message.typeDef);
            return;
          case "setValue":
            await this._setValue(message.dbName, message.key, message.value);
            return;
          case "getValue":
            await this._getValue(message.dbName, message.key);
            return;
          case "getAllValues":
            await this._getAllValues(message.dbName);
            return;
        }
      },
      null,
      this._disposables,
    );
  }

  private _makeRequest(path: string): Promise<{ status: number; body: string }> {
    return new Promise((resolve, reject) => {
      const options: http.RequestOptions = {
        hostname: "dark-packages.dlio.localhost",
        port: 11001,
        path: path,
        method: "GET",
        headers: {
          "Content-Type": "application/json",
        },
      };

      const req = http.request(options, res => {
        let data = "";
        res.on("data", chunk => {
          data += chunk;
        });
        res.on("end", () => {
          resolve({ status: res.statusCode || 0, body: data });
        });
      });

      req.on("error", error => {
        reject(error);
      });

      req.end();
    });
  }

  private async _createDB(dbName: string, typeDef: string) {
    // For now, we'll simulate DB creation since DBs are defined in .dark files
    // In a real implementation, this would write to a .dark file or call a backend API
    this._panel.webview.postMessage({
      command: "dbCreated",
      dbName: dbName,
      typeDef: typeDef,
    });
  }

  private async _setValue(dbName: string, key: string, value: string) {
    try {
      // Call the set endpoint
      const path = `/db/set?db=${encodeURIComponent(dbName)}&key=${encodeURIComponent(key)}&value=${encodeURIComponent(value)}`;
      const result = await this._makeRequest(path);

      this._panel.webview.postMessage({
        command: "valueSet",
        success: result.status === 200,
        key: key,
        value: value,
        result: result.body,
      });
    } catch (error) {
      this._panel.webview.postMessage({
        command: "valueSet",
        success: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  private async _getValue(dbName: string, key: string) {
    try {
      const path = `/db/get?db=${encodeURIComponent(dbName)}&key=${encodeURIComponent(key)}`;
      const result = await this._makeRequest(path);

      let body = result.body;
      try {
        const json = JSON.parse(result.body);
        body = JSON.stringify(json, null, 2);
      } catch {
        // not JSON, use as-is
      }

      this._panel.webview.postMessage({
        command: "valueResult",
        success: result.status === 200,
        key: key,
        value: body,
      });
    } catch (error) {
      this._panel.webview.postMessage({
        command: "valueResult",
        success: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  private async _getAllValues(dbName: string) {
    try {
      const path = `/db/getAll?db=${encodeURIComponent(dbName)}`;
      const result = await this._makeRequest(path);

      let body = result.body;
      try {
        const json = JSON.parse(result.body);
        body = JSON.stringify(json, null, 2);
      } catch {
        // not JSON, use as-is
      }

      this._panel.webview.postMessage({
        command: "allValuesResult",
        success: result.status === 200,
        values: body,
      });
    } catch (error) {
      this._panel.webview.postMessage({
        command: "allValuesResult",
        success: false,
        error: error instanceof Error ? error.message : String(error),
      });
    }
  }

  public dispose() {
    DatabasePanel.currentPanel = undefined;
    this._panel.dispose();
    while (this._disposables.length) {
      const disposable = this._disposables.pop();
      if (disposable) {
        disposable.dispose();
      }
    }
  }

  private _update() {
    this._panel.webview.html = this._getHtmlForWebview();
  }

  private _getHtmlForWebview() {
    return `<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Database</title>
    <style>
        * { box-sizing: border-box; margin: 0; padding: 0; }

        body {
            font-family: var(--vscode-font-family);
            font-size: var(--vscode-font-size);
            color: var(--vscode-foreground);
            background-color: var(--vscode-editor-background);
            padding: 24px;
        }

        .container { max-width: 800px; margin: 0 auto; }

        .header {
            display: flex;
            align-items: center;
            gap: 12px;
            margin-bottom: 32px;
        }

        .header h1 { font-size: 20px; font-weight: 500; }

        .header-icon { width: 24px; height: 24px; color: #22c55e; }

        .section {
            background: var(--vscode-textBlockQuote-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 20px;
        }

        .form-group { margin-bottom: 16px; }
        .form-group:last-child { margin-bottom: 0; }

        label {
            display: block;
            font-size: 12px;
            font-weight: 500;
            color: var(--vscode-descriptionForeground);
            text-transform: uppercase;
            letter-spacing: 0.5px;
            margin-bottom: 6px;
        }

        input {
            width: 100%;
            background: var(--vscode-input-background);
            border: 1px solid var(--vscode-input-border);
            color: var(--vscode-input-foreground);
            border-radius: 4px;
            padding: 10px 12px;
            font-family: var(--vscode-editor-font-family);
            font-size: 14px;
        }

        input:focus {
            outline: none;
            border-color: var(--vscode-focusBorder);
        }

        .btn {
            padding: 10px 20px;
            border-radius: 4px;
            border: none;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            transition: all 0.15s;
        }

        .btn-primary { background: #22c55e; color: white; }
        .btn-primary:hover { background: #16a34a; }

        .btn-secondary {
            background: var(--vscode-button-secondaryBackground);
            color: var(--vscode-button-secondaryForeground);
        }
        .btn-secondary:hover { background: var(--vscode-button-secondaryHoverBackground); }

        .hidden { display: none; }

        .db-created-badge {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            background: rgba(34, 197, 94, 0.15);
            color: #22c55e;
            padding: 6px 12px;
            border-radius: 4px;
            font-size: 13px;
            margin-bottom: 16px;
        }

        .op-row {
            display: flex;
            gap: 12px;
            align-items: flex-end;
            margin-bottom: 12px;
        }

        .op-row .form-group { margin-bottom: 0; flex: 1; }

        .result-box {
            background: var(--vscode-editor-background);
            border: 1px solid var(--vscode-panel-border);
            border-radius: 4px;
            padding: 12px;
            font-family: var(--vscode-editor-font-family);
            font-size: 13px;
            margin-top: 12px;
            white-space: pre-wrap;
            max-height: 200px;
            overflow-y: auto;
        }

        .result-box.error {
            border-color: #ef4444;
            color: #ef4444;
        }

        .section-title {
            font-size: 14px;
            font-weight: 600;
            margin-bottom: 16px;
            padding-bottom: 8px;
            border-bottom: 1px solid var(--vscode-panel-border);
        }
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <svg class="header-icon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                <ellipse cx="12" cy="5" rx="9" ry="3"/>
                <path d="M21 12c0 1.66-4 3-9 3s-9-1.34-9-3"/>
                <path d="M3 5v14c0 1.66 4 3 9 3s9-1.34 9-3V5"/>
            </svg>
            <h1>Database</h1>
        </div>

        <!-- Create DB Section -->
        <div class="section" id="createSection">
            <div class="form-group">
                <label>DB Name</label>
                <input type="text" id="dbName" placeholder="TestDB" value="TestDB" />
            </div>
            <div class="form-group">
                <label>Type</label>
                <input type="text" id="typeDef" placeholder="{ name: String }" value="{ name: String }" />
            </div>
            <button class="btn btn-primary" id="createBtn">Create Database</button>
        </div>

        <!-- Operations Section (shown after create) -->
        <div class="section hidden" id="operationsSection">
            <div class="db-created-badge">
                <svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2">
                    <path d="M20 6L9 17l-5-5"/>
                </svg>
                <span id="createdDbName">TestDB</span> created
            </div>

            <!-- Set Value -->
            <div class="section-title">Set Value</div>
            <div class="op-row">
                <div class="form-group">
                    <label>Key</label>
                    <input type="text" id="setKey" placeholder="key1" value="key1" />
                </div>
                <div class="form-group" style="flex: 2;">
                    <label>Value (name)</label>
                    <input type="text" id="setValue" placeholder='test' value='test' />
                </div>
                <button class="btn btn-primary" id="setBtn">Set</button>
            </div>
            <div class="result-box hidden" id="setResult"></div>

            <!-- Get Value -->
            <div class="section-title" style="margin-top: 24px;">Get Value</div>
            <div class="op-row">
                <div class="form-group">
                    <label>Key</label>
                    <input type="text" id="getKey" placeholder="key1" value="key1" />
                </div>
                <button class="btn btn-secondary" id="getBtn">Get</button>
                <button class="btn btn-secondary" id="getAllBtn">Get All</button>
            </div>
            <div class="result-box hidden" id="getResult"></div>
        </div>
    </div>

    <script>
        const vscode = acquireVsCodeApi();

        const createSection = document.getElementById('createSection');
        const operationsSection = document.getElementById('operationsSection');
        const createdDbName = document.getElementById('createdDbName');

        let currentDbName = '';
        let currentTypeDef = '';

        document.getElementById('createBtn').addEventListener('click', () => {
            currentDbName = document.getElementById('dbName').value;
            currentTypeDef = document.getElementById('typeDef').value;

            vscode.postMessage({
                command: 'createDB',
                dbName: currentDbName,
                typeDef: currentTypeDef
            });
        });

        document.getElementById('setBtn').addEventListener('click', () => {
            const key = document.getElementById('setKey').value;
            const value = document.getElementById('setValue').value;

            vscode.postMessage({
                command: 'setValue',
                dbName: currentDbName,
                key: key,
                value: value
            });
        });

        document.getElementById('getBtn').addEventListener('click', () => {
            const key = document.getElementById('getKey').value;

            vscode.postMessage({
                command: 'getValue',
                dbName: currentDbName,
                key: key
            });
        });

        document.getElementById('getAllBtn').addEventListener('click', () => {
            vscode.postMessage({
                command: 'getAllValues',
                dbName: currentDbName
            });
        });

        window.addEventListener('message', event => {
            const message = event.data;

            switch (message.command) {
                case 'dbCreated':
                    createdDbName.textContent = message.dbName;
                    createSection.classList.add('hidden');
                    operationsSection.classList.remove('hidden');
                    break;

                case 'valueSet':
                    const setResult = document.getElementById('setResult');
                    setResult.classList.remove('hidden', 'error');
                    if (message.success) {
                        setResult.textContent = message.result || 'Value set successfully';
                    } else {
                        setResult.classList.add('error');
                        setResult.textContent = message.error || message.result;
                    }
                    break;

                case 'valueResult':
                    const getResult = document.getElementById('getResult');
                    getResult.classList.remove('hidden', 'error');
                    if (message.success) {
                        getResult.textContent = message.value;
                    } else {
                        getResult.classList.add('error');
                        getResult.textContent = message.error || 'Error getting value';
                    }
                    break;

                case 'allValuesResult':
                    const getAllResult = document.getElementById('getResult');
                    getAllResult.classList.remove('hidden', 'error');
                    if (message.success) {
                        getAllResult.textContent = message.values;
                    } else {
                        getAllResult.classList.add('error');
                        getAllResult.textContent = message.error || 'Error getting values';
                    }
                    break;
            }
        });
    </script>
</body>
</html>`;
  }
}
