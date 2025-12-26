import * as vscode from "vscode";
import { LanguageClient } from "vscode-languageclient/node";
import { AccountService } from "../services/accountService";
import { ApprovalsTreeDataProvider } from "../providers/treeviews/approvalsTreeDataProvider";

export interface ApprovalItem {
  locationId: string;
  modules: string[];
  name: string;
  itemType: string;
}

export class ApprovalRequestPanel {
  private readonly _panel: vscode.WebviewPanel;
  private _disposed = false;

  private constructor(
    panel: vscode.WebviewPanel,
    private readonly client: LanguageClient,
    private readonly approvalsProvider: ApprovalsTreeDataProvider | null,
    private readonly namespace: string,
    allItems: ApprovalItem[],
    initiallySelected: ApprovalItem[],
  ) {
    this._panel = panel;
    this._panel.webview.html = this._getHtmlForWebview(allItems, initiallySelected);
    this._setupMessageHandling();

    this._panel.onDidDispose(() => {
      this._disposed = true;
    });
  }

  public static create(
    client: LanguageClient,
    approvalsProvider: ApprovalsTreeDataProvider | null,
    namespace: string,
    allItems: ApprovalItem[],
    initiallySelected: ApprovalItem[],
  ): ApprovalRequestPanel {
    const panel = vscode.window.createWebviewPanel(
      "approvalRequest",
      `Request: ${namespace}`,
      vscode.ViewColumn.Active,
      { enableScripts: true }
    );

    return new ApprovalRequestPanel(
      panel,
      client,
      approvalsProvider,
      namespace,
      allItems,
      initiallySelected
    );
  }

  private _setupMessageHandling(): void {
    this._panel.webview.onDidReceiveMessage(async message => {
      if (message.type === "submit") {
        try {
          const response = (await this.client.sendRequest(
            "dark/createApprovalRequest",
            {
              accountID: AccountService.getCurrentAccountId(),
              targetNamespace: this.namespace,
              locationIds: message.locationIds,
              title: message.title.trim() || `Changes to ${this.namespace}`,
              description: message.description.trim() || null,
            },
          )) as { directApproval?: boolean; success?: boolean; count?: number };

          const itemCount = response.count ?? message.locationIds.length;
          const successMessage = response.directApproval
            ? `Approved ${itemCount} item(s)`
            : `Request created for ${itemCount} item(s)`;

          this._panel.webview.postMessage({
            type: "success",
            message: successMessage,
          });

          this.approvalsProvider?.refresh();
        } catch (error) {
          console.error("Failed to create request:", error);
          this._panel.webview.postMessage({
            type: "error",
            message: "Failed to create request. Please try again.",
          });
        }
      } else if (message.type === "cancel" || message.type === "close") {
        this._panel.dispose();
      }
    });
  }

  private _getHtmlForWebview(
    allItems: ApprovalItem[],
    initiallySelected: ApprovalItem[]
  ): string {
    const initiallySelectedIds = new Set(initiallySelected.map(i => i.locationId));

    const itemsHtml = allItems
      .map(item => {
        const checked = initiallySelectedIds.has(item.locationId) ? "checked" : "";
        let icon = "ƒ";
        if (item.itemType === "fn") {
          icon = "ƒ";
        } else if (item.itemType === "type") {
          icon = "T";
        } else {
          icon = "c";
        }
        const modulePath = item.modules && item.modules.length > 0
          ? item.modules.join(".") + "."
          : "";
        const fullName = modulePath + item.name;
        return `
          <label class="item">
            <input type="checkbox" name="items" value="${item.locationId}" ${checked}>
            <span class="checkbox-custom"></span>
            <span class="icon ${item.itemType}">${icon}</span>
            <span class="name">${fullName}</span>
            <span class="type-badge">${item.itemType}</span>
          </label>
        `;
      })
      .join("");

    return `
      <!DOCTYPE html>
      <html>
      <head>
        <style>
          ${this._getStyles()}
        </style>
      </head>
      <body>
        <div class="header">
          <div class="header-left">
            <svg class="header-icon" viewBox="0 0 16 16" fill="currentColor">
              <path d="M8 1a7 7 0 1 0 0 14A7 7 0 0 0 8 1zM0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8z"/>
              <path d="M8 4a.5.5 0 0 1 .5.5v3h3a.5.5 0 0 1 0 1h-3v3a.5.5 0 0 1-1 0v-3h-3a.5.5 0 0 1 0-1h3v-3A.5.5 0 0 1 8 4z"/>
            </svg>
            <h1>Create Approval Request for ${this.namespace}</h1>
          </div>
          <div class="header-actions">
            <button class="cancel" onclick="cancel()">Cancel</button>
            <button class="submit" id="submitBtn" onclick="submit()">Submit</button>
          </div>
        </div>

        <div class="content">
          <div class="form-group">
            <label class="field-label" for="title">Title</label>
            <input type="text" id="title" placeholder="Changes to ${this.namespace}" autofocus>
          </div>

          <div class="form-group">
            <label class="field-label" for="description">Description (optional)</label>
            <textarea id="description" placeholder="Describe your changes..."></textarea>
          </div>

          <div class="items-section">
            <div class="items-header">
              <h3>Items to include <span class="selected-count" id="selectedCount">(${initiallySelected.length} selected)</span></h3>
              <div class="select-buttons">
                <button onclick="selectAll()">Select All</button>
                <button onclick="selectNone()">Select None</button>
              </div>
            </div>
            <div class="items-list">
              ${itemsHtml}
            </div>
          </div>
        </div>

        <div class="status" id="status"></div>

        <script>
          const vscode = acquireVsCodeApi();

          function updateCount() {
            const checked = document.querySelectorAll('input[name="items"]:checked').length;
            document.getElementById('selectedCount').textContent = '(' + checked + ' selected)';
            document.getElementById('submitBtn').disabled = checked === 0;
          }

          document.querySelectorAll('input[name="items"]').forEach(cb => {
            cb.addEventListener('change', updateCount);
          });

          function selectAll() {
            document.querySelectorAll('input[name="items"]').forEach(cb => cb.checked = true);
            updateCount();
          }

          function selectNone() {
            document.querySelectorAll('input[name="items"]').forEach(cb => cb.checked = false);
            updateCount();
          }

          function submit() {
            const title = document.getElementById('title').value;
            const description = document.getElementById('description').value;
            const locationIds = Array.from(document.querySelectorAll('input[name="items"]:checked'))
              .map(cb => cb.value);

            if (locationIds.length === 0) {
              return;
            }

            document.getElementById('submitBtn').disabled = true;
            document.getElementById('submitBtn').textContent = 'Submitting...';

            vscode.postMessage({
              type: 'submit',
              title: title,
              description: description,
              locationIds: locationIds
            });
          }

          function cancel() {
            vscode.postMessage({ type: 'cancel' });
          }

          function showStatus(message, isError) {
            const status = document.getElementById('status');
            status.textContent = message;
            status.className = 'status ' + (isError ? 'error' : 'success');
          }

          window.addEventListener('message', event => {
            const message = event.data;
            if (message.type === 'success') {
              showStatus(message.message, false);
              setTimeout(() => vscode.postMessage({ type: 'close' }), 1500);
            } else if (message.type === 'error') {
              showStatus(message.message, true);
              document.getElementById('submitBtn').disabled = false;
              document.getElementById('submitBtn').textContent = 'Submit';
            }
          });

          // Submit on Ctrl+Enter
          document.addEventListener('keydown', (e) => {
            if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
              submit();
            }
            if (e.key === 'Escape') {
              cancel();
            }
          });

          updateCount();
        </script>
      </body>
      </html>
    `;
  }

  private _getStyles(): string {
    return `
      * {
        box-sizing: border-box;
        margin: 0;
        padding: 0;
      }
      body {
        font-family: var(--vscode-font-family);
        font-size: var(--vscode-font-size);
        color: var(--vscode-foreground);
        background: var(--vscode-editor-background);
      }

      /* Fixed header */
      .header {
        position: sticky;
        top: 0;
        background: var(--vscode-editor-background);
        border-bottom: 1px solid var(--vscode-panel-border);
        padding: 14px 24px;
        display: flex;
        align-items: center;
        justify-content: space-between;
        z-index: 100;
      }
      .header-left {
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .header-icon {
        width: 14px;
        height: 14px;
        color: #8B5CF6;
      }
      .header h1 {
        margin: 0;
        font-size: 15px;
        font-weight: 500;
      }
      .header-actions {
        display: flex;
        gap: 8px;
      }

      /* Content area */
      .content {
        padding: 24px;
      }

      .form-group {
        margin-bottom: 16px;
      }
      label.field-label {
        display: block;
        margin-bottom: 6px;
        font-weight: 500;
        font-size: 11px;
        text-transform: uppercase;
        letter-spacing: 0.5px;
        color: var(--vscode-descriptionForeground);
      }
      input[type="text"], textarea {
        width: 100%;
        padding: 10px 12px;
        border: 1px solid var(--vscode-input-border);
        background: var(--vscode-input-background);
        color: var(--vscode-input-foreground);
        border-radius: 6px;
        font-family: inherit;
        font-size: 13px;
      }
      input[type="text"]:focus, textarea:focus {
        outline: none;
        border-color: var(--vscode-focusBorder);
      }
      textarea {
        min-height: 70px;
        resize: vertical;
      }

      /* Items section */
      .items-section {
        margin-top: 24px;
      }
      .items-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        margin-bottom: 12px;
      }
      .items-header h3 {
        margin: 0;
        font-size: 13px;
        font-weight: 600;
      }
      .selected-count {
        color: var(--vscode-descriptionForeground);
        font-weight: 400;
      }
      .select-buttons {
        display: flex;
        gap: 4px;
      }
      .select-buttons button {
        padding: 4px 10px;
        background: transparent;
        color: var(--vscode-descriptionForeground);
        border: 1px solid var(--vscode-button-border);
        border-radius: 4px;
        cursor: pointer;
        font-size: 11px;
        transition: all 0.15s;
      }
      .select-buttons button:hover {
        background: var(--vscode-button-secondaryHoverBackground);
        color: var(--vscode-foreground);
      }

      /* Items list */
      .items-list {
        border: 1px solid var(--vscode-panel-border);
        border-radius: 6px;
        overflow: hidden;
      }
      .item {
        display: flex;
        align-items: center;
        padding: 10px 12px;
        cursor: pointer;
        border-bottom: 1px solid var(--vscode-panel-border);
        transition: background 0.15s;
      }
      .item:last-child {
        border-bottom: none;
      }
      .item:hover {
        background: var(--vscode-list-hoverBackground);
      }

      /* Custom checkbox */
      .item input[type="checkbox"] {
        display: none;
      }
      .checkbox-custom {
        width: 16px;
        height: 16px;
        border: 2px solid #666;
        border-radius: 4px;
        margin-right: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        transition: all 0.15s;
        flex-shrink: 0;
      }
      .item input[type="checkbox"]:checked + .checkbox-custom {
        background: #8B5CF6;
        border-color: #8B5CF6;
      }
      .item input[type="checkbox"]:checked + .checkbox-custom::after {
        content: "";
        width: 4px;
        height: 8px;
        border: solid white;
        border-width: 0 2px 2px 0;
        transform: rotate(45deg);
        margin-bottom: 2px;
      }

      /* Icons */
      .item .icon {
        width: 16px;
        height: 16px;
        display: flex;
        align-items: center;
        justify-content: center;
        margin-right: 10px;
        flex-shrink: 0;
        font-size: 12px;
        font-weight: 600;
        font-style: italic;
      }
      .item .icon.fn { color: #8B5CF6; }
      .item .icon.type { color: #61afef; font-style: normal; }
      .item .icon.value { color: #e5c07b; font-style: normal; }

      .item .name {
        flex: 1;
        font-size: 13px;
      }
      .type-badge {
        font-size: 9px;
        padding: 2px 6px;
        border-radius: 3px;
        background: var(--vscode-badge-background);
        color: var(--vscode-badge-foreground);
        text-transform: uppercase;
        letter-spacing: 0.4px;
        font-weight: 600;
      }

      /* Buttons */
      button.submit {
        padding: 6px 16px;
        background: #8B5CF6;
        color: white;
        border: none;
        border-radius: 4px;
        cursor: pointer;
        font-size: 12px;
        font-weight: 500;
        transition: all 0.15s;
      }
      button.submit:hover {
        background: #7C3AED;
      }
      button.submit:disabled {
        opacity: 0.5;
        cursor: not-allowed;
      }
      button.cancel {
        padding: 6px 12px;
        background: var(--vscode-button-secondaryBackground);
        color: var(--vscode-button-secondaryForeground);
        border: 1px solid var(--vscode-button-border);
        border-radius: 4px;
        cursor: pointer;
        font-size: 12px;
        transition: all 0.15s;
      }
      button.cancel:hover {
        background: var(--vscode-button-secondaryHoverBackground);
      }

      /* Status message */
      .status {
        margin: 16px 24px;
        padding: 10px 14px;
        border-radius: 4px;
        display: none;
        font-size: 12px;
      }
      .status.success {
        display: block;
        background: rgba(34, 197, 94, 0.15);
        border: 1px solid rgba(34, 197, 94, 0.3);
        color: #86EFAC;
      }
      .status.error {
        display: block;
        background: rgba(239, 68, 68, 0.15);
        border: 1px solid rgba(239, 68, 68, 0.3);
        color: #FCA5A5;
      }
    `;
  }
}
