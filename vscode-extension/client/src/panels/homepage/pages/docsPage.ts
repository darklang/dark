/** Docs page - renders documentation topics from the LSP server */

export interface DocTopic {
  name: string;
  description: string;
  context: string;
}

export interface DocsPageData {
  topics: DocTopic[];
  activeTopic: string | null;
  content: string | null;
}

export function getDefaultDocsData(): DocsPageData {
  return {
    topics: [],
    activeTopic: null,
    content: null,
  };
}

/** Escape HTML entities */
function escapeHtml(text: string): string {
  return text
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;");
}

/** Convert plain text doc content to HTML.
 * Handles # headers, indented code blocks, and blank lines. */
function formatContent(text: string): string {
  const lines = text.split("\n");
  const html: string[] = [];
  let inCode = false;

  for (const line of lines) {
    const trimmed = line.trimStart();

    // Heading lines
    if (trimmed.startsWith("## ")) {
      if (inCode) { html.push("</code></pre>"); inCode = false; }
      html.push(`<h3>${escapeHtml(trimmed.slice(3))}</h3>`);
    } else if (trimmed.startsWith("# ")) {
      if (inCode) { html.push("</code></pre>"); inCode = false; }
      html.push(`<h2>${escapeHtml(trimmed.slice(2))}</h2>`);
    } else if (line.startsWith("  ") && trimmed.length > 0) {
      // Indented = code
      if (!inCode) { html.push('<pre class="doc-code"><code>'); inCode = true; }
      html.push(escapeHtml(line.slice(2)));
    } else if (trimmed === "") {
      if (inCode) { html.push("</code></pre>"); inCode = false; }
      html.push("<br/>");
    } else {
      if (inCode) { html.push("</code></pre>"); inCode = false; }
      html.push(`<p>${escapeHtml(trimmed)}</p>`);
    }
  }

  if (inCode) html.push("</code></pre>");
  return html.join("\n");
}

function renderTopicList(topics: DocTopic[]): string {
  if (topics.length === 0) {
    return `<p class="docs-empty">Loading documentation topics...</p>`;
  }

  const cards = topics.map(t => `
    <div class="doc-topic-card" data-topic="${escapeHtml(t.name)}">
      <div class="doc-topic-name">${escapeHtml(t.name)}</div>
      <div class="doc-topic-desc">${escapeHtml(t.description)}</div>
    </div>
  `).join("");

  return `
    <div class="docs-topic-list">
      ${cards}
    </div>
  `;
}

function renderTopicContent(name: string, content: string): string {
  return `
    <div class="docs-content-view">
      <button class="docs-back-btn" data-action="docs-back">&larr; All Topics</button>
      <div class="docs-content-body">
        ${formatContent(content)}
      </div>
    </div>
  `;
}

export function renderDocsPage(data: DocsPageData): string {
  const styles = `
    <style>
      .docs-topic-list {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
        gap: 12px;
        padding: 16px 0;
      }
      .doc-topic-card {
        background: var(--vscode-editor-background, #1e1e1e);
        border: 1px solid var(--vscode-widget-border, #333);
        border-radius: 8px;
        padding: 14px 16px;
        cursor: pointer;
        transition: border-color 0.15s;
      }
      .doc-topic-card:hover {
        border-color: var(--vscode-focusBorder, #007fd4);
      }
      .doc-topic-name {
        font-weight: 600;
        font-size: 14px;
        margin-bottom: 4px;
        color: var(--vscode-foreground, #ccc);
      }
      .doc-topic-desc {
        font-size: 12px;
        color: var(--vscode-descriptionForeground, #888);
      }
      .docs-back-btn {
        background: none;
        border: 1px solid var(--vscode-widget-border, #333);
        color: var(--vscode-foreground, #ccc);
        padding: 6px 14px;
        border-radius: 4px;
        cursor: pointer;
        font-size: 13px;
        margin-bottom: 16px;
      }
      .docs-back-btn:hover {
        background: var(--vscode-toolbar-hoverBackground, #333);
      }
      .docs-content-body {
        line-height: 1.6;
        color: var(--vscode-foreground, #ccc);
      }
      .docs-content-body h2 {
        font-size: 20px;
        margin: 20px 0 8px;
        color: var(--vscode-foreground, #eee);
        border-bottom: 1px solid var(--vscode-widget-border, #333);
        padding-bottom: 4px;
      }
      .docs-content-body h3 {
        font-size: 15px;
        margin: 16px 0 6px;
        color: var(--vscode-foreground, #ddd);
      }
      .docs-content-body p {
        margin: 4px 0;
        font-size: 13px;
      }
      .doc-code {
        background: var(--vscode-textCodeBlock-background, #111);
        border: 1px solid var(--vscode-widget-border, #333);
        border-radius: 4px;
        padding: 10px 14px;
        font-family: var(--vscode-editor-font-family, monospace);
        font-size: 12px;
        overflow-x: auto;
        margin: 6px 0;
        line-height: 1.5;
      }
      .docs-empty {
        color: var(--vscode-descriptionForeground, #888);
        font-style: italic;
        padding: 20px 0;
      }
    </style>
  `;

  const body = data.activeTopic && data.content
    ? renderTopicContent(data.activeTopic, data.content)
    : renderTopicList(data.topics);

  return `${styles}<div class="dashboard-content">${body}</div>`;
}
