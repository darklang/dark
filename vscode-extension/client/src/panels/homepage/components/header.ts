/** Header bar component */

export interface HeaderConfig {
  title: string;
  icon: string;
  showDocs?: boolean;
  showNew?: boolean;
  showAvatar?: boolean;
}

/** Render the header bar */
export function renderHeader(config: HeaderConfig): string {
  const {
    title,
    icon,
    showDocs = true,
    showNew = true,
    showAvatar = true,
  } = config;

  const docsButton = showDocs
    ? `<button class="header-btn">
        <span>📄</span>
        <span>Docs</span>
       </button>`
    : "";

  const newButton = showNew
    ? `<button class="header-btn primary">
        <span>+</span>
        <span>New</span>
       </button>`
    : "";

  const avatar = showAvatar ? `<div class="user-avatar"></div>` : "";

  return `
    <div class="dashboard-header">
        <div class="header-left">
            <span class="header-icon">${icon}</span>
            <h1 class="header-title">${title}</h1>
        </div>
        <div class="header-right">
            ${docsButton}
            ${newButton}
            ${avatar}
        </div>
    </div>`;
}
