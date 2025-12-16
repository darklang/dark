/** Header bar component */

import { uiIcons } from "../icons";

export interface HeaderConfig {
  title: string;
  icon: string;
  showDocs?: boolean;
  showNew?: boolean;
  showAvatar?: boolean;
  currentAccount?: string;
  availableAccounts?: string[];
}

/** Render the header bar */
export function renderHeader(config: HeaderConfig): string {
  const {
    title,
    icon,
    showDocs = true,
    showNew = true,
    showAvatar = true,
    currentAccount = "",
    availableAccounts = [],
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

  // Build account dropdown items
  const dropdownItems = availableAccounts
    .map(account => {
      const isSelected = account === currentAccount;
      return `<div class="account-dropdown-item${isSelected ? " selected" : ""}" data-account="${account}">
        <span class="account-dropdown-avatar">${account.charAt(0)}</span>
        <span class="account-dropdown-name">${account}</span>
        ${isSelected ? '<span class="account-dropdown-check">✓</span>' : ""}
      </div>`;
    })
    .join("");

  const avatar = showAvatar
    ? `<div class="account-switcher-container">
        <div class="user-avatar" id="accountSwitcher" title="Switch account${currentAccount ? ` (${currentAccount})` : ""}">
          ${uiIcons.user}
          ${currentAccount ? `<span class="account-badge">${currentAccount.charAt(0)}</span>` : ""}
        </div>
        <div class="account-dropdown" id="accountDropdown">
          <div class="account-dropdown-header">Switch Account</div>
          ${dropdownItems}
        </div>
       </div>`
    : "";

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
