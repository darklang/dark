/** Sidebar/navbar component */

import type { PageName } from "../homepagePanel";
import { navIcons, uiIcons } from "../icons";

export { navIcons };

export interface NavItem {
  page: PageName;
  label: string;
  icon: string;
  badge?: string;
  tag?: string;
}

/** Default nav items configuration */
export const defaultNavItems: NavItem[] = [
  { page: "dashboard", label: "Dashboard", icon: navIcons.dashboard },
  { page: "packages", label: "Packages", icon: navIcons.packages },
  { page: "docs", label: "Docs", icon: navIcons.docs },
  { page: "settings", label: "Settings", icon: navIcons.settings },
  {
    page: "changelog",
    label: "Changelog",
    icon: navIcons.changelog,
    tag: "NEW",
  },
];

function renderNavItem(item: NavItem, activePage: PageName): string {
  const isActive = item.page === activePage;
  const badge = item.badge
    ? `<span class="nav-badge">${item.badge}</span>`
    : "";
  const tag = item.tag ? `<span class="nav-tag">${item.tag}</span>` : "";

  return `
    <div class="nav-item${isActive ? " active" : ""}" data-page="${item.page}">
        <span class="nav-icon">${item.icon}</span>
        <span class="nav-label">${item.label}</span>
        ${badge}${tag}
    </div>`;
}

/** Render the sidebar component */
export function renderSidebar(
  logoUri: string,
  activePage: PageName,
  navItems: NavItem[] = defaultNavItems,
): string {
  return `
    <div class="sidebar">
        <div class="logo-section">
            <img src="${logoUri}" alt="Darklang" class="logo-image" />
            <button class="toggle-btn" id="sidebarToggle">
                ${uiIcons.toggle}
            </button>
        </div>

        <nav class="nav-menu">
            ${navItems.map(item => renderNavItem(item, activePage)).join("")}
        </nav>
    </div>`;
}
