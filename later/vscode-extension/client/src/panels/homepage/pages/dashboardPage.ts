/** Dashboard page content renderer */

import { itemIcons, uiIcons } from "../icons";

export interface DashboardRecentItem {
  id: string;
  title: string;
  type: "branch" | "app" | "module" | "function" | "type" | "value";
  meta: string[];
}

export interface DashboardPinnedItem {
  id: string;
  title: string;
  treeId: string; // Full path for tree view (e.g., "Darklang.Stdlib.List.map")
  kind: "function" | "type" | "value" | "module";
  meta: string[];
}

export interface DashboardData {
  recentItems: DashboardRecentItem[];
  pinnedItems: DashboardPinnedItem[];
}

interface CardOptions {
  id: string;
  title: string;
  type: string;
  meta: string[];
  treeId?: string;
  pinned?: boolean;
}

function renderCard(options: CardOptions): string {
  const { id, title, type, meta, treeId, pinned } = options;
  const icon = itemIcons[type as keyof typeof itemIcons] || itemIcons.module;

  const dataAttrs = treeId
    ? `data-project="${id}" data-tree-id="${treeId}" data-kind="${type}"`
    : `data-project="${id}" data-type="${type}"`;

  const unpinBtn = pinned && treeId
    ? `<button class="unpin-btn" data-tree-id="${treeId}" title="Unpin">${uiIcons.unpin}</button>`
    : "";

  const cardClass = pinned ? "project-card pinned-card" : "project-card";

  return `
    <div class="${cardClass}" ${dataAttrs}>
        <div class="card-icon-wrapper">${icon}</div>
        <div class="card-content">
            <div class="card-title">${title}</div>
            <div class="card-meta">
                ${meta
                  .map(
                    (m, i) =>
                      `<span class="card-meta-item" title="${m}">${m}</span>${
                        i < meta.length - 1
                          ? '<span class="meta-separator">.</span>'
                          : ""
                      }`,
                  )
                  .join("")}
            </div>
        </div>
        ${unpinBtn}
        <span class="card-arrow">→</span>
    </div>`;
}

function renderRecentCard(item: DashboardRecentItem): string {
  return renderCard({
    id: item.id,
    title: item.title,
    type: item.type,
    meta: item.meta,
  });
}

function renderPinnedCard(item: DashboardPinnedItem): string {
  return renderCard({
    id: item.id,
    title: item.title,
    type: item.kind,
    meta: item.meta,
    treeId: item.treeId,
    pinned: true,
  });
}

/** Render the dashboard page content */
export function renderDashboard(data: DashboardData): string {
  return `
        <div class="dashboard-content">
            <div class="search-container">
                <div class="search-box">
                    <span class="search-icon">🔍</span>
                    <input
                        type="text"
                        class="search-input"
                        placeholder="Search packages, functions, apps, docs ..."
                    />
                </div>
            </div>

            <div class="section">
                <div class="section-header">
                    <h2 class="section-title">Recent</h2>
                </div>
                <div class="cards-grid">
                    ${data.recentItems.map(renderRecentCard).join("")}
                </div>
            </div>

            <div class="section">
                <div class="section-header">
                    <h2 class="section-title">Pinned</h2>
                </div>
                <div class="cards-grid">
                    ${data.pinnedItems.map(renderPinnedCard).join("")}
                </div>
            </div>
        </div>`;
}

/** Default data for the dashboard */
export function getDefaultDashboardData(): DashboardData {
  return {
    recentItems: [],
    pinnedItems: [],
  };
}
