// =============================================================================
// Navigation Icons (used in sidebar)
// =============================================================================

import dashboardSvg from "./icons/nav/dashboard.svg";
import packagesSvg from "./icons/nav/packages.svg";
import branchesSvg from "./icons/nav/branches.svg";
import appsSvg from "./icons/nav/apps.svg";
import approvalRequestsSvg from "./icons/nav/approval-requests.svg";
import contributionsSvg from "./icons/nav/contributions.svg";
import tracesSvg from "./icons/nav/traces.svg";
import settingsSvg from "./icons/nav/settings.svg";
import changelogSvg from "./icons/nav/changelog.svg";
import logoutSvg from "./icons/nav/logout.svg";

export const navIcons = {
  dashboard: dashboardSvg,
  packages: packagesSvg,
  branches: branchesSvg,
  apps: appsSvg,
  "approval-requests": approvalRequestsSvg,
  contributions: contributionsSvg,
  traces: tracesSvg,
  settings: settingsSvg,
  changelog: changelogSvg,
  logout: logoutSvg,
} as const;

// =============================================================================
// Item Type Icons (used in dashboard cards)
// =============================================================================

import branchItemSvg from "./icons/item/branch.svg";
import appItemSvg from "./icons/item/app.svg";
import moduleSvg from "./icons/item/module.svg";
import functionSvg from "./icons/item/function.svg";
import typeSvg from "./icons/item/type.svg";
import valueSvg from "./icons/item/value.svg";

export const itemIcons = {
  branch: branchItemSvg,
  app: appItemSvg,
  module: moduleSvg,
  function: functionSvg,
  type: typeSvg,
  value: valueSvg,
} as const;

// =============================================================================
// UI Icons (buttons, controls, etc.)
// =============================================================================

import toggleSvg from "./icons/ui/toggle.svg";
import unpinSvg from "./icons/ui/unpin.svg";
import userSvg from "./icons/ui/user.svg";

export const uiIcons = {
  toggle: toggleSvg,
  unpin: unpinSvg,
  user: userSvg,
} as const;
