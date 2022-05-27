import { expect, ConsoleMessage, Page, TestInfo } from "@playwright/test";
import { apiserverBaseUrl, bwdserverBaseUrl } from "./fixture";

// TODO: tidy below

export function canvasUrl(canvasName: string) {
  return `${apiserverBaseUrl}/a/test-${canvasName}?integration-test=true&use-blazor=true`;
}

export async function waitForPageToStopMoving(page: Page): Promise<void> {
  // We can do better in the future
  await page.waitForTimeout(500);
}
export async function createEmptyHTTPHandler(page: Page) {
  await page.click(".sidebar-category.http i.fa-plus-circle");
  await waitForPageToStopMoving(page);
  await waitForEmptyEntryBox(page);
}

export async function createHTTPHandler(
  page: Page,
  method: string,
  path: string,
) {
  await createEmptyHTTPHandler(page);
  await page.type(entryBox, method);
  await expectExactText(page, acHighlightedValue, method);
  await page.keyboard.press("Enter");
  await waitForEmptyEntryBox(page);
  await page.type(entryBox, path);
  await expectExactText(page, acHighlightedValue, path);
  await page.keyboard.press("Enter");
  await waitForEmptyFluidEntryBox(page);
}

export async function createWorkerHandler(page) {
  await page.click(".sidebar-category.worker i.fa-plus-circle");
  await waitForPageToStopMoving(page);
  await waitForEmptyEntryBox(page);
}

export async function createRepl(page) {
  await page.click(".sidebar-category.repl i.fa-plus-circle");
  await waitForPageToStopMoving(page);
  await waitForEmptyFluidEntryBox(page);
}

export async function gotoAST(page: Page): Promise<void> {
  await page.click("#active-editor > span");
}

export function bwdUrl(testInfo: TestInfo, path: string) {
  return "http://test-" + testInfo.title + bwdserverBaseUrl + path;
}

export async function pressShortcut(page: Page, shortcut: string) {
  if (process.platform == "darwin") {
    page.keyboard.press(`Meta+${shortcut}`);
  } else {
    page.keyboard.press(`Control+${shortcut}`);
  }
}

export async function get(page: Page, url: string): Promise<string> {
  return await page.evaluate(
    async ({ url }) => {
      const response = await fetch(url, {
        method: "GET",
      });
      return response.text();
    },
    { url: url },
  );
}
export async function post(
  page: Page,
  url: string,
  body: string,
): Promise<string> {
  return await page.evaluate(
    async ({ url, body }) => {
      const response = await fetch(url, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: body,
      });
      return response.text();
    },
    { url: url, body: body },
  );
}

//********************************
// Locators
//********************************
export const entryBox = "#entry-box";
export const acHighlightedValue = ".autocomplete-item.highlighted > .name";
export const fluidACHighlightedValue = ".autocomplete-item.fluid-selected";
export const dbLockLocator = ".db .spec-header.lock";

//********************************
// Utilities
//********************************
export async function selectAll(page: Page): Promise<void> {
  // Do these multiple times to make sure it actually selects
  if (process.platform == "darwin") {
    await page.keyboard.press("Meta+a");
    await page.keyboard.press("Meta+a");
    await page.keyboard.press("Meta+a");
  } else {
    await page.keyboard.press("Control+a");
    await page.keyboard.press("Control+a");
    await page.keyboard.press("Control+a");
  }
}

export async function expectExactText(
  page: Page,
  selector: string,
  text: string,
) {
  await expect(page.locator(selector)).toHaveText(text);
}
export async function expectContainsText(
  page: Page,
  selector: string,
  text: string,
) {
  await expect(page.locator(selector)).toContainText(text);
}

export async function expectPlaceholderText(page: Page, text: string) {
  await expect(page.locator(entryBox)).toHaveAttribute("placeholder", text);
}

// Entry-box sometimes carries state over briefly, so wait til it's clear
export async function waitForEmptyEntryBox(page: Page): Promise<void> {
  await page.waitForSelector("#entry-box >> text=''");
}

export async function waitForEmptyFluidEntryBox(page: Page): Promise<void> {
  await page.waitForSelector("#active-editor >> text=''");
}

export async function gotoHash(page: Page, testInfo: TestInfo, hash: string) {
  const testName = testInfo.title;
  var url = canvasUrl(testName);
  // networkidle needed for hash
  // await page.goto(`${url}#${hash}`, { waitUntil: "networkidle" });
  await page.goto(`${url}#${hash}`);
}

export async function selectText(
  page: Page,
  locator: string,
  initial: number,
  final: number,
) {
  await page.focus(locator);
  await page.keyboard.press("Home");
  for (let i = 0; i < initial; i++) {
    await page.keyboard.press("ArrowRight");
  }
  await page.keyboard.press("Shift");
  for (let i = 0; i < final; i++) {
    await page.keyboard.press("ArrowRight");
  }
}

export function caretPos(pos: number) {
  return { position: { x: pos * 8, y: 4 } };
}

export async function getStyleProperty(
  page: Page,
  selector: string,
  property: string,
): Promise<string> {
  return await page.$eval(
    selector,
    (el, prop) => {
      return window.getComputedStyle(el).getPropertyValue(prop);
    },
    property,
  );
}

export async function getElementSelectionStart(
  page: Page,
  selector: string,
): Promise<number> {
  return page.$eval(selector, el => (<HTMLInputElement>el).selectionStart);
}

export function awaitAnalysisLoaded(page: Page) {
  return new Promise((resolve, _reject) => {
    page.on("console", async (msg: ConsoleMessage) => {
      if (msg.text() === "Blazor loaded") {
        resolve(true);
      }
    });
  });
}

export async function awaitAnalysis(page: Page, lastTimestamp: number) {
  let analysisFunction = (lastTimestamp: number) => {
    let newTimestamp = window.Dark.analysis.lastRun;
    if (newTimestamp > lastTimestamp) {
      const diffInSecs = (newTimestamp - lastTimestamp) / 1000.0;
      console.info("Analysis ran in ~ " + diffInSecs + "secs");
      return true;
    }
    return false;
  };
  await page.waitForFunction(analysisFunction, lastTimestamp);
}
