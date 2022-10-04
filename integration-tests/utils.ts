import { expect, ConsoleMessage, Page, TestInfo } from "@playwright/test";
import { BWD_BASE_URL, BASE_URL } from "./tests";

// TODO there are likely more locators within tests that we could add here
export const Locators = {
  entryBox: "#entry-box",
  acHighlightedValue: ".autocomplete-item.highlighted > .name",
  fluidACHighlightedValue: ".autocomplete-item.fluid-selected",
  dbLockLocator: ".db .spec-header.lock",
};

//********************************
// Navigation
//********************************
export async function gotoAST(page: Page): Promise<void> {
  await page.click("#active-editor > span");
}

export function bwdUrl(testInfo: TestInfo, path: string) {
  return "http://test-" + testInfo.title + BWD_BASE_URL + path;
}

export function canvasUrl(canvasName: string) {
  return `${BASE_URL}/a/test-${canvasName}?integration-test=true`;
}

export async function gotoHash(page: Page, testInfo: TestInfo, hash: string) {
  await page.goto(`${canvasUrl(testInfo.title)}#${hash}`);
}

//********************************
// Expectations
//********************************
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
  await expect(page.locator(Locators.entryBox)).toHaveAttribute(
    "placeholder",
    text,
  );
}

//********************************
// Create handlers
//********************************

async function createHandler(page: Page, title: string) {
  // Based on the html structure - the plus is the sibling of the handler
  await page.click(`[title=${title}] + div`);
  await page.hover("text='Docs'"); // move mouse off the sidebar
  await waitForPageToStopMoving(page);
}

export async function createEmptyHTTPHandler(page: Page) {
  await createHandler(page, "HTTP");
}

export async function createHTTPHandler(
  page: Page,
  method: string,
  path: string,
) {
  await createEmptyHTTPHandler(page);
  await page.type(Locators.entryBox, method);
  await expectExactText(page, Locators.acHighlightedValue, method);
  await page.keyboard.press("Enter");
  await waitForEmptyEntryBox(page);
  await page.type(Locators.entryBox, path);
  await expectExactText(page, Locators.acHighlightedValue, path);
  await page.keyboard.press("Enter");
  await waitForEmptyFluidEntryBox(page);
}

export async function createWorkerHandler(page) {
  await createHandler(page, "Worker");
  await waitForEmptyEntryBox(page);
}

export async function createRepl(page) {
  await createHandler(page, "REPL");
  await waitForEmptyFluidEntryBox(page);
}

export async function createSecret(page) {
  await page.click(`[title='Secret Keys'] + div`);
}

//********************************
// Make API calls
//********************************
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
// Selections
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

export async function getElementSelectionStart(
  page: Page,
  selector: string,
): Promise<number> {
  return page.$eval(selector, el => (<HTMLInputElement>el).selectionStart);
}
export async function getElementSelectionEnd(
  page: Page,
  selector: string,
): Promise<number> {
  return page.$eval(selector, el => (<HTMLInputElement>el).selectionEnd);
}

//********************************
// Waiting
//********************************
// Entry-box sometimes carries state over briefly, so wait til it's clear
export async function waitForEmptyEntryBox(page: Page): Promise<void> {
  await page.waitForSelector("#entry-box >> text=''");
}

export async function waitForEmptyFluidEntryBox(page: Page): Promise<void> {
  await page.waitForSelector("#active-editor >> text=''");
}

export async function waitForPageToStopMoving(page: Page): Promise<void> {
  // We can do better in the future
  await page.waitForTimeout(500);
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

export async function awaitAnalysisLoaded(page: Page) {
  await page.waitForFunction(() => window.Dark.analysis.initialized);
}

//********************************
// Misc.
//********************************
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

export async function pressShortcut(page: Page, shortcut: string) {
  if (process.platform == "darwin") {
    page.keyboard.press(`Meta+${shortcut}`);
  } else {
    page.keyboard.press(`Control+${shortcut}`);
  }
}
