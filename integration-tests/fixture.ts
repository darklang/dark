import {
  test as base,
  expect,
  ConsoleMessage,
  Page,
  TestInfo,
  BrowserContext,
} from "@playwright/test";
import fs from "fs";

import { awaitAnalysisLoaded, canvasUrl, expectExactText } from "./utilities";

// todo: these are in at least 2 files; fix that?
export const apiserverBaseUrl = process.env.BASE_URL;
export const bwdserverBaseUrl = process.env.BWD_BASE_URL;

base.use({ baseURL: apiserverBaseUrl });

/**
 * Set local storage for userState and editorState. We don't want various UI
 * elements to get in the way, including Fullstory consent, the "welcome to
 * Dark" modal, etc.
 */
async function prepSettings(page: Page, testName: string) {
  async function setLocalStorage(key: string, value: any) {
    await page.evaluate(
      ([k, v]) => {
        localStorage.setItem(k, v);
      },
      [key, JSON.stringify(value)],
    );
  }

  // Turn on fluid debugger
  let editorState = {
    editorSettings: { showFluidDebugger: true },
    sidebarState: {
      mode: ["AbridgedMode"],
    },
    firstVisitToThisCanvas: false,
  };

  let userState = {
    showUserWelcomeModal: false, // Disable the modal
    firstVisitToDark: false,
    // Don't show recordConsent modal or record to Fullstory, unless it's the modal test
    recordConsent:
      testName === "record_consent_saved_across_canvases" ? null : false,
  };
  await setLocalStorage(`editorState-test-${testName}`, editorState);
  await setLocalStorage("userState-test", userState);
}

/** TODO */
interface MessagesHolder {
  messages: ConsoleMessage[];
}
function initMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}

function saveMessage(testInfo: TestInfo, msg: ConsoleMessage) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages.push(msg);
}

function getMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  return ti.messages;
}

export function clearMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}

/**
 * Whether or not the test has passed, we'd flush all console logs to a logfile
 */
async function flushLogs(page: Page, testInfo: TestInfo) {
  function flush(testInfo: TestInfo): boolean {
    let logs = getMessages(testInfo).map(
      (msg: ConsoleMessage) => `${msg.type()}: ${msg.text()}`,
    );
    const testName = testInfo.title;
    let filename = `rundir/integration-tests/console-logs/${testName}.log`;
    fs.writeFile(filename, logs.join("\n"), () => {});
    return true;
  }

  let flushedLogs = false;

  if (testInfo.status === testInfo.expectedStatus) {
    // Only run final checks if we're on the road to success
    try {
      // TODO: clicks on this button are not registered in function space
      // We should probably figure out why.
      // For now, putting a more helpful error message
      await page.click("#finishIntegrationTest");

      // Ensure the test has completed correctly
      await page.waitForSelector("#integrationTestSignal");
      await expectExactText(page, "#integrationTestSignal", "success");

      // check the class
      let class_ = await page.getAttribute("#integrationTestSignal", "class");
      expect(class_).toContain("success");
      expect(class_).not.toContain("failure");

      // Ensure there are no errors in the logs
      var errorMessages = getMessages(testInfo)
        .filter((msg: ConsoleMessage) => msg.type() == "error")
        .map(msg => `[console ${msg.type()}]: ${msg.text()}`);
      expect(errorMessages).toHaveLength(0);
      flushedLogs = flush(testInfo);
    } catch (e) {
      if (flushedLogs === false) {
        flush(testInfo);
      }
      throw e;
    }
  } else {
    flushedLogs = flush(testInfo);
  }
}

/**
 * We define a custom PlayWright fixture so that we may customize
 * what objects are available to us in tests. We mostly set this up so
 * we may have a shared browser context across tests, since Blazor loading
 * takes a bit of time.
 */
type DarkFixtures = {
  sharedBrowser: BrowserContext;
};

export const test = base.extend<{} & DarkFixtures>({
  sharedBrowser: [
    async ({ browser }, use) => {
      let sharedContext = await browser.newContext();

      // load a non-test page, and wait until Blazor is loaded
      let pg = await sharedContext.newPage();
      const canvasName = "just-loading-analysis";
      await pg.goto(canvasUrl(canvasName), { waitUntil: "networkidle" });

      // todo: deal with `record_consent_saved_across_canvases` test
      await prepSettings(pg, canvasName);
      await pg.type("#username", "test");
      await pg.type("#password", "fVm2CUePzGKCwoEQQdNJktUQ");
      await pg.click("text=Login");
      await pg.waitForSelector("#finishIntegrationTest");
      await pg.mouse.move(0, 0); // can interfere with autocomplete keyboard movements
      await awaitAnalysisLoaded(pg);
      await pg.pause();

      await use(sharedContext);

      await sharedContext.close();
      await browser.close();
    },
    // This is what allows `sharedBrowser` to be used across tests/pages
    { scope: "worker" },
  ],

  /**
   * We override the `page` object, so that tests requesting a page
   * are set up appropriately, using the shared browser context.
   */
  page: async ({ sharedBrowser }, use, testInfo: TestInfo) => {
    const testName = testInfo.title;

    let page = await sharedBrowser.newPage();

    // TODO: figure out how we can populate this from the value in `playwright.config.ts`
    page.setDefaultTimeout(25000);

    initMessages(testInfo);

    page.on("pageerror", async err => {
      console.error(err);
    });
    page.on("console", async (msg: ConsoleMessage) => {
      if (msg.text().includes("ERR_CONNECTION_REFUSED")) {
        // We don't load fullstory in tests
        return;
      }
      if (msg.type() == "error") {
        console.error(msg.text());
      }
      saveMessage(testInfo, msg);
    });

    // set settings (e.g. "don't show the 'first time' dialog"), on a separate page
    await page.goto(canvasUrl("test"), {
      waitUntil: "networkidle",
    });

    // Q: why doesn't this override the setting? is it too late?
    // (for the case of `record_consent...`)
    // can we move it up?
    await prepSettings(page, testName);

    // go to URL and wait for analysis
    var url = canvasUrl(testName);
    await page.goto(url, { waitUntil: "networkidle" });
    await page.waitForSelector("#finishIntegrationTest");
    await page.mouse.move(0, 0); // can interfere with autocomplete keyboard movements

    await awaitAnalysisLoaded(page);

    // run the actual test
    await page.pause();
    await use(page);
    await page.pause();

    // clean up - write out all logs
    // TODO: only do this if we're executing within the container
    await flushLogs(page, testInfo);

    await page.close();
  },
});
