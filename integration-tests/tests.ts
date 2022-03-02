import {
  test,
  expect,
  ConsoleMessage,
  Page,
  TestInfo,
} from "@playwright/test";
import fs from "fs";

const BASE_URL = process.env.BASE_URL || "http://darklang.localhost:9000";
const BWD_BASE_URL = process.env.BWD_BASE_URL || ".builtwithdark.localhost:11000";
const options = {
  baseURL: BASE_URL,
  bwdBaseURL: BWD_BASE_URL
};
test.use(options);

async function prepSettings(page: Page, testInfo: TestInfo) {
  let setLocalStorage = async (key: string, value: any) => {
    await page.evaluate(
      ([k, v]) => {
        localStorage.setItem(k, v);
      },
      [key, JSON.stringify(value)],
    );
  };

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
      testInfo.title === "record_consent_saved_across_canvases" ? null : false,
  };
  await setLocalStorage(`editorState-test-${testInfo.title}`, editorState);
  await setLocalStorage("userState-test", userState);
}

async function awaitAnalysis(page: Page, lastTimestamp: number) {
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

// Wait until the frontend is loaded - this is when we know we are able to do analyses

interface AnalyisLoadPromiseHolder {
  analysisLoadPromise: Promise<boolean>;
}

function createLibfrontendLoadPromise(
  testInfo: TestInfo,
  page: Page,
): AnalyisLoadPromiseHolder & TestInfo {
  let ti = <TestInfo & AnalyisLoadPromiseHolder>testInfo;
  ti.analysisLoadPromise = new Promise((resolve, reject) => {
    page.on("console", async (msg: ConsoleMessage) => {
      if (msg.text() === "libfrontend reporting in") {
        resolve(true);
      }
    });
  });
  return ti;
}
async function awaitAnalysisLoad(testInfo: TestInfo): Promise<void> {
  let ti = <AnalyisLoadPromiseHolder & TestInfo>testInfo;
  expect(await ti.analysisLoadPromise).toBe(true);
}

interface MessagesHolder {
  messages: ConsoleMessage[];
}
function initMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}

function getMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  return ti.messages;
}

function saveMessage(testInfo: TestInfo, msg: ConsoleMessage) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages.push(msg);
}

function clearMessages(testInfo: TestInfo) {
  let ti = <MessagesHolder & TestInfo>testInfo;
  ti.messages = [];
}

test.describe.parallel("Integration Tests", async () => {
  // To add this user, run the backend tests
  test.beforeEach(async ({ page }, testInfo) => {
    initMessages(testInfo);
    createLibfrontendLoadPromise(testInfo, page);
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
    const testname = testInfo.title;
    var url = `/a/test-${testname}?integration-test=true`;

    var username = "test";
    if (testname.match(/_as_admin/)) {
      username = "test_admin";
    }

    await page.goto(url, { waitUntil: "networkidle" });
    await prepSettings(page, testInfo);
    await page.type("#username", username);
    await page.type("#password", "fVm2CUePzGKCwoEQQdNJktUQ");
    await page.click("text=Login");
    await page.waitForSelector("#finishIntegrationTest");
    await page.mouse.move(0, 0); // can interfere with autocomplete keyboard movements
    await page.pause();
  });

  test.afterEach(async ({ page }, testInfo) => {
    const testname = testInfo.title;

    // write out all logs
    let flushedLogs = false;
    function flushLogs(): boolean {
      let logs = getMessages(testInfo).map(
        (msg: ConsoleMessage) => `${msg.type()}: ${msg.text()}`,
      );
      let filename = `rundir/integration-tests/console-logs/${testname}.log`;
      fs.writeFile(filename, logs.join("\n"), () => {});
      return true;
    }

    await page.pause();
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
        flushedLogs = flushLogs();
      } catch (e) {
        if (flushedLogs === false) {
          flushLogs();
        }
        throw e;
      }
    } else {
      flushedLogs = flushLogs();
    }
  });

  //********************************
  // Utilities
  //********************************
  async function waitForPageToStopMoving(page: Page): Promise<void> {
    // We can do better in the future
    await page.waitForTimeout(500);
  }
  async function createEmptyHTTPHandler(page: Page) {
    await page.click(".sidebar-category.http i.fa-plus-circle");
    await waitForPageToStopMoving(page);
    await waitForEmptyEntryBox(page);
  }

  async function createHTTPHandler(page: Page, method: string, path: string) {
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

  async function createWorkerHandler(page) {
    await page.click(".sidebar-category.worker i.fa-plus-circle");
    await waitForPageToStopMoving(page);
    await waitForEmptyEntryBox(page);
  }

  async function createRepl(page) {
    await page.click(".sidebar-category.repl i.fa-plus-circle");
    await waitForPageToStopMoving(page);
    await waitForEmptyFluidEntryBox(page);
  }

  async function gotoAST(page: Page): Promise<void> {
    await page.click("#active-editor > span");
  }

  function bwdUrl(testInfo: TestInfo, path: string) {
    return (
      "http://test-" + testInfo.title + options.bwdBaseURL + path
    );
  }

  async function pressShortcut(page: Page, shortcut: string) {
    if (process.platform == "darwin") {
      page.keyboard.press(`Meta+${shortcut}`);
    } else {
      page.keyboard.press(`Control+${shortcut}`);
    }
  }

  async function get(page: Page, url: string): Promise<string> {
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
  async function post(page: Page, url: string, body: string): Promise<string> {
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
  const entryBox = "#entry-box";
  const acHighlightedValue = ".autocomplete-item.highlighted > .name";
  const fluidACHighlightedValue = ".autocomplete-item.fluid-selected";
  const dbLockLocator = ".db .spec-header.lock";

  //********************************
  // Utilities
  //********************************
  async function selectAll(page: Page): Promise<void> {
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

  async function expectExactText(page: Page, selector: string, text: string) {
    await expect(page.locator(selector)).toHaveText(text);
  }
  async function expectContainsText(
    page: Page,
    selector: string,
    text: string,
  ) {
    await expect(page.locator(selector)).toContainText(text);
  }

  async function expectPlaceholderText(page: Page, text: string) {
    await expect(page.locator(entryBox)).toHaveAttribute("placeholder", text);
  }

  // Entry-box sometimes carries state over briefly, so wait til it's clear
  async function waitForEmptyEntryBox(page: Page): Promise<void> {
    await page.waitForSelector("#entry-box >> text=''");
  }

  async function waitForEmptyFluidEntryBox(page: Page): Promise<void> {
    await page.waitForSelector("#active-editor >> text=''");
  }

  async function gotoHash(page: Page, testInfo: TestInfo, hash: string) {
    const testname = testInfo.title;
    var url = `/a/test-${testname}?integration-test=true`;
    // networkidle needed for hash
    // await page.goto(`${url}#${hash}`, { waitUntil: "networkidle" });
    await page.goto(`${url}#${hash}`);
  }

  async function selectText(
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

  function caretPos(pos: number) {
    return { position: { x: pos * 8, y: 4 } };
  }

  async function getStyleProperty(
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

  // const scrollBy = ClientFunction((id, dx, dy) => {
  //   document.getElementById(id).scrollBy(dx, dy);
  // });

  async function getElementSelectionStart(
    page: Page,
    selector: string,
  ): Promise<number> {
    return page.$eval(selector, el => (<HTMLInputElement>el).selectionStart);
  }
  async function getElementSelectionEnd(
    page: Page,
    selector: string,
  ): Promise<number> {
    return page.$eval(selector, el => (<HTMLInputElement>el).selectionEnd);
  }

  // ------------------------
  // Tests below here. Don't forget to update client/src/IntegrationTest.ml
  // ------------------------

  test("switching_from_http_to_cron_space_removes_leading_slash", async ({
    page,
  }) => {
    await createHTTPHandler(page, "POST", "/spec_name");

    // edit space
    await page.click(".spec-header > .toplevel-type > .space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await waitForEmptyEntryBox(page);
    await page.type(entryBox, "CRON");
    await page.pause();
    await page.keyboard.press("Enter");
  });

  test("switching_from_http_to_repl_space_removes_leading_slash", async ({
    page,
  }) => {
    await createHTTPHandler(page, "POST", "/spec_name");

    // edit space
    await page.click(".spec-header > .toplevel-type > .space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await waitForEmptyEntryBox(page);
    await page.type(entryBox, "REPL");
    await page.keyboard.press("Enter");
  });

  test("switching_from_http_space_removes_variable_colons", async ({
    page,
  }) => {
    await createHTTPHandler(page, "POST", "/spec_name/:variable");

    // edit space
    await page.click(".spec-header > .toplevel-type > .space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await waitForEmptyEntryBox(page);
    await page.type(entryBox, "REPL");
    await page.keyboard.press("Enter");
  });

  test("enter_changes_state", async ({ page }) => {
    await page.keyboard.press("Enter");
    await page.waitForSelector(entryBox);
  });

  test("field_access_closes", async ({ page }, testInfo) => {
    await createEmptyHTTPHandler(page);
    await gotoAST(page);
    await awaitAnalysisLoad(testInfo);

    await page.type("#active-editor", "re");
    let start = Date.now();
    await page.type("#active-editor", "q");
    await expectContainsText(page, fluidACHighlightedValue, "request");
    // There's a race condition here, sometimes the client doesn't manage to load the
    // trace for quite some time, and the autocomplete box ends up in a weird
    // condition
    await awaitAnalysis(page, start);
    await expectExactText(page, fluidACHighlightedValue, "requestDict");
    await page.type("#active-editor", ".bo");
    await expectExactText(page, fluidACHighlightedValue, "bodyfield");
    await page.keyboard.press("Enter");
  });

  // CLEANUP flaky - often the `request` field is not available
  // test("field_access_pipes", async ({ page }, testInfo) => {
  //   await createEmptyHTTPHandler(page);
  //   await awaitAnalysisLoad(testInfo);
  //   await gotoAST(page);

  //   await page.type("#active-editor", "req");
  //   await expectContainsText(page, fluidACHighlightedValue, "request");
  //   await page.type("#active-editor", ".");
  //   await page.type("#active-editor", "bo");
  //   await expectExactText(page, fluidACHighlightedValue, "bodyfield");
  //   await page.keyboard.press("Shift+Enter");
  // });

  test("tabbing_works", async ({ page }) => {
    await createRepl(page);

    // Fill in "then" box in if stmt
    await page.type("#active-editor", "if ");
    await page.keyboard.press("Tab");
    await page.type("#active-editor", "5");
  });

  test("autocomplete_highlights_on_partial_match", async ({ page }) => {
    await createRepl(page);
    await gotoAST(page);

    await page.type("#active-editor", "Int::add");

    await expectContainsText(page, fluidACHighlightedValue, "Int::add");
    await page.keyboard.press("Enter");
  });

  test("no_request_global_in_non_http_space", async ({ page }) => {
    await createWorkerHandler(page);
    await gotoAST(page);
    await page.type("#active-editor", "request");
    await page.keyboard.press("ArrowDown");
    await expectContainsText(page, fluidACHighlightedValue, "Http::badRequest");
    await page.keyboard.press("Enter");
  });

  test("ellen_hello_world_demo", async ({ page }) => {
    await createEmptyHTTPHandler(page);

    // verb
    await page.type(entryBox, "g");
    await page.keyboard.press("Enter");

    // route
    await page.type(entryBox, "/hello");
    await page.keyboard.press("Enter");

    // string
    await page.type("#active-editor", '"Hello world!"');
  });

  test("editing_headers", async ({ page }) => {
    await createHTTPHandler(page, "POST", "/hello");

    // edit them
    await page.click(".spec-header > .toplevel-name");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "/myroute");
    await page.keyboard.press("Enter");

    await page.click(".spec-header > .toplevel-type > .modifier");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "GET");
    await page.keyboard.press("Enter");
  });

  test("switching_to_http_space_adds_slash", async ({ page }) => {
    await createWorkerHandler(page);

    // add headers
    await page.click(".spec-header > .toplevel-name");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "spec_name");
    await page.keyboard.press("Enter");

    // edit space
    await page.click(".spec-header > .toplevel-type > .space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "HTTP");
    await page.keyboard.press("Enter");
  });

  test("switching_from_default_repl_space_removes_name", async ({ page }) => {
    await createRepl(page);

    // edit space
    await page.click(".spec-header > .toplevel-type >.space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await waitForEmptyEntryBox(page);
    await page.type(entryBox, "CRON");
    await page.keyboard.press("Enter");
  });

  test("tabbing_through_let", async ({ page }) => {
    await createRepl(page);
    await gotoAST(page);

    await page.type("#active-editor", "let");
    await page.keyboard.press("Enter");
    // round trip through the let blanks once
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    // go to the body and fill it in
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    await page.type("#active-editor", "5");
    // go to the rhs and fill it in
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    await page.type("#active-editor", "5");
    // fill in the var
    await page.keyboard.press("Tab");
    await page.keyboard.press("Tab");
    await page.type("#active-editor", "myvar");
  });

  test("rename_db_fields", async ({ page }, testInfo) => {
    // rename
    await page.click(".name >> text='field1'");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "field6");
    await page.keyboard.press("Enter");
    await page.waitForResponse(`${BASE_URL}/api/test-rename_db_fields/add_op`);

    // add data and check we can't rename again
    let url = bwdUrl(testInfo, "/add");
    await post(page, url, '{ "field6": "a", "field2": "b" }');
    await page.waitForSelector(dbLockLocator);

    await page.click(".name >> text='field6'");
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
  });

  test("rename_db_type", async ({ page }, testInfo) => {
    // rename
    await page.click(".type >> text='Int'");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "String");
    await page.keyboard.press("Enter");
    await page.waitForResponse(`${BASE_URL}/api/test-rename_db_type/add_op`);

    // add data and check we can't rename again
    let url = bwdUrl(testInfo, "/add");
    await post(page, url, '{ "field1": "str", "field2": 5 }');
    await page.waitForSelector(dbLockLocator, { timeout: 8000 });

    await page.click(".type >> text='String'");
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
  });

  /* Disable for now, will bring back as command palette fn
test("feature_flag_works", async ({ page }) => {

    // Create an empty let
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "let");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "a");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "13");
    await page.keyboard.press("Enter");
    await page.keyboard.press("ArrowDown")
    await page.keyboard.press("TODO: esc);

    // Click feature name
    .click('.expr-actions .flag')

    // Name it
    await page.waitForSelector(".feature-flag");
    await page.type(entryBox, "myflag");
    await page.keyboard.press("Enter");

    // Set condition
    await page.type(entryBox, "Int::greaterThan");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "a");
    await page.keyboard.press("Enter");
    await page.type(entryBox, "10");
    await page.keyboard.press("Enter");

    // Case A
    await page.type(entryBox, "\"");
    await page.type(entryBox, "A");
    await page.keyboard.press("Enter");

    // Case B
    await page.type(entryBox, "\"");
    await page.type(entryBox, "B");
    await page.keyboard.press("Enter");

});

test("feature_flag_in_function", async ({ page }) => {

    // Go to function
    await page.click(".fun1")
    await page.click(".fa-edit")

    await page.waitForSelector(".tl-2296485551");
    await page.click(".tl-2296485551")
    await page.keyboard.press("Enter");

    // Make feature Flag
    .click('.expr-actions .flag')

    await page.waitForSelector(".feature-flag");
    await page.type(entryBox, "myflag");
    await page.keyboard.press("Enter");

    // Set condition
    await page.type(entryBox, "true");
    await page.keyboard.press("Enter");

    // Case B
    await page.type(entryBox, "3");
    await page.keyboard.press("Enter");

    // Return to main canvas to finish tests
    await page.click(".return-to-canvas")
    await page.waitForSelector(".tl-180770093");
});
*/
  test("rename_function", async ({ page }, testInfo) => {
    const fnNameBlankOr = ".fn-name-content";
    await gotoHash(page, testInfo, "fn=123");
    await page.waitForSelector(fnNameBlankOr);

    // check not changing function name does not cause error message to show
    await page.dblclick(fnNameBlankOr);
    await gotoAST(page);
    await expect(page.locator(".error-panel.show")).not.toBeVisible();

    // now actually rename the function to a different name
    await page.click(fnNameBlankOr);
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, "hello");
    await page.keyboard.press("Enter");
  });

  test("execute_function_works", async ({ page }) => {
    await createRepl(page);
    await page.waitForSelector("#active-editor");
    await page.type("#active-editor", "Uuid::gen");
    await page.keyboard.press("Enter");

    const t1 = Date.now();
    await page.click(".execution-button");
    await awaitAnalysis(page, t1);
    await expect(page.locator(".selected .live-value.loaded")).not.toHaveText(
      "Function is executing",
    );
    let v1 = await page.textContent(".selected .live-value.loaded");

    const t2 = Date.now();
    await page.click(".fa-redo");
    await awaitAnalysis(page, t2);

    await expect(page.locator(".selected .live-value.loaded")).not.toHaveText(
      v1,
    );
    await expect(page.locator(".selected .live-value.loaded")).not.toHaveText(
      "Function is executing",
    );
    let v2 = await page.textContent(".selected .live-value.loaded");

    let re =
      /<UUID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}>/;
    expect(v1).toMatch(re);
    expect(v2).toMatch(re);
  });

  test("correct_field_livevalue", async ({ page }) => {
    await page.click(".fluid-editor"); // this click required to activate the editor
    await page.click(".fluid-field-name >> text='gth'");

    await expectExactText(page, ".selected .live-value.loaded", "5");
  });

  test("int_add_with_float_error_includes_fnname", async ({ page }) => {
    const timestamp = Date.now();
    await page.click(".tl-123 .fluid-category-function"); // required to see the return value (navigate is insufficient)
    await awaitAnalysis(page, timestamp);

    await page.waitForSelector(".return-value");

    const expectedText = "Try using Float::+, or use Float::truncate to truncate Floats to Ints.";
    await expectContainsText(page, ".return-value", expectedText);
  });

  test("function_version_renders", async ({ page }) => {
    await createRepl(page);

    await page.type("#active-editor", "DB::del");
    let selector = ".autocomplete-item.fluid-selected .version";
    await expectExactText(page, selector, "v1");
  });

  test("delete_db_col", async ({ page }) => {
    await page.click(".delete-col");
    await expect(page.locator(".delete-col")).not.toBeVisible();
  });

  test("cant_delete_locked_col", async ({ page }) => {
    await page.click(".fluid-fn-name"); // this click is required due to caching
    await page.waitForSelector(".execution-button-needed");

    await page.click(".execution-button-needed");

    await page.waitForSelector(dbLockLocator, { timeout: 8000 });

    await page.click(".db"); // this click is required due to caching
    await expect(page.locator(".delete-col")).not.toBeVisible();
  });

  test("select_route", async ({ page }) => {
    const categoryHeader = ".sidebar-category.http .category-summary";
    const httpVerbLink =
      ".sidebar-category.http .category-content a.toplevel-link";
    const toplevelElement = ".node .toplevel";

    await page.click(categoryHeader);
    await page.waitForSelector(httpVerbLink);

    await page.click(httpVerbLink);
    await page.waitForSelector(toplevelElement);

    await expect(page.locator(toplevelElement)).toHaveClass(/selected/);
  });

  // TODO: This needs Stroller/Pusher in CI
  // test('passwords_are_redacted', async ({ page }) => {
  //   const callBackend = ClientFunction(
  //     function (url) {
  //       var xhttp = new XMLHttpRequest();
  //       xhttp.open("POST", url, true);
  //       xhttp.setRequestHeader("Content-type", "application/json");
  //       xhttp.send('{ "password": "redactme!" }');
  //     });

  //   .click(Selector('.Password\\:\\:hash'))
  //   await callBackend(user_content_url(t, "/signup"));
  //   await expect(Selector('.live-value').textContent).eql('<Password: Redacted>', { timeout: 5000 })
  // })

  // TODO: Add test that verifies pasting text/plain when Entering works
  // See: https://github.com/darklang/dark/pull/725#pullrequestreview-213661810

  test("function_analysis_works", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, `fn=1039370895`);
    await page.waitForSelector(".user-fn-toplevel");
    await page.click(".user-fn-toplevel #active-editor .fluid-binop");
    await expectExactText(page, ".selected .live-value.loaded", "10");
  });

  test("jump_to_error", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.click(".fluid-entry");
    const timestamp = Date.now();
    await page.click(".fluid-entry.id-675551618");
    await awaitAnalysis(page, timestamp);
    await page.click(".jump-src");
  });

  test("fourohfours_parse", async ({ page }) => {
    await page.evaluate(() => {
      const data = [
        "HTTP",
        "/nonexistant",
        "GET",
        "2019-03-15T22:16:40Z",
        "0623608c-a339-45b3-8233-0eec6120e0df",
      ];
      var event = new CustomEvent("new404Push", { detail: data });
      document.dispatchEvent(event);
    });
  });

  test("fn_page_to_handler_pos", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "fn=890");
    await page.waitForSelector(".user-fn-toplevel");
    const fnOffset = await getStyleProperty(page, "#canvas", "transform");

    await gotoHash(page, testInfo, "handler=123");
    await page.waitForSelector(".tl-123");

    expect(await getStyleProperty(page, "#canvas", "transform")).not.toBe(
      fnOffset,
    );
  });

  test("autocomplete_visible_height", async ({ page }) => {
    await createRepl(page);

    await page.keyboard.press("r");
    await expect(
      page.locator("li.autocomplete-item.valid").nth(5),
    ).toBeVisible();
  });

  // Disabled the feature for now
  // test("create_new_function_from_autocomplete", async ({ page }) => {
  //   await createRepl(page);
  //
  //     await page.type("#active-editor", "myFunctionName");
  //     .expect(fluidACHighlightedValue(page)())
  //     .eql("Create new function: myFunctionName")
  //     await page.keyboard.press("Enter");;
  // });

  test("load_with_unnamed_function", async ({ page }) => {
    await page.keyboard.press("Enter");
    await page.waitForSelector(entryBox);
  });

  test("extract_from_function", async ({ page }, testInfo) => {
    const exprElem = ".user-fn-toplevel #active-editor > span";
    await gotoHash(page, testInfo, "fn=123");
    await page.waitForSelector(".tl-123");
    await page.click(exprElem);
    await selectText(page, exprElem, 0, 1);
    await page.keyboard.press("Control+\\");
    await page.type("#cmd-filter", "extract-function");
    await page.keyboard.press("Enter");
  });

  test("fluid_execute_function_shows_live_value", async ({ page }, ti) => {
    /* NOTE: This test is intended to determine if clicking a play button in fluid
    makes the live value visible. There is some extra complication in that clicking
    on a play button as it stands does not actually "count" as clicking on the play button
    unless the handler is "active" with a placed caret. To account for this, we
    click on "hello" within Crypto::sha256 ("hello" |> String::toBytes) after focusing
    in order to place the caret. Then we click on the button and see if the live value
    corresponds to the result of `Crypto::sha256`. */
    await gotoHash(page, ti, "handler=1013604333");
    await page.click(".id-1045574047.fluid-string");
    await page.click(".id-1334251057 .execution-button-needed"); // wait for it to be green
    await page.waitForSelector(".selected .live-value.loaded");
    await expectExactText(
      page,
      ".selected .live-value.loaded",
      "<Bytes: length=32>",
    );
  });

  test("fluid_single_click_on_token_in_deselected_handler_focuses", async ({
    page,
  }) => {
    let target = ".id-2068425241.fluid-let-var-name";
    await page.waitForSelector(target);
    await page.click(target, caretPos(4));
  });

  test("fluid_click_2x_on_token_places_cursor", async ({ page }) => {
    let target = ".id-549681748.fluid-let-var-name";
    await page.waitForSelector(target);
    await page.click(target, caretPos(2));
    await page.click(target, caretPos(2));
  });

  test("fluid_click_2x_in_function_places_cursor", async ({
    page,
  }, testInfo) => {
    await gotoHash(page, testInfo, "fn=1352039682");
    await page.waitForSelector(".id-677483670.fluid-let-var-name");
    await page.click(".id-677483670.fluid-let-var-name", caretPos(2));
    await page.waitForSelector(".id-96908617.fluid-category-string");
    await page.click(".id-96908617.fluid-category-string", caretPos(2));
  });

  test("fluid_doubleclick_selects_token", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    await page.dblclick(".fluid-match-keyword", caretPos(3));
  });

  test("fluid_doubleclick_selects_word_in_string", async ({ page }, ti) => {
    await gotoHash(page, ti, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    await page.dblclick(".fluid-string");
  });

  test("fluid_doubleclick_selects_entire_fnname", async ({
    page,
  }, testInfo) => {
    await gotoHash(page, testInfo, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    await page.dblclick(".fluid-fn-name", caretPos(8));
  });

  test("fluid_doubleclick_with_alt_selects_expression", async ({
    page,
  }, ti) => {
    await gotoHash(page, ti, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    const options = { modifiers: ["Alt"], position: { x: 24, y: 4 } };
    await page.dblclick(".fluid-match-keyword", options);
  });

  test("fluid_shift_right_selects_chars_in_front", async ({ page }, ti) => {
    await gotoHash(page, ti, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    await page.click(".fluid-category-string", caretPos(2));
    await page.keyboard.press("Shift+ArrowRight");
    await page.keyboard.press("Shift+ArrowDown");
    await page.keyboard.press("Shift+ArrowRight");
  });

  test("fluid_shift_left_selects_chars_at_back", async ({ page }, ti) => {
    await gotoHash(page, ti, "handler=123");
    await page.waitForSelector(".tl-123");
    await page.waitForSelector(".selected #active-editor");
    await page.click(".fluid-category-string", caretPos(2));
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("Shift+ArrowLeft");
    await page.keyboard.press("Shift+ArrowUp");
  });

  test("fluid_undo_redo_happen_exactly_once", async ({ page }) => {
    await page.waitForSelector(".tl-608699171");
    await page.click(".id-68470584.fluid-category-string");
    await page.waitForSelector(".selected #active-editor");
    await expectExactText(page, ".fluid-category-string", '"12345"');
    await pressShortcut(page, "z");
    await expectExactText(page, ".fluid-category-string", '"1234"');
    await pressShortcut(page, "Shift+z");
    await expectExactText(page, ".fluid-category-string", '"12345"');
  });

  test("fluid_ctrl_left_on_string", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=428972234");
    await page.waitForSelector(".tl-428972234");
    await page.waitForSelector(".selected #active-editor");
    await page.click(".fluid-string", caretPos(10));
    await page.keyboard.press("Control+ArrowLeft");
  });

  test("fluid_ctrl_right_on_string", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=428972234");
    await page.waitForSelector(".tl-428972234");
    await page.waitForSelector(".selected #active-editor");
    await page.click(".fluid-string", caretPos(10));
    await page.keyboard.press("Control+ArrowRight");
  });

  test("fluid_ctrl_left_on_empty_match", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=281413634");
    await page.waitForSelector(".tl-281413634");
    await page.waitForSelector(".selected #active-editor");
    await page.click(".fluid-category-pattern.id-63381027", caretPos(0));
    await page.keyboard.press("Control+ArrowLeft");
  });

  test("varnames_are_incomplete", async ({ page }) => {
    await page.click(".toplevel");
    await page.click(".spec-header > .toplevel-name");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type(entryBox, ":a");
    await expectExactText(page, acHighlightedValue, "/:a");
    await page.keyboard.press("Tab");
    await page.keyboard.press("a");
    await page.keyboard.press("Enter");
    await expectContainsText(page, ".live-value.loaded", "<Incomplete>");
  });

  test("center_toplevel", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "handler=1445447347");
    await page.waitForSelector(".tl-1445447347");
  });

  test("max_callstack_bug", async ({ page }) => {
    await createRepl(page);
    await gotoAST(page);

    // I don't know what the threshold is exactly, but 1500 didn't tickle
    // the bug
    await page.keyboard.type("List::range 0 2000 ");
  });

  test("sidebar_opens_function", async ({ page }) => {
    await page.waitForSelector(".sidebar-category.fns .category-summary");
    await page.click(".sidebar-category.fns .category-summary");
    await page.waitForSelector(
      ".sidebar-category.fns a[href='#fn=1352039682']",
    );
    await page.click(".sidebar-category.fns a[href='#fn=1352039682']");
    expect(page.url()).toMatch(/.+#fn=1352039682$/);
  });

  test("empty_fn_never_called_result", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "fn=602952746");
    const timestamp = Date.now();

    await page.click(".id-1276585567");
    // clicking twice in hopes of making the test more stable
    await page.click(".id-1276585567");
    await awaitAnalysis(page, timestamp);

    await page.waitForSelector(".return-value .warning-message");
    let expected =
      "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.";
    await expectContainsText(page, ".return-value", expected);
  });

  test("empty_fn_been_called_result", async ({ page }, testInfo) => {
    await page.waitForSelector(".execution-button");
    await page.click(".execution-button");
    await gotoHash(page, testInfo, "fn=602952746");
    await page.click(".id-1276585567");
    // clicking twice makes the test more stable
    await page.click(".id-1276585567");
    await page.waitForSelector(".return-value .warning-message");
    let expected =
      "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.";
    await expectContainsText(page, ".return-value", expected);
  });

  // This runs through
  // https://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html
  // It duplicates backend/test/test_otherlibs.ml's "Crypto::sha256hmac works for
  // AWS", _but_ its value _here_ is that we do not have any other tests that push
  // a handler's trigger play button; getting that working was surprisingly hard,
  // and so lets keep it around to use in future integration tests.
  //
  // See integration-tests/README.md for docs on this.
  //
  // I have tried several other approaches, including wait(3000) between
  // navigateTo() and click(), and putting navigateTo() and click() on separate
  // await ts, but only calling click() twice worked here.
  test("sha256hmac_for_aws", async ({ page }, testInfo) => {
    let before = Date.now();
    await gotoHash(page, testInfo, "handler=1471262983");

    // click into the body, then wait for the button to appear
    await page.click(".fluid-let-var-name >> text='scope'");
    await awaitAnalysisLoad(testInfo);
    await awaitAnalysis(page, before); // wait for the first analysis to get the trigger visible

    let beforeClick = Date.now();
    await page.click("div.handler-trigger");
    await awaitAnalysis(page, beforeClick);

    let expected =
      '"5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"';
    await expectContainsText(page, ".return-value", expected);
  });

  test("fluid_fn_pg_change", async ({ page }, testInfo) => {
    await gotoHash(page, testInfo, "fn=2091743543");
    await page.waitForSelector(".tl-2091743543");

    await gotoHash(page, testInfo, "fn=1464810122");
    await page.waitForSelector(".tl-1464810122");

    // Click into code to edit
    await page.click(".fluid-entry.id-1154335426");

    //Make sure we stay on the page
    await page.waitForSelector(".tl-1464810122");
  });

  test("fluid_creating_an_http_handler_focuses_the_verb", async ({ page }) => {
    await createEmptyHTTPHandler(page);

    await page.keyboard.press("ArrowDown"); // enter AC
    await expectExactText(page, acHighlightedValue, "GET");
  });

  // CLEANUP: broken
  // test("fluid_tabbing_from_an_http_handler_spec_to_ast", async ({ page }) => {
  //   await createEmptyHTTPHandler(page);
  //   await expectPlaceholderText(page, "verb");

  //   await page.keyboard.press("Tab"); // verb -> route
  //   await expectPlaceholderText(page, "route");
  //   await page.keyboard.press("Tab"); // route -> ast
  //   await page.waitForSelector(".fluid-entry.cursor-on");
  //   await page.keyboard.press("r"); // enter AC
  //   await expectContainsText(page, fluidACHighlightedValue, "request");
  // });

  // CLEANUP: tabbing is broken and should be fixed
  // test("fluid_tabbing_from_handler_spec_past_ast_back_to_verb", async ({
  //   page,
  // }) => {
  //   await createEmptyHTTPHandler(page);
  //   await expectPlaceholderText(page, "verb");

  //   await page.keyboard.press("Tab"); // verb -> route
  //   await expectPlaceholderText(page, "route");
  //   await page.keyboard.press("Tab"); // route -> ast
  //   await page.waitForSelector(".fluid-entry.cursor-on");
  //   await page.keyboard.press("Tab"); // ast -> loop back to verb;
  //   await expectPlaceholderText(page, "verb");
  //   await page.keyboard.press("ArrowDown"); // enter AC
  //   await expectExactText(page, acHighlightedValue, "GET");
  // });

  // CLEANUP: tabbing is broken and should be fixed
  // test("fluid_shift_tabbing_from_handler_ast_back_to_route", async ({
  //   page,
  // }) => {
  //   await createEmptyHTTPHandler(page);
  //   await expectPlaceholderText(page, "verb");

  //   await page.keyboard.press("Tab"); // verb -> route
  //   await expectPlaceholderText(page, "route");
  //   await page.keyboard.press("Tab"); // route -> ast
  //   await page.waitForSelector(".fluid-entry.cursor-on");
  //   await page.keyboard.press("Shift+Tab"); // ast -> back to route
  //   await expectPlaceholderText(page, "route");
  //   await page.keyboard.press("ArrowDown"); // enter route
  //   await expectExactText(page, acHighlightedValue, "/");
  // });

  test("fluid_test_copy_request_as_curl", async ({ page }, testInfo) => {
    await awaitAnalysisLoad(testInfo);

    let before = Date.now();
    await page.click(".toplevel.tl-91390945");
    await page.waitForSelector(".tl-91390945");
    await awaitAnalysis(page, before); // CLEANUP shouldn't be necessary, race condition in 5% of cases

    let expected = "Click Play to execute function";
    await page.click(".id-753586717");

    // Ensure the anaysis has completed
    await expectContainsText(page, ".live-value.loaded", expected);
    // test logic in IntegrationTest.ml; we load it here because we need an
    // analysis done before we can call the command
  });

  test("fluid_ac_validate_on_lose_focus", async ({ page }, testInfo) => {
    await createEmptyHTTPHandler(page);
    await gotoAST(page);

    await page.type("#active-editor", "request.body");
    await page.click("#app", { position: { x: 500, y: 50 } }); // click away from fluid
    // validate AST in IntegrationTest.ml

    // CLEANUP: there's a log that shouldn't happen here
    clearMessages(testInfo);
  });

  async function upload_pkg_for_tlid(
    page: Page,
    testInfo: TestInfo,
    tlid: number,
  ) {
    await gotoHash(page, testInfo, `fn=${tlid}`);
    await page.click(".fn-actions > .menu > .more-actions > .toggle-btn");
    await page.click(
      ".fn-actions > .menu > .more-actions > .actions > .item >> text='Upload Function'",
    );
    await page.waitForSelector(".error-panel.show");
  }

  // FSTODO
  // // this tests:
  // // - happy path upload
  // // - upload fails b/c the db already has a fn with this name + version
  // // - upload fails b/c the version we're trying to upload is too low (eg, if you
  // // already have a v1, you can't upload a v0)
  // test("upload_pkg_fn_as_admin", async ({ page }, testInfo) => {
  //   // upload v1/2/3 depending whether this is test run 1/2/3
  //   const tlid = testInfo.retry + 1;

  //   // it should succeed, it's a new package_fn
  //   await upload_pkg_for_tlid(page, testInfo, tlid);

  //   await expectExactText(
  //     page,
  //     ".error-panel.show",
  //     "Successfully uploaded functionDismiss",
  //   );
  //   await page.click(".dismissBtn");

  //   // attempting to upload v0 should fail, because we already have a version
  //   // greater than 0 in the db
  //   await upload_pkg_for_tlid(page, testInfo, 0);
  //   // this failureMsg2 is the same as failureMsg above, because its text dpends
  //   // on the latest version (and the next valid version of the fn), not the
  //   // version you tried to upload
  //   const failureMsg2 = `Bad status: Bad Request - Function already exists with this name and versions up to ${tlid}, try version ${
  //     tlid + 1
  //   }? (UploadFnAPICallback)Dismiss`;
  //   await expectExactText(page, ".error-panel.show", failureMsg2);
  //   await page.click(".dismissBtn");

  //   // second (attempted) upload should fail, as we've already uploaded this
  //   await upload_pkg_for_tlid(page, testInfo, tlid);
  //   const failureMsg = `Bad status: Bad Request - Function already exists with this name and versions up to ${tlid}, try version ${
  //     tlid + 1
  //   }? (UploadFnAPICallback)Dismiss`;
  //   await expectExactText(page, ".error-panel.show", failureMsg);
  //   await page.click(".dismissBtn");

  //   // CLEANUP: this is a hack to get the test to pass, but really the errors should be cleared up
  //   clearMessages(testInfo);
  // });

  test("use_pkg_fn", async ({ page }, testInfo) => {
    const attempt = testInfo.retry + 1;
    const url = `/${attempt}`;
    await createHTTPHandler(page, "GET", url);
    await gotoAST(page);
    await awaitAnalysisLoad(testInfo);

    // this await confirms that test_admin/stdlib/Test::one_v1 is in fact
    // in the autocomplete
    let before = Date.now();
    await page.type("#active-editor", "test_admin");
    await awaitAnalysis(page, before);
    await expectExactText(
      page,
      ".autocomplete-item.fluid-selected.valid",
      "test_admin/stdlib/Test::one_v1Any",
    );
    await page.keyboard.press("Enter");

    // this await confirms that we can get a live value in the editor
    await page.click(".execution-button");
    await expectContainsText(page, ".return-value", "0");

    // check if we can get a result from the bwd endpoint
    let response = await get(page, bwdUrl(testInfo, url));
    expect(response).toBe("0");
  });

  test("fluid_show_docs_for_command_on_selected_code", async ({ page }) => {
    await createRepl(page);
    await gotoAST(page);
    await page.type("#active-editor", "1999");
    await page.keyboard.press("Control+\\");

    await page.waitForSelector("#cmd-filter");
    await page.waitForSelector(".documentation-box");
  });

  // Regression test:
  // Pre-fix, we expect the response body to be "Zm9v" ("foo" |> base64).
  // Post-fix, we expect "foo"
  test("fluid-bytes-response", async ({ page }, testInfo) => {
    const resp = await get(page, bwdUrl(testInfo, "/"));
    expect(resp).toBe("foo");
  });

  test("double_clicking_blankor_selects_it", async ({ page }) => {
    // This is part of fixing double-click behaviour in HTTP headers and other
    // blank-ors. When you clicked on the HTTP header, the caret did not stay in
    // the header. This checks that it does.
    //
    // I managed to fix it by determining that the doubleclick handler was
    // failing, and fixing that. However, it didn't give it the ideal behaviour,
    // where the word double-clicked on would be highlighted.
    //
    // So this test is not for the ideal behaviour, but for the
    // non-obviously-broken behaviour.
    let selector = ".toplevel .spec-header .toplevel-name";
    await page.dblclick(selector);

    // Selected text is /hello
    selector = ".toplevel .spec-header .toplevel-name #entry-box";
    let result = await getElementSelectionStart(page, selector);
    expect(typeof result).toBe("number");
  });

  test("abridged_sidebar_content_visible_on_hover", async ({ page }) => {
    // uncollapse sidebar first (collapsed for easier testing via localstorage)
    await page.click(".toggle-sidebar-btn");
    await page.waitForSelector("text='Collapse sidebar'");
    // collapse sidebar to abridged mode
    await page.click(".toggle-sidebar-btn");
    await page.waitForSelector(".viewing-table.abridged");

    const httpCatSelector = ".sidebar-category.http";

    // hovering over a category makes its contents visible
    let locator = page.locator(httpCatSelector + " .category-content");
    await expect(locator).not.toBeVisible();

    await page.hover(httpCatSelector);
    await expect(locator).toBeVisible();
  });

  test("abridged_sidebar_category_icon_click_disabled", async ({ page }) => {
    const httpCatSelector = ".sidebar-category.http";
    const dbCatSelector = ".sidebar-category.dbs";

    // clicking on a category icon does not keep it open if you mouse elsewhere
    await page.click(httpCatSelector + " .category-icon");
    await page.click(dbCatSelector + " .category-icon");

    await expect(
      page.locator(httpCatSelector + " .category-content"),
    ).not.toBeVisible();
  });

  test("function_docstrings_are_valid", async ({ page }) => {
    // validate functions in IntegrationTest.ml
  });

  test("record_consent_saved_across_canvases", async ({ page }, testInfo) => {
    await page.click("#fs-consent-yes");
    await page.waitForSelector(".fullstory-modal.hide");
    await page.waitForFunction(() => {
      let result = localStorage.getItem("userState-test");
      result = JSON.parse(result);
      return result["recordConsent"];
    });

    // navigate to another canvas
    await page.goto(`${BASE_URL}/a/test-another-canvas`, {
      waitUntil: "networkidle",
    });

    await page.waitForSelector(".fullstory-modal.hide");

    // go back to original canvas to end the test
    const testname = testInfo.title;
    await page.goto(`${BASE_URL}/a/test-${testname}?integration-test=true`);

    // there are errors loading some assets cause we're changing pages pretty fast.
    // Doesn't really matter, so ignore.
    clearMessages(testInfo);
  });

  // This test is flaky; last attempt to fix it added the 1000ms timeout, but that
  // didn't solve the problem
  /*
  test("exe_flow_fades", async ({ page }) => {
    const timestamp = new Date();
    await page.click(".fluid-entry");
    awaitAnalysis(t, timestamp);
    // wait up to 1000ms for this selector to appear

      .expect(Selector(".fluid-not-executed", { timeout: 1000 }).exists)
      .ok();
  });
  */

  test("unexe_code_unfades_on_focus", async ({ page }) => {
    const timestamp = Date.now();
    await page.click(".fluid-entry");
    await awaitAnalysis(page, timestamp);
    // move caret into a single line
    await page.click(".id-1459002816");
    await page.waitForSelector(
      ".id-1459002816.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".id-2073307217.fluid-not-executed.fluid-code-focus",
    );

    // move caret into multiline string
    await page.click(".fluid-string-ml-start", {
      timeout: 500,
      position: { x: 10, y: 4 }, // otherwise it sometimes clicks on the sidebar
    });
    await page.waitForSelector(
      ".fluid-string-ml-start.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".fluid-string-ml-middle.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".fluid-string-ml-end.fluid-not-executed.fluid-code-focus",
    );

    // move caret into list literal
    await page.click(".fluid-list-comma");
    await page.waitForSelector(
      ".fluid-list-open.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".fluid-list-close.fluid-not-executed.fluid-code-focus",
    );

    // move caret into object literal
    await page.click(".fluid-record-sep");
    await page.waitForSelector(
      ".fluid-record-open.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".fluid-record-fieldname.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".id-2108109721.fluid-not-executed.fluid-code-focus",
    );
    await page.waitForSelector(
      ".fluid-record-close.fluid-not-executed.fluid-code-focus",
    );
  });

  test("create_from_404", async ({ page }) => {
    const f0fCategory = ".sidebar-category.fof";

    await page.evaluate(() => {
      const data = [
        "HTTP",
        "/nonexistant",
        "GET",
        "2019-03-15T22:16:40Z",
        "0623608c-a339-45b3-8233-0eec6120e0df",
      ];
      var event = new CustomEvent("new404Push", { detail: data });
      document.dispatchEvent(event);
    });

    await expect(page.locator(f0fCategory)).not.toHaveClass(/empty/, {
      timeout: 5000,
    });
    await page.hover(f0fCategory);
    await page.click(".fof > .category-content > .simple-item > .add-button");

    await page.waitForSelector(".toplevel .http-get");
  });

  test("unfade_command_palette", async ({ page }) => {
    await page.dblclick(".fluid-let-keyword");
    await page.keyboard.press("Control+\\");
    await page.waitForSelector("#cmd-filter");

    // Checks Command Palette opens inside a token with full opacity
    await page.waitForSelector(".fluid-code-focus > .command-palette");
  });

  test("redo_analysis_on_toggle_erail", async ({ page }) => {
    const fnCall = ".id-108391798";
    const justExpr = ".id-21312903";
    const nothingExpr = ".id-1409226084";
    const errorRail = ".fluid-error-rail";
    const returnValue = ".return-value";

    const t0 = Date.now();
    await page.click(".handler-trigger");
    await awaitAnalysis(page, t0);
    await expect(page.locator(errorRail)).toHaveClass(/show/);
    await expectContainsText(page, returnValue, "<Incomplete>");
    await expect(page.locator(justExpr)).toHaveClass(/fluid-not-executed/);
    await expect(page.locator(nothingExpr)).toHaveClass(/fluid-not-executed/);

    // takes function off rail
    await page.dblclick(fnCall);
    await page.keyboard.press("Control+\\");
    await page.waitForSelector("#cmd-filter");
    await page.type("#cmd-filter", "rail");
    await expectExactText(page, ".fluid-selected", "take-function-off-rail");

    // analysis is reruns
    const t1 = Date.now();
    await page.keyboard.press("Enter");
    await awaitAnalysis(page, t1);

    // assert values have changed
    await expect(page.locator(errorRail)).not.toHaveClass(/show/);
    await expectContainsText(page, returnValue, "1");
    await expect(page.locator(justExpr)).not.toHaveClass(/fluid-not-executed/);
    await expect(page.locator(nothingExpr)).toHaveClass(/fluid-not-executed/);
  });

  test("redo_analysis_on_commit_ff", async ({ page }) => {
    const returnValue = ".return-value";
    const t0 = Date.now();
    await page.click(".handler-trigger");
    await awaitAnalysis(page, t0);
    await expectContainsText(page, returnValue, "farewell Vanessa Ives");

    // commits feature flag

    await page.dblclick(".in-flag");
    await page.keyboard.press("Control+\\");
    await page.waitForSelector("#cmd-filter");
    await page.type("#cmd-filter", "commit");
    await expectExactText(page, ".fluid-selected", "commit-feature-flag");

    // analysis is reruns
    const t1 = Date.now();
    await page.keyboard.press("Enter");
    await awaitAnalysis(page, t1);

    await expectContainsText(page, returnValue, "farewell Dorian Gray");
  });

  // Given a Handler that references a package Function,
  // navigating to that Handler should work,
  // and show a visual reference to the Function.
  //
  // At that point, we should be able to navigate to the Function,
  // and then back to our Handler.
  test("package_function_references_work", async ({ page }, testInfo) => {
    const repl = ".toplevel.tl-92595864";
    const refersTo = ".ref-block.refers-to.pkg-fn";
    const usedIn = ".ref-block.used-in.handler";

    // Start at this specific repl handler
    await gotoHash(page, testInfo, "handler=92595864");
    await page.waitForSelector(repl);

    // Test that the handler we navigated to
    // has a reference to a package manager function
    await page.waitForSelector(refersTo);
    await expectExactText(
      page,
      ".ref-block.refers-to .fnheader",
      "test_admin/stdlib/Test::one_v1",
    );

    // Clicking on it should bring us to that package function
    await page.click(refersTo);
    await page.waitForSelector(".toplevel .pkg-fn-toplevel");

    // which should contain a reference to where we just came from
    await page.waitForSelector(usedIn);
    await expectExactText(page, usedIn, "REPLpkgFnTest");

    // and clicking on that should bring us back.
    await page.click(usedIn);
    await page.waitForSelector(repl);
  });

  test("focus_on_secret_field_on_insert_modal_open", async ({ page }) => {
    await page.waitForSelector(".sidebar-category.secrets .create-tl-icon");

    await createRepl(page);
    await page.type("#active-editor", '"Hello world!"');

    await page.click(".sidebar-category.secrets .create-tl-icon");

    const nameInput = "#new-secret-name-input";

    await expect(page.locator(nameInput)).toBeFocused();

    await page.click(".modal.insert-secret .close-btn");
  });
});
