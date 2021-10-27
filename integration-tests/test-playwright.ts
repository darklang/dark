import {
  test,
  expect,
  ConsoleMessage,
  Page,
  Locator,
  TestInfo,
  TestFixture,
} from "@playwright/test";
import fs from "fs";

const BASE_URL = process.env.BASE_URL || "http://darklang.localhost:8000";
const options = {
  baseURL: BASE_URL,
};
test.use(options);

// const getPageUrl = ClientFunction(() => window.location.href);

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
    console.log("open");
    let newTimestamp = window.Dark.analysis.lastRun;
    if (newTimestamp > lastTimestamp) {
      const diffInSecs = (newTimestamp - lastTimestamp) / 1000.0;
      console.info("Analysis ran in ~ " + diffInSecs + "secs");
      return true;
    }
    return false;
  };
  console.log("checking analysis");
  await page.waitForFunction(analysisFunction, lastTimestamp, {
    timeout: 10000,
    polling: 1000,
  });
}

// fixture`Integration Tests`
test.describe.parallel("Integration Tests", async () => {
  test.beforeAll(async () => {
    test.messages = [];
  });
  // To add this user, run the backend tests
  test.beforeEach(async ({ page }, testInfo) => {
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
      test.messages.push(msg);
    });
    const testname = testInfo.title;
    var url = `/a/test-${testname}?integration-test=true`;

    var username = "test";
    if (testname.match(/_as_admin/)) {
      username = "test_admin";
    }
    await page.goto(url);
    await prepSettings(page, testInfo);
    await page.type("#username", username);
    await page.type("#password", "fVm2CUePzGKCwoEQQdNJktUQ");
    await page.click("text=Login");
    await page.waitForSelector("#finishIntegrationTest");
    await page.pause();
  });

  /* Testcafe runs everything through a proxy, wrapping all values and
   * objects such that it seems like nothing happened. However, they forgot
   * to wrap objects in Webworker contexts, so calls to Fetch in the worker
   * thinks it's on a different domain. This breaks cookies, auth, CORS,
   * basically everything. So we thread the right url through to do the
   * proxying ourselves. Hopefully they'll fix this and we can remove this
   * code someday */
  // .eval(
  //   () => {
  //     window.testcafeInjectedPrefix = prefix;
  //   },
  //   {
  //     dependencies: {
  //       prefix: `${new URL(t.testRun.browserConnection.url).origin}/${
  //         t.testRun.session.id
  //       }/`,
  //     },
  //   },
  // );

  test.afterEach(async ({ page }, testInfo) => {
    const testname = testInfo.title;

    // write out all logs
    let flushedLogs = false;
    function flushLogs(): boolean {
      let logs = test.messages.map(
        (msg: ConsoleMessage) => `${msg.type()}: ${msg.text()}`,
      );
      let filename = `rundir/integration_test_logs/${testname}.log`;
      fs.writeFile(filename, logs.join("\n"), () => {});
      return true;
    }

    await page.pause();
    if (testInfo.status === testInfo.expectedStatus) {
      // Only run final checks if we're on the road to success
      try {
        // Ensure the test has completed correctly
        const finish = page.locator("#finishIntegrationTest");
        const signal =
          // TODO: clicks on this button are not registered in function space
          // We should probably figure out why.
          // For now, putting a more helpful error message
          await finish.click();
        await page.waitForSelector("#integrationTestSignal");
        // When I tried using the locator, the signal could never be found. But it works this way 🤷‍♂️
        await expect(await page.isVisible("#integrationTestSignal")).toBe(true);

        // check the content
        let content = await page.textContent("#integrationTestSignal");
        await expect(content).toBe("success");
        await expect(content).not.toContain("failure");

        // check the class
        let class_ = await page.getAttribute("#integrationTestSignal", "class");
        await expect(class_).toContain("success");
        await expect(class_).not.toContain("failure");

        // Ensure there are no errors in the logs
        let errorMessages = test.messages.filter(
          (msg: ConsoleMessage) => msg.type() == "error",
        );
        await expect(errorMessages).toHaveLength(0);

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
  // // Utilities
  //********************************
  async function createEmptyHTTPHandler(page: Page) {
    await page.keyboard.press("Enter");
    await waitForEmptyEntryBox(page);
    await page.keyboard.press("ArrowDown");
    await expectExactText(page, acHighlightedValue, "New HTTP handler");
    await page.keyboard.press("Enter");
    await waitForEmptyEntryBox(page);
  }

  async function createHTTPHandler(page: Page, method: string, path: string) {
    await createEmptyHTTPHandler(page);
    await page.type("#entry-box", method);
    await expectExactText(page, acHighlightedValue, method);
    await page.keyboard.press("Enter");
    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", path);
    await expectExactText(page, acHighlightedValue, path);
    await page.keyboard.press("Enter");
  }

  async function createWorkerHandler(page) {
    await page.keyboard.press("Enter");
    await waitForEmptyEntryBox(page);
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("ArrowDown");
    await expectExactText(page, acHighlightedValue, "New Worker");
    await page.keyboard.press("Enter");
  }

  async function createRepl(page) {
    await page.keyboard.press("Enter");
    await page.keyboard.press("Enter");
  }

  async function gotoAST(page: Page): Promise<void> {
    await page.click("#active-editor > span");
  }

  function bwdUrl(testInfo: TestInfo, path: string) {
    return (
      "http://test-" + testInfo.title + ".builtwithdark.lvh.me:8000" + path
    );
  }

  // // pressShortcut will use ctrl on Linux or meta on Mac, depending on which
  // // platform the tests are running.
  // async function pressShortcut(t, shortcutUsingCtrl) {
  //   if (shortcutUsingCtrl === undefined) {
  //     throw (
  //       "pressShortcut expecting a shortcut string like 'ctrl-a' but got undefined. " +
  //       "Did you forget to pass t?"
  //     );
  //   }
  //   var shortcut;
  //   if (t.browser.os.name == "macOS") {
  //     shortcut = shortcutUsingCtrl.replace("ctrl", "meta");
  //   } else {
  //     shortcut = shortcutUsingCtrl;
  //   }
  //   .pressKey(shortcut);
  // }

  async function req(page: Page, method: string, url: string, body: string) {
    await page.evaluate(
      ({ method, url, body }) => {
        var xhttp = new XMLHttpRequest();
        xhttp.open(method, url, true);
        xhttp.setRequestHeader("Content-type", "application/json");
        xhttp.send(body);
      },
      { method: method, url: url, body: body },
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

  // Entry-box sometimes carries state over briefly, so wait til it's clear
  async function waitForEmptyEntryBox(page: Page): Promise<void> {
    await page.waitForSelector("#entry-box >> text=''");
  }

  function createLibfrontendLoadPromise(page: Page) {
    return new Promise((resolve, reject) => {
      page.on("console", async (msg: ConsoleMessage) => {
        if (msg.text() === "libfrontend reporting in") {
          resolve(true);
        }
      });
    });
  }

  async function gotoHash(page: Page, testInfo: TestInfo, hash: string) {
    const testname = testInfo.title;
    var url = `/a/test-${testname}?integration-test=true`;
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

  // // NOTE: this is synchronous, not async, so we don't have to fuss with promises
  // const getBwdResponse = ClientFunction(function (url) {
  //   var xhttp = new XMLHttpRequest();
  //   xhttp.open("GET", url, false);
  //   xhttp.send(null);
  //   return xhttp.responseText;
  // });

  // const getElementSelectionStart = ClientFunction(
  //   selector => selector().selectionStart,
  // );
  // const getElementSelectionEnd = ClientFunction(
  //   selector => selector().selectionEnd,
  // );

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
    await page.type("#entry-box", "CRON");
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
    await page.type("#entry-box", "REPL");
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
    await page.type("#entry-box", "REPL");
    await page.keyboard.press("Enter");
  });

  test("enter_changes_state", async ({ page }) => {
    await page.keyboard.press("Enter");
    await page.waitForSelector(entryBox);
  });

  test("field_access_closes", async ({ page }) => {
    let frontendLoaded = createLibfrontendLoadPromise(page);

    await createEmptyHTTPHandler(page);
    await gotoAST(page);
    await expect(await frontendLoaded).toBe(true, { timeout: 10000 });

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

  test("field_access_pipes", async ({ page }) => {
    let frontendLoaded = createLibfrontendLoadPromise(page);

    await createEmptyHTTPHandler(page);
    await gotoAST(page);
    await expect(await frontendLoaded).toBe(true, { timeout: 10000 });

    await page.type("#active-editor", "req");
    await expectContainsText(page, fluidACHighlightedValue, "request");
    await page.type("#active-editor", ".bo");
    await expectExactText(page, fluidACHighlightedValue, "bodyfield");
    await page.keyboard.press("Shift+Enter");
  });

  test("tabbing_works", async ({ page }) => {
    await createRepl(page);

    // Fill in "then" box in if stmt
    await page.type("#active-editor", "if");
    await page.keyboard.press("Space");
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
    await page.type("#entry-box", "g");
    await page.keyboard.press("Enter");

    // route
    await page.type("#entry-box", "/hello");
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
    await page.type("#entry-box", "/myroute");
    await page.keyboard.press("Enter");

    await page.click(".spec-header > .toplevel-type > .modifier");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type("#entry-box", "GET");
    await page.keyboard.press("Enter");
  });

  test("switching_to_http_space_adds_slash", async ({ page }) => {
    await createWorkerHandler(page);

    // add headers
    await page.click(".spec-header > .toplevel-name");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "spec_name");
    await page.keyboard.press("Enter");

    // edit space
    await page.click(".spec-header > .toplevel-type > .space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type("#entry-box", "HTTP");
    await page.keyboard.press("Enter");
  });

  test("switching_from_default_repl_space_removes_name", async ({ page }) => {
    await createRepl(page);

    // edit space
    await page.click(".spec-header > .toplevel-type >.space");
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type("#entry-box", "CRON");
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
    await page.type("#entry-box", "field6");
    await page.keyboard.press("Enter");

    // add data and check we can't rename again
    let url = bwdUrl(testInfo, "/add");
    await req(page, "POST", url, '{ "field6": "a", "field2": "b" }');
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
    await page.type("#entry-box", "String");
    await page.keyboard.press("Enter");

    // add data and check we can't rename again
    let url = bwdUrl(testInfo, "/add");
    await req(page, "POST", url, '{ "field1": "str", "field2": 5 }');
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
    await page.type("#entry-box", "let");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "a");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "13");
    await page.keyboard.press("Enter");
    await page.keyboard.press("ArrowDown")
    await page.keyboard.press("TODO: esc);

    // Click feature name
    .click('.expr-actions .flag')

    // Name it
    await page.waitForSelector(".feature-flag");
    await page.type("#entry-box", "myflag");
    await page.keyboard.press("Enter");

    // Set condition
    await page.type("#entry-box", "Int::greaterThan");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "a");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "10");
    await page.keyboard.press("Enter");

    // Case A
    await page.type("#entry-box", "\"");
    await page.type("#entry-box", "A");
    await page.keyboard.press("Enter");

    // Case B
    await page.type("#entry-box", "\"");
    await page.type("#entry-box", "B");
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
    await page.type("#entry-box", "myflag");
    await page.keyboard.press("Enter");

    // Set condition
    await page.type("#entry-box", "true");
    await page.keyboard.press("Enter");

    // Case B
    await page.type("#entry-box", "3");
    await page.keyboard.press("Enter");

    // Return to main canvas to finish tests
    await page.click(".return-to-canvas")
    await page.waitForSelector(".tl-180770093");
});
*/
  test("rename_function", async ({ page }, testInfo) => {
    const fnNameBlankOr = ".fn-name-content";
    await gotoHash(page, testInfo, "fn=123");
    await expect(page.locator(fnNameBlankOr)).toBeVisible();

    // check not changing function name does not cause error message to show
    await page.dblclick(fnNameBlankOr);
    await gotoAST(page);
    await expect(page.locator(".error-panel.show")).not.toBeVisible();

    // now actually rename the function to a different name
    await page.click(fnNameBlankOr);
    await selectAll(page);
    await page.keyboard.press("Backspace");
    await page.type("#entry-box", "hello");
    await page.keyboard.press("Enter");
  });

  test("execute_function_works", async ({ page }) => {
    await createRepl(page);
    await expect(page.locator("#active-editor")).toBeVisible({ timeout: 5000 });
    await page.type("#active-editor", "Uuid::gen");
    await page.keyboard.press("Enter");

    const t1 = Date.now();
    await page.click(".execution-button", { timeout: 5000 });
    await awaitAnalysis(page, t1);

    let v1 = await page.textContent(".selected .live-value.loaded");

    const t2 = Date.now();
    await page.click(".fa-redo");
    await awaitAnalysis(page, t2);

    await expect(page.textContent(".selected .live-value.loaded")).not.toBe(v1);
    let v2 = await page.textContent(".selected .live-value.loaded");

    let re =
      /<UUID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}>/;
    await expect(v1).toMatch(re);
    await expect(v2).toMatch(re);
  });

  test("correct_field_livevalue", async ({ page }) => {
    await page.click(".fluid-editor"); // this click required to activate the editor
    await page.click(".fluid-field-name >> text='gth'");

    let v1 = await page.textContent(".selected .live-value.loaded");
    await expect(v1).toBe("5");
  });

  test("int_add_with_float_error_includes_fnname", async ({ page }) => {
    const timestamp = Date.now();
    await page.click(".tl-123 .fluid-category-function"); // required to see the return value (navigate is insufficient)
    await awaitAnalysis(page, timestamp);

    await page.waitForSelector(".return-value");
    await expect(page.locator(".return-value")).toContainText(
      "but + only works on Ints.",
    );
  });

  test("function_version_renders", async ({ page }) => {
    await createRepl(page);

    await page.type("#active-editor", "DB::del");
    await expect(
      page.locator(".autocomplete-item.fluid-selected .version"),
    ).toHaveText("v1");
  });

  test("delete_db_col", async ({ page }) => {
    await page.click(".delete-col");
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
    await expect(page.locator(".selected .live-value.loaded")).toHaveText("10");
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

    await expect(await getStyleProperty(page, "#canvas", "transform")).not.toBe(
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
    await page.waitForSelector("#entry-box");
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

    await expect(
      page.locator(".id-1334251057 .execution-button"),
    ).toBeVisible();
    await page.click(".id-1045574047.fluid-string");
    await page.click(".id-1334251057 .execution-button");
    await page.waitForSelector(".selected .live-value.loaded");
    await expect(page.locator(".selected .live-value.loaded")).toHaveText(
      "<Bytes: length=32>",
    );
  });

  test("fluid_single_click_on_token_in_deselected_handler_focuses", async ({
    page,
  }) => {
    let target = ".id-2068425241.fluid-let-var-name";
    await page.waitForSelector(target);
    await page.click(target, { caretPos: 2 });
  });

  // test("fluid_click_2x_on_token_places_cursor", async ({ page }) => {
  //
  //     await page.waitForSelector(".id-549681748.fluid-let-var-name");
  //     .ok()
  //     .click(Selector(".id-549681748.fluid-let-var-name"), { caretPos: 2 })
  //     .click(Selector(".id-549681748.fluid-let-var-name"), { caretPos: 2 });
  // });

  // test("fluid_click_2x_in_function_places_cursor", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, fn=1352039682);
  //     await page.waitForSelector(".id-677483670.fluid-let-var-name");
  //     .ok()
  //     .click(Selector(".id-677483670.fluid-let-var-name"), { caretPos: 2 })
  //     await page.waitForSelector(".id-96908617.fluid-category-string");
  //     .ok()
  //     .click(Selector(".id-96908617.fluid-category-string"), { caretPos: 2 });
  // });

  // test("fluid_doubleclick_selects_token", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=123);
  //     await page.waitForSelector(".tl-123");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .doubleClick(Selector(".fluid-match-keyword"), { caretPos: 3 });
  // });

  // // This works in practice, but doesn't appear to work in TestCafe 🤨 *)
  // // test("fluid_doubleclick_selects_word_in_string", async ({ page }) => {
  // //
  // //     await gotoHash(page, testInfo, handler=123);
  // //     await page.waitForSelector(".tl-123");
  // //     .ok()
  // //     await page.waitForSelector(".selected #active-editor");
  // //     .ok()
  // //     .doubleClick(Selector(".fluid-string"), { caretPos: 15 });
  // // });
  // //
  // test("fluid_doubleclick_selects_entire_fnname", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=123);
  //     await page.waitForSelector(".tl-123");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .doubleClick(Selector(".fluid-fn-name"), { caretPos: 8 });
  // });

  // test("fluid_doubleclick_with_alt_selects_expression", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=123);
  //     await page.waitForSelector(".tl-123");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .doubleClick(Selector(".fluid-match-keyword"), {
  //       caretPos: 3,
  //       modifiers: { alt: true },
  //     });
  // });

  // test("fluid_shift_right_selects_chars_in_front", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=123);
  //     await page.waitForSelector(".tl-123");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .click(Selector(".fluid-category-string"), { caretPos: 2 })
  //     await page.keyboard.press("TODO: shift+right shift+down shift+right);;
  // });

  // test("fluid_shift_left_selects_chars_at_back", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=123);
  //     await page.waitForSelector(".tl-123");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .click(Selector(".fluid-category-string"), { caretPos: 2 })
  //     await page.keyboard.press("TODO: down shift+left shift+up);;
  // });

  // test("fluid_undo_redo_happen_exactly_once", async ({ page }) => {
  //
  //     await page.waitForSelector(".tl-608699171");
  //     .ok()
  //     await page.click(".id-68470584.fluid-category-string");
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .expect(Selector(".fluid-category-string").textContent)
  //     .eql('"12345"');
  //   await pressShortcut(t, "ctrl+z");
  //   await expect(Selector(".fluid-category-string").textContent).eql('"1234"');
  //   await pressShortcut(t, "ctrl+shift+z");
  //   await expect(Selector(".fluid-category-string").textContent).eql('"12345"');
  // });

  // test("fluid_ctrl_left_on_string", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=428972234);
  //     await page.waitForSelector(".tl-428972234");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .click(Selector(".fluid-string"), { caretPos: 10 })
  //     await page.keyboard.press("Control+ArrowLeft");
  // });

  // test("fluid_ctrl_right_on_string", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=428972234);
  //     await page.waitForSelector(".tl-428972234");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .click(Selector(".fluid-string"), { caretPos: 10 })
  //     await page.keyboard.press("TODO: ctrl+right);;
  // });

  // test("fluid_ctrl_left_on_empty_match", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=281413634);
  //     await page.waitForSelector(".tl-281413634");
  //     .ok()
  //     await page.waitForSelector(".selected #active-editor");
  //     .ok()
  //     .click(Selector(".fluid-category-pattern.id-63381027"), { caretPos: 0 })
  //     await page.keyboard.press("Control+ArrowLeft");
  // });

  // test("varnames_are_incomplete", async ({ page }) => {
  //
  //     await page.click(".toplevel")
  //     await page.click(".spec-header > .toplevel-name");
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", ":a");
  //     expect(await acHighlightedText(page)).toBe("/:a")
  //     .ok()
  //     await page.keyboard.press("TODO: tab a enter);;

  //
  //     .expect(Selector(".live-value.loaded").textContent)
  //     .contains("<Incomplete>");
  // });

  // test("center_toplevel", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=1445447347);
  //     await page.waitForSelector(".tl-1445447347");
  //     .ok();
  // });

  // test("max_callstack_bug", async ({ page }) => {
  //   await createRepl(page);
  //   await gotoAST(page);
  //
  //     // I don't know what the threshold is exactly, but 1500 didn't tickle
  //     // the bug
  //     await page.keyboard.press("TODO: L i s t : : r a n g e space 0 space 2 0 0 0 space);;
  // });

  // test("sidebar_opens_function", async ({ page }) => {
  //
  //     await page.waitForSelector(".sidebar-category.fns .category-summary");
  //     .ok()
  //     await page.click(".sidebar-category.fns .category-summary");
  //     await page.waitForSelector(".sidebar-category.fns a[href='#fn=1352039682']");
  //     .ok()
  //     await page.click(".sidebar-category.fns a[href='#fn=1352039682']");
  //     .expect(getPageUrl())
  //     .match(/.+#fn=1352039682$/, "Url is incorrect");
  // });

  // test("empty_fn_never_called_result", async ({ page }) => {
  //   await gotoHash(page, testInfo, fn=602952746);;
  //   const timestamp = new Date();
  //
  //     await page.click(".id-1276585567")
  //     // clicking twice in hopes of making the test more stable
  //     await page.click(".id-1276585567");
  //   await awaitAnalysis(t, timestamp);
  //
  //     await page.waitForSelector(".return-value .warning-message");
  //     .ok()
  //     .expect(Selector(".return-value").innerText)
  //     .contains(
  //       "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.",
  //     );
  // });

  // test("empty_fn_been_called_result", async ({ page }) => {
  //
  //     await page.waitForSelector(".execution-button");
  //     .ok()
  //     await page.click(".execution-button")
  //     await gotoHash(page, testInfo, fn=602952746);
  //     await page.click(".id-1276585567")
  //     // clicking twice makes the test more stable
  //     await page.click(".id-1276585567")
  //     await page.waitForSelector(".return-value .warning-message");
  //     .ok()
  //     .expect(Selector(".return-value").innerText)
  //     .contains(
  //       "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.",
  //     );
  // });

  // // This runs through
  // // https://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html
  // // It duplicates backend/test/test_otherlibs.ml's "Crypto::sha256hmac works for
  // // AWS", _but_ its value _here_ is that we do not have any other tests that push
  // // a handler's trigger play button; getting that working was surprisingly hard,
  // // and so ismith wants to leave it around to use in future integration tests.
  // //
  // // See integration-tests/README.md for docs on this.
  // //
  // // I have tried several other approaches, including wait(3000) between
  // // navigateTo() and click(), and putting navigateTo() and click() on separate
  // // await ts, but only calling click() twice worked here.
  // test("sha256hmac_for_aws", async ({ page }) => {
  //
  //     await gotoHash(page, testInfo, handler=1471262983);
  //     await page.click("div.handler-trigger");
  //     await page.click("div.handler-trigger");;
  //
  //     .expect(Selector(".return-value").innerText)
  //     .contains(
  //       '"5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"',
  //     );
  // });

  // test("fluid_fn_pg_change", async ({ page }) => {
  //   await gotoHash(page, testInfo, fn=2091743543);;
  //   awaitawait page.waitForSelector(".tl-2091743543");;

  //   await gotoHash(page, testInfo, fn=1464810122);;
  //   awaitawait page.waitForSelector(".tl-1464810122");;

  //   // Click into code to edit
  //   await page.click(".fluid-entry.id-1154335426");;

  //   //Make sure we stay on the page
  //   awaitawait page.waitForSelector(".tl-1464810122");.ok({ timeout: 1000 });
  // });

  // test("fluid_creating_an_http_handler_focuses_the_verb", async ({ page }) => {
  //   await createHTTPHandler(page);

  //
  //     await page.keyboard.press("ArrowDown") // enter AC
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok();
  // });

  // test("fluid_tabbing_from_an_http_handler_spec_to_ast", async ({ page }) => {
  //   await createHTTPHandler(page);
  //
  //     await page.keyboard.press("Tab") // verb -> route
  //     await page.keyboard.press("Tab") // route -> ast
  //     await page.keyboard.press("TODO: r); // enter AC
  //     .expect(fluidACHighlightedValue(page)("request"))
  //     .ok();
  // });

  // test("fluid_tabbing_from_handler_spec_past_ast_back_to_verb", async ({ page }) => {
  //   await createHTTPHandler(page);
  //
  //     await page.keyboard.press("Tab") // verb -> route
  //     await page.keyboard.press("Tab") // route -> ast
  //     await page.keyboard.press("Tab") // ast -> loop back to verb;
  //     await page.keyboard.press("ArrowDown") // enter AC
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok();
  // });

  // test("fluid_shift_tabbing_from_handler_ast_back_to_route", async ({ page }) => {
  //   await createHTTPHandler(page);
  //
  //     await page.keyboard.press("Tab") // verb -> route
  //     await page.keyboard.press("Tab") // route -> ast
  //     await page.keyboard.press("TODO: shift+tab); // ast -> back to route;
  //     await page.keyboard.press("ArrowDown") // enter route
  //     expect(await acHighlightedText(page)).toBe("/")
  //     .ok();
  // });

  test("fluid_test_copy_request_as_curl", async ({ page }) => {
    await page.click(".toplevel.tl-91390945");
    expect(page.locator(".tl-91390945")).toBeVisible();
    await page.click(".id-753586717");
    // Ensure the anaysis has completed
    await expect(page.locator(".live-value.loaded")).toContainText(
      "Click Play to execute function",
    );
    // test logic in IntegrationTest.ml; we load it here because we need an
    // analysis done before we can call the command
  });

  // test("fluid_ac_validate_on_lose_focus", async ({ page }) => {
  //   await createHTTPHandler(page);
  //   await gotoAST(page);
  //
  //     await page.type("#active-editor", "request.body");
  //     .click("#app", { offsetX: 500, offsetY: 50 }) //click away from fluid
  //     .expect(true)
  //     .ok();
  //   // validate AST in IntegrationTest.ml
  // });

  // async function upload_pkg_for_tlid(t, tlid) {
  //   .navigateTo(`#fn=${tlid}`);
  //
  //     await page.click(".fn-actions > .menu > .more-actions > .toggle-btn");
  //     .expect(true)
  //     .ok();
  //
  //     .click(
  //       Selector(
  //         ".fn-actions > .menu > .more-actions > .actions > .item",
  //       ).withText("Upload Function"),
  //     )
  //     await page.waitForSelector(".error-panel.show");
  //     .ok({ timeout: 1000 });
  // }

  // // this tests:
  // // - happy path upload
  // // - upload fails b/c the db already has a fn with this name + version
  // // - upload fails b/c the version we're trying to upload is too low (eg, if you
  // // already have a v1, you can't upload a v0)
  // test("upload_pkg_fn_as_admin", async ({ page }) => {
  //   // upload v1/2/3 depending whether this is test run 1/2/3
  //   const tlid = t.testRun.quarantine.attempts.length + 1;

  //   // it should succeed, it's a new package_fn
  //   await upload_pkg_for_tlid(t, tlid);
  //
  //     .expect(Selector(".error-panel.show").textContent)
  //     .eql("Successfully uploaded functionDismiss");
  //   await page.click(".dismissBtn");

  //   // second (attempted) upload should fail, as we've already uploaded this
  //   await upload_pkg_for_tlid(t, tlid);
  //   const failureMsg = `Bad status: Bad Request - Function already exists with this name and versions up to ${tlid}, try version ${
  //     tlid + 1
  //   }? (UploadFnAPICallback)Dismiss`;
  //   await expect(Selector(".error-panel.show").textContent).eql(failureMsg);
  //   await page.click(".dismissBtn");

  //   // attempting to upload v0 should fail, because we already have a version
  //   // greater than 0 in the db
  //   await upload_pkg_for_tlid(t, 0);
  //   // this failureMsg2 is the same as failureMsg above, because its text dpends
  //   // on the latest version (and the next valid version of the fn), not the
  //   // version you tried to upload
  //   const failureMsg2 = `Bad status: Bad Request - Function already exists with this name and versions up to ${tlid}, try version ${
  //     tlid + 1
  //   }? (UploadFnAPICallback)Dismiss`;
  //   await expect(Selector(".error-panel.show").textContent).eql(failureMsg2);
  //   await page.click(".dismissBtn");
  // });

  // test("use_pkg_fn", async ({ page }) => {
  //   const attempt = t.testRun.quarantine.attempts.length + 1;
  //   const url = `/${attempt}`;
  //   await createHTTPHandler(page);
  //
  //     // add headers
  //     await page.type("#entry-box", "GE");
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok()
  //     await page.keyboard.press("Enter");
  //     await page.type("#entry-box", url);
  //     await page.keyboard.press("Enter");;

  //   await gotoAST(page);

  //   // this await confirms that we have test_admin/stdlib/Test::one_v0 is in fact
  //   // in the autocomplete
  //
  //     await page.type("#active-editor", "test_admin");
  //     .expect(Selector(".autocomplete-item.fluid-selected.valid").textContent)
  //     .eql("test_admin/stdlib/Test::one_v0Any")
  //     await page.keyboard.press("Enter");;

  //   // this await confirms that we can get a live value in the editor
  //
  //     await page.click(".execution-button")
  //     .expect(Selector(".return-value", { timeout: 3000 }).textContent)
  //     .contains("0");

  //   // check if we can get a result from the bwd endpoint
  //   const callBackend = ClientFunction(function (url) {
  //     var xhttp = new XMLHttpRequest();
  //     xhttp.open("GET", url, false);
  //     xhttp.send(null);
  //     return xhttp.responseText;
  //   });
  //   const resp = await callBackend(user_content_url(t, url));
  //   await expect(resp).eql("0");
  // });

  // test("fluid_show_docs_for_command_on_selected_code", async ({ page }) => {
  //   await createRepl(page);
  //   await gotoAST(page);
  //   .typeText("#active-editor", "1999")await page.keyboard.press("TODO: ctrl+\\);;

  //   await expect(Selector("#cmd-filter").exists).ok();
  //   await expect(Selector(".documentation-box").exists).ok();
  // });

  // // Regression test:
  // // Pre-fix, we expect the response body to be "Zm9v" ("foo" |> base64).
  // // Post-fix, we expect "foo"
  // test("fluid-bytes-response", async ({ page }) => {
  //   const url = "/";
  //   const resp = await getBwdResponse(user_content_url(t, url));
  //   await expect(resp).eql("foo");
  // });

  // test("double_clicking_blankor_selects_it", async ({ page }) => {
  //   // This is part of fixing double-click behaviour in HTTP headers and other
  //   // blank-ors. When you clicked on the HTTP header, the caret did not stay in
  //   // the header. This checks that it does.
  //   //
  //   // I managed to fix it by determining that the doubleclick handler was
  //   // failing, and fixing that. However, it didn't give it the ideal behaviour,
  //   // where the word double-clicked on would be highlighted.
  //   //
  //   // So this test is not for the ideal behaviour, but for the
  //   // non-obviously-broken behaviour.
  //   let selector = Selector(".toplevel .spec-header .toplevel-name");
  //   await expect(selector.exists).ok();
  //   .doubleClick(selector);

  //   // Selected text is /hello
  //   selector = Selector(".toplevel .spec-header .toplevel-name #entry-box");
  //   await expect(selector.exists).ok();
  //   await expect(await getElementSelectionStart(selector)).typeOf("number");
  // });

  // test("abridged_sidebar_content_visible_on_hover", async ({ page }) => {
  //   // collapse sidebar to abridged mode
  //   await page.click(".toggle-sidebar-btn");
  //   await Selector(".viewing-table.abridged", { timeout: 5000 })();
  //   await expect(Selector(".viewing-table.abridged").exists).ok();

  //   const httpCatSelector = ".sidebar-category.http";

  //   // hovering over a category makes its contents visible
  //
  //     .expect(Selector(httpCatSelector + " .category-content").visible)
  //     .notOk();

  //
  //     .hover(httpCatSelector)
  //     .expect(Selector(httpCatSelector + " .category-content").visible)
  //     .ok();
  // });

  // test("abridged_sidebar_category_icon_click_disabled", async ({ page }) => {
  //   // collapse sidebar to abridged mode
  //   await page.click(".toggle-sidebar-btn");
  //   await Selector(".viewing-table.abridged", { timeout: 5000 })();
  //   await expect(Selector(".viewing-table.abridged").exists).ok();

  //   const httpCatSelector = ".sidebar-category.http";
  //   const dbCatSelector = ".sidebar-category.dbs";

  //   // clicking on a category icon does not keep it open if you mouse elsewhere
  //   .click(httpCatSelector + " .category-icon");
  //   .click(dbCatSelector + " .category-icon");
  //
  //     .expect(Selector(httpCatSelector + " .category-content").visible)
  //     .notOk();
  // });

  // test("function_docstrings_are_valid", async ({ page }) => {
  //   // validate functions in IntegrationTest.ml
  // });

  // test("record_consent_saved_across_canvases", async ({ page }) => {
  //   await page.click("#fs-consent-yes");
  //   .wait(1500);
  //   await expect(Selector(".fullstory-modal.hide").exists).ok();

  //   // navigate to another canvas
  //   .navigateTo(`${BASE_URL}another-canvas`);
  //   await expect(Selector(".fullstory-modal.hide").exists).ok();

  //   // go back to original canvas to end the test
  //   const testname = t.testRun.test.name;
  //   .navigateTo(`${BASE_URL}${testname}?integration-test=true`);
  // });

  // // This test is flaky; last attempt to fix it added the 1000ms timeout, but that
  // // didn't solve the problem
  // /*
  // test("exe_flow_fades", async ({ page }) => {
  //   const timestamp = new Date();
  //   await page.click(".fluid-entry");
  //   awaitAnalysis(t, timestamp);
  //   // wait up to 1000ms for this selector to appear
  //
  //     .expect(Selector(".fluid-not-executed", { timeout: 1000 }).exists)
  //     .ok();
  // });
  // */

  // test("unexe_code_unfades_on_focus", async ({ page }) => {
  //   const timestamp = new Date();
  //   await page.click(".fluid-entry");
  //   awaitAnalysis(t, timestamp);
  //   // move caret into a single line
  //   .click(".id-1459002816", { timeout: 500 });
  //
  //     .expect(
  //       Selector(".id-1459002816.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".id-2073307217.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();

  //   // move caret into multiline string
  //   .click(".fluid-string-ml-start", { timeout: 500 });
  //
  //     .expect(
  //       Selector(".fluid-string-ml-start.fluid-not-executed.fluid-code-focus")
  //         .exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".fluid-string-ml-middle.fluid-not-executed.fluid-code-focus")
  //         .exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".fluid-string-ml-end.fluid-not-executed.fluid-code-focus")
  //         .exists,
  //     )
  //     .ok();

  //   // move caret into list literal
  //   .click(".fluid-list-comma", { timeout: 500 });
  //
  //     .expect(
  //       Selector(".fluid-list-open.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".fluid-list-close.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();

  //   // move caret into object literal
  //   .click(".fluid-record-sep", { timeout: 500 });
  //
  //     .expect(
  //       Selector(".fluid-record-open.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".fluid-record-fieldname.fluid-not-executed.fluid-code-focus")
  //         .exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".id-2108109721.fluid-not-executed.fluid-code-focus").exists,
  //     )
  //     .ok();
  //
  //     .expect(
  //       Selector(".fluid-record-close.fluid-not-executed.fluid-code-focus")
  //         .exists,
  //     )
  //     .ok();
  // });

  // test("create_from_404", async ({ page }) => {
  //   const f0fCategory = Selector(".sidebar-category.fof");

  //   const sendPushEvent = ClientFunction(function () {
  //     const data = [
  //       "HTTP",
  //       "/nonexistant",
  //       "GET",
  //       "2019-03-15T22:16:40Z",
  //       "0623608c-a339-45b3-8233-0eec6120e0df",
  //     ];
  //     var event = new CustomEvent("new404Push", { detail: data });
  //     document.dispatchEvent(event);
  //   });

  //   await sendPushEvent();

  //   .wait(5000);
  //   await expect(f0fCategory.hasClass("empty")).notOk();
  //   .click(f0fCategory, { timeout: 2500 });
  //   .click(".fof > .category-content > .simple-item > .add-button", {
  //     timeout: 100,
  //   });

  //
  //     .expect(Selector(".toplevel .http-get", { timeout: 2500 }).exists)
  //     .ok();
  // });

  // test("unfade_command_palette", async ({ page }) => {
  //
  //     .doubleClick(".fluid-let-keyword")
  //     await page.keyboard.press("TODO: ctrl+\\);
  //     .expect(Selector("#cmd-filter", { timeout: 1500 }).exists)
  //     .ok();

  //   // Checks Command Palette opens inside a token with full opacity
  //   await expect(Selector(".fluid-code-focus > .command-palette").exists).ok();
  // });

  // test("redo_analysis_on_toggle_erail", async ({ page }) => {
  //   const fnCall = Selector(".id-108391798");
  //   const justExpr = Selector(".id-21312903");
  //   const nothingExpr = Selector(".id-1409226084");
  //   const errorRail = Selector(".fluid-error-rail");
  //   const returnValue = Selector(".return-value");

  //   const t0 = new Date();
  //   await page.click(".handler-trigger");
  //   awaitAnalysis(t, t0);
  //   await expect(errorRail.hasClass("show")).ok();
  //   await expect(returnValue.innerText).contains("<Incomplete>");
  //   await expect(justExpr.hasClass("fluid-not-executed")).ok();
  //   await expect(nothingExpr.hasClass("fluid-not-executed")).ok();

  //   // takes function off rail
  //
  //     .doubleClick(fnCall)
  //     await page.keyboard.press("TODO: ctrl+\\);
  //     .expect(Selector("#cmd-filter", { timeout: 1500 }).exists)
  //     .ok();
  //   .typeText("#cmd-filter", "rail");
  //
  //     .expect(Selector(".fluid-selected").innerText)
  //     .eql("take-function-off-rail");

  //   // analysis is reruns
  //   const t1 = new Date();
  //   await page.keyboard.press("Enter");
  //   awaitAnalysis(t, t1);

  //   // assert values have changed
  //   await expect(errorRail.hasClass("show")).notOk();
  //   await expect(returnValue.innerText).contains("1");
  //   await expect(justExpr.hasClass("fluid-not-executed")).notOk();
  //   await expect(nothingExpr.hasClass("fluid-not-executed")).ok();
  // });

  // test("redo_analysis_on_commit_ff", async ({ page }) => {
  //   const returnValue = Selector(".return-value");
  //   const t0 = new Date();
  //   await page.click(".handler-trigger");
  //   awaitAnalysis(t, t0);
  //   await expect(returnValue.innerText).contains("farewell Vanessa Ives");

  //   // commits feature flag
  //
  //     .doubleClick(".in-flag")
  //     await page.keyboard.press("TODO: ctrl+\\);
  //     .expect(Selector("#cmd-filter", { timeout: 1500 }).exists)
  //     .ok();
  //   .typeText("#cmd-filter", "commit");
  //
  //     .expect(Selector(".fluid-selected").innerText)
  //     .eql("commit-feature-flag");

  //   // analysis is reruns
  //   const t1 = new Date();
  //   await page.keyboard.press("Enter");
  //   awaitAnalysis(t, t1);

  //   await expect(returnValue.innerText).contains("farewell Dorian Gray");
  // });

  // test("package_function_references_work", async ({ page }) => {
  //   const repl = Selector(".toplevel.tl-92595864");
  //   const refersTo = Selector(".ref-block.refers-to.pkg-fn");
  //   const usedIn = Selector(".ref-block.used-in.handler");

  //
  //     // Start at this specific repl handler
  //     await gotoHash(page, testInfo, handler=92595864);
  //     .expect(available(repl))
  //     .ok()
  //     // Test that the handler we navigated to has a reference to a package manager function
  //     .expect(available(refersTo))
  //     .ok()
  //     .expect(Selector(".ref-block.refers-to .fnheader").textContent)
  //     .eql("test_admin/stdlib/Test::one_v0")
  //     .click(refersTo)
  //     // Clicking on it should bring us to that function
  //     await page.waitForSelector(".toplevel .pkg-fn-toplevel");
  //     .ok()
  //     // which should contain a reference to where we just came from
  //     .expect(available(usedIn))
  //     .ok()
  //     .expect(usedIn.textContent)
  //     .eql("REPLpkgFnTest")
  //     // and clicking on that should bring us back.
  //     .click(usedIn)
  //     .expect(available(repl))
  //     .ok();
  // });

  // test("focus_on_secret_field_on_insert_modal_open", async ({ page }) => {
  //
  //     .expect(
  //       Selector(".sidebar-category.secrets .create-tl-icon", { timeout: 1500 })
  //         .exists,
  //     )
  //     .ok();

  //   await createRepl(page);
  //   .typeText("#active-editor", '"Hello world!"');

  //   await page.click(".sidebar-category.secrets .create-tl-icon");

  //   const nameInput = Selector("#new-secret-name-input");

  //   await expect(nameInput.focused).ok();

  //   await page.click(".modal.insert-secret .close-btn");
  // });
});
