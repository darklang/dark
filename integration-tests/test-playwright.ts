import {
  test,
  expect,
  ConsoleMessage,
  Page,
  Locator,
  TestInfo,
} from "@playwright/test";
import fs from "fs";

const BASE_URL = "http://darklang.localhost:8000";
const options = {
  baseURL: BASE_URL,
};
test.use(options);

// const getPageUrl = ClientFunction(() => window.location.href);
// const analysisLastRun = ClientFunction(() => window.Dark.analysis.lastRun);

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

// async function awaitAnalysis(t, ts, trial = 0) {
//   if (trial > 10) {
//     return t.expect(true).notOk("Max wait count for analysis exceeded");
//   }

//   const lastRun = await analysisLastRun();

//   if (lastRun > ts) {
//     // analysis has returned result since ts (timestamp)
//     const diffInSecs = (lastRun - ts) / 1000.0;
//     console.info("Analysis ran in ~ " + diffInSecs + "secs");
//     return t;
//   } else {
//     await t.wait(1000);
//     return awaitAnalysis(t, ts, trial + 1);
//   }
// }

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
    await expect(page.locator("#finishIntegrationTest")).toBeVisible();
    await page.pause();
  });

  /* Testcafe runs everything through a proxy, wrapping all values and
   * objects such that it seems like nothing happened. However, they forgot
   * to wrap objects in Webworker contexts, so calls to Fetch in the worker
   * thinks it's on a different domain. This breaks cookies, auth, CORS,
   * basically everything. So we thread the right url through to do the
   * proxying ourselves. Hopefully they'll fix this and we can remove this
   * code someday */
  // await t.eval(
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
        expect(await page.isVisible("#integrationTestSignal")).toBe(true);

        // check the content
        let content = await page.textContent("#integrationTestSignal");
        expect(content).toBe("success");
        expect(content).not.toContain("failure");

        // check the class
        let class_ = await page.getAttribute("#integrationTestSignal", "class");
        expect(class_).toContain("success");
        expect(class_).not.toContain("failure");

        // Ensure there are no errors in the logs
        let errorMessages = test.messages.filter(
          (msg: ConsoleMessage) => msg.type() == "error",
        );
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
  // // Utilities
  //********************************
  async function createHTTPHandler(page: Page) {
    await page.keyboard.press("Enter");
    await page.keyboard.press("ArrowDown");
    await page.keyboard.press("Enter");
  }

  // async function createWorkerHandler(t) {
  //
  //     await page.keyboard.press("Enter");
  //     await page.keyboard.press("ArrowDown")
  //     await page.keyboard.press("ArrowDown")
  //     await page.keyboard.press("ArrowDown")
  //     await page.keyboard.press("ArrowDown")
  //     await page.keyboard.press("Enter");;
  // }

  // async function createRepl(t) {
  //   await page.keyboard.press("Enter");await page.keyboard.press("Enter");;
  // }

  async function gotoAST(page: Page): Promise<void> {
    await page.click("#active-editor > span");
  }

  // function user_content_url(t, endpoint) {
  //   return (
  //     "http://test-" +
  //     t.testRun.test.name +
  //     ".builtwithdark.lvh.me:8000" +
  //     endpoint
  //   );
  // }

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
  //   await t.pressKey(shortcut);
  // }

  //********************************
  // Avoiding test race conditions
  //********************************
  // // Testcafe automatically waits for the next thing you've specified. So
  // // if you .typeText("#entry-box", ...), it will wait for the entryBox.
  // // But we sometimes need to explicitly wait if TestCafe can't tell what
  // // we're waiting on.

  // function available(css) {
  //   return Selector(css).exists;
  // }
  function entryBox(page: Page): Locator {
    return page.locator("#entry-box");
  }

  // // Return the highlighted autocomplete entry
  function acHighlightedValue(page: Page): Locator {
    return page.locator(".autocomplete-item.highlighted > .name");
  }

  function fluidACHighlightedValue(page: Page): Locator {
    return page.locator(".autocomplete-item.fluid-selected");
  }

  async function selectAll(page: Page): Promise<void> {
    // Do these multiple times to make sure it actually selects
    await page.keyboard.press("Control+a"); // on linux
    await page.keyboard.press("Control+a"); // on linux
    await page.keyboard.press("Control+a"); // on linux
    await page.keyboard.press("Meta+a"); // on mac
    await page.keyboard.press("Meta+a"); // on mac
    await page.keyboard.press("Meta+a"); // on mac
  }

  // Entry-box sometimes carries state over briefly, so wait til it's clear
  async function waitForEmptyEntryBox(page: Page): Promise<void> {
    await page.waitForSelector("#entry-box >> text=''");
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
    await createHTTPHandler(page);

    // add headers
    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", "PO");
    await expect(acHighlightedValue(page)).toHaveText("POST");
    await page.keyboard.press("Enter");

    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", "/spec_name");
    await page.keyboard.press("Enter");

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
    await createHTTPHandler(page);

    // add headers
    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", "PO");
    await expect(acHighlightedValue(page)).toHaveText("POST");
    await page.keyboard.press("Enter");
    await page.type("#entry-box", "/spec_name");
    await page.keyboard.press("Enter");

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
    await createHTTPHandler(page);

    // add headers
    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", "PO");
    await expect(acHighlightedValue(page)).toHaveText("POST");
    await page.keyboard.press("Enter");

    await waitForEmptyEntryBox(page);
    await page.type("#entry-box", "/spec_name/:variable");
    await page.keyboard.press("Enter");

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
    await expect(entryBox(page)).toBeVisible();
  });

  test.skip("field_access_closes", async ({ page }) => {
    await createHTTPHandler(page);
    await gotoAST(page);
    test.setTimeout(30000);

    await page.type("#active-editor", "req");
    // There's a race condition here, sometimes the client doesn't manage to load the
    // trace for quite some time, and the autocomplete box ends up in a weird
    // condition
    await expect(fluidACHighlightedValue(page)).toHaveText("requestDict");
    await page.type("#active-editor", ".bo");
    await expect(fluidACHighlightedValue(page)).toHaveText("bodyfield");
    await page.keyboard.press("Enter");
  });

  // test("field_access_pipes", async ({ page }) => {
  //   await createHTTPHandler(t);
  //   await gotoAST(t);
  //
  //     await page.type("#active-editor", "req");
  //     .expect(fluidAcHighlightedText())
  //     .contains("request")
  //     await page.type("#active-editor", ".bo");
  //     .expect(fluidAcHighlightedText())
  //     .eql("bodyfield")
  //     await page.keyboard.press("TODO: shift+enter);;
  // });

  // test("tabbing_works", async ({ page }) => {
  //   await createRepl(t);
  //   // Fill in "then" box in if stmt
  //
  //     await page.type("#active-editor", "if");
  //     await page.keyboard.press("TODO: space tab);
  //     await page.type("#active-editor", "5");;
  // });

  // test("autocomplete_highlights_on_partial_match", async ({ page }) => {
  //   await createRepl(t);
  //   await gotoAST(t);
  //
  //     await page.type("#active-editor", "nt::add");
  //     .expect(fluidAcHighlightedText("Int::add"))
  //     .ok()
  //     await page.keyboard.press("Enter");;
  // });

  // test("no_request_global_in_non_http_space", async ({ page }) => {
  //   await createWorkerHandler(t);
  //   await gotoAST(t);
  //     await page.type("#active-editor", "request");
  //     await page.keyboard.press("ArrowDown")
  //     .expect(fluidAcHighlightedText("Http::badRequest"))
  //     .ok()
  //     await page.keyboard.press("Enter");;
  // });

  // test("ellen_hello_world_demo", async ({ page }) => {
  //   await createHTTPHandler(t);
  //     // verb
  //   await page.type("#entry-box", "g");
  //   await page.keyboard.press("Enter");

  //     // route
  //     await page.type("#entry-box", "/hello");
  //     await page.keyboard.press("Enter");

  //     // string
  //     await page.type("#active-editor", '"Hello world!"');;
  // });

  // test("editing_headers", async ({ page }) => {
  //   await createHTTPHandler(t);
  //
  //     // add headers
  //     await page.type("#entry-box", "PO");
  //     expect(await acHighlightedText(page)).toBe("POST")
  //     .ok()
  //     await page.keyboard.press("Enter");

  //     await page.type("#entry-box", "/hello");
  //     await page.keyboard.press("Enter");

  //     // edit them
  //     await page.click(".spec-header > .toplevel-name")
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "/myroute");
  //     await page.keyboard.press("Enter");

  //     await page.click(".spec-header > .toplevel-type > .modifier")
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "GET");
  //     await page.keyboard.press("Enter");;
  // });

  // test("switching_to_http_space_adds_slash", async ({ page }) => {
  //   await createWorkerHandler(t);
  //
  //     // add headers
  //     await page.click(".spec-header > .toplevel-name")
  //     await page.keyboard.press("Enter");
  //     await page.type("#entry-box", "spec_name");
  //     await page.keyboard.press("Enter");

  //     // edit space
  //     await page.click(".spec-header > .toplevel-type > .space")
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "HTTP");
  //     await page.keyboard.press("Enter");;
  // });

  // test("switching_from_default_repl_space_removes_name", async ({ page }) => {
  //   await createRepl(t);
  //
  //     // edit space
  //     await page.click(".spec-header > .toplevel-type >.space")
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "CRON");
  //     await page.keyboard.press("Enter");;
  // });

  // test("tabbing_through_let", async ({ page }) => {
  //   await createRepl(t);
  //   await gotoAST(t);
  //
  //     await page.type("#active-editor", "let");
  //     await page.keyboard.press("Enter");
  //     // round trip through the let blanks once
  //     await page.keyboard.press("TODO: tab tab tab);
  //     // go to the body and fill it in
  //     await page.keyboard.press("TODO: tab tab);
  //     await page.type("#active-editor", "5");
  //     // go to the rhs and fill it in
  //     await page.keyboard.press("TODO: tab tab);
  //     await page.type("#active-editor", "5");
  //     // fill in the var
  //     await page.keyboard.press("TODO: tab tab);
  //     await page.type("#active-editor", "myvar");;
  // });

  // test("rename_db_fields", async ({ page }) => {
  //   const callBackend = ClientFunction(function (url) {
  //     var xhttp = new XMLHttpRequest();
  //     xhttp.open("POST", url, true);
  //     xhttp.setRequestHeader("Content-type", "application/json");
  //     xhttp.send('{ "field6": "a", "field2": "b" }');
  //   });

  //   // rename
  //
  //     .click(Selector(".name").withText("field1"))
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "field6");
  //     await page.keyboard.press("Enter");;

  //   // add data and check we can't rename again
  //   await callBackend(user_content_url(t, "/add"));

  //   // This is super-shaky if we remove this. There's some timing things
  //   // around when the .fa-lock appears, and the selectors we'd expect
  //   // (below) doesn't work. But if we split it into two it works. Who
  //   // knows.
  //   // await expect(Selector('.fa-lock', {timeout: 8000})().exists).ok() ;

  //   const lockSel = ".db .spec-header.lock";
  //   await Selector(lockSel, { timeout: 5000 })();
  //   await expect(Selector(lockSel).exists).ok();

  //
  //     .click(Selector(".name").withText("field6"))
  //     await page.keyboard.press("Enter");
  //     await page.keyboard.press("Enter");;
  // });

  // test("rename_db_type", async ({ page }) => {
  //   const callBackend = ClientFunction(function (url) {
  //     var xhttp = new XMLHttpRequest();
  //     xhttp.open("POST", url, true);
  //     xhttp.setRequestHeader("Content-type", "application/json");
  //     xhttp.send('{ "field1": "a", "field2": 5 }');
  //   });

  //   // rename
  //
  //     .click(Selector(".type").withText("Int"))
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "String");
  //     await page.keyboard.press("Enter");;

  //   // add data and check we can't rename again
  //   await callBackend(user_content_url(t, "/add"));

  //   // This is super-shaky if we remove this. There's some timing things
  //   // around when the .fa-lock appears, and the selectors we'd expect
  //   // (below) doesn't work. But if we split it into two it works. Who
  //   // knows.
  //   // await expect(Selector('.fa-lock', {timeout: 5000})().exists).ok() ;

  //   const lockSel = ".db .spec-header.lock";
  //   await Selector(lockSel, { timeout: 5000 })();
  //   await expect(Selector(lockSel).exists).ok();

  //
  //     .click(Selector(".type").withText("String"))
  //     await page.keyboard.press("Enter");
  //     await page.keyboard.press("Enter");;
  // });

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
    .expect(available(".feature-flag")).ok()
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

    .expect(available(".tl-2296485551")).ok()
    await page.click(".tl-2296485551")
    await page.keyboard.press("Enter");

    // Make feature Flag
    .click('.expr-actions .flag')

    .expect(available(".feature-flag")).ok()
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
    .expect(available(".tl-180770093")).ok()
});
*/
  // test("rename_function", async ({ page }) => {
  //   const fnNameBlankOr = ".fn-name-content";
  //
  //     .navigateTo("#fn=123")
  //     .expect(available(fnNameBlankOr))
  //     .ok({ timeout: 1000 });

  //   // check not changing function name does not cause error message to show
  //   await t.doubleClick(Selector(fnNameBlankOr));
  //   await gotoAST(t);
  //   await expect(available(".error-panel.show")).notOk();

  //   // now actually rename the function to a different name
  //
  //     .click(Selector(fnNameBlankOr))
  //     await selectAll(page);
  // await page.keyboard.press("Backspace");
  //     await page.type("#entry-box", "hello");
  //     await page.keyboard.press("Enter");;
  // });

  // // not sure why this test is flaky - possibly pressing the button changes state,
  // // and re-running (testcafe's quarantine mode) fails?

  // test("execute_function_works", async ({ page }) => {
  //   await createRepl(t);
  //   await expect(Selector("#active-editor", { timeout: 5000 }).exists).ok();
  //   await t.typeText("#active-editor", "Uuid::gen")await page.keyboard.press("TODO: enter);;

  //   const t1 = new Date();
  //   await t.click(Selector(".execution-button", { timeout: 500 }));
  //   await awaitAnalysis(t, t1);

  //   let v1 = await Selector(".selected .live-value.loaded").innerText;

  //   const t2 = new Date();
  //   await t.click(Selector(".fa-redo"));
  //   await awaitAnalysis(t, t2);

  //   let v2 = await Selector(".selected .live-value.loaded").innerText;

  //   let re = /<UUID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}>/;
  //   await expect(v1).match(re);
  //   await expect(v2).match(re);
  //   await expect(v1).notEql(v2);
  // });

  // test("correct_field_livevalue", async ({ page }) => {
  //
  //     .click(Selector(".fluid-editor")) // this click required to activate the editor
  //     .click(Selector(".fluid-field-name").withExactText("gth"));

  //   let v1 = await Selector(".selected .live-value.loaded").innerText;

  //   await expect(v1).eql("5");
  // });

  // test("int_add_with_float_error_includes_fnname", async ({ page }) => {
  //   const timestamp = new Date();
  //   await t.click(Selector(".tl-123 .fluid-editor")); // required to see the return value (navigate is insufficient)
  //   await awaitAnalysis(t, timestamp);

  //
  //     .expect(available(".return-value"))
  //     .ok()
  //     .expect(Selector(".return-value").innerText)
  //     .contains("but + only works on Ints.");
  // });

  // test("function_version_renders", async ({ page }) => {
  //   await createRepl(t);
  //
  //     await page.type("#active-editor", "DB::del");
  //     .expect(
  //       Selector(".autocomplete-item.fluid-selected .version").withText("v1"),
  //     )
  //     .ok();
  // });

  // test("delete_db_col", async ({ page }) => {
  //   await t.click(Selector(".delete-col"));
  // });

  // test("cant_delete_locked_col", async ({ page }) => {
  //   await t.click(Selector(".fluid-fn-name")); // this click is required due to caching
  //   await Selector(".execution-button-needed", { timeout: 5000 })();
  //
  //     .expect(Selector(".execution-button-needed").exists)
  //     .ok()
  //     .click(Selector(".execution-button-needed"));

  //   const lockSel = ".db .spec-header.lock";
  //   await Selector(lockSel, { timeout: 5000 })();
  //   await expect(Selector(lockSel).exists).ok();

  //
  //     .click(Selector(".db")) // this click is required due to caching
  //     .expect(Selector(".delete-col").exists)
  //     .notOk();
  // });

  // test("select_route", async ({ page }) => {
  //   const categoryHeader = ".sidebar-category.http .category-summary";
  //   const httpVerbLink =
  //     ".sidebar-category.http .category-content a.toplevel-link";
  //   const toplevelElement = ".node .toplevel";

  //   await t.click(Selector(categoryHeader));

  //   await Selector(httpVerbLink, { timeout: 5000 })();
  //   await t.click(Selector(httpVerbLink));

  //   await Selector(toplevelElement, { timeout: 5000 })();
  //   await expect(Selector(toplevelElement).hasClass("selected")).ok();
  // });

  // // TODO: This needs Stroller/Pusher in CI
  // // test('passwords_are_redacted', async ({ page }) => {
  // //   const callBackend = ClientFunction(
  // //     function (url) {
  // //       var xhttp = new XMLHttpRequest();
  // //       xhttp.open("POST", url, true);
  // //       xhttp.setRequestHeader("Content-type", "application/json");
  // //       xhttp.send('{ "password": "redactme!" }');
  // //     });

  // //   await t.click(Selector('.Password\\:\\:hash'))
  // //   await callBackend(user_content_url(t, "/signup"));
  // //   await expect(Selector('.live-value').textContent).eql('<Password: Redacted>', { timeout: 5000 })
  // // })
  // //
  // //
  // // TODO: Add test that verifies pasting text/plain when Entering works
  // // See: https://github.com/darklang/dark/pull/725#pullrequestreview-213661810

  // test("function_analysis_works", async ({ page }) => {
  //
  //     .navigateTo("#fn=1039370895")
  //     .expect(available(".user-fn-toplevel"))
  //     .ok({ timeout: 1000 })
  //     .click(Selector(".user-fn-toplevel #active-editor .fluid-binop"))
  //     .expect(Selector(".selected .live-value.loaded").textContent)
  //     .eql("10", { timeout: 5000 });
  // });

  // test("jump_to_error", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     await page.click(".fluid-entry");
  //   const timestamp = new Date();
  //   await page.click(".fluid-entry.id-675551618");
  //   awaitAnalysis(t, timestamp);
  //   await page.click(".jump-src");
  // });

  // test("fourohfours_parse", async ({ page }) => {
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
  // });

  // test("fn_page_to_handler_pos", async ({ page }) => {
  //
  //     .navigateTo("#fn=890")
  //     .expect(available(".user-fn-toplevel"))
  //     .ok({ timeout: 1000 });
  //   const fnOffset = await Selector("#canvas").getStyleProperty("transform");

  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok({ timeout: 1000 });

  //
  //     .expect(Selector("#canvas").getStyleProperty("transform"))
  //     .notEql(fnOffset);
  // });

  // test("autocomplete_visible_height", async ({ page }) => {
  //   await createRepl(t);
  //
  //     await page.keyboard.press("TODO: r);
  //     .expect(Selector("li.autocomplete-item.valid").nth(5).visible)
  //     .ok();
  // });

  // // Disabled the feature for now
  // // test("create_new_function_from_autocomplete", async ({ page }) => {
  // //   await createRepl(t);
  // //
  // //     await page.type("#active-editor", "myFunctionName");
  // //     .expect(fluidAcHighlightedText())
  // //     .eql("Create new function: myFunctionName")
  // //     await page.keyboard.press("Enter");;
  // // });
  // //
  // test("load_with_unnamed_function", async ({ page }) => {
  //   await tawait page.keyboard.press("Enter").expect(entryBoxAvailable()).ok();
  // });

  // test("extract_from_function", async ({ page }) => {
  //   const exprElem = Selector(".user-fn-toplevel #active-editor > span");

  //
  //     .navigateTo("#fn=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .click(exprElem)
  //     .selectText(exprElem, 0, 1)
  //     await page.keyboard.press("TODO: ctrl+\\);
  //     await page.type("#cmd-filter", "extract-function");
  //     await page.keyboard.press("Enter");;
  // });

  // test("fluid_execute_function_shows_live_value", async ({ page }) => {
  //   /* NOTE: This test is intended to determine if clicking a play button in fluid
  //   makes the live value visible. There is some extra complication in that clicking
  //   on a play button as it stands does not actually "count" as clicking on the play button
  //   unless the handler is "active" with a placed caret. To account for this, we
  //   click on "hello" within Crypto::sha256 ("hello" |> String::toBytes) after focusing
  //   in order to place the caret. Then we click on the button and see if the live value
  //   corresponds to the result of `Crypto::sha256`. */
  //
  //     .navigateTo("#handler=1013604333")
  //     .expect(available(".id-1334251057 .execution-button"))
  //     .ok()
  //     .click(Selector(".id-1045574047.fluid-string"))
  //     .click(Selector(".id-1334251057 .execution-button"))
  //     .expect(available(".selected .live-value.loaded"))
  //     .ok()
  //     .expect(Selector(".selected .live-value.loaded").innerText)
  //     .eql("<Bytes: length=32>");
  // });

  // test("fluid_single_click_on_token_in_deselected_handler_focuses", async ({ page }) => {
  //
  //     .expect(available(".id-2068425241.fluid-let-var-name"))
  //     .ok()
  //     .click(Selector(".id-2068425241.fluid-let-var-name"), { caretPos: 2 });
  // });

  // test("fluid_click_2x_on_token_places_cursor", async ({ page }) => {
  //
  //     .expect(available(".id-549681748.fluid-let-var-name"))
  //     .ok()
  //     .click(Selector(".id-549681748.fluid-let-var-name"), { caretPos: 2 })
  //     .click(Selector(".id-549681748.fluid-let-var-name"), { caretPos: 2 });
  // });

  // test("fluid_click_2x_in_function_places_cursor", async ({ page }) => {
  //
  //     .navigateTo("#fn=1352039682")
  //     .expect(available(".id-677483670.fluid-let-var-name"))
  //     .ok()
  //     .click(Selector(".id-677483670.fluid-let-var-name"), { caretPos: 2 })
  //     .expect(available(".id-96908617.fluid-category-string"))
  //     .ok()
  //     .click(Selector(".id-96908617.fluid-category-string"), { caretPos: 2 });
  // });

  // test("fluid_doubleclick_selects_token", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .doubleClick(Selector(".fluid-match-keyword"), { caretPos: 3 });
  // });

  // // This works in practice, but doesn't appear to work in TestCafe 🤨 *)
  // // test("fluid_doubleclick_selects_word_in_string", async ({ page }) => {
  // //
  // //     .navigateTo("#handler=123")
  // //     .expect(available(".tl-123"))
  // //     .ok()
  // //     .expect(available(".selected #active-editor"))
  // //     .ok()
  // //     .doubleClick(Selector(".fluid-string"), { caretPos: 15 });
  // // });
  // //
  // test("fluid_doubleclick_selects_entire_fnname", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .doubleClick(Selector(".fluid-fn-name"), { caretPos: 8 });
  // });

  // test("fluid_doubleclick_with_alt_selects_expression", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .doubleClick(Selector(".fluid-match-keyword"), {
  //       caretPos: 3,
  //       modifiers: { alt: true },
  //     });
  // });

  // test("fluid_shift_right_selects_chars_in_front", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .click(Selector(".fluid-category-string"), { caretPos: 2 })
  //     await page.keyboard.press("TODO: shift+right shift+down shift+right);;
  // });

  // test("fluid_shift_left_selects_chars_at_back", async ({ page }) => {
  //
  //     .navigateTo("#handler=123")
  //     .expect(available(".tl-123"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .click(Selector(".fluid-category-string"), { caretPos: 2 })
  //     await page.keyboard.press("TODO: down shift+left shift+up);;
  // });

  // test("fluid_undo_redo_happen_exactly_once", async ({ page }) => {
  //
  //     .expect(available(".tl-608699171"))
  //     .ok()
  //     .click(Selector(".id-68470584.fluid-category-string"))
  //     .expect(available(".selected #active-editor"))
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
  //     .navigateTo("#handler=428972234")
  //     .expect(available(".tl-428972234"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .click(Selector(".fluid-string"), { caretPos: 10 })
  //     await page.keyboard.press("Control+ArrowLeft");
  // });

  // test("fluid_ctrl_right_on_string", async ({ page }) => {
  //
  //     .navigateTo("#handler=428972234")
  //     .expect(available(".tl-428972234"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .click(Selector(".fluid-string"), { caretPos: 10 })
  //     await page.keyboard.press("TODO: ctrl+right);;
  // });

  // test("fluid_ctrl_left_on_empty_match", async ({ page }) => {
  //
  //     .navigateTo("#handler=281413634")
  //     .expect(available(".tl-281413634"))
  //     .ok()
  //     .expect(available(".selected #active-editor"))
  //     .ok()
  //     .click(Selector(".fluid-category-pattern.id-63381027"), { caretPos: 0 })
  //     await page.keyboard.press("Control+ArrowLeft");
  // });

  // test("varnames_are_incomplete", async ({ page }) => {
  //
  //     await page.click(".toplevel")
  //     .click(Selector(".spec-header > .toplevel-name"))
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
  //     .navigateTo("#handler=1445447347")
  //     .expect(available(".tl-1445447347"))
  //     .ok();
  // });

  // test("max_callstack_bug", async ({ page }) => {
  //   await createRepl(t);
  //   await gotoAST(t);
  //
  //     // I don't know what the threshold is exactly, but 1500 didn't tickle
  //     // the bug
  //     await page.keyboard.press("TODO: L i s t : : r a n g e space 0 space 2 0 0 0 space);;
  // });

  // test("sidebar_opens_function", async ({ page }) => {
  //
  //     .expect(available(".sidebar-category.fns .category-summary"))
  //     .ok()
  //     .click(Selector(".sidebar-category.fns .category-summary"))
  //     .expect(available(".sidebar-category.fns a[href='#fn=1352039682']"))
  //     .ok()
  //     .click(Selector(".sidebar-category.fns a[href='#fn=1352039682']"))
  //     .expect(getPageUrl())
  //     .match(/.+#fn=1352039682$/, "Url is incorrect");
  // });

  // test("empty_fn_never_called_result", async ({ page }) => {
  //   await t.navigateTo("#fn=602952746");
  //   const timestamp = new Date();
  //
  //     await page.click(".id-1276585567")
  //     // clicking twice in hopes of making the test more stable
  //     await page.click(".id-1276585567");
  //   await awaitAnalysis(t, timestamp);
  //
  //     .expect(available(".return-value .warning-message"))
  //     .ok()
  //     .expect(Selector(".return-value").innerText)
  //     .contains(
  //       "This function has not yet been called, so there are no values assigned to the parameters. Call this function in another handler.",
  //     );
  // });

  // test("empty_fn_been_called_result", async ({ page }) => {
  //
  //     .expect(available(".execution-button"))
  //     .ok()
  //     await page.click(".execution-button")
  //     .navigateTo("#fn=602952746")
  //     await page.click(".id-1276585567")
  //     // clicking twice makes the test more stable
  //     await page.click(".id-1276585567")
  //     .expect(available(".return-value .warning-message"))
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
  //     .navigateTo("#handler=1471262983")
  //     .click(Selector("div.handler-trigger"))
  //     .click(Selector("div.handler-trigger"));
  //
  //     .expect(Selector(".return-value").innerText)
  //     .contains(
  //       '"5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"',
  //     );
  // });

  // test("fluid_fn_pg_change", async ({ page }) => {
  //   await t.navigateTo("#fn=2091743543");
  //   await expect(available(".tl-2091743543")).ok();

  //   await t.navigateTo("#fn=1464810122");
  //   await expect(available(".tl-1464810122")).ok();

  //   // Click into code to edit
  //   await t.click(Selector(".fluid-entry.id-1154335426"));

  //   //Make sure we stay on the page
  //   await expect(available(".tl-1464810122")).ok({ timeout: 1000 });
  // });

  // test("fluid_creating_an_http_handler_focuses_the_verb", async ({ page }) => {
  //   await createHTTPHandler(t);

  //
  //     await page.keyboard.press("ArrowDown") // enter AC
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok();
  // });

  // test("fluid_tabbing_from_an_http_handler_spec_to_ast", async ({ page }) => {
  //   await createHTTPHandler(t);
  //
  //     await page.keyboard.press("Tab") // verb -> route
  //     await page.keyboard.press("Tab") // route -> ast
  //     await page.keyboard.press("TODO: r); // enter AC
  //     .expect(fluidAcHighlightedText("request"))
  //     .ok();
  // });

  // test("fluid_tabbing_from_handler_spec_past_ast_back_to_verb", async ({ page }) => {
  //   await createHTTPHandler(t);
  //
  //     await page.keyboard.press("Tab") // verb -> route
  //     await page.keyboard.press("Tab") // route -> ast
  //     await page.keyboard.press("Tab") // ast -> loop back to verb;
  //     await page.keyboard.press("ArrowDown") // enter AC
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok();
  // });

  // test("fluid_shift_tabbing_from_handler_ast_back_to_route", async ({ page }) => {
  //   await createHTTPHandler(t);
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
  //   await createHTTPHandler(t);
  //   await gotoAST(t);
  //
  //     await page.type("#active-editor", "request.body");
  //     .click("#app", { offsetX: 500, offsetY: 50 }) //click away from fluid
  //     .expect(true)
  //     .ok();
  //   // validate AST in IntegrationTest.ml
  // });

  // async function upload_pkg_for_tlid(t, tlid) {
  //   await t.navigateTo(`#fn=${tlid}`);
  //
  //     .click(Selector(".fn-actions > .menu > .more-actions > .toggle-btn"))
  //     .expect(true)
  //     .ok();
  //
  //     .click(
  //       Selector(
  //         ".fn-actions > .menu > .more-actions > .actions > .item",
  //       ).withText("Upload Function"),
  //     )
  //     .expect(available(".error-panel.show"))
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
  //   await createHTTPHandler(t);
  //
  //     // add headers
  //     await page.type("#entry-box", "GE");
  //     expect(await acHighlightedText(page)).toBe("GET")
  //     .ok()
  //     await page.keyboard.press("Enter");
  //     await page.type("#entry-box", url);
  //     await page.keyboard.press("Enter");;

  //   await gotoAST(t);

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
  //   await createRepl(t);
  //   await gotoAST(t);
  //   await t.typeText("#active-editor", "1999")await page.keyboard.press("TODO: ctrl+\\);;

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
  //   await t.doubleClick(selector);

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
  //   await t.click(httpCatSelector + " .category-icon");
  //   await t.click(dbCatSelector + " .category-icon");
  //
  //     .expect(Selector(httpCatSelector + " .category-content").visible)
  //     .notOk();
  // });

  // test("function_docstrings_are_valid", async ({ page }) => {
  //   // validate functions in IntegrationTest.ml
  // });

  // test("record_consent_saved_across_canvases", async ({ page }) => {
  //   await page.click("#fs-consent-yes");
  //   await t.wait(1500);
  //   await expect(Selector(".fullstory-modal.hide").exists).ok();

  //   // navigate to another canvas
  //   await t.navigateTo(`${BASE_URL}another-canvas`);
  //   await expect(Selector(".fullstory-modal.hide").exists).ok();

  //   // go back to original canvas to end the test
  //   const testname = t.testRun.test.name;
  //   await t.navigateTo(`${BASE_URL}${testname}?integration-test=true`);
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
  //   await t.click(".id-1459002816", { timeout: 500 });
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
  //   await t.click(".fluid-string-ml-start", { timeout: 500 });
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
  //   await t.click(".fluid-list-comma", { timeout: 500 });
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
  //   await t.click(".fluid-record-sep", { timeout: 500 });
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

  //   await t.wait(5000);
  //   await expect(f0fCategory.hasClass("empty")).notOk();
  //   await t.click(f0fCategory, { timeout: 2500 });
  //   await t.click(".fof > .category-content > .simple-item > .add-button", {
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
  //   await t.typeText("#cmd-filter", "rail");
  //
  //     .expect(Selector(".fluid-selected").innerText)
  //     .eql("take-function-off-rail");

  //   // analysis is reruns
  //   const t1 = new Date();
  //   await tawait page.keyboard.press("Enter");
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
  //   await t.typeText("#cmd-filter", "commit");
  //
  //     .expect(Selector(".fluid-selected").innerText)
  //     .eql("commit-feature-flag");

  //   // analysis is reruns
  //   const t1 = new Date();
  //   await tawait page.keyboard.press("Enter");
  //   awaitAnalysis(t, t1);

  //   await expect(returnValue.innerText).contains("farewell Dorian Gray");
  // });

  // test("package_function_references_work", async ({ page }) => {
  //   const repl = Selector(".toplevel.tl-92595864");
  //   const refersTo = Selector(".ref-block.refers-to.pkg-fn");
  //   const usedIn = Selector(".ref-block.used-in.handler");

  //
  //     // Start at this specific repl handler
  //     .navigateTo("#handler=92595864")
  //     .expect(available(repl))
  //     .ok()
  //     // Test that the handler we navigated to has a reference to a package manager function
  //     .expect(available(refersTo))
  //     .ok()
  //     .expect(Selector(".ref-block.refers-to .fnheader").textContent)
  //     .eql("test_admin/stdlib/Test::one_v0")
  //     .click(refersTo)
  //     // Clicking on it should bring us to that function
  //     .expect(available(".toplevel .pkg-fn-toplevel"))
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

  //   await createRepl(t);
  //   await t.typeText("#active-editor", '"Hello world!"');

  //   await page.click(".sidebar-category.secrets .create-tl-icon");

  //   const nameInput = Selector("#new-secret-name-input");

  //   await expect(nameInput.focused).ok();

  //   await page.click(".modal.insert-secret .close-btn");
  // });
});
