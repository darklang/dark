import { ClientFunction, Selector } from "testcafe";
import fs from "fs";
const child_process = require("child_process");
const BASE_URL = "http://darklang.localhost:8000/a/test-";
const getPageUrl = ClientFunction(() => window.location.href);
function startXvfb(testname) {
  if (process.env.IN_DEV_CONTAINER != "true") return;
  const script = "scripts/support/start-recording-xvfb";
  child_process.execFileSync(script, [testname]);
}

function stopXvfb(testname) {
  if (process.env.IN_DEV_CONTAINER != "true") return;
  const script = "scripts/support/stop-recording-xvfb";
  child_process.execFileSync(script, [testname]);
}

async function setDebugging(t) {
  let key = `editorState-test-${t.testRun.test.name}`;
  let value = '{"editorSettings":{"showFluidDebugger":true}}';
  const setLocalStorageItem = ClientFunction((key, value) => {
    localStorage.setItem(key, value);
  });
  await setLocalStorageItem(key, value);
}

fixture`Integration Tests`
  // To add this user, run the backend tests
  .beforeEach(async t => {
    const testname = t.testRun.test.name;
    const sessionName = `${testname}-${t.testRun.quarantine.attempts.length}`;
    // startXvfb(sessionName);
    var url = `${BASE_URL}${testname}?integration-test=true`;
    await t.navigateTo(url);
    await setDebugging(t);
    await t
      .typeText("#username", "test")
      .typeText("#password", "fVm2CUePzGKCwoEQQdNJktUQ")
      .pressKey("enter");

    await Selector("#finishIntegrationTest").exists;

    /* Testcafe runs everything through a proxy, wrapping all values and
     * objects such that it seems like nothing happened. However, they forgot
     * to wrap objects in Webworker contexts, so calls to Fetch in the worker
     * thinks it's on a different domain. This breaks cookies, auth, CORS,
     * basically everything. So we thread the right url through to do the
     * proxying ourselves. Hopefully they'll fix this and we can remove this
     * code someday */
    await t.eval(
      () => {
        window.testcafeInjectedPrefix = prefix;
      },
      {
        dependencies: {
          prefix: `${new URL(t.testRun.browserConnection.url).origin}/${
            t.testRun.session.id
          }/`,
        },
      },
    );

    await t.takeScreenshot();
  })
  .afterEach(async t => {
    const testname = t.testRun.test.name;
    const sessionName = `${testname}-${t.testRun.quarantine.attempts.length}`;
    // stopXvfb(sessionName);
    const finish = Selector("#finishIntegrationTest");
    const signal = Selector("#integrationTestSignal");
    let flushLogs = async () => {
      const { log, warn, error, info } = await t.getBrowserConsoleMessages();
      const msgs = Array.concat(
        ["Console Logs:"],
        log,
        ["\n\nConsole Warnings"],
        warn,
        ["\n\nConsole Errors:"],
        error,
        ["\n\nConsole Infos:"],
        info,
      );
      fs.writeFile(
        `rundir/integration_test_logs/${testname}.log`,
        msgs.join("\n"),
        () => {},
      );

      return true;
    };

    let flushedLogs = false;
    try {
      // TODO: clicks on this button are not registered in function space
      // We should probably figure out why.
      // For now, putting a more helpful error message
      await t
        .click(finish)
        .expect(finish.exists)
        .notOk(
          "Finish button click failed: did you try to click it from the function space?",
        )
        .expect(signal.exists)
        .ok("Test evaluation has timed out. Has the application crashed?");

      const { error } = await t.getBrowserConsoleMessages();
      await t.expect(error).eql([]);

      if ((await t.testRun.errs).length > 0 || !(await signal.hasClass("success"))) {
        await t.takeScreenshot();
        flushedLogs = flushLogs();
        if ((await signal.textContent) != "success") {
          await t.expect("test state").eql(await signal.textContent);
        }
      }

      await t.expect(signal.hasClass("success")).eql(true);
    } catch (e) {
      if (!flushedLogs) {
        flushLogs();
      }
      throw e;
    }
  });

//********************************
// Utilities
//********************************
async function createHTTPHandler(t) {
  await t
    .pressKey("enter")
    .pressKey("down")
    .pressKey("enter");
}

async function createWorkerHandler(t) {
  await t
    .pressKey("enter")
    .pressKey("down")
    .pressKey("down")
    .pressKey("down")
    .pressKey("down")
    .pressKey("enter");
}

async function createRepl(t) {
  await t.pressKey("enter").pressKey("enter");
}

async function gotoAST(t) {
  await t.click("#fluid-editor");
}

function user_content_url(t, endpoint) {
  return "http://test-" + t.testRun.test.name + ".builtwithdark.lvh.me:8000" + endpoint;
}

//********************************
// Avoiding test race conditions
//********************************
// Testcafe automatically waits for the next thing you've specified. So
// if you .typeText("#entry-box", ...), it will wait for the entryBox.
// But we sometimes need to explicitly wait if TestCafe can't tell what
// we're waiting on.

function available(css) {
  return Selector(css).exists;
}
function entryBoxAvailable() {
  return Selector("#entry-box").exists;
}

// Return the highlighted autocomplete entry
function acHighlightedText() {
  return Selector(".autocomplete-item.highlighted").textContent;
}

function fluidAcHighlightedText() {
  return Selector(".autocomplete-item.fluid-selected").textContent;
}

const scrollBy = ClientFunction((id, dx, dy) => {
  document.getElementById(id).scrollBy(dx, dy);
});

// ------------------------
// Tests below here. Don't forget to update client/src/IntegrationTest.ml
// ------------------------

test("switching_from_http_to_cron_space_removes_leading_slash", async t => {
  await createHTTPHandler(t);
  await t
    // add headers
    .typeText("#entry-box", "PO")
    .expect(acHighlightedText("POST"))
    .ok()
    .pressKey("enter")

    .typeText("#entry-box", "/spec_name")
    .pressKey("enter")

    // edit space
    .click(".spec-header > .handler-type > .space")
    .pressKey("ctrl+a")
    .pressKey("backspace")
    .typeText("#entry-box", "CRON")
    .pressKey("enter");
});

test("switching_from_http_to_repl_space_removes_leading_slash", async t => {
  await createHTTPHandler(t);
  await t
    // add headers
    .typeText("#entry-box", "PO")
    .expect(acHighlightedText("POST"))
    .ok()
    .pressKey("enter")

    .typeText("#entry-box", "/spec_name")
    .pressKey("enter")

    // edit space
    .click(".spec-header > .handler-type > .space")
    .pressKey("ctrl+a")
    .pressKey("backspace")
    .typeText("#entry-box", "REPL")
    .pressKey("enter");
});

test("switching_from_http_space_removes_variable_colons", async t => {
  await createHTTPHandler(t);
  await t
    // add headers
    .typeText("#entry-box", "PO")
    .expect(acHighlightedText("POST"))
    .ok()
    .pressKey("enter")

    .typeText("#entry-box", "/spec_name/:variable")
    .pressKey("enter")

    // edit space
    .click(".spec-header > .handler-type > .space")
    .pressKey("ctrl+a")
    .pressKey("backspace")
    .typeText("#entry-box", "REPL")
    .pressKey("enter");
});

test("enter_changes_state", async t => {
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable())
    .ok();
});

test("field_access_closes", async t => {
  await createHTTPHandler(t);
  await gotoAST(t);
  await t
    .pressKey("r e q")
    .expect(fluidAcHighlightedText("requestdict"))
    .ok()
    .pressKey(". b o")
    .expect(fluidAcHighlightedText("bodyfield"))
    .ok()
    .pressKey("enter");
});

test("field_access_pipes", async t => {
  await createHTTPHandler(t);
  await gotoAST(t);
  await t
    .pressKey("r e q")
    .expect(fluidAcHighlightedText())
    .contains("request")

    .pressKey(". b o")
    .expect(fluidAcHighlightedText())
    .eql("bodyfield")
    .pressKey("shift+enter");
});

test("tabbing_works", async t => {
  await createRepl(t);
  // Fill in "then" box in if stmt
  await t.pressKey("i f space tab 5");
});

test("autocomplete_highlights_on_partial_match", async t => {
  await createRepl(t);
  await gotoAST(t);
  await t
    .pressKey(" n t : : a d d")
    .expect(fluidAcHighlightedText("Int::add"))
    .ok()
    .pressKey("enter");
});

test("no_request_global_in_non_http_space", async t => {
  await createWorkerHandler(t);
  await gotoAST(t);
  await t
    .pressKey("r e q u e s t")
    .expect(fluidAcHighlightedText("Http::badRequest"))
    .ok()
    .pressKey("enter");
});

test("ellen_hello_world_demo", async t => {
  await createHTTPHandler(t);
  await t
    // verb
    .typeText("#entry-box", "g")
    .pressKey("enter")

    // route
    .typeText("#entry-box", "/hello")
    .pressKey("enter")

    // string
    .pressKey(' " H e l l o space w o r l d ! "');
});

test("editing_headers", async t => {
  await createHTTPHandler(t);
  await t
    // add headers
    .typeText("#entry-box", "PO")
    .expect(acHighlightedText("POST"))
    .ok()
    .pressKey("enter")

    .typeText("#entry-box", "/hello")
    .pressKey("enter")

    // edit them
    .click(".spec-header > .handler-name")
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "/myroute")
    .pressKey("enter")

    .click(".spec-header > .handler-type > .modifier")
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "GET")
    .pressKey("enter");
});

test("switching_to_http_space_adds_slash", async t => {
  await createWorkerHandler(t);
  await t
    // add headers
    .click(".spec-header > .handler-name")
    .pressKey("enter")
    .typeText("#entry-box", "spec_name")
    .pressKey("enter")

    // edit space
    .click(".spec-header > .handler-type > .space")
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "HTTP")
    .pressKey("enter");
});

test("switching_from_default_repl_space_removes_name", async t => {
  await createRepl(t);
  await t
    // edit space
    .click(".spec-header > .handler-type >.space")
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "CRON")
    .pressKey("enter");
});

test("tabbing_through_let", async t => {
  await createRepl(t);
  await gotoAST(t);
  await t
    .pressKey("l e t enter")

    // round trip through the let blanks once
    .pressKey("tab tab tab")

    // go to the body and fill it in
    .pressKey("tab tab 5")

    // go to the rhs and fill it in
    .pressKey("tab tab 5")

    // fill in the var
    .pressKey("tab m y v a r");
});

test("rename_db_fields", async t => {
  const callBackend = ClientFunction(function(url) {
    var xhttp = new XMLHttpRequest();
    xhttp.open("POST", url, true);
    xhttp.setRequestHeader("Content-type", "application/json");
    xhttp.send('{ "field6": "a", "field2": "b" }');
  });

  // rename
  await t
    .click(Selector(".name").withText("field1"))
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "field6")
    .pressKey("enter");

  // add data and check we can't rename again
  await callBackend(user_content_url(t, "/add"));

  // This is super-shaky if we remove this. There's some timing things
  // around when the .fa-lock appears, and the selectors we'd expect
  // (below) doesn't work. But if we split it into two it works. Who
  // knows.
  // await t.expect(Selector('.fa-lock', {timeout: 5000})().exists).ok() ;

  await Selector(".fa-lock", { timeout: 5000 })();
  await t.expect(Selector(".fa-lock").exists).ok();

  await t
    .click(Selector(".name").withText("field6"))
    .pressKey("enter")
    .pressKey("enter");
});

test("rename_db_type", async t => {
  const callBackend = ClientFunction(function(url) {
    var xhttp = new XMLHttpRequest();
    xhttp.open("POST", url, true);
    xhttp.setRequestHeader("Content-type", "application/json");
    xhttp.send('{ "field1": "a", "field2": 5 }');
  });

  // rename
  await t
    .click(Selector(".type").withText("Int"))
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "String")
    .pressKey("enter");

  // add data and check we can't rename again
  await callBackend(user_content_url(t, "/add"));

  // This is super-shaky if we remove this. There's some timing things
  // around when the .fa-lock appears, and the selectors we'd expect
  // (below) doesn't work. But if we split it into two it works. Who
  // knows.
  // await t.expect(Selector('.fa-lock', {timeout: 5000})().exists).ok() ;

  await Selector(".fa-lock", { timeout: 5000 })();
  await t.expect(Selector(".fa-lock").exists).ok();

  await t
    .click(Selector(".type").withText("String"))
    .pressKey("enter")
    .pressKey("enter");
});

// Testcafe doesn't actually generate ClipboardEvents for copy/paste
// keystrokes. So I've disabled these tests for now.
/*
test("paste_right_number_of_blanks", async t => {
  await t
    .click(Selector('.fnname').withText('-'))
    .pressKey("meta+c")
    .click(Selector('.fnname').withText('(+)'))
    .pressKey("meta+v")
});

*/

/* Disable for now, will bring back as command palette fn
test("feature_flag_works", async t => {
  await t
    // Create an empty let
    .pressKey("enter")
    .pressKey("enter")
    .typeText("#entry-box", "let")
    .pressKey("enter")
    .typeText("#entry-box", "a")
    .pressKey("enter")
    .typeText("#entry-box", "13")
    .pressKey("enter")
    .pressKey("down")
    .pressKey("esc")

    // Click feature name
    .click('.expr-actions .flag')

    // Name it
    .expect(available(".feature-flag")).ok()
    .typeText("#entry-box", "myflag")
    .pressKey("enter")

    // Set condition
    .typeText("#entry-box", "Int::greaterThan")
    .pressKey("enter")
    .typeText("#entry-box", "a")
    .pressKey("enter")
    .typeText("#entry-box", "10")
    .pressKey("enter")

    // Case A
    .typeText("#entry-box", "\"")
    .typeText("#entry-box", "A")
    .pressKey("enter")

    // Case B
    .typeText("#entry-box", "\"")
    .typeText("#entry-box", "B")
    .pressKey("enter")

});

test("feature_flag_in_function", async t => {
  await t
    // Go to function
    .click(".fun1")
    .click(".fa-edit")

    .expect(available(".tl-2296485551")).ok()
    .click(".tl-2296485551")
    .pressKey("enter")

    // Make feature Flag
    .click('.expr-actions .flag')

    .expect(available(".feature-flag")).ok()
    .typeText("#entry-box", "myflag")
    .pressKey("enter")

    // Set condition
    .typeText("#entry-box", "true")
    .pressKey("enter")

    // Case B
    .typeText("#entry-box", "3")
    .pressKey("enter")

    // Return to main canvas to finish tests
    .click(".return-to-canvas")
    .expect(available(".tl-180770093")).ok()
});
*/
test("rename_function", async t => {
  const fnNameBlankOr = ".fn-name-content";
  await t
    .navigateTo("#fn=123")
    .expect(available(fnNameBlankOr))
    .ok({ timeout: 1000 })
    .click(Selector(fnNameBlankOr))
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", "hello")
    .pressKey("enter");
});

test("execute_function_works", async t => {
  await createRepl(t);
  await t.pressKey("U u i d : : g e n enter").click(Selector(".execution-button-needed"));

  let v1 = await Selector(".selected .live-value").innerText;

  await t.click(Selector(".fa-redo"));

  let v2 = await Selector(".selected .live-value").innerText;

  let re = /<UUID: [0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}>/;
  await t.expect(v1).match(re);
  await t.expect(v2).match(re);
  await t.expect(v1).notEql(v2);
});

test("function_version_renders", async t => {
  await createRepl(t);
  await t
    .pressKey("D B : :  d e l")
    .expect(Selector(".autocomplete-item.fluid-selected .version").withText("v1"))
    .ok();
});

test("delete_db_col", async t => {
  await t.click(Selector(".delete-col"));
});

test("cant_delete_locked_col", async t => {
  await t.click(Selector(".fluid-fn-name")); // this click is required due to caching
  await Selector(".execution-button-needed", { timeout: 5000 })();
  await t
    .expect(Selector(".execution-button-needed").exists)
    .ok()
    .click(Selector(".execution-button-needed"));

  await Selector(".fa-lock", { timeout: 5000 })();
  await t.expect(Selector(".fa-lock").exists).ok();

  await t
    .click(Selector(".db")) // this click is required due to caching
    .expect(Selector(".delete-col").exists)
    .notOk();
});

test("select_route", async t => {
  const categoryHeader = "summary.headerSummary";
  const httpVerbLink = "a.verb.verb-link";
  const toplevelElement = ".node .toplevel";

  await t.click(Selector(categoryHeader));

  await Selector(httpVerbLink, { timeout: 5000 })();
  await t.click(Selector(httpVerbLink));

  await Selector(toplevelElement, { timeout: 5000 })();
  await t.expect(Selector(toplevelElement).hasClass("selected")).ok();
});

// TODO: This needs Stroller/Pusher in CI
// test('passwords_are_redacted', async t => {
//   const callBackend = ClientFunction(
//     function (url) {
//       var xhttp = new XMLHttpRequest();
//       xhttp.open("POST", url, true);
//       xhttp.setRequestHeader("Content-type", "application/json");
//       xhttp.send('{ "password": "redactme!" }');
//     });

//   await t.click(Selector('.Password\\:\\:hash'))
//   await callBackend(user_content_url(t, "/signup"));
//   await t.expect(Selector('.live-value').textContent).eql('<Password: Redacted>', { timeout: 5000 })
// })
//
//
// TODO: Add test that verifies pasting text/plain when Entering works
// See: https://github.com/darklang/dark/pull/725#pullrequestreview-213661810

test("function_analysis_works", async t => {
  await t
    .navigateTo("#fn=1039370895")
    .expect(available(".user-fn-toplevel"))
    .ok({ timeout: 1000 })
    .click(Selector(".user-fn-toplevel #fluid-editor .fluid-binop"))
    .expect(Selector(".selected .live-value").textContent)
    .eql("10", { timeout: 5000 });
});

test("fourohfours_parse", async t => {
  const sendPushEvent = ClientFunction(function() {
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

  await sendPushEvent();
});

test("fn_page_to_handler_pos", async t => {
  await t
    .navigateTo("#fn=890")
    .expect(available(".user-fn-toplevel"))
    .ok({ timeout: 1000 });
  const fnOffset = await Selector("#canvas").getStyleProperty("transform");

  await t
    .navigateTo("#handler=123")
    .expect(available(".tl-123"))
    .ok({ timeout: 1000 });

  await t.expect(Selector("#canvas").getStyleProperty("transform")).notEql(fnOffset);
});

test("autocomplete_visible_height", async t => {
  await createRepl(t);
  await t
    .pressKey("r")
    .expect(Selector("li.autocomplete-item.valid").nth(5).visible)
    .ok();
});

test("load_with_unnamed_function", async t => {
  await t
    .pressKey("enter")
    .expect(entryBoxAvailable())
    .ok();
});

test("extract_from_function", async t => {
  await t
    .navigateTo("#fn=123")
    .expect(available(".tl-123"))
    .ok()
    .click(Selector(".user-fn-toplevel #fluid-editor > span"))
    .pressKey("alt+x")
    .typeText("#cmd-filter", "extract-function")
    .pressKey("enter");
});

test("fluid_execute_function_shows_live_value", async t => {
  /* NOTE: This test is intended to determine if clicking a play button in fluid
  makes the live value visible. There is some extra complication in that clicking
  on a play button as it stands does not actually "count" as clicking on the play button
  unless the handler is "active" with a placed caret. To account for this, we
  click on "hello" within Crypto::sha256 ("hello" |> String::toBytes) after focusing
  in order to place the caret. Then we click on the button and see if the live value
  corresponds to the result of `Crypto::sha256`. */
  await t
    .navigateTo("#handler=1013604333")
    .expect(available(".id-1334251057 .execution-button"))
    .ok()
    .click(Selector(".id-1045574047.fluid-string"))
    .click(Selector(".id-1334251057 .execution-button"))
    .expect(available(".selected .live-value"))
    .ok()
    .expect(Selector(".selected .live-value").innerText)
    .eql("<Bytes: length=32>");
});

test("fluid_single_click_on_token_in_deselected_handler_focuses", async t => {
  await t
    .expect(available(".id-2068425241.fluid-let-lhs"))
    .ok()
    .click(Selector(".id-2068425241.fluid-let-lhs"), { caretPos: 2 });
});

test("fluid_click_2x_on_token_places_cursor", async t => {
  await t
    .expect(available(".id-549681748.fluid-let-lhs"))
    .ok()
    .click(Selector(".id-549681748.fluid-let-lhs"), { caretPos: 2 })
    .click(Selector(".id-549681748.fluid-let-lhs"), { caretPos: 2 });
});

test("fluid_click_2x_in_function_places_cursor", async t => {
  await t
    .navigateTo("#fn=1352039682")
    .expect(available(".id-677483670.fluid-let-lhs"))
    .ok()
    .click(Selector(".id-677483670.fluid-let-lhs"), { caretPos: 2 })
    .expect(available(".id-96908617.fluid-category-string"))
    .ok()
    .click(Selector(".id-96908617.fluid-category-string"), { caretPos: 2 });
});

test("fluid_double_click_selects_token", async t => {
  await t
    .navigateTo("#handler=123")
    .expect(available(".tl-123"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .doubleClick(Selector(".fluid-match-keyword"), { caretPos: 3 });
});

test("fluid_double_click_with_alt_selects_expression", async t => {
  await t
    .navigateTo("#handler=123")
    .expect(available(".tl-123"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .doubleClick(Selector(".fluid-match-keyword"), {
      caretPos: 3,
      modifiers: { alt: true },
    });
});

test("fluid_shift_right_selects_chars_in_front", async t => {
  await t
    .navigateTo("#handler=123")
    .expect(available(".tl-123"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .click(Selector(".fluid-category-string"), { caretPos: 2 })
    .pressKey("shift+right shift+down shift+right");
});

test("fluid_shift_left_selects_chars_at_back", async t => {
  await t
    .navigateTo("#handler=123")
    .expect(available(".tl-123"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .click(Selector(".fluid-category-string"), { caretPos: 2 })
    .pressKey("down shift+left shift+up");
});

test("fluid_undo_redo_happen_exactly_once", async t => {
  await t
    .expect(available(".tl-608699171"))
    .ok()
    .click(Selector(".id-68470584.fluid-category-string"))
    .expect(available(".selected #fluid-editor"))
    .ok()
    .expect(Selector(".fluid-category-string").textContent)
    .eql('"12345"')
    .pressKey("ctrl+z")
    .expect(Selector(".fluid-category-string").textContent)
    .eql('"1234"')
    .pressKey("ctrl+shift+z")
    .expect(Selector(".fluid-category-string").textContent)
    .eql('"12345"');
});

test("fluid_ctrl_left_on_string", async t => {
  await t
    .navigateTo("#handler=428972234")
    .expect(available(".tl-428972234"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .click(Selector(".fluid-string"), { caretPos: 10 })
    .pressKey("ctrl+left");
});

test("fluid_ctrl_right_on_string", async t => {
  await t
    .navigateTo("#handler=428972234")
    .expect(available(".tl-428972234"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .click(Selector(".fluid-string"), { caretPos: 10 })
    .pressKey("ctrl+right");
});

test("fluid_ctrl_left_on_empty_match", async t => {
  await t
    .navigateTo("#handler=281413634")
    .expect(available(".tl-281413634"))
    .ok()
    .expect(available(".selected #fluid-editor"))
    .ok()
    .click(Selector(".fluid-category-pattern.id-63381027"), { caretPos: 0 })
    .pressKey("ctrl+left");
});

test("varnames_are_incomplete", async t => {
  await t
    .click(".toplevel")
    .click(Selector(".spec-header > .handler-name"))
    .pressKey("ctrl+a backspace")
    .typeText("#entry-box", ":a")
    .expect(acHighlightedText("/:a"))
    .ok()
    .pressKey("tab a enter");

  await t.expect(Selector(".live-value").textContent).contains("<Incomplete>");
});

test("center_toplevel", async t => {
  await t
    .navigateTo("#handler=1445447347")
    .expect(available(".tl-1445447347"))
    .ok();
});

test("max_callstack_bug", async t => {
  await createRepl(t);
  await gotoAST(t);
  await t
    // I don't know what the threshold is exactly, but 1500 didn't tickle
    // the bug
    .pressKey("L i s t : : r a n g e space 0 space 2 0 0 0 space");
});

test("sidebar_opens_function", async t => {
  await t
    .expect(available(".sidebar-section.fns .headerSummary"))
    .ok()
    .click(Selector(".sidebar-section.fns .headerSummary"))
    .expect(available(".sidebar-section.fns a[href='#fn=1352039682']"))
    .ok()
    .click(Selector(".sidebar-section.fns a[href='#fn=1352039682']"))
    .expect(getPageUrl())
    .match(/.+#fn=1352039682$/, "Url is incorrect");
});

// This runs through
// https://docs.aws.amazon.com/general/latest/gr/sigv4-add-signature-to-request.html
// It duplicates backend/test/test_otherlibs.ml's "Crypto::sha256hmac works for
// AWS", _but_ its value _here_ is that we do not have any other tests that push
// a handler's trigger play button; getting that working was surprisingly hard,
// and so ismith wants to leave it around to use in future integration tests.
//
// See integration-tests/README.md for docs on this.
//
// I have tried several other approaches, including wait(3000) between
// navigateTo() and click(), and putting navigateTo() and click() on separate
// await ts, but only calling click() twice worked here.
test("sha256hmac_for_aws", async t => {
  await t
    .navigateTo("#handler=1471262983")
    .click(Selector("div.handler-trigger"))
    .click(Selector("div.handler-trigger"));
  await t
    .expect(Selector(".return-value").innerText)
    .eql('"5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7"');
});

test("fluid_fn_pg_change", async t => {
  await t.navigateTo("#fn=2091743543");
  await t.expect(available(".tl-2091743543")).ok();

  await t.navigateTo("#fn=1464810122");
  await t.expect(available(".tl-1464810122")).ok();

  // Click into code to edit
  await t.click(Selector(".fluid-entry.id-1154335426"));

  //Make sure we stay on the page
  await t.expect(available(".tl-1464810122")).ok({ timeout: 1000 });
});

test("fluid_creating_an_http_handler_focuses_the_verb", async t => {
  await createHTTPHandler(t);

  await t
    .pressKey("down") // enter AC
    .expect(acHighlightedText("GET"))
    .ok();
});

test("fluid_tabbing_from_an_http_handler_spec_to_ast", async t => {
  await createHTTPHandler(t);
  await t
    .pressKey("tab") // verb -> route
    .pressKey("tab") // route -> ast
    .pressKey("r") // enter AC
    .expect(fluidAcHighlightedText("request"))
    .ok();
});

test("fluid_tabbing_from_handler_spec_past_ast_back_to_verb", async t => {
  await createHTTPHandler(t);
  await t
    .pressKey("tab") // verb -> route
    .pressKey("tab") // route -> ast
    .pressKey("tab") // ast -> loop back to verb;
    .pressKey("down") // enter AC
    .expect(acHighlightedText("GET"))
    .ok();
});

test("fluid_shift_tabbing_from_handler_ast_back_to_route", async t => {
  await createHTTPHandler(t);
  await t
    .pressKey("tab") // verb -> route
    .pressKey("tab") // route -> ast
    .pressKey("shift+tab") // ast -> back to route;
    .pressKey("down") // enter route
    .expect(acHighlightedText("/"))
    .ok();
});
