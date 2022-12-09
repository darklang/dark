# How the integration tests work

##### (Updated Feb, 2022)

## Setup

Ensure your host machine is able to access http://lvh.me.

We use this domain as 'localhost' isn't a fully-qualified domain name. See https://nickjanetakis.com/blog/ngrok-lvhme-nipio-a-trilogy-for-local-development-and-testing#lvh-me.

If it isn't accessible by default, you can use Google's DNS server (8.8.8.8).

## Running

Integration tests can be run in three forms:

### On your machine

The debugger and trace tooling can be used on your machine. To simply run the tests on your machine, use:
`./integration-tests/run.sh `

You need playwright installed on your machine:
`cd integration-tests && npm install`

### Limit tests

To run a subset of tests, run with `--pattern`:

`./integration-tests/run.sh --pattern='my_test_name`

`./integration-tests/run.sh --pattern='my_test_name|other_test_name`

### In debug mode

If you want to debug a test, run the tests
with `--debug`:
`./integration-tests/run.sh --debug --pattern='my_test_name'`

Note: debugging will not work if executing within the devcontainer, as the host's display server is not available.
Run the tests from your host machine for debugging.

### Defeat flaky tests

If you're trying to eliminate a flaky test that is hard to reproduce, use `--repeat`:

`./integration-tests/run.sh --pattern='my_test_name' --repeat=200 --concurrency=12`

### In the container

`./scripts/run-in-docker ./integration-tests/run.sh`

## Troubleshooting

### Debugging errors

Run the tests in debug mode (see above) and step through.

### Traces

Playwright automatically saves traces. The test output will link to the trace, and you can replay it step-by-step using:

`playwright show-trace tracefile.zip`

### Test failures in CI

Test traces are automatically saved in CI (videos are too). You can download the test traces from the CircleCI articifacts tab and use `playwright show-trace` to very easily figure out what went wrong.

### Causes of intermittent failures

Tests will likely fail if you do any of the following:

- call an `async` action, such as `page.waitFo...`, but forget to `await` it (the
  next action will happen when the previous one is not complete)

- call an `expect` without an `await`, unless you explicitly waited for the condition
  in advance (this means you're using an expectation which is not retried until it is
  successful. Instead, do a `page.waitFor`, which will allow you to know the page is in
  the right start before you click it.)

- click somewhere that causes an element to move, but do not wait until it has
  stopped moving to click some part of it (it will click an old ppage)

- have a test where the middle of an element you're clicking on is covered by another
  element (the click even won't land in the right place). Also true if the element is offscreen (can possibly be solved in the test_appdata test JSON file)

## Writing a new test

Our integration test files are scattered across the code base. There are multiple steps and changes you have to make to write a new integration test.

1. If your test required contents on the canvas, add a file in `backend/test_appdata`. File names follow the format of `test-{your_test_name}.json`. To start these files off, either copy from existing files, or press **Save Test** in the button-bar in Dark.

2. Add a new function to `integration-tests/tests.ts`.

```
test('{your_test_name}', async t => {
  // UI interactions and assertions go here
});
```

3. To complete the test, write a validation function in `client/src/IntegrationTest.res`.

```
let {your_test_name} (m : model) : testResult =
    (* Model assertions go here; common ones include checking the AST or looking for allowed/disallowed states *)
    (*  `fail ~f:('a -> string) 'a` - f is a function that takes an object and returns a string for test output *)
```

4. Lastly to verify your newly written test works without running all the other tests, run the script with `--pattern={your_test_name}`

## How it works:

Uses playwright: https://playwright.dev

run.sh calls playwright , which runs tests.js on chrome in the container. Our
tests load the server and get the test programs from `backend/test_appdata`.
The client loads it, recognizes from the url that it's a test, then gets the test code
from `IntegrationTest.res` and stores it in the model.

At the end of a test, the harness clicks the "finish integration test"
button in the browser, which runs the testing function for the test
(usually tests something on the model). The client updates the dom with
"success" or "failure" which the harness reads.

## Files

- `integration-tests/run.sh`

  - Basically just triggers playwright

- `integration-tests/prep.sh`

  - prepare tests (clean database, etc)

- `integration-tests/tests.js`

  - test harness and tests

- `client/src/IntegrationTest.res`

  - This contains the code to check that the tests were successful.
    Note that this is compiled into app.js, so we have a
    single app for testing and production.

- `rundir/integration-tests/`

  - videos, traces, and console logs from the test executions

- `backend/test_appdata`