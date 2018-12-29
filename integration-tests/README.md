# How the integration tests work

##### (Updated Dec 27, 2018)

## Running

Integration tests can be run in three forms:

### On your machine
If you want to watch the integration tests, run it on your machine:

- `./integration-tests/run.sh`

You need testcafe installed on your machine:

- `npm install -g testcafe`

### In debug mode

If you want to use the testcafe debugger, run the tests in debug mode:

- `./integration-tests/run.sh --debug`

When Chrome loads, use the buttons at the bottom to step through execution. You can see what step you're on in your terminal. You can run the chrome debugger and inspect, watch what's happening in the console, etc.

You can also debug the test code:
https://devexpress.github.io/testcafe/documentation/recipes/debug-in-chrome-dev-tools.html


## In the container

- `./scripts/run-in-docker ./integration-tests/run.sh`

This runs Chrome in xvfb, so it will not appear on your screen. Unlike other modes, this will run 4 tests at once.

The tests will be recorded to video automatically, and are saved to rundir/videos/testname.mp4. They are viable using VLC.

## Troubleshooting

### Debugging errors

Run the tests in debug mode (see above) and step through.

### Errors
- the testcafe error "Failed to find a DNS-record for the resource"
actually means "Can't connect to the server".

### Causes of intermittent failures

- An `expect` call doesn't actually wait for the item in question, especially   if you use `ok()`.
- A click can land anywhere on the selector, but some parts of the selector
  have clicks that have other effects
- Expectations fail when the element is offscreen. Often moving the element to the left (esp in the JSON test_appdata file) will solve it.



## How it works:

Uses testcafe: https://devexpress.github.io/testcafe

run.sh calls testcafe, which runs tests.js on chrome in the container. Our
testcafe tests load the server and get the test programs from
backend/test_appdata. The client loads it, recognizes from the url that it's a
test, then gets the testcode from IntegrationTest.ml and stores it in the
model.

At the end of a test, the harness clicks the "finish integration test"
button in the browser, which runs the testing function for the test
(usually tests something on the model). The client updates the dom with
"success" or "failure" which the harness reads.

TODO: All this should be called from scripts/build-server and
scripts/compile.


## Files

- integration-tests/run.sh
  - Basically just triggers testcafe

- integration-tests/prep.sh
  - prepare tests

- integration-tests/tests.js
  - test harness and tests

- client/src/IntegrationTest.ml
  - This contains the code to check that the tests were successful.
    Note that this is compiled into app.js, so we have a
    single app for testing and production.

- rundir/screenshots/
  - screenshots

- rundir/videos/
  - videos from the in-container test executions


