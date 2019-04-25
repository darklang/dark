# How the integration tests work

##### (Updated Dec 27, 2018)

## Running

Integration tests can be run in three forms:

### On your machine
If you want to watch the integration tests, run it on your machine:  
 `./integration-tests/run.sh`  

You need testcafe installed on your machine:  
 `npm install -g testcafe`  

### In debug mode

If you want to use the testcafe debugger, run the tests in debug mode:  
 `./integration-tests/run.sh --debug`  

When Chrome loads, use the buttons at the bottom to step through execution. You can see what step you're on in your terminal. You can run the chrome debugger and inspect, watch what's happening in the console, etc.

You can also debug the test code:
https://devexpress.github.io/testcafe/documentation/recipes/debug-in-chrome-dev-tools.html


## In the container

`./scripts/run-in-docker ./integration-tests/run.sh`  

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

## Writing a new test

Unfortunately in our setup integration test files are scattered across the code base. There are multiple steps and changes you have to make to write a new integration test.


1. If your test require preloaded data (are you starting with a non-empty canvas), you will need to add an ops file inside **backend/test_appdata**. File names follow the format of `test-{your_test_name}.json`. You may copy from other setup files that have similiar initial conditions as your new test.  


2. Add a new function to **integration-tests/test.js** (ES6 syntax will work). 
```
test('{your_test_name}', async t => {
  IF YOU WANT TO VALIDATE UI INTERACTIONS, WRITE YOUR LOGIC HERE
});
```  


3. For the test to run you must write a function and call it inside **client/IntegrationTest.ml**.
```
let {your_test_name} (m : model) : testResult = 
    IF YOU WANT TO DO MODEL CHANGES VALIDATION, A COMMON VALIDATION IS AST CHECKS
    YOU CAN CHECK FOR ALLOWED/DISALLOWED STATES YOU EXPECT YOUR MODEL TO CHANGE.
    EACH CASE SHOULD RETURN EITHER pass OR fail.
    fail ~f:{stringify function} (object that does not seem to match expected results)
```  


4. Lastly to verify your newly written test works without running all the other tests, run the script with `--pattern={your_test_name}`

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


