# How the integration tests work

##### (Written Dec 19, 2017. Who knows if anyone updated it since then)

## Files

- run.sh
  - test runner. It handles test listing, parallelization.

- driveby.js
  - we started using github.com/alltonp/elm-driveby, so the file has the
    same name. It bears no resemblence to the original. It is called
    within phantomjs, and provides the harness.

- TESTNAME.js (lots of them)
  - This is the code to run the test. See client/IntegrationTest.elm

- ../client/IntegrationTest.elm
  - This contains the code to check that the tests were successful. See
    TESTNAME.js. Note that this is compiled into elm.js, so we have a
    single app for testing and production.

- elm.js.patch:
  - Phantomjs sends keyevents in a format elm doesn't recognize.
    Specifically, the event that phantomjs sends lacks the "repeat"
    field. This triggers other errors which make this tricky to track
    down. Solution is this patch, which we run after compiling our elm
    app.


## How it works:

run.sh calls phantomjs on driveby.js, with the test name as an arg.
driveby.js loads the server and gets the test output from
server/appdata. The client loads it, recognizes from the url that it's a
test, then gets the testcode from IntegrationTest.elm and stores it in
the model.

driveby.js then loads the test code from this directory (called
TESTNAME.js above, where TESTNAME refers to many test files), and
executes it. At the end, it clicks the "finish integration test" button
in the browser, which runs the testing function for the test (usually
tests something on the model). It then updates the dom with "success" or
"failure" which driveby.js reads, returning an exit code of 0/1.

run.sh coalesces all these tests in parallel, then succeeds or fails.
All this is called from scripts/build-server and scripts/compile.

