# How the integration tests work

Note: the testcafe error "Failed to find a DNS-record for the resource"
actually mean "Can't connect to the server".

##### (Updated Nov 20, 2018)

## Files

- integration-tests/run.sh
  - Basically just triggers testcafe

- integration-tests/tests.js
  - test harness and tests

- integration-tests/screenshots/
  - screenshots

- client2/src/IntegrationTest.ml
  - This contains the code to check that the tests were successful.
    Note that this is compiled into bsmain.js, so we have a
    single app for testing and production.


## How it works:

Uses testcafe with headless-chrome:
- https://devexpress.github.io/testcafe

run.sh calls testcafe, which runs tests.js on headless-chrome in the
container. Our testcafe tests load the server and get the test programs
from server/test_appdata. The client loads it, recognizes from the url
that it's a test, then gets the testcode from IntegrationTest.ml and
stores it in the model.

At the end of a test, the harness clicks the "finish integration test"
button in the browser, which runs the testing function for the test
(usually tests something on the model). The client updates the dom with
"success" or "failure" which the harness reads.

TODO: All this should be called from scripts/build-server and
scripts/compile.

