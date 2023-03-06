# Dark HttpClient test files

The files in this directory are tests of the `HttpClient::request` standard
library function. The test suite sets up a server that we can make HTTP requests
against, using HttpClient. It then checks the request is as expected, and
returns a response to be parsed by the HTTP client.

These files are read, parsed, and evaluated by `HttpClient.Tests.fs`.

---

This all works almost exactly how the `HttpClient::` functions are tested, in the
corresponding `httpclient` directory. Please refer to the `README.md` there for
details on how these tests are set up.
