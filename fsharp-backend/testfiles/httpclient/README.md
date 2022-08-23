# Dark HTTP client tests

The files in this directory are tests of the HTTP client libraries. The test suite
sets up a server that we can make HTTP requests against, using HttpClient. It then
checks the request is as expected, and returns a response to be parsed by the HTTP
client.

A test comprises some Dark code to make a request. The request string will be
compared to the expected test string, and if it is identical, the response will
be sent back and parsed by the HTTP client. The test code then compares the
response to the expected value, in the same way that testfiles do.

The implementation of the tests is in Tests/HttpClient.Tests.fs.

# Code

The code to make the HTTP request should be put in a `[test]` section. For example:

```
[test]
HttpClient::get_v0 "http://HOST/path" {} {} {} = Test.error_v0 "x"
```

Note that this section has an equality test, this is so that the same test can
check how it sends and how it receives data.

# Expected Requests

You test should make a request, which is compared, byte-for-byte, against the
expected request. For example:

```
[expected-request]
POST / HTTP/1.1
Host: HOST
Date: Sun, 08 Nov 2020 15:38:01 GMT
Content-Type: application/json; charset=utf-8
Content-Length: 22

{ "field1": "value1" }
```

If the server receives the exact request it expects, it returns the response.
Otherwise is returns a HTTP code 400.

Note that while HTTP requires headers to end lines with \r\n instead of \n, the
test files use \n (the files are parsed and corrected before being sent to the
server).

# Responses

Responses are returned by the server if the request is exactly as expected. An example is:

```
[response]
HTTP/1.1 200 OK
Date: xxx, xx xxx xxxx xx:xx:xx xxx
Content-Type: application/json; charset=utf-8
Access-Control-Allow-Origin: *
x-darklang-execution-id: 0123456789
Server: Kestrel
Content-Length: LENGTH

{
  "b": { "field1": "value1"},
  "f": null,
  "fb": "{ \"field1\": \"value1\" }",
  "j": { "field1": "value1"}
}
```

The response will presumably be parsed by the HttpClient::\* call.

# Adjusting for minor differences

In test cases, you can use some standard strings in some places to make writing test
cases a little but easier.

## `Host: HOST` header

In the `Host` header, the string `HOST` is replaced with the appropriate host for
server that's running as part of the test suite

## `Content-Type: LENGTH` header

In the `Content-type` header, the test suite will replace the header with the body's length.

## `PATH` in status line

In the status line, `PATH` is replaced by the right path to get the test server to
respond to this test.

## URL in code

In the test body, `URL` is replaced by the actual URL used in the test (this isn't really a URL, it's actually just the hostname and port and the test path, not the scheme or the rest of the path or query string).
