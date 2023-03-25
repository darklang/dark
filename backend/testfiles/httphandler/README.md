# Dark HTTP server tests

The files in this directory are tests of the HTTP server known as BwdServer,
which supports Dark users' handlers. A test typically comprises a request and a
response, and may also include one or more handler.

The implementation of the tests is in `BwdServer.Tests.fs`.

# HTTP handlers

An example HTTP handler looks like this:

```
[http-handler POST /]
"hello post"
```

The header has the method and path. The body is Dark code using the same F#
syntax we use elsewhere (see `testfiles/README.md` for a guide).

Multiple handlers may be defined within a single test file.

`[http-handler POST /]` syntax is also available, to test a new (thinner)
HTTP handler and middleware. It behaves the same way has `http-handler`, apart
from setting up the new type of HTTP handler.

# Requests

A request is in the following format:

```
[request]
POST / HTTP/1.1
Host: HOST
Date: Sun, 08 Nov 2020 15:38:01 GMT
Content-Type: application/json; charset=utf-8
Content-Length: 22

{ "field1": "value1" }

[response] // Note single blank line above
```

Responses are sent to the server, where presumably they'll hit the HTTP handler
that you've set up (or not, if that's what you're testing).

Note that while HTTP requires headers to end lines with `\r\n` instead of `\n`,
the test files use `\n` (the files are parsed and corrected before being sent
to the server). They also request an empty line after the headers - this is how
HTTP indicates that the headers are finished.

The Content-Length must be set correctly (it's also allowed to not send it). The test
suite will check that the Content-length matches the request body's length. After the
request body, include exactly one blank line to separate it from the start of
"\[response\]".

If you want a Content-length of 0, this is the correct format:

```
[request]
GET /?param1=!@#$%^&*()_+-=[]\{}|;':",./<>? HTTP/1.1
Host: HOST
Date: Sun, 08 Nov 2020 15:38:01 GMT
Content-Length: 0



[response]
```

The three blank lines are:

- the blank line to indicate to HTTP the end of headers
- the content (ending in a newline which is removed by the test suite)
- a blank link to separate from the "\[response\]"

# Responses

Responses are the expected response from the server. An example looks like this:

```
[response]
HTTP/1.1 200 OK
Date: xxx, xx xxx xxxx xx:xx:xx xxx
Content-Type: application/json; charset=utf-8
Access-Control-Allow-Origin: *
x-darklang-execution-id: 0123456789
Server: darklang
Content-Length: LENGTH

{
  "b": { "field1": "value1"},
  "f": null,
  "fb": "{ \"field1\": \"value1\" }",
  "j": { "field1": "value1"}
}
```

The response is expected to be perfect down to the byte. However, JSON responses are
not necessarily identical due to these tests originally being used to test both the
OCaml and the F# servers, and so JSON is parsed and compared (_CLEANUP address this_).

Response headers are normalized, removing the specific values from Date, Expires,
x-darklang-execution-id, and some other headers. See BwdServer.Tests.fs for full
details.

# Editing test files

It may be a struggle to edit these files, since automatic changes made by text
editors can change the contents, for example stripping \r characters, or adding or
removing newlines. To get around this, you can use a hex editor, or `vim -b` with
`noeol` set. You should also add the files to .gitattributes in this directory.

In case you're not well-versed in `vim`, here are some steps to follow:

- `vim ./file-path.test -b`
- hit `i` to enter insert mode
- type/paste your changes
- hit `esc` to escape edit mode
- type `:wq` and press `enter` to escape

One of the automatic changes made by text editors is the trimming of whitespace
at the end of lines of code. To handle this, any usages of `<SPACE>` within the
request or response are replaced with a space character (` `).

Additionally, any usages of `<IMPORT_DATA_FROM_FILE=name>` will result in the
test runner replacing this string with the contents of the referenced binary
file - a file with the corresponding name should exist in `../data`.

# Adjusting for minor differences

# Configuration

## ALLOW-INCORRECT-CONTENT-LENGTH

You can allow a test to have the incorrect content-length.

## Secrets

You can add Secrets:

```
[secrets NAME1:value1,NAME2:value2]
```

## Domain

You can set the domain:

```
[domain my.special.domainname.com]
```

## LENGTH

The token "LENGTH" will be replaced with the length of the request body or the
response body.

## HOST

The token "HOST" will be replaced with domain/host the request is being sent to.
