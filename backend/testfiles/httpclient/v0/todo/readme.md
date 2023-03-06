These test files were copied over from `httpclient/v5` tests. I've been reviewing
these one by one, seeing if they make sense to test against the HttpClient.

Many of these may be best to just ignore/delete - for example, query params are
input differently now (just as part of the URL) and likely don't need so many
test files.
