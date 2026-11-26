# Documentation link checker

`Darklang.Tools.LinkCheck` crawls a site's HTML and checks its HTTP(S) links.
It replaces the crawling and reporting part of Dr. Link Check.

Build with `scripts/dev/build`, then run:

```sh
./scripts/run-cli run \
  --allow stdout \
  --allow 'http GET *' \
  --allow 'http HEAD *' \
  Darklang.Tools.LinkCheck.checkSite https://docs.darklang.com
```

These permissions last for this run and cover external links and redirects too.
For a local site, the runtime also needs `DARK_CONFIG_HTTP_LOOPBACK=on`; HTTP
permissions alone do not enable loopback access.

The checker handles up to four URLs at a time. Each URL gets one result, even if
several pages link to it. Redirects are followed up to ten times, and relative
links use the final page URL. Each check starts with an empty cookie jar.
Cookies received during redirects can be sent back to the same origin and are
discarded when that check finishes. The checker discovers links in HTML and CSS on the
starting origin, including paths outside the starting page's directory.
External URLs are checked with HEAD, using
GET if HEAD returns 404, 405, 410 or 501. Some servers return 404 to HEAD even when
GET serves or redirects the URL. A request times out after 15 seconds; an
initial GET or HEAD that times out is tried once more.

Pass `--audit` after the starting URL to use GET for external URLs as well. Audit
batches start at most one URL per host. Redirects use their starting URL's slot.
The HTTP client retries eligible failures once and honors `Retry-After` when the
delay fits within the request timeout.

Results appear as each batch is processed:

```text
[1] OK 200 https://docs.darklang.com/
[2] FAIL https://docs.darklang.com/missing: HTTP 404
```

A slow request can delay other results in its batch. The final report starts
with totals and groups findings by HTTP status or error type, including DNS,
TLS, timeouts and redirect limits. Each finding shows the method, URL,
redirect destination when it differs, and up to three referring pages. The
report shows how many more pages link to that URL. Missing fragments are grouped
by fragment name and show up to three target pages. Pass `--all-sources` to show
every referring page and fragment target. The console adds color on terminals.

The default limit is 3,000 distinct URLs. Add `--limit N` after the starting URL to
change it. Redirects, retries and HEAD fallbacks count as part of the same URL's
check. If the limit is reached, the summary includes the number of unchecked URLs.
Exit codes are 0 for a complete passing crawl, 1 for failures, and 2 for invalid
arguments or an incomplete crawl.

The HTML scanner reads href/src and srcset attributes, common Open Graph and
Twitter URL fields, and CSS in style attributes, style elements, and fetched
same-origin stylesheets. CSS discovery covers common `url()` and `@import` syntax.
It also checks destinations from meta refresh and a standalone literal
`location` assignment in an inline JavaScript block.
It validates ordinary fragments against IDs and named anchors in successfully
fetched same-origin HTML. Fragment findings do not add to the HTTP URL count.
Connection hints such as `preconnect`, non-HTTP schemes and URLs containing
credentials are skipped. The scanner does
not execute JavaScript or implement browser handling of malformed HTML and CSS.

Tests use a fake HTTP function and do not need public websites:

```sh
./scripts/run-cli test run --force Darklang.Tools.LinkCheck
```
