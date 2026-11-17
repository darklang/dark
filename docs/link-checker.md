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

The checker handles four URLs at a time. Each URL gets one result, even if several
pages link to it. Redirects are followed up to ten times, and relative links use
the final page URL. Only HTML on the starting origin is crawled, including paths
outside the starting page's directory. External URLs are checked with HEAD, using
GET if the server returns 405 or 501. A request times out after 15 seconds; an
initial GET or HEAD that times out is tried once more.

Results appear as each batch is processed:

```text
[1] OK 200 https://docs.darklang.com/
[2] FAIL https://docs.darklang.com/missing: HTTP 404
```

A slow request can delay other results in its batch. At the end, the report lists
all failures with the pages that link to them, followed by the totals. A 403 or 429
can mean the server blocked the checker, rather than that the page was removed.

The default limit is 3,000 distinct URLs. Add `--limit N` after the starting URL to
change it. Redirects, retries and HEAD fallbacks count as part of the same URL's
check. If the limit is reached, the summary includes the number of unchecked URLs.
Exit codes are 0 for a complete passing crawl, 1 for failures, and 2 for invalid
arguments or an incomplete crawl.

The HTML scanner reads href/src attributes, relative URLs, base href, quoted and
unquoted attributes, common named entities and numeric entities. It skips
fragments, non-HTTP schemes, invalid URLs, URLs with credentials, comments, and
script/style text. It does not check anchor IDs, run JavaScript, read CSS URLs or
srcset, or reproduce a browser's handling of malformed HTML. There is no
robots.txt handling, saved history, scheduler or email sender.

Tests use a fake HTTP function and do not need public websites:

```sh
./scripts/run-cli test run --force Darklang.Tools.LinkCheck
```
