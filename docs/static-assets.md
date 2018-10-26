Static Assets Notes
===================
We're storing static assets in a google backend bucket now, which is then
fronted by a gcloud loadbalancer (which does the CDN work).

Of note:
- gcloud can do "decompressive transcoding", which means that what we upload
  must already be gzipped, and the 'Content-Encoding: gzip' header attached
(see: `scripts/push-assets-to-cdn` script for details). Assuming that is done,
gcloud cloud cdn will handle sending either gzipped or decompressed files
depending on whether the request headers include 'Content-Encoding: gzip'.
  - NOTE: if you do one but not the other (`gsutil cp -h 'Content-Encoding:
    gzip'`, but with decompressed files), Chrome will give you CONTENT_ENCODING
    errors. To fix: re-upload with `gsutil cp` and bust the cache in GCloud Console.
    You are not likely to run into this, because `scripts/push-assets-to-cdn`
    does the work.
- The bucket must send appropriate Access-Control-Allow-Origin headers, or the
  browser won't be able to load some things. (We've seen this with
`vendor/fontawesome-*/css/all.css`, and with `*.{woff,ttf}`.)
  - To fix: create a file cors.json:
```json
[
  {
    "origin": [
      "https://*.darklang.com",
      "https://darklang.com"
    ],
    "responseHeader": [
      "Content-Type"
    ],
    "method": [
      "GET"
    ],
    "maxAgeSeconds": 3600
  }
]
```
and attach it to the bucket: `gsutil cors set cors.json gsutil://<bucket_name>`.
  - If we add more origins, obviously this document (and the CORS policy) will
    need to be updated.
