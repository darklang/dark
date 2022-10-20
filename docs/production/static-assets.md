# Darklang.com Static Assets Notes

_Note: this is about how we store assets for darklang.com, not how customers use assets (though we should move to using the customer solution)_

We're storing static assets in a google backend bucket now, which is then
fronted by a gcloud loadbalancer (which does the CDN work).

Of note:

- The bucket must send appropriate Access-Control-Allow-Origin headers, or the
  browser won't be able to load some things. (We've seen this with
  `vendor/fontawesome-*/css/all.css`, and with `*.{woff,ttf}`.) - To fix: create a file
  cors.json:

```json
[
  {
    "origin": ["https://*.darklang.com", "https://darklang.com"],
    "responseHeader": ["Content-Type"],
    "method": ["GET"],
    "maxAgeSeconds": 3600
  }
]
```

and attach it to the bucket: `gcloud storage buckets update --set-cors-file cors.json gs://<bucket_name>`.

- If we add more origins, obviously this document (and the CORS policy) will
  need to be updated.
