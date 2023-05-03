Some of our experimental canvases here require some static assets to load, some of them shared by multiple canvases. The assets are stored in `backend/static`, and this canvas serves those assets. at the endpoint `http://dark-serve-static.dlio.localhost:11003/:path

Ideally, some of these will be hosted in a CDN or something, but this hacky canvas should work well-enough locally for now.

The http handler must be accessed via the experimental BwdServer, at :11003.
