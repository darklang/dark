Our "Wasm" project needs its assets hosted somewhere, including .js, .json, .dll, and .wasm files.

In classic-dark, these assets were served by ApiServer, which no longer exists.

Ideally, these will be hosted in a CDN or something, but this hacky canvas should work well-enough locally for now.

The http handler must be accessed via the experimental BwdServer, at :11003.