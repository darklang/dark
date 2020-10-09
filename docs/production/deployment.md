# Deploying

The client is compiled into assets that are pushed to our CDN.

The backend is built into libraries and binaries that use those libraries. We
build containers that use those binaries, then deploy services that use those
containers.
