# A dark sync server: `dark sync serve` on one store, kept in /data.
#
# The image holds a release build of the CLI, copied in as `dark` beside this file
# (`deploy/sync-server/prepare` puts it there). It listens on 8080 with plain HTTP; TLS is the
# platform's (Fly's proxy, Caddy on a VM).
FROM ubuntu:24.04

RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates curl libicu74 \
  && rm -rf /var/lib/apt/lists/* \
  && useradd --uid 1000 --user-group --home-dir /data --no-create-home dark

COPY --chmod=0755 dark /usr/local/bin/dark
COPY --chmod=0755 entrypoint.sh /usr/local/bin/dark-sync-entrypoint

ENV HOME=/data
VOLUME /data
EXPOSE 8080
STOPSIGNAL SIGTERM

HEALTHCHECK --interval=30s --timeout=5s --start-period=120s \
  CMD curl -sf http://127.0.0.1:8080/health || exit 1

ENTRYPOINT ["/usr/local/bin/dark-sync-entrypoint"]
