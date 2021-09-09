# Dockerfile for the main Dark app

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs bin/legacyserver.exe

CMD "./gcp-run-legacyserver"
