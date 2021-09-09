# Dockerfile for the main Dark app

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs bin/legacy_serialization_server.exe

CMD "./gcp-run-legacyserver"
