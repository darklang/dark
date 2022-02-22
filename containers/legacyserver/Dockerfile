# Dockerfile for the main Dark app

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN sudo mkdir -p /home/dark/gcp-rundir

RUN ./scripts/linting/_check-linked-libs bin/legacy_serialization_server.exe

CMD "./gcp-run-legacyserver"
