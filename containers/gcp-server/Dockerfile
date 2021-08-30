# Dockerfile for the main Dark app

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs bin/server.exe
RUN ./scripts/linting/_check-linked-libs bin/emergency_login_script.exe

CMD "./gcp-run-server"
