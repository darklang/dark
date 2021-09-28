# Dockerfile for the main Dark app

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs app/ApiServer

CMD "./gcp-run-fsharp-apiserver"
