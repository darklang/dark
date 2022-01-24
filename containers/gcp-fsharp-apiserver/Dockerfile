# Dockerfile for the editor-facing backend

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark backend/templates templates
COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark containers/gcp-fsharp-apiserver/gcp-run-fsharp-apiserver .

RUN mkdir app

COPY --chown=dark:dark fsharp-backend/Build/out/ApiServer/Release/net6.0/linux-x64/publish/* app/

RUN ./scripts/linting/_check-linked-libs app/ApiServer

CMD "./gcp-run-fsharp-apiserver"
