# Dockerfile for the editor-facing backend

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark backend/src/ApiServer/Templates templates
COPY --chown=dark:dark scripts scripts

RUN mkdir -p webroot/static
COPY --chown=dark:dark backend/static/etags.json webroot/static/etags.json

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark backend/Build/out/ApiServer/Release/net6.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/ApiServer

CMD ./app/ApiServer
