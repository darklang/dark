# Dockerfile for the the prodexec container

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark backend/migrations migrations

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark backend/Build/out/ProdExec/Release/net7.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/ProdExec

COPY --chown=dark:dark containers/gcp-prodexec/sleep.sh .
RUN chmod +x ./sleep.sh

CMD "./sleep.sh"
