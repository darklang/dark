# Dockerfile for the processing the event queue

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark backend/Build/out/QueueWorker/Release/net6.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/QueueWorker

CMD ./app/QueueWorker
