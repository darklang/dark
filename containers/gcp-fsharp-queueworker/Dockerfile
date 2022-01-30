# Dockerfile for the service checking crons and loading them into the event queue

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark containers/gcp-fsharp-queueworker/gcp-run-fsharp-queueworker .

RUN mkdir app

COPY --chown=dark:dark fsharp-backend/Build/out/QueueWorker/Release/net6.0/linux-x64/publish/* app/

RUN ./scripts/linting/_check-linked-libs app/QueueWorker

CMD "./gcp-run-fsharp-queueworker"
