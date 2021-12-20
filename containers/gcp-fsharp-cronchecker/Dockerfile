# Dockerfile for the service checking crons and loading them into the event queue

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs app/CronChecker

CMD "./gcp-run-fsharp-cronchecker"
