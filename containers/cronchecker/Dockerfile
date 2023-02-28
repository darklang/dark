# Dockerfile for the service checking crons and loading them into the event queue

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark containers/gcp-fsharp-cronchecker/gcp-run-fsharp-cronchecker .

RUN mkdir app

COPY --chown=dark:dark backend/Build/out/CronChecker/Release/net7.0/linux-x64/publish/* app/

RUN ./scripts/linting/_check-linked-libs app/CronChecker

CMD "./gcp-run-fsharp-cronchecker"
