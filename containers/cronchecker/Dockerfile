# Dockerfile for the service checking crons and loading them into the event queue

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark backend/Build/out/CronChecker/Release/net8.0/linux-x64/publish/CronChecker app/

RUN ./scripts/linting/_check-linked-libs app/CronChecker

CMD ./app/CronChecker