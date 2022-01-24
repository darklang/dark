# Dockerfile for the the execHost container

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark backend/templates templates
COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark backend/migrations migrations

COPY --chown=dark:dark containers/gcp-exechost/sleep.sh .
RUN chmod +x ./sleep.sh

RUN mkdir app
COPY --chown=dark:dark fsharp-backend/Build/out/ExecHost/Release/net6.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/ExecHost

CMD "./sleep.sh"
