# Dockerfile for the grand-user facing Dark app at builtwithdark.com

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark backend/templates templates
COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark containers/gcp-fsharp-bwdserver/gcp-run-fsharp-bwdserver .

RUN mkdir app

COPY --chown=dark:dark fsharp-backend/Build/out/BwdServer/Release/net6.0/linux-x64/publish/* app/

RUN ./scripts/linting/_check-linked-libs app/BwdServer

CMD "./gcp-run-fsharp-bwdserver"
