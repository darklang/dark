# Dockerfile for the grand-user facing Dark app at darklang.io

FROM darkfsharpservice:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts

# Add favicon
RUN mkdir -p webroot/static
COPY --chown=dark:dark backend/static/favicon-32x32.png webroot/static/favicon-32x32.png

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark backend/Build/out/BwdServer/Release/net8.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/BwdServer

CMD ./app/BwdServer