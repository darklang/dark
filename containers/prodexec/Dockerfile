# Dockerfile for the the prodexec container

FROM darkfsharpservice:latest

WORKDIR /home/dark

RUN sudo apt update && sudo apt install openssh-server -y \
      --no-install-recommends \
    && sudo rm -rf /var/lib/apt/lists/*

COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark backend/migrations migrations

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

COPY --chown=dark:dark ./backend/Build/out/ProdExec/Release/net8.0/linux-x64/publish/* app/
RUN ./scripts/linting/_check-linked-libs app/ProdExec

# Add chisel. This allows us to tunnel SSH over HTTP
COPY --from=jpillora/chisel /app/bin /usr/bin/chisel

COPY --chmod=755 --chown=dark:dark containers/prodexec/run.sh .

CMD "./run.sh"
