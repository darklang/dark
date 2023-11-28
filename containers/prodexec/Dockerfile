# Dockerfile for the the prodexec container

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark:dark scripts scripts
COPY --chown=dark:dark backend/migrations migrations

RUN mkdir app

# Setting this now means we can set the filesystem to readonly
ENV DARK_CONFIG_RUNDIR=/home/dark/gcp-rundir
RUN ./scripts/devcontainer/_create-app-directories

# COPY --chown=dark:dark backend/Build/out/ProdExec/Release/net8.0/linux-x64/publish/ProdExec app/
# RUN ./scripts/linting/_check-linked-libs app/ProdExec

# Add chisel. This allows us to tunnel SSH over HTTP
COPY --from=jpillora/chisel /app/chisel /usr/bin

RUN apt update && apt install openssh-server -y

COPY --chmod=755 --chown=dark:dark containers/prodexec/sleep.sh .

CMD "./run.sh"
