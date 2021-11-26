# Dockerfile for the the execHost container

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs app/ExecHost

CMD "./sleep.sh"
