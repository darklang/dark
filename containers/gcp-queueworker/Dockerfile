# Dockerfile for the queueworker (queues)

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/check-linked-libs bin/queue_worker.exe

CMD "./gcp-run-queueworker"
