# Dockerfile for the queueworker (queues)

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/_check-linked-libs bin/queue_worker.exe

CMD "./gcp-run-queueworker"
