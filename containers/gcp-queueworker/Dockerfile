# Dockerfile for the queueworker (queues)

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/support/check-linked-libs bin/queue_worker.exe

CMD "./gcp-run-queueworker"