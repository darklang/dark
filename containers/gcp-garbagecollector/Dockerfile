# Dockerfile for the garbage collector image

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/support/check-linked-libs bin/garbage_collector_worker.exe

CMD "./gcp-run-garbagecollector"
