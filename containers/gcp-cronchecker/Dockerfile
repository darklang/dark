# Dockerfile for the main container of the cron-checker service

FROM dark-ocaml-service:latest

COPY --chown=dark . ./

RUN ./scripts/linting/check-linked-libs bin/cron_checker.exe

CMD "./gcp-run-cronchecker"
