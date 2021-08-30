# Dockerfile for the main container of the queue-scheduler service

FROM dark-base-service:latest

COPY --chown=dark . ./

RUN ./_check-linked-libs bin/dark-queue-scheduler

CMD "bin/dark-queue-scheduler"
