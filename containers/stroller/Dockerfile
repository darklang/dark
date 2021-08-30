FROM dark-base-service:latest

COPY --chown=dark . ./

RUN ./_check-linked-libs bin/dark-stroller

CMD "bin/dark-stroller"
