FROM dark-base-service:latest

COPY --chown=dark . ./

RUN ./check-linked-libs bin/dark-stroller

CMD "bin/dark-stroller"
