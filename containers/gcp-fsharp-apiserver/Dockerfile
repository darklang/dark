# Dockerfile for the main Dark app

FROM dark-fsharp-service:latest

WORKDIR /home/dark

COPY --chown=dark . ./

RUN ls -laR
RUN pwd



RUN ./scripts/linting/_check-linked-libs app/ApiServer
# FSTODO
#RUN ./scripts/linting/_check-linked-libs bin/emergency_login_script.exe

CMD "./gcp-run-fsharp-apiserver"
