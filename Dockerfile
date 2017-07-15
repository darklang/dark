# Development dockerfile for Dark

# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:16.04

RUN apt-get update
RUN apt-get upgrade -y
# TODO: version all ubuntu packages
RUN apt-get install -y python3 make m4 rsync git curl ocaml opam nodejs-legacy npm libpq-dev

# dont run as root
RUN adduser --disabled-password --gecos '' dark
USER dark
WORKDIR /home/dark

RUN npm install elm@0.17.0

RUN opam init --auto-setup
RUN opam switch 4.04.2
RUN opam update
RUN opam install oasis.0.4.10 cohttp.0.22.0 lwt.3.0.0 core.0.9.1 yojson.1.3.3 ppx_jane.0.9.0 postgresql.4.0.1


CMD ["scripts", "build"]
