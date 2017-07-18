# Development dockerfile for Dark

# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:16.10

# TODO: version all ubuntu packages
RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install -y software-properties-common \
                       python3.6 make m4 rsync git curl \
                       ocaml opam \
                       nodejs-legacy npm \
                       libpq-dev \
                       wget sudo

# FSWatch for the build script
RUN wget https://github.com/emcrisostomo/fswatch/releases/download/1.9.3/fswatch-1.9.3.tar.gz
RUN tar zxvf fswatch-1.9.3.tar.gz
RUN cd fswatch-1.9.3 && ./configure && make
RUN cd fswatch-1.9.3 && make install && ldconfig


# dont run as root
RUN adduser --disabled-password --gecos '' dark
USER dark
WORKDIR /home/dark

# Elm
RUN npm install elm@0.18.0
ENV PATH "$PATH:/home/dark/node_modules/.bin"

# Ocaml
RUN opam init --auto-setup
RUN opam switch 4.04.2
ENV PATH "/home/dark/.opam/4.04.2/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/4.04.2/lib/stublibs"
ENV MANPATH "/home/dark/.opam/4.04.2/man:"
ENV PERL5LIB "/home/dark/.opam/4.04.2/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/4.04.2/lib/toplevel"
RUN opam update
RUN opam install core.v0.9.1 \
                 oasis.0.4.10 \
                 cohttp.0.22.0 \
                 lwt.2.7.1 \
                 yojson.1.3.3 \
                 postgresql.4.0.1

USER root
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && locale-gen
USER dark
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

CMD ["app", "scripts", "builder"]
