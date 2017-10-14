# Development dockerfile for Dark

# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:17.04

## apt-get

# We pin the exact package version so that we don't have any surprises.
# However, sometimes the versions upgrade from under us and break the
# build. To fix that, you need the actual package version, which you can
# find by installing it directly:
# $ docker run <HASH> apt-get install mypackage
# Notes
# - replace <HASH> with a recent hash from the docker build output.
# - just use the package name, not the version.

# Deps:
# - apt-transport-https for npm
# - expect for unbuffer
# - libpcre3-dev for better-errors
# everything after "ocaml" for ocaml
RUN apt-get update && \
    apt-get install -y software-properties-common=0.96.24.13 \
                       python3.6=3.6.1-1ubuntu0~17.04.0 \
                       make=4.1-9.1 \
                       m4=1.4.18-1 \
                       rsync=3.1.2-1 \
                       git=1:2.11.0-2ubuntu0.3 \
                       curl=7.52.1-4ubuntu1.2 \
                       wget=1.18-2ubuntu1 \
                       sudo=1.8.19p1-1ubuntu1.1 \
                       locales=2.24-9ubuntu2.2 \
                       apt-transport-https=1.4 \
                       expect=5.45-7 \
                       ocaml=4.02.3-6ubuntu2 \
                       opam=1.2.2-5build5 \
                       libpq-dev=9.6.5-0ubuntu0.17.04 \
                        libev-dev=1:4.22-1 \
                       libgmp-dev=2:6.1.2+dfsg-1 \
                       pkg-config=0.29.1-0ubuntu1 \
                       libcurl4-gnutls-dev=7.52.1-4ubuntu1.2 \
                       libpcre3-dev=2:8.39-3 \
                       && rm -rf /var/lib/apt/lists/*

# Latest NPM (taken from  https://deb.nodesource.com/setup_8.x )
RUN echo 'deb https://deb.nodesource.com/node_8.x zesty main' > /etc/apt/sources.list.d/nodesource.list
RUN echo 'deb-src https://deb.nodesource.com/node_8.x zesty main' >> /etc/apt/sources.list.d/nodesource.list
RUN curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN apt-get update
RUN apt-get install -y nodejs # this changes a lot so let’s not pin it
RUN npm install -g yarn

# dont run as root
RUN adduser --disabled-password --gecos '' dark
RUN echo "dark:dark" | chpasswd && adduser dark sudo
RUN chown -R dark:dark /home/dark
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER dark
WORKDIR /home/dark
RUN mkdir bin

# Locales
RUN sudo locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en

# Elm
RUN yarn add elm@0.18.0
RUN yarn add elm-test@0.18.8 # test
RUN yarn add elm-oracle@1.1.1 # dev
ENV PATH "$PATH:/home/dark/node_modules/.bin"

# Ocaml
RUN opam init --auto-setup
RUN opam switch 4.04.2
ENV PATH "/home/dark/.opam/4.04.2/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/4.04.2/lib/stublibs"
ENV MANPATH "/home/dark/.opam/4.04.2/man:"
ENV PERL5LIB "/home/dark/.opam/4.04.2/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/4.04.2/lib/toplevel"
ENV FORCE_BUILD 1
RUN opam update

RUN opam install ppx_deriving.4.1
RUN opam install core.v0.9.1
RUN opam install conf-libev lwt.3.1.0
RUN opam install yojson.1.3.3
RUN opam install postgresql.4.0.1
RUN opam install ppx_deriving_yojson.3.0
RUN opam install tls.0.8.0
RUN opam install cohttp-lwt-unix.0.99.0
RUN opam install ocurl.0.7.10
RUN opam install alcotest.0.8.1 # test
RUN opam install merlin.3.0.2 # dev
RUN opam install utop.2.0.1 # dev
RUN opam install ocp-indent.1.6.1 # dev
RUN opam install batteries.2.7.0
RUN opam install landmarks.1.1

# better-errors
RUN opam install ansiterminal.0.7
RUN opam install pcre.7.3.0
# The npm installer might be unreliable. Keeping these here
# RUN git clone https://github.com/gasche/ocaml-better-errors
# RUN sed -i ocaml-better-errors/_oasis -e s/Batteries/batteries/g
# RUN sed -i ocaml-better-errors/_oasis -e s/Pcre/pcre/g
# RUN cd ocaml-better-errors && oasis setup -setup-update dynamic && make
# RUN mv ocaml-better-errors/main.byte bin/better-errors
RUN yarn add ocamlBetterErrors
RUN cp node_modules/ocamlBetterErrors/berror.native ~/bin/better-errors
RUN chmod +x ~/bin/better-errors

# Environment
ENV TERM=xterm-256color

## ADD NEW PACKAGES HERE
# Doing otherwise will force a large recompile of the container for
# everyone


CMD ["app", "scripts", "builder"]
