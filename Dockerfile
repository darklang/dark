# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:17.10

ENV FORCE_BUILD 0

## apt-get

# We pin the exact package version so that we don't have any surprises.
# However, sometimes the versions upgrade from under us and break the
# build. To fix that, you need the actual package version, which you can
# find by installing it directly:
# $ docker run <HASH> apt-get install mypackage
# Notes
# - replace <HASH> with a recent hash from the docker build output.
# - just use the package name, not the version.

# Just enough to add keys and sources for npm and chrome

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get install \
      -y \
      --no-install-recommends \
      curl \
      apt-transport-https \
      ca-certificates \
      lsb-core

# Latest NPM (taken from  https://deb.nodesource.com/setup_8.x )
RUN curl -sSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN curl -sSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -

RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ zesty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list
RUN echo "deb https://deb.nodesource.com/node_9.x zesty main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_9.x zesty main" >> /etc/apt/sources.list.d/nodesource.list
RUN export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -c -s)" && \
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" | tee -a /etc/apt/sources.list.d/google-cloud-sdk.list

# Deps:
# - apt-transport-https for npm
# - expect for unbuffer
# everything after "ocaml" for ocaml
RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get install \
      --no-install-recommends \
      -y \
      software-properties-common=0.96.24.17 \
      python3.6=3.6.3-1ubuntu1 \
      make=4.1-9.1 \
      m4=1.4.18-1 \
      rsync=3.1.2-2ubuntu0.2 \
      git=1:2.14.1-1ubuntu4 \
      wget=1.19.1-3ubuntu1.1 \
      sudo=1.8.20p2-1ubuntu1 \
      locales=2.26-0ubuntu2.1 \
      expect=5.45-8 \
      tcl8.6 \
      ocaml=4.04.0-2ubuntu4 \
      opam=1.2.2-6 \
      libev-dev=1:4.22-1 \
      libgmp-dev=2:6.1.2+dfsg-1 \
      pkg-config=0.29.1-0ubuntu2 \
      libcurl4-gnutls-dev=7.55.1-1ubuntu2.4 \
      python-software-properties=0.96.24.17 \
      libpq-dev=10.1-1.pgdg17.04+1 \
      postgresql-10=10.1-1.pgdg17.04+1 \
      postgresql-client-10=10.1-1.pgdg17.04+1 \
      chromium-browser \
      firefox \
      gnupg \
      nodejs \
      google-chrome-stable \
      dnsmasq \
      cron \
      google-cloud-sdk \
      jq \
      aspcud \
      && rm -rf /var/lib/apt/lists/*
      # todo apt-clean

# dont run as root (allow sudo)
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
USER root
RUN npm install -g yarn
USER dark
RUN yarn add \
  elm@0.18.0 \
  elm-test@0.18.12 \
  elm-oracle@1.1.1 \
  elm-live@2.7.5 \
  less@2.7.3 \
  testcafe@0.18.6
ENV PATH "$PATH:/home/dark/node_modules/.bin"

# Speed up elm compiles
RUN git clone https://github.com/obmarg/libsysconfcpus.git;
RUN cd libsysconfcpus \
       && ./configure \
       && make \
       && sudo make install


# Postgres
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER dark WITH SUPERUSER PASSWORD 'eapnsdc';" && \
    createdb -O dark proddb

# Adjust PostgreSQL configuration so that remote connections to the
# database are possible.
#RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.3/main/pg_hba.conf

RUN echo "listen_addresses='*'" >> /etc/postgresql/10/main/postgresql.conf

user dark
# Add VOLUMEs to allow backup of config, logs and databases
VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

# No idea what caused this, but we get permission problems otherwise.
RUN sudo chown postgres:postgres -R /etc/postgresql
RUN sudo chown postgres:postgres -R /var/log/postgresql
RUN sudo chown postgres:postgres -R /var/lib/postgresql


# dns for integration tests
USER root
RUN echo "address=/localhost/127.0.0.1" > /etc/dnsmasq.d/dnsmasq-localhost.conf

# Ocaml
USER dark
ENV OPAMJOBS 4
RUN opam init --auto-setup
RUN opam switch 4.06.0
ENV PATH "/home/dark/.opam/4.06.0/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/4.06.0/lib/stublibs"
ENV MANPATH "/home/dark/.opam/4.06.0/man:"
ENV PERL5LIB "/home/dark/.opam/4.06.0/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/4.06.0/lib/toplevel"
ENV FORCE_OCAML_BUILD 2
RUN opam update

#ENV OPAMDEBUG true
RUN opam install ppx_deriving.4.2.1
RUN opam install core.v0.11.0
RUN opam install conf-libev lwt.3.1.0
RUN opam install yojson.1.4.0
RUN opam install postgresql.4.0.1
RUN opam install ppx_deriving_yojson.3.1
RUN opam install cohttp-lwt-unix.1.0.0
RUN opam install ocurl.0.8.0
RUN opam install alcotest.0.8.1 # test
RUN opam install merlin.3.0.5 # dev
RUN opam install utop.2.0.2 # dev
RUN opam install ocp-indent.1.6.1 # dev
RUN opam install batteries.2.8.0
RUN opam install landmarks.1.1
# RUN opam install tls.0.8.0 # breaks build, hence specific packages below
RUN opam install cstruct.3.2.0
RUN opam install nocrypto.0.5.4
RUN opam install re2.v0.10.0
RUN opam install core_extended.v0.11.0

# Environment
ENV TERM=xterm-256color

######################
# Quick hacks below this line, to avoid massive recompiles

USER root
RUN curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
RUN sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
RUN apt-get update && apt-get install -y docker-ce

RUN apt-get install -y vim dnsutils

USER dark
RUN yarn add testcafe@0.19.0

RUN opam install session.0.4.0
RUN opam install session-cohttp.0.4.0
RUN opam install session-cohttp-lwt.0.4.0
RUN opam install session-postgresql.0.4.0
RUN opam install session-postgresql-lwt.0.4.0
RUN opam update
RUN opam install lwt.3.3.0

user dark
CMD ["app", "scripts", "builder"]
