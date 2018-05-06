# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:17.10

ENV FORCE_BUILD 0

############################
## apt-get
############################
USER root
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
RUN curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN curl -sSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN curl -sSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
RUN curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -

# RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ zesty-pgdg main" > /etc/apt/sources.list.d/pgdg.list
RUN echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list
RUN echo "deb https://deb.nodesource.com/node_9.x zesty main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_9.x zesty main" >> /etc/apt/sources.list.d/nodesource.list
RUN export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -cs)" && \
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list
RUN echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

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
# - most libs re for ocaml
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
      libev-dev=1:4.22-1 \
      libgmp-dev=2:6.1.2+dfsg-1 \
      pkg-config=0.29.1-0ubuntu2 \
      libcurl4-gnutls-dev=7.55.1-1ubuntu2.4 \
      python-software-properties=0.96.24.17 \
      libpq-dev=9.6.8-0ubuntu0.17.10 \
      postgresql-9.6=9.6.8-0ubuntu0.17.10 \
      postgresql-client-9.6=9.6.8-0ubuntu0.17.10 \
      chromium-browser \
      firefox \
      gnupg \
      nodejs \
      google-chrome-stable \
      dnsmasq \
      cron \
      google-cloud-sdk \
      jq \
      vim \
      dnsutils \
      docker-ce \
      unzip \
      build-essential \
      && apt-get clean \
      && rm -rf /var/lib/apt/lists/*

############################
# Dark user
############################
USER root
RUN adduser --disabled-password --gecos '' dark
RUN echo "dark:dark" | chpasswd && adduser dark sudo
RUN chown -R dark:dark /home/dark
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER dark
WORKDIR /home/dark
RUN mkdir bin

############################
# Locales
############################
USER dark
RUN sudo locale-gen "en_US.UTF-8"
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

############################
# Elm
############################
USER root
RUN npm install -g yarn
USER dark
RUN yarn add \
  elm@0.18.0 \
  elm-test@0.18.12 \
  elm-oracle@1.1.1 \
  elm-live@2.7.5 \
  less@2.7.3 \
  testcafe@0.19.0

ENV PATH "$PATH:/home/dark/node_modules/.bin"

# Speed up elm compiles
RUN git clone https://github.com/obmarg/libsysconfcpus.git;
RUN cd libsysconfcpus \
       && ./configure \
       && make \
       && sudo make install


############################
# Postgres
############################
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER dark WITH SUPERUSER PASSWORD 'eapnsdc';" && \
    createdb -O dark proddb

# Adjust PostgreSQL configuration so that remote connections to the
# database are possible.
RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.6/main/pg_hba.conf

# RUN echo "listen_addresses='*'" >> /etc/postgresql/10/main/postgresql.conf

user dark
# Add VOLUMEs to allow backup of config, logs and databases
VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

# No idea what caused this, but we get permission problems otherwise.
RUN sudo chown postgres:postgres -R /etc/postgresql
RUN sudo chown postgres:postgres -R /var/log/postgresql
RUN sudo chown postgres:postgres -R /var/lib/postgresql


############################
# dns for integration tests
############################
USER root
RUN echo "address=/localhost/127.0.0.1" > /etc/dnsmasq.d/dnsmasq-localhost.conf

############################
# Google cloud
############################
# New authentication for docker - not supported via apt-get
user root
RUN curl -sSL "https://github.com/GoogleCloudPlatform/docker-credential-gcr/releases/download/v1.4.3/docker-credential-gcr_linux_amd64-1.4.3.tar.gz" \
    | tar xz --to-stdout docker-credential-gcr > /usr/bin/docker-credential-gcr \
    && chmod +x /usr/bin/docker-credential-gcr

RUN docker-credential-gcr config --token-source="gcloud"



############################
# Ocaml
############################
USER dark
RUN curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh | bash
ENV OPAMJOBS 4
RUN opam init --comp 4.06.0 --auto-setup
ENV PATH "/home/dark/.opam/4.06.0/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/4.06.0/lib/stublibs"
ENV MANPATH "/home/dark/.opam/4.06.0/man:"
ENV PERL5LIB "/home/dark/.opam/4.06.0/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/4.06.0/lib/toplevel"
ENV FORCE_OCAML_BUILD 3
RUN opam update

#ENV OPAMDEBUG true
# This set of package number is very delicate.
# https://github.com/mirleft/ocaml-nocrypto/issues/143
RUN opam install -y \
  ppx_deriving.4.2.1 \
  core.v0.10.0  \
  core_extended.v0.10.0  \
  jbuilder.1.0+beta20 \
  re2.v0.10.1 \
  conf-libev \
  lwt.3.3.0 \
  yojson.1.4.0 \
  postgresql.4.0.1 \
  ppx_deriving_yojson.3.1 \
  cohttp-lwt-unix.1.0.0 \
  ocurl.0.8.0 \
  alcotest.0.8.1 \
  merlin.3.0.5 \
  utop.2.0.2 \
  ocp-indent.1.6.1 \
  batteries.2.8.0 \
  landmarks.1.1 \
  cstruct.3.2.0 \
  nocrypto.0.5.4 \
  session.0.4.0 \
  session-cohttp.0.4.0 \
  session-cohttp-lwt.0.4.0 \
  session-postgresql.0.4.0 \
  session-postgresql-lwt.0.4.0 \
  junit_alcotest.2.0 \
  junit.2.0

# RUN opam install tls.0.8.0 # breaks build, hence specific packages below

############################
# Environment
############################
USER dark
ENV TERM=xterm-256color

######################
# Quick hacks here, to avoid massive recompiles
######################



############################
# Finish
############################
user dark
CMD ["app", "scripts", "builder"]
