# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:18.04

ENV FORCE_BUILD 1

############################
## apt
############################
USER root
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      -y \
      --no-install-recommends \
      curl \
      apt-transport-https \
      ca-certificates \
      lsb-core \
      less \
      gpg \
      gpg-agent

# Latest NPM (taken from  https://deb.nodesource.com/setup_8.x )
RUN curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN curl -sSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN curl -sSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
RUN curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -

# We want postgres 9.6, but it is not in ubuntu 18.04
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list

RUN echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list

# Testcafe needs node >= 11
RUN echo "deb https://deb.nodesource.com/node_11.x bionic main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_11.x bionic main" >> /etc/apt/sources.list.d/nodesource.list

RUN export CLOUD_SDK_REPO="cloud-sdk-$(lsb_release -cs)" && \
    echo "deb http://packages.cloud.google.com/apt $CLOUD_SDK_REPO main" > /etc/apt/sources.list.d/google-cloud-sdk.list
RUN echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

# We pin the exact package version so that we don't have any surprises.
# However, sometimes the versions upgrade from under us and break the
# build. To fix that, you need the actual package version, which you can
# find by installing it directly:
# $ docker run <HASH> apt install mypackage
# Notes
# - replace <HASH> with a recent hash from the docker build output.
# - just use the package name, not the version.

# Deps:
# - apt-transport-https for npm
# - expect for unbuffer
# - most libs re for ocaml
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      software-properties-common \
      python3.6 \
      make \
      m4 \
      rsync \
      git \
      wget \
      sudo \
      locales \
      expect \
      tcl8.6 \
      libev-dev \
      libgmp-dev \
      pkg-config \
      libcurl4-gnutls-dev \
      libpq-dev \
      postgresql-9.6 \
      postgresql-client-9.6 \
      postgresql-contrib-9.6 \
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
      ruby \
      kubectl \
      python3-pip \
      python-pip \
      libsodium-dev \
      gcc \
      python-dev \
      python-setuptools \
      pgcli \
      xvfb \
      ffmpeg \
      tmux \
      libssl-dev \
      libssl-ocaml-dev \
      zlib1g-dev \
      pv \
      && apt clean \
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
# otherwise this gets created by the mount, and it'll be owned by root if your docker host os is linux
RUN mkdir .config

############################
# Locales
############################
USER dark
RUN sudo locale-gen "en_US.UTF-8"
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

############################
# Frontend
############################
USER root
# node 11.11 introduced a bug that cased all Jest tests to fail.
# It is fixed in 11.12, so for now we force our containers to update to 11.12
#
# And then nodesource took down 11.13, all that's left is 11.14: https://deb.nodesource.com/node_11.x/dists/bionic/main/binary-amd64/Packages
# And then 11.14
RUN sudo apt update && sudo apt install nodejs=11.15.0-1nodesource1

RUN npm install -g yarn@1.12.3

ENV PATH "$PATH:/home/dark/node_modules/.bin"

############################
# Postgres
############################
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER dark WITH SUPERUSER PASSWORD 'eapnsdc';" && \
    createdb -O dark devdb

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
# Google cloud
############################
# New authentication for docker - not supported via apt
user root
RUN curl -sSL "https://github.com/GoogleCloudPlatform/docker-credential-gcr/releases/download/v1.4.3/docker-credential-gcr_linux_amd64-1.4.3.tar.gz" \
    | tar xz --to-stdout docker-credential-gcr > /usr/bin/docker-credential-gcr \
    && chmod +x /usr/bin/docker-credential-gcr

RUN docker-credential-gcr config --token-source="gcloud"

# crcmod for gsutil
RUN pip install -U crcmod


############################
# Ocaml
############################
USER dark
ENV FORCE_OCAML_BUILD 5
RUN curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh | bash
ENV OPAMJOBS 4
# disabling sandboxing as it breaks and isn't necessary cause Docker

ENV OCAML_SWITCH ocaml-base-compiler.4.06.1
# env vars below here replace `eval $(opam env)` in dotfiles; by doing it here,
# we avoid having to source .bashrc before every command
RUN opam init --comp ${OCAML_SWITCH} --auto-setup --disable-sandboxing
ENV PATH "/home/dark/.opam/${OCAML_SWITCH}/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/${OCAML_SWITCH}/lib/stublibs"
ENV MANPATH "/home/dark/.opam/${OCAML_SWITCH}/man:"
ENV PERL5LIB "/home/dark/.opam/${OCAML_SWITCH}/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/${OCAML_SWITCH}/lib/toplevel"
ENV FORCE_OCAML_UPDATE 4
RUN opam update

#ENV OPAMDEBUG true
RUN opam pin -y nocrypto git+https://github.com/gasche/ocaml-nocrypto.git#master-ocamlbuild-pack \
  && opam pin -y jwt git+https://github.com/ismith/ocaml-jwt.git#rsa256-verification \
  && opam pin -y gcloud git+https://github.com/ismith/ocaml-gcloud.git#builds-on-ocaml-4.07.0 \
  && opam pin -y multipart-form-data git+https://github.com/darklang/multipart-form-data.git#master \
  && opam install -y \
  ppx_deriving.4.3 \
  core.v0.11.2  \
  core_extended.v0.11.0 \
  dune.1.10.0 \
  re2.v0.11.0 \
  conf-libev.4-11 \
  lwt.4.2.1 \
  lwt_ppx.1.2.2 \
  yojson.1.7.0 \
  postgresql.4.4.0 \
  ppx_deriving_yojson.3.4 \
  cohttp-lwt-unix.2.0.0 \
  ocurl.0.8.2 \
  alcotest.0.8.3 \
  merlin.3.2.2 \
  ocp-indent.1.6.1 \
  landmarks.1.1 \
  cstruct.3.1.1 \
  cstruct-lwt.3.1.1 \
  lwt_ssl.1.1.2 \
  ssl.0.5.5 \
  uuidm.0.9.6 \
  junit_alcotest.2.0 \
  junit.2.0 \
  js_of_ocaml.3.2.0 \
  js_of_ocaml-ppx.3.2.0 \
  js_of_ocaml-lwt.3.2.0 \
  sodium.0.6.0 \
  utop.2.4.0 \
  ocamlformat.0.8 \
  uuseg.11.0.0 \
  uunf.11.0.0 \
  magic-mime.1.1.1 \
  ezgzip.0.2.1 \
  tablecloth-native.0.0.6 \
  session.0.4.1 \
  session-cohttp.0.4.1 \
  session-cohttp-lwt.0.4.1 \
  session-postgresql.0.4.1 \
  session-postgresql-lwt.0.4.1

############################
# Shellcheck
# Ubuntu has a very old version
############################

RUN \
  VERSION=v0.6.0 \
  && FILENAME=shellcheck-$VERSION.linux.x86_64.tar.xz  \
  && wget -P tmp_install_folder/ https://shellcheck.storage.googleapis.com/$FILENAME \
  && tar xvf tmp_install_folder/$FILENAME -C tmp_install_folder \
  && sudo cp tmp_install_folder/shellcheck-$VERSION/shellcheck /usr/bin/shellcheck \
  && rm -Rf tmp_install_folder


########################
# Install Rust toolchain
########################

USER root

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    RUST_VERSION=1.34.1

RUN dpkgArch="$(dpkg --print-architecture)"; \
    case "${dpkgArch##*-}" in \
        amd64) rustArch='x86_64-unknown-linux-gnu'; rustupSha256='0077ff9c19f722e2be202698c037413099e1188c0c233c12a2297bf18e9ff6e7' ;; \
        armhf) rustArch='armv7-unknown-linux-gnueabihf'; rustupSha256='f139e5be4ea2db7ff151c122f5d24af3c587c4fc74a7414e262cb34403278ad3' ;; \
        arm64) rustArch='aarch64-unknown-linux-gnu'; rustupSha256='c7d5471e71a315134e7499af75eb177d1f574858f1c6b8e61b436702d671a4e2' ;; \
        i386) rustArch='i686-unknown-linux-gnu'; rustupSha256='909ce4e2d0c9bf60ba5a85426c38cceb5ae77979ab2b1e354e76b9851b5ec5ed' ;; \
        *) echo >&2 "unsupported architecture: ${dpkgArch}"; exit 1 ;; \
    esac; \
    url="https://static.rust-lang.org/rustup/archive/1.14.0/${rustArch}/rustup-init"; \
    wget "$url"; \
    echo "${rustupSha256} *rustup-init" | sha256sum -c -; \
    chmod +x rustup-init; \
    ./rustup-init -y --no-modify-path --default-toolchain $RUST_VERSION; \
    rm rustup-init; \
    chmod -R a+w $RUSTUP_HOME $CARGO_HOME; \
    rustup --version; \
    cargo --version; \
    rustc --version;

# install Rust dev tools
RUN rustup component add clippy-preview rustfmt-preview


########################
# DNS for integration tests
########################
RUN echo "address=/localhost/127.0.0.1" | sudo tee -a /etc/dnsmasq.d/dnsmasq-integration-tests.conf

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
