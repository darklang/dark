# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

FROM ubuntu:17.10

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
      software-properties-common=0.96.24.17 \
      python3.6=3.6.3-1ubuntu1 \
      make=4.1-9.1 \
      m4=1.4.18-1 \
      rsync=3.1.2-2ubuntu0.2 \
      git=1:2.14.1-1ubuntu4 \
      wget=1.19.1-3ubuntu1.2 \
      sudo=1.8.20p2-1ubuntu1 \
      locales=2.26-0ubuntu2.1 \
      expect=5.45-8 \
      tcl8.6 \
      libev-dev=1:4.22-1 \
      libgmp-dev=2:6.1.2+dfsg-1 \
      pkg-config=0.29.1-0ubuntu2 \
      libcurl4-gnutls-dev \
      python-software-properties=0.96.24.17 \
      libpq-dev=9.6.9-0ubuntu0.17.10 \
      postgresql-9.6=9.6.9-0ubuntu0.17.10 \
      postgresql-client-9.6=9.6.9-0ubuntu0.17.10 \
      postgresql-contrib-9.6=9.6.9-0ubuntu0.17.10 \
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
      libsodium-dev \
      gcc \
      python-dev \
      python-setuptools \
      pgcli \
      xvfb \
      ffmpeg \
      tmux \
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
# apt install is done above
# RUN sudo apt update && sudo apt install -y gcc python-dev python-setuptools
RUN easy_install -U pip
RUN pip uninstall crcmod
RUN pip install -U crcmod


############################
# Ocaml
############################
USER dark
ENV FORCE_OCAML_BUILD 5
RUN curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh | bash
ENV OPAMJOBS 4
# disabling sandboxing as it breaks and isn't necessary cause Docker
RUN opam init --comp 4.07.0 --auto-setup --disable-sandboxing
ENV PATH "/home/dark/.opam/4.07.0/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/4.07.0/lib/stublibs"
ENV MANPATH "/home/dark/.opam/4.07.0/man:"
ENV PERL5LIB "/home/dark/.opam/4.07.0/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/4.07.0/lib/toplevel"
ENV FORCE_OCAML_UPDATE 0
RUN opam update

#ENV OPAMDEBUG true
RUN opam install -y \
  ppx_deriving.4.2.1 \
  core.v0.11.2  \
  core_extended.v0.11.0 \
  dune.1.6.2 \
  re2.v0.11.0 \
  conf-libev.4-11 \
  lwt.3.3.0 \
  yojson.1.4.1 \
  postgresql.4.4.0 \
  ppx_deriving_yojson.3.1 \
  cohttp-lwt-unix.1.0.2 \
  ocurl.0.8.2 \
  alcotest.0.8.3 \
  merlin.3.1.0 \
  ocp-indent.1.6.1 \
  landmarks.1.1 \
  cstruct.3.2.1 \
  nocrypto.0.5.4-1 \
  uuidm.0.9.6 \
  session.0.4.0 \
  session-cohttp.0.4.0 \
  session-cohttp-lwt.0.4.0 \
  session-postgresql.0.4.0 \
  session-postgresql-lwt.0.4.0 \
  junit_alcotest.2.0 \
  junit.2.0 \
  js_of_ocaml.3.2.0 \
  js_of_ocaml-ppx.3.2.0 \
  js_of_ocaml-lwt.3.2.0 \
  sodium.0.6.0 \
  utop \
  ocamlformat

# To use PPXes in bucklescript, we need to install them from opam
RUN opam switch create 4.02.3
RUN eval $(opam env) \
  && opam install -y \
    ppx_deriving_yojson \
    ppx_deriving
RUN opam switch 4.07.0

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


############################
# Incredibly -- and reproducibly -- if we move this to the top the
# execute_function_works integration test fails
############################
RUN DEBIAN_FRONTEND=noninteractive \
    sudo apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    sudo apt-get install \
      --no-install-recommends \
      -y \
      less


########################
# Install Rust toolchain
########################

USER root

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    RUST_VERSION=1.30.1

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

############################
# Environment
############################
USER dark
ENV TERM=xterm-256color

######################
# Quick hacks here, to avoid massive recompiles
######################

RUN echo "address=/localhost/127.0.0.1" | sudo tee -a /etc/dnsmasq.d/dnsmasq-integration-tests.conf
RUN opam install -y \
  uuseg.11.0.0 \
  uunf.11.0.0 \
  multipart-form-data.0.1.0 \
  magic-mime.1.1.1

# for ocaml-gcloud
RUN sudo apt install -y zlib1g-dev libssl-dev \
  && opam pin nocrypto -y git+https://github.com/gasche/ocaml-nocrypto.git#master-ocamlbuild-pack \
  && opam pin -y jwt git+https://github.com/ismith/ocaml-jwt.git#rsa256-verification \
  && opam pin -y gcloud git+https://github.com/ismith/ocaml-gcloud.git#builds-on-ocaml-4.07.0


# required by Rust hyper-tls crate
USER root
RUN apt install \
      -y \
      --no-install-recommends \
      libssl-dev=1.0.2g-1ubuntu13.6

# testcafe needs node 11
USER root
RUN echo "deb https://deb.nodesource.com/node_11.x bionic main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_11.x bionic main" >> /etc/apt/sources.list.d/nodesource.list
RUN sudo apt update && sudo apt install --reinstall -y nodejs
USER dark


############################
# Finish
############################
user dark
CMD ["app", "scripts", "builder"]
