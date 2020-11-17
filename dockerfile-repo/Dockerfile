# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

# DOCKERFILE_REPO: VERY IMPORTANT: this dockerfile is stored in the
# darklang/dockerfile repo, and is copied into darklang/dark via a git subtree.
# The copy allows developers to develop Dark directly without pulling a docker
# image.
#
# You can make changes to this file in this repo, and but pushing directly via
# the subtree is broken, so you need to make the changes to darklang/dockerfile
# and then pull them using scripts/dockerfile-pull (or edit the commands within
# appropiately).
#
# The CircleCI workflow is a little complicated. To actually use any changes to
# the image, you need to change the sha used in config/circleci.yml. You can
# find the new sha after pushing to darklang/dockerfile - the sha is generated
# as part of that build. Search for DOCKERFILE_REPO for where to make that
# change.

FROM ubuntu:18.04@sha256:3235326357dfb65f1781dbc4df3b834546d8bf914e82cce58e6e6b676e23ce8f as dark-base

ENV FORCE_BUILD 1

# These are reasonable defaults, and what the dark uid/gid would be if we didn't
# specify values. By exposing them as build-args, we can set these values to
# match the host user's uid/gid - allowing for dark-owned files in-container to
# be owned by the host user on the host fs.
#
# We didn't need this in OS X because Docker for Mac handles it for you, and we
# avoided it on Linux for a while because often the first non-root user has uid
# 1000. But that's not always the case, and when it's not, you get files owned
# by 1000:1000 that need to be sudo chown'd on the host.
ARG uid=1000
ARG gid=1000

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
      gpg-agent \
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

# Latest NPM (taken from  https://deb.nodesource.com/setup_8.x )
RUN curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
RUN curl -sSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN curl -sSL https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
RUN curl -sSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
RUN curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
RUN curl -sSL https://nginx.org/keys/nginx_signing.key | apt-key add -


# We want postgres 9.6, but it is not in ubuntu 18.04
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list

RUN echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list

RUN echo "deb https://nginx.org/packages/ubuntu/ bionic nginx" > /etc/apt/sources.list.d/nginx.list

# Testcafe needs node >= 11
RUN echo "deb https://deb.nodesource.com/node_13.x bionic main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_13.x bionic main" >> /etc/apt/sources.list.d/nodesource.list

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
# - net-tools for netstat
# - esy packages need texinfo
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      software-properties-common \
      make \
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
      libcurl4-openssl-dev \
      libpq-dev \
      postgresql-9.6 \
      postgresql-client-9.6 \
      postgresql-contrib-9.6 \
      google-chrome-stable \
      nodejs \
      google-cloud-sdk \
      jq \
      vim \
      unzip \
      docker-ce \
      build-essential \
      kubectl \
      python3-pip \
      python3-setuptools \
      python3-dev \
      libsodium-dev \
      gcc \
      pgcli \
      ffmpeg \
      libssl-dev \
      libssl-ocaml-dev \
      zlib1g-dev \
      pv \
      net-tools \
      nginx=1.16.1-1~bionic \
      bash-completion \
      texinfo \
      openssh-server \
      jq \
      dnsutils \
      # .NET dependencies - https://github.com/dotnet/dotnet-docker/blob/master/src/runtime-deps/3.1/bionic/amd64/Dockerfile
      libc6 \
      libgcc1 \
      libgssapi-krb5-2 \
      libicu60 \
      libssl1.1 \
      libstdc++6 \
      zlib1g \
      # end .NET dependencies
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

############################
# Dark user
############################
USER root
RUN groupadd -g ${gid} dark \
    && adduser --disabled-password --gecos '' --uid ${uid} --gid ${gid} dark
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

# Esy is currently a nightmare. Upgrading to esy 6.6 is stalled because:
# - esy 6.6 copies from ~/.esy to _esy, and in our container, that copy is
#   cross-volume, so it fails with an EXDEV error.
#
# - this is solved by adding `{ prefixPath: "/home/dark/app/_esy/.esy" }` to
#   .esyrc. However, this changes where esy keeps esy.lock, breaking the delicate
#   balance of versions we have installed, and breaks in different ways involving
#   dune-configurator, alcotest, and others.


# esy uses the _build directory, none of the platform dirs are needed but
# they take 150MB
RUN npm install -g esy@0.5.8 --unsafe-perm=true \
     && rm -Rf /root/.npm \
     && rm -Rf /usr/lib/node_modules/esy/platform-*

USER dark
ENV PATH "$PATH:/home/dark/node_modules/.bin"
ENV ESY__PROJECT=/home/dark/app

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

USER dark
# Add VOLUMEs to allow backup of config, logs and databases
VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

# No idea what caused this, but we get permission problems otherwise.
RUN sudo chown postgres:postgres -R /etc/postgresql
RUN sudo chown postgres:postgres -R /var/log/postgresql
RUN sudo chown postgres:postgres -R /var/lib/postgresql

############################
# Nginx
############################
# We'll use our app's version
RUN sudo rm /etc/nginx/conf.d/default.conf
RUN sudo rm -r /etc/nginx/nginx.conf
RUN sudo chown -R dark:dark /var/log/nginx

############################
# Kubernetes
############################
RUN sudo kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl > /dev/null


############################
# Google cloud
############################
# New authentication for docker - not supported via apt
USER root

RUN wget https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64 \
        -O /usr/bin/cloud_sql_proxy \
  && chmod +x /usr/bin/cloud_sql_proxy

# crcmod for gsutil; this gets us the compiled (faster), not pure Python
# (slower) crcmod, as described in `gsutil help crcmod`
#
# It requires that python3-pip, python3-dev, python3-setuptools, and gcc be
# installed. You'll also need CLOUDSDK_PYTHON=python3 to be set when you use
# gsutil. (Which the ENV line handles.)
#
# The last line greps to confirm that gsutil has a compiled crcmod.
# Possible failure modes; missing deps above (-pip, -dev, -setuptools, gcc); a
# pre-installed crcmod that needs to be uninstalled first.  Added that because
# this install is a bit brittle, and it's easy to invisibly install the pure
# Python crcmod.
ENV CLOUDSDK_PYTHON=python3
RUN pip3 install -U --no-cache-dir -U crcmod \
  && ((gsutil version -l | grep compiled.crcmod:.True) \
      || (echo "Compiled crcmod not installed." && false))

############################
# Pip packages
############################
RUN pip3 install --no-cache-dir yq yamllint && echo 'PATH=~/.local/bin:$PATH' >> ~/.bashrc

############################
# Shellcheck
# Ubuntu has a very old version
############################

RUN \
  VERSION=v0.7.1 \
  && FILENAME=shellcheck-$VERSION.linux.x86_64.tar.xz  \
  && wget -P tmp_install_folder/ https://github.com/koalaman/shellcheck/releases/download/$VERSION/$FILENAME \
  && tar xvf tmp_install_folder/$FILENAME -C tmp_install_folder \
  && sudo cp tmp_install_folder/shellcheck-$VERSION/shellcheck /usr/bin/shellcheck \
  && rm -Rf tmp_install_folder

############################
# Kubeval - for linting k8s files
############################
RUN \
  VERSION=0.15.0 \
  && wget -P tmp_install_folder/ https://github.com/instrumenta/kubeval/releases/download/${VERSION}/kubeval-linux-amd64.tar.gz \
  && tar xvf tmp_install_folder/kubeval-linux-amd64.tar.gz -C  tmp_install_folder \
  && sudo cp tmp_install_folder/kubeval /usr/bin/ \
  && rm -Rf tmp_install_folder

####################################
# Honeytail and honeymarker installs
####################################
RUN wget -q https://honeycomb.io/download/honeytail/v1.1.4/honeytail_1.1.4_amd64.deb && \
      echo '7adbd3c64200cabcaff3adc8a2beb54f73895cc4b091981b3b2be280a0f08c02  honeytail_1.1.4_amd64.deb' | sha256sum -c && \
      sudo dpkg -i honeytail_1.1.4_amd64.deb && \
      rm honeytail_1.1.4_amd64.deb

RUN wget -q https://honeycomb.io/download/honeymarker/linux/honeymarker_1.9_amd64.deb && \
      echo '5aa10dd42f4f369c9463a8c8a361e46058339e6273055600ddad50e1bcdf2149  honeymarker_1.9_amd64.deb' | sha256sum -c && \
      sudo dpkg -i honeymarker_1.9_amd64.deb && \
      rm honeymarker_1.9_amd64.deb


####################################
# dotnet / F#
####################################
# This section was created copying the commands from the 3.1 dockerfiles.
# https://github.com/dotnet/dotnet-docker/blob/master/src/sdk/3.1/bionic/amd64/Dockerfile

# TODO: switch to 5.0. Note that the 5.0 dockerfules are split among 3
# different dockerfile (runtime-deps, runtime, and sdk), see
# https://github.com/dotnet/dotnet-docker/blob/master/src.

ENV DOTNET_SDK_VERSION=5.0.100 \
     # Skip extraction of XML docs - generally not useful within an
     # image/container - helps performance
    NUGET_XMLDOC_MODE=skip \
    # Enable detection of running in a container
    DOTNET_RUNNING_IN_CONTAINER=true \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true

RUN curl -SL --output dotnet.tar.gz https://dotnetcli.azureedge.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz \
    && dotnet_sha512='bec37bfb327c45cc01fd843ef93b22b556f753b04724bba501622df124e7e144c303a4d7e931b5dbadbd4f7b39e5adb8f601cb6293e317ad46d8fe7d52aa9a09' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && mkdir -p /usr/share/dotnet \
    && tar -ozxf dotnet.tar.gz -C /usr/share/dotnet \
    && rm dotnet.tar.gz \
    && ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet

# Trigger first run experience by running arbitrary cmd
RUN dotnet help


#############
# tunnel user
#############
USER root
RUN adduser --disabled-password --gecos '' --gid ${gid} tunnel

############################
# Environment
############################
USER dark
ENV TERM=xterm-256color

############################
# Finish
############################
USER dark

########################
# Install Rust toolchain
# This is in a separate container to save time in CI
########################
FROM dark-base

USER root

ENV RUSTUP_HOME=/usr/local/rustup \
    CARGO_HOME=/usr/local/cargo \
    PATH=/usr/local/cargo/bin:$PATH \
    RUST_VERSION=1.40.0

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --profile minimal --default-toolchain $RUST_VERSION \
  && rustup --version \
  && cargo --version \
  && rustc --version

# install Rust dev tools
RUN rustup component add clippy-preview rustfmt-preview rls
RUN cargo install cargo-cache --no-default-features --features ci-autoclean


# Once we have cargo and things installed in /usr/local/cargo and that added to PATH,
# reset CARGO_HOME so that we can use it as a project cache directory like normal.
ENV CARGO_HOME=/home/dark/.cargo

USER dark
