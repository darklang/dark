# syntax=docker/dockerfile:1
# ^^ The line above is to allow heredocs. It must be before any other content in the file ^^

# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

# DOCKERFILE_REPO: VERY IMPORTANT: this dockerfile is stored in the
# darklang/dockerfile repo, and is copied into darklang/dark.
# The copy allows developers to develop Dark directly without pulling a docker
# image.
#
# You can make changes to this file in this repo, and then copy them to
# darklang/dockerfile.
#
# The CircleCI workflow is a little complicated. To actually use any changes to
# the image, you need to change the sha used in config/circleci.yml. You can
# find the new sha after pushing to darklang/dockerfile - the sha is generated
# as part of that build. Search for DOCKERFILE_REPO for where to make that
# change.


FROM ubuntu:23.04 as dark-base

ENV FORCE_BUILD 3

# Creates variables to allow builds to work on both amd64 and arm64
ARG TARGETARCH

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
RUN curl -sSL https://apt.releases.hashicorp.com/gpg | apt-key add -

# We want postgres 9.6, but it is not in later ubuntus
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list

RUN echo "deb https://nginx.org/packages/ubuntu/ jammy nginx" > /etc/apt/sources.list.d/nginx.list

RUN echo "deb https://deb.nodesource.com/node_14.x jammy main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_14.x jammy main" >> /etc/apt/sources.list.d/nodesource.list

RUN echo "deb http://packages.cloud.google.com/apt cloud-sdk main" > /etc/apt/sources.list.d/google-cloud-sdk.list
RUN echo "deb [arch=${TARGETARCH}] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

RUN echo "deb https://apt.releases.hashicorp.com $(lsb_release -cs) main" > /etc/apt/sources.list.d/hashicorp.list

# Mostly, we use the generic version. However, for things in production we want
# to pin the exact package version so that we don't have any surprises.  As a
# result, sometimes the versions upgrade from under us and break the build. To
# fix that, you need the actual package version, which you can find by
# installing it directly:
# $ docker run <HASH> apt install mypackage
# Notes
# - replace <HASH> with a recent hash from the docker build output.
# - just use the package name, not the version.

# Deps:
# - apt-transport-https for npm
# - net-tools for netstat
# - libgbm1 for playwright/chrome
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
      postgresql-9.6 \
      postgresql-client-9.6 \
      postgresql-contrib-9.6 \
      git-restore-mtime \
      nodejs \
      libgbm1 \
      google-cloud-sdk \
      google-cloud-sdk-pubsub-emulator \
      google-cloud-sdk-gke-gcloud-auth-plugin \
      terraform \
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
      zlib1g-dev \
      pv \
      htop \
      net-tools \
      nginx \
      bash-completion \
      openssh-server \
      dnsutils \
      # .NET dependencies - https://github.com/dotnet/dotnet-docker/blob/master/src/runtime-deps/3.1/bionic/amd64/Dockerfile
      libc6 \
      libgcc1 \
      libgssapi-krb5-2 \
      libicu70 \
      libssl3 \
      libstdc++6 \
      zlib1g \
      lldb \
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
RUN sudo chown -R dark:dark /home/dark
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
## Although dark should get permissions via sudoers, this failed for one contributor using WSL
RUN echo 'dark ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# From here on in, use Dark as the user for everything and use sudo when necessary
USER dark
WORKDIR /home/dark
RUN mkdir -p .config
RUN mkdir -p .config/configstore
RUN mkdir -p bin

############################
# Locales
############################
RUN sudo locale-gen "en_US.UTF-8"
ENV LANGUAGE en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LC_ALL en_US.UTF-8

############################
# Frontend
############################
RUN sudo npm install -g prettier@2.7.1

############################
# Postgres
############################
USER postgres
RUN /etc/init.d/postgresql start && \
    psql --command "CREATE USER dark WITH SUPERUSER PASSWORD 'darklang';" && \
    createdb -O dark devdb && \
    createdb -O dark testdb

# Adjust PostgreSQL configuration so that remote connections to the
# database are possible.
RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/9.6/main/pg_hba.conf
RUN echo "listen_addresses='*'" >> /etc/postgresql/9.6/main/postgresql.conf

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
# Scripts to install files from the internet
############################
COPY <<-"EOF" /home/dark/install-targz-file
#!/bin/bash

# Script to install binary files, checking the sha

set -euo pipefail

for i in "$@" ; do
  case "${i}" in
    --arm64-sha256=*)
    ARM64_SHA256=${1/--arm64-sha256=/''}
    shift
    ;;
    --amd64-sha256=*)
    AMD64_SHA256=${1/--amd64-sha256=/''}
    shift
    ;;
    --url=*)
    URL=${1/--url=/''}
    shift
    ;;
    --extract-file=*)
    EXTRACT_FILE=${1/--extract-file=/''}
    shift
    ;;
    --target=*)
    TARGET=${1/--target=/''}
    shift
    ;;
  esac
done
DIR=$(echo $URL | sed 's/[^0-9A-Za-z]*//g')
FILENAME=$(basename $URL)
case $(dpkg --print-architecture) in
  arm64) CHECKSUM=$ARM64_SHA256;;
  amd64) CHECKSUM=$AMD64_SHA256;;
  *) exit 1;;
esac
mkdir -p $DIR
wget -P $DIR $URL
echo "$CHECKSUM $DIR/$FILENAME" | sha256sum -c -
tar xvf $DIR/$FILENAME -C $DIR
ls $DIR
sudo cp $DIR/${EXTRACT_FILE} ${TARGET}
sudo chmod +x ${TARGET}
rm -Rf $DIR
EOF

COPY <<-"EOF" /home/dark/install-exe-file
#!/bin/bash

# Script to install single files from tar.gz files, checking the sha

set -euo pipefail

for i in "$@" ; do
  case "${i}" in
    --arm64-sha256=*)
    ARM64_SHA256=${1/--arm64-sha256=/''}
    shift
    ;;
    --amd64-sha256=*)
    AMD64_SHA256=${1/--amd64-sha256=/''}
    shift
    ;;
    --url=*)
    URL=${1/--url=/''}
    shift
    ;;
    --target=*)
    TARGET=${1/--target=/''}
    shift
    ;;
  esac
done
DIR=$(echo $URL | sed 's/[^0-9A-Za-z]*//g')
FILENAME=$(basename $URL)
case $(dpkg --print-architecture) in
  arm64) CHECKSUM=$ARM64_SHA256;;
  amd64) CHECKSUM=$AMD64_SHA256;;
  *) exit 1;;
esac
sudo wget -O ${TARGET} $URL
echo "$CHECKSUM ${TARGET}" | sha256sum -c -
sudo chmod +x ${TARGET}
EOF

RUN sudo chown dark:dark /home/dark/install-targz-file
RUN chmod +x /home/dark/install-targz-file
RUN sudo chown dark:dark /home/dark/install-exe-file
RUN chmod +x /home/dark/install-exe-file

############################
# Kubernetes
############################
RUN sudo kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl > /dev/null

RUN /home/dark/install-targz-file \
  --arm64-sha256=57fa17b6bb040a3788116557a72579f2180ea9620b4ee8a9b7244e5901df02e4 \
  --amd64-sha256=2315941a13291c277dac9f65e75ead56386440d3907e0540bf157ae70f188347 \
  --url=https://get.helm.sh/helm-v3.10.2-linux-${TARGETARCH}.tar.gz \
  --extract-file=linux-${TARGETARCH}/helm \
  --target=/usr/bin/helm

############################
# Google cloud
############################
# Cloud SQL proxy
RUN /home/dark/install-exe-file \
  --arm64-sha256=834ecd08f54960ee88121ab70b05002bcfb99cd08a63bcd7a1a952c53e30a3ca \
  --amd64-sha256=fb66afb1cb8ee730314088eb7b299398bda6c0434b9b383b27a26b8951e775c5 \
  --url=https://storage.googleapis.com/cloudsql-proxy/v1.33.1/cloud_sql_proxy.linux.${TARGETARCH} \
  --target=/usr/bin/cloud_sql_proxy

# PubSub
ENV PUBSUB_EMULATOR_HOST=0.0.0.0:8085

# GCS emulator
RUN /home/dark/install-targz-file \
  --arm64-sha256=74b5d65027b19167854705f273c32b1b295e9ea0c7c03f9cb421e53c99ed3ef5 \
  --amd64-sha256=c38b83b813d15f554003b5c7823174ee23f3097ac977f7267a2cdc8b479524d3 \
  --url=https://github.com/fsouza/fake-gcs-server/releases/download/v1.42.2/fake-gcs-server_1.42.2_Linux_${TARGETARCH}.tar.gz\
  --extract-file=fake-gcs-server \
  --target=/usr/bin/fake-gcs-server

# GKE
ENV USE_GKE_GCLOUD_AUTH_PLUGIN=True

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
RUN sudo pip3 install -U --no-cache-dir -U crcmod \
  && ((gsutil version -l | grep compiled.crcmod:.True) \
      || (echo "Compiled crcmod not installed." && false))

############################
# Pip packages
############################
RUN sudo pip3 install --no-cache-dir yq yamllint watchfiles yapf==0.32.0
ENV PATH "$PATH:/home/dark/.local/bin"

####################################
# CircleCI
####################################
RUN curl -fLSs https://raw.githubusercontent.com/CircleCI-Public/circleci-cli/master/install.sh | sudo bash

############################
# Shellcheck
# Ubuntu has a very old version
############################

RUN \
  VERSION=v0.8.0 \
  && case ${TARGETARCH} in \
       arm64) FILENAME=shellcheck-$VERSION.linux.aarch64.tar.xz;; \
       amd64) FILENAME=shellcheck-$VERSION.linux.x86_64.tar.xz;; \
       *) exit 1;; \
     esac \
  && /home/dark/install-targz-file \
  --arm64-sha256=9f47bbff5624babfa712eb9d64ece14c6c46327122d0c54983f627ae3a30a4ac \
  --amd64-sha256=ab6ee1b178f014d1b86d1e24da20d1139656c8b0ed34d2867fbb834dad02bf0a \
  --url=https://github.com/koalaman/shellcheck/releases/download/$VERSION/$FILENAME \
  --extract-file=shellcheck-${VERSION}/shellcheck \
  --target=/usr/bin/shellcheck

############################
# Kubeconform - for linting k8s files
############################

RUN \
  VERSION=v0.4.14 \
  && /home/dark/install-targz-file \
  --arm64-sha256=0ff34c19b3b19905a9c87906c801d9d4325d0614ae48bc1b2543dc9ec908cf13 \
  --amd64-sha256=140044a5eb44a18e52d737ba15936f87b0e5fca3d34a02ae13b2d68025a449f3 \
  --url=https://github.com/yannh/kubeconform/releases/download/$VERSION/kubeconform-linux-${TARGETARCH}.tar.gz \
  --extract-file=kubeconform \
  --target=/usr/bin/kubeconform

####################################
# Honeytail and honeymarker installs
####################################
RUN /home/dark/install-exe-file \
  --arm64-sha256=c5a57a729b0ccf4ca0f2287c862538812604f5fd67d102372e91215701afdbe1 \
  --amd64-sha256=d774112265ee8e98c6221232461cf36c35faf844005cc98b43b55bb375761766 \
  --url=https://github.com/honeycombio/honeytail/releases/download/v1.8.2/honeytail-linux-${TARGETARCH} \
  --target=/usr/bin/honeytail

RUN /home/dark/install-exe-file \
  --arm64-sha256=fef8c383419c86ceabb0bbffd3bcad2bf9223537fba9f848218480f873a96e8d \
  --amd64-sha256=6e08038f4587d515856076746ad3a69e67376eddd38d8657f449aad393b95cd8 \
  --url=https://github.com/honeycombio/honeymarker/releases/download/v0.2.10/honeymarker-linux-${TARGETARCH} \
  --target=/usr/bin/honeymarker


####################################
# dotnet / F#
####################################

# This section was created copying the commands from the dotnet dockerfiles.
# Note that the Dockerfiles are split among 3 different dockerfile
# (runtime-deps, runtime, and sdk), see
# https://github.com/dotnet/dotnet-docker/blob/master/src

ENV DOTNET_SDK_VERSION=6.0.300 \
    # Skip extraction of XML docs - generally not useful within an
    # image/container - helps performance
    NUGET_XMLDOC_MODE=skip \
    # Enable detection of running in a container
    DOTNET_RUNNING_IN_CONTAINER=true \
    # Do not generate certificate
    DOTNET_GENERATE_ASPNET_CERTIFICATE=false \
    # Do not show first run text
    DOTNET_NOLOGO=true \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true

RUN <<EOF
set -e
case ${TARGETARCH} in
  arm64)
    ARCH=arm64
    CHECKSUM=67eb088ccad197a39f104af60f3e6d12ea9b17560e059c0f7c8e956005d919d00bf0f3e487b06280be63ad57aa8895f16ebc8c92107c5019c9cf47bd620ea925
    ;;
  amd64)
    ARCH=x64
    CHECKSUM=52d720e90cfb889a92d605d64e6d0e90b96209e1bd7eab00dab1d567017d7a5a4ff4adbc55aff4cffcea4b1bf92bb8d351859d00d8eb65059eec5e449886c938
    ;;
  *) exit 1;;
esac
curl -SL --output dotnet.tar.gz https://dotnetcli.azureedge.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-${ARCH}.tar.gz
echo "$CHECKSUM dotnet.tar.gz" | sha512sum -c -
sudo mkdir -p /usr/share/dotnet
sudo tar -C /usr/share/dotnet -oxzf dotnet.tar.gz .
sudo rm dotnet.tar.gz
# Trigger first run experience by running arbitrary cmd
sudo ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet
dotnet --help
EOF

# Not supported on arm64 until maybe dotnet 8 - https://github.com/dotnet/runtime/issues/75613
RUN sudo dotnet workload install wasm-tools

# formatting
RUN dotnet tool install fantomas-tool --version 4.7.9 -g
ENV PATH "$PATH:/home/dark/bin:/home/dark/.dotnet/tools"

#############
# tunnel user
#############
RUN sudo adduser --disabled-password --gecos '' --gid ${gid} tunnel
# Remove use_pty as it messes up `su tunnel` commands
RUN sudo sed -i 's!Defaults\s\+use_pty!!' /etc/sudoers

############################
# Environment
############################
ENV TERM=xterm-256color

############################
# Finish
############################
USER dark

# Add all the mounts here so that they have the right permissions
RUN touch .bash_history
RUN mkdir -p .config/gcloud
RUN mkdir -p .config/configstore
RUN mkdir -p app
RUN mkdir -p app/_build
RUN mkdir -p app/_esy
RUN mkdir -p .esy
RUN mkdir -p app/node_modules
RUN mkdir -p app/lib
RUN mkdir -p app/backend/Build

RUN mkdir -p \
      /home/dark/.vscode-server/extensions \
      /home/dark/.vscode-server-insiders/extensions \
    && chown -R dark \
      /home/dark/.vscode-server \
      /home/dark/.vscode-server-insiders

USER dark
