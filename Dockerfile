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


FROM ubuntu:22.04 as dark-base

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
RUN curl -sSL https://apt.releases.hashicorp.com/gpg | apt-key add -

# We want postgres 9.6, but it is not in later ubuntus
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list

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
      brotli \
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
      jq \
      vim \
      unzip \
      docker-ce \
      build-essential \
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
RUN sudo npm install -g prettier@2.8.4

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
ARCHIVE_TYPE=$(file --mime-type $DIR/$FILENAME | awk -F "/" '{print $NF}')
echo "Archive type: $ARCHIVE_TYPE"
case $ARCHIVE_TYPE in
  zip)  unzip $DIR/$FILENAME -d $DIR;;
  gzip | x-xz) tar xvf $DIR/$FILENAME -C $DIR;;
  *) exit 1;;
esac
ls -l $DIR
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
# Terraform
############################
RUN /home/dark/install-targz-file \
  --arm64-sha256=da571087268c5faf884912c4239c6b9c8e1ed8e8401ab1dcb45712df70f42f1b \
  --amd64-sha256=53048fa573effdd8f2a59b726234c6f450491fe0ded6931e9f4c6e3df6eece56 \
  --url=https://releases.hashicorp.com/terraform/1.3.9/terraform_1.3.9_linux_${TARGETARCH}.zip \
  --extract-file=terraform \
  --target=/usr/bin/terraform

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
  --arm64-sha256=31f39066aa0d2ece95458d98ed86a7ac21cc82f734a5202196bef64574e586dd \
  --amd64-sha256=b426a537811d505809caf21f5b5fa27bb00b7fa081bafd9f90ca7a4b26398220 \
  --url=https://github.com/fsouza/fake-gcs-server/releases/download/v1.44.0/fake-gcs-server_1.44.0_Linux_${TARGETARCH}.tar.gz\
  --extract-file=fake-gcs-server \
  --target=/usr/bin/fake-gcs-server

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
  VERSION=v0.9.0 \
  && case ${TARGETARCH} in \
       arm64) FILENAME=shellcheck-$VERSION.linux.aarch64.tar.xz;; \
       amd64) FILENAME=shellcheck-$VERSION.linux.x86_64.tar.xz;; \
       *) exit 1;; \
     esac \
  && /home/dark/install-targz-file \
  --arm64-sha256=179c579ef3481317d130adebede74a34dbbc2df961a70916dd4039ebf0735fae \
  --amd64-sha256=700324c6dd0ebea0117591c6cc9d7350d9c7c5c287acbad7630fa17b1d4d9e2f \
  --url=https://github.com/koalaman/shellcheck/releases/download/$VERSION/$FILENAME \
  --extract-file=shellcheck-${VERSION}/shellcheck \
  --target=/usr/bin/shellcheck

####################################
# Honeymarker installs
####################################

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

ENV DOTNET_SDK_VERSION=7.0.201 \
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
    CHECKSUM=a4c4d0e7d51643d6a7ff3322f795f0cdf174f62689606304e4dbfb6b38717b111d0a21ecfe2efea0234947deb87383b7cdf38e96b7e4b7bc13127b0d70431b9b
    ;;
  amd64)
    ARCH=x64
    CHECKSUM=fc9d224bf1d3600e878991fc1e8d3b1a0f21c7a8aac7b3cae0e4925ad33172cc12f56210eabfd66cfedd5f70f85918b889673401172b3999cecbeb8f2fe58863
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
RUN mkdir -p app/backend/Build

RUN mkdir -p \
      /home/dark/.vscode-server/extensions \
      /home/dark/.vscode-server-insiders/extensions \
    && chown -R dark \
      /home/dark/.vscode-server \
      /home/dark/.vscode-server-insiders

USER dark
