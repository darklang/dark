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


FROM ubuntu:24.04 AS dark-base

ENV FORCE_BUILD=8

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
      lsb-release \
      less \
      gpg \
      gpg-agent \
      file \
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

# Latest NPM (taken from https://deb.nodesource.com)
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg

RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list

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
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      git \
      wget \
      sudo \
      locales \
      git-restore-mtime \
      nodejs \
      sqlite3 \
      jq \
      parallel \
      vim \
      unzip \
      python3-pip \
      python3-setuptools \
      python3-dev \
      python3-venv \
      libsodium-dev \
      libssl-dev \
      zlib1g-dev \
      net-tools \
      bash-completion \
      dnsutils \
      # .NET dependencies - https://github.com/dotnet/dotnet-docker/blob/master/src/runtime-deps/3.1/bionic/amd64/Dockerfile
      libc6 \
      libgcc1 \
      libgssapi-krb5-2 \
      libicu74 \
      libssl3 \
      libstdc++6 \
      zlib1g \
      # end .NET dependencies
      # parser (tree-sitter) dependencies
      build-essential \
      # end parser dependencies
      psmisc \
      # CLI integration tests
      expect \
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

# As of Ubuntu 24.04, an install includes
# an 'ubuntu' user, that we don't use,
# who takes id 1000. Let's reassign the id.
# https://bugs.launchpad.net/cloud-images/+bug/2005129
RUN usermod -u 2000 ubuntu && groupmod -g 2000 ubuntu


############################
# Dark user
############################
USER root
RUN groupadd -g ${gid} dark \
    && useradd --create-home --uid ${uid} --gid ${gid} --shell /bin/bash dark
RUN echo "dark:dark" | chpasswd && usermod -aG sudo dark
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
ENV LANGUAGE=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

############################
# Frontend
############################
RUN sudo npm install -g prettier@3.0.2

############################
# Scripts to install files from the internet
############################

COPY --chown=dark:dark --chmod=755 ./scripts/installers/* .

############################
# Pip packages
############################
RUN python3 -m venv /home/dark/.local \
  && /home/dark/.local/bin/pip install --no-cache-dir setuptools yq yamllint watchfiles yapf==0.40.1
ENV PATH="/home/dark/.local/bin:$PATH"

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
# dotnet / F#
####################################

# This section was created copying the commands from the dotnet dockerfiles.
# Note that the Dockerfiles are split among 3 different dockerfile
# (runtime-deps, runtime, and sdk), see
# https://github.com/dotnet/dotnet-docker/blob/master/src

    # Skip extraction of XML docs - generally not useful within an
    # image/container - helps performance
ENV NUGET_XMLDOC_MODE=skip \
    # Enable detection of running in a container
    DOTNET_RUNNING_IN_CONTAINER=true \
    # Do not generate certificate
    DOTNET_GENERATE_ASPNET_CERTIFICATE=false \
    # Do not show first run text
    DOTNET_NOLOGO=true \
    # Enable correct mode for dotnet watch (only mode supported in a container)
    DOTNET_USE_POLLING_FILE_WATCHER=true

RUN /home/dark/install-dotnet10 \
  --version=10.0.102 \
  --arm64-sha256=1254141153d29b5b926e0e7b0b172a25f9c096b8ed6a182f54062c5e0b41384b30e10e2bf1ebe86ed0f58f4ff762203acd83bcf23fefb59c07af45332d794700 \
  --amd64-sha256=7adf40e8e5547970391cfbe474c3874c6918ce3575ac398f376c78502134e1c8a2fa3da9aca281fdaeda81671f56c851ebe9e74c5b57c5a298bd45deba63565d

# formatting
RUN dotnet tool install fantomas --version 6.2.3 -g
ENV PATH="$PATH:/home/dark/bin:/home/dark/.dotnet/tools"

# without this, dotnet restore seems to fail, accessing the wrong path
ENV NUGET_SCRATCH=/tmp/NuGetScratch

#############
# Emscripten,
# for compiling the tree-sitter parser to wasm
#############
# RUN git clone https://github.com/emscripten-core/emsdk.git --depth 1 \
#   && cd emsdk \
#   # TODO pin to a recent stable version (i.e. 3.1.37)
#   # we are using the latest version because Linux arm64 binaries aren't available in all releases.
#   # see: https://github.com/emscripten-core/emscripten/issues/19275
#   && ./emsdk install latest \
#   && ./emsdk activate latest
# ENV PATH="$PATH:/home/dark/emsdk/upstream/emscripten"


#############
# Zig,
# for (cross-)compiling our `tree-sitter-darklang` parser,
# along with the `tree-sitter` library itself.
# TODO Occasionally, check https://ziglang.org/download to see if we're using the latest version
ENV ZIG_VERSION=0.11.0
ENV ZIG_ARM64_MINISIG="RUSGOq2NVecA2XPwbgbN5SvU46UcCmhhfcfrjVC+YvcwUcjAYfIXQmqE//df1Mes7iyGZvGoy2+PSJ8pog7QGLE+3nvP8gtlSAs="
ENV ZIG_AMD64_MINISIG="RUSGOq2NVecA2X2did6P61CXthPLZEUwi07GDWQ2MWU58W+asm3v85+PRVHN5SljhdsKoAMmbg4fdyseAcbVZayGaV1Iv6chcgE="
#############
RUN set -e; \
  case ${TARGETARCH} in \
  arm64) \
  ZIG_ARCH="aarch64"; \
  ZIG_MINISIG=$ZIG_ARM64_MINISIG; \
  ;; \
  amd64) \
  ZIG_ARCH="x86_64"; \
  ZIG_MINISIG=$AMD64_MINISIG; \
  ;; \
  *) exit 1;; \
  esac;  \
  curl -o zig.tar.xz "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz"; \
  # TODO: verify signature
  # RUN minisign -Vm zig.tar.xz -P "RWSGOq2NVecA2UPNdBUZykf1CCb147pkmdtYxgb3Ti+JO/wCYvhbAb/U" \
  #   || (echo "Verification failed!" && exit 1)
  mkdir ~/zig; \
  tar -xf zig.tar.xz -C ~/zig; \
  rm zig.tar.xz; \
  mv ~/zig/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}/* ~/zig;

ENV PATH="$PATH:~/zig"


############################
# VSCE, used for publishing VS Code extension
#
# TODO reassess whether this should be done here,
#   or in `publish-vs-code-extension`.
############################
RUN sudo npm install -g @vscode/vsce


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
