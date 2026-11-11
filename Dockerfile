# syntax=docker/dockerfile:1
# ^^ The line above is to allow heredocs. It must be before any other content in the file ^^

# This is an image used to compile and test Dark. Later, we will use this to
# create another dockerfile to deploy.

# Nothing here is pinned or copied by hand. Images are tagged with a hash of what they're
# built from (scripts/devcontainer/image-tag), and CI makes sure the ones a commit needs
# exist (scripts/deployment/ci-images):
#
#   - On a branch or a PR, fork or not, change this file and push. If Docker Hub has no
#     image for the new hash, the pipeline builds one into ttl.sh, a throwaway registry,
#     and every job runs in it, pinned by digest. Dockerfile.build-base works the same
#     way for the release jobs. Locally, `scripts/dev/start --recreate` builds it.
#   - On merge, main builds the images and pushes them to Docker Hub. Only main holds
#     the push token, and main rebuilds an image whenever a commit changes its inputs
#     rather than trust a tag it didn't push. Release tags run in what main pushed.
#
# Two stages. `dark-ci` holds what CI jobs run and nothing else, because every
# CI job pulls it before doing anything. `dark-base` is the dev container: it
# is dark-ci plus the tools people use interactively, so the two share every
# dark-ci layer. It is the last stage, which is what a plain `docker build` and
# .devcontainer/devcontainer.json produce. CI's image is `--target dark-ci`.


FROM ubuntu:24.04 AS dark-ci

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

# Mostly, we use the generic version. However, for things in production we want
# to pin the exact package version so that we don't have any surprises.  As a
# result, sometimes the versions upgrade from under us and break the build. To
# fix that, you need the actual package version, which you can find by
# installing it directly:
# $ docker run <HASH> apt install mypackage
# Notes
# - replace <HASH> with a recent hash from the docker build output.
# - just use the package name, not the version.

# What the CI jobs run. Anything only used interactively goes in dark-base below.
USER root
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      curl \
      # scripts/installers/install-targz-file
      wget \
      ca-certificates \
      git \
      # CircleCI's `checkout` step clones with git over ssh, and without ssh it
      # falls back to its own git client
      openssh-client \
      sudo \
      locales \
      sqlite3 \
      jq \
      unzip \
      xz-utils \
      file \
      procps \
      python3 \
      python3-venv \
      # .NET dependencies - https://github.com/dotnet/dotnet-docker/blob/master/src/runtime-deps/3.1/bionic/amd64/Dockerfile
      libc6 \
      libgcc1 \
      libgssapi-krb5-2 \
      libicu74 \
      libssl3 \
      libstdc++6 \
      zlib1g \
      # end .NET dependencies
      # NativeAOT builds (`scripts/build/build-release-cli-exes.sh --aot`) need
      # gcc as the platform linker, and zlib1g-dev to link against. gcc also
      # brings in binutils, whose objcopy strips symbols.
      gcc \
      zlib1g-dev \
      # killall, in scripts/build/_dotnet-wrapper and scripts/run-backend-tests
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
# Scripts to install files from the internet
############################

COPY --chown=dark:dark --chmod=755 ./scripts/installers/* .

############################
# Pip packages
############################
# yamllint for scripts/linting/yamllinter, and it brings pathspec, which
# scripts/build/_buildplan.py imports. dark-base adds the interactive ones.
RUN python3 -m venv /home/dark/.local \
  && /home/dark/.local/bin/pip install --no-cache-dir yamllint
ENV PATH="/home/dark/.local/bin:$PATH"

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

RUN /home/dark/install-dotnet \
  --version=10.0.115 \
  --arm64-sha256=3d2fc61a3913e3ba65f1c6188f0a193c58eb219c31fdd19a0d07ab2f4ec663fc3c5338ed3382941bac45f666c6c9b58d9082a7464223244e3ca4b18295d164a0 \
  --amd64-sha256=01bf048cf0495a7ce76be07b9623d385c9d9f172237d0267ded25c31f4b19604d74f1d328d0ee5fe6bcfb23cb20c41fb072c9b84fce6582c20edb1e3d93ea012 \
  # Parts of the SDK nothing here runs: `dotnet format` (fantomas formats), `dotnet watch`
  # (scripts/dev/watch has its own watcher), and the .NET Framework test host, which is
  # Windows-only. Removed in the same layer, or they would still be pulled.
  && sudo rm -rf \
    /usr/share/dotnet/sdk/*/DotnetTools/dotnet-format \
    /usr/share/dotnet/sdk/*/DotnetTools/dotnet-watch \
    /usr/share/dotnet/sdk/*/TestHostNetFramework

# formatting
RUN dotnet tool install fantomas --version 6.2.3 -g

ENV PATH="$PATH:/home/dark/bin:/home/dark/.dotnet/tools"

# without this, dotnet restore seems to fail, accessing the wrong path
ENV NUGET_SCRATCH=/tmp/NuGetScratch

#############
# Zig,
# for cross-compiling native libraries (e.g. SQLite) for the release CLIs.
# TODO Occasionally, check https://ziglang.org/download to see if we're using the latest version
# NOTE: release builds use 0.13.0 (scripts/build/install-release-toolchain), so release SQLite
# archives and local ones are built with different zig versions.
# The hashes are the ones https://ziglang.org/download/index.json lists.
ENV ZIG_VERSION=0.11.0
#############
RUN set -e; \
  case ${TARGETARCH} in \
  arm64) \
  ZIG_ARCH="aarch64"; \
  ZIG_SHA256="956eb095d8ba44ac6ebd27f7c9956e47d92937c103bf754745d0a39cdaa5d4c6"; \
  ;; \
  amd64) \
  ZIG_ARCH="x86_64"; \
  ZIG_SHA256="2d00e789fec4f71790a6e7bf83ff91d564943c5ee843c5fd966efc474b423047"; \
  ;; \
  *) exit 1;; \
  esac;  \
  curl -fsSL -o zig.tar.xz "https://ziglang.org/download/${ZIG_VERSION}/zig-linux-${ZIG_ARCH}-${ZIG_VERSION}.tar.xz"; \
  echo "${ZIG_SHA256}  zig.tar.xz" | sha256sum -c -; \
  mkdir ~/zig; \
  # Not the bundled docs (46 MB): nothing builds from them.
  tar -xf zig.tar.xz -C ~/zig --strip-components=1 --exclude="zig-linux-${ZIG_ARCH}-${ZIG_VERSION}/doc"; \
  rm zig.tar.xz;

ENV PATH="$PATH:/home/dark/zig"

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


############################
# The dev container: dark-ci plus what people use by hand
############################
FROM dark-ci AS dark-base

USER root

# Latest NPM (taken from https://deb.nodesource.com)
#
# Node is here for one reason: `scripts/perf/crosslang` runs the reference workload in node and
# python to compare allocation per iteration. Nothing in the tree needs a global npm package any
# more, so there are none; node itself is still a dependency of that perf tool, so it stays.
#
# Deps:
# - gpg for the nodesource key
# - net-tools for netstat
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install -y --no-install-recommends gpg gpg-agent \
    && curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg \
    && echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" > /etc/apt/sources.list.d/nodesource.list \
    && apt update \
    && DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      apt-transport-https \
      lsb-release \
      less \
      git-restore-mtime \
      nodejs \
      parallel \
      vim \
      python3-pip \
      python3-setuptools \
      python3-dev \
      libsodium-dev \
      libssl-dev \
      net-tools \
      bash-completion \
      dnsutils \
      && apt clean \
      && rm -rf /var/lib/apt/lists/*

USER dark

RUN /home/dark/.local/bin/pip install --no-cache-dir setuptools yq watchfiles yapf==0.40.1 brotli

####################################
# CircleCI
####################################
# A release tarball checked against the sha256 in its release's checksums file, rather than
# piping the project's install script from master into a root shell.
ARG TARGETARCH
ARG CIRCLECI_CLI_VERSION=1.11.0
RUN set -e; \
  case ${TARGETARCH} in \
  arm64) SHA256="c1cc8db9d74fa6c892cc056acee816170bb5bf8c5418687f8c3ed0d353a1a839" ;; \
  amd64) SHA256="70c3a935e0c2d71d42ba65f0005696eb17fd65fb625c678f45311b826d56de47" ;; \
  *) exit 1 ;; \
  esac; \
  NAME="circleci-cli_${CIRCLECI_CLI_VERSION}_linux_${TARGETARCH}"; \
  curl -fsSL -o /tmp/circleci.tar.gz \
    "https://github.com/CircleCI-Public/circleci-cli/releases/download/v${CIRCLECI_CLI_VERSION}/${NAME}.tar.gz"; \
  echo "${SHA256}  /tmp/circleci.tar.gz" | sha256sum -c -; \
  mkdir /tmp/circleci; \
  tar -xzf /tmp/circleci.tar.gz -C /tmp/circleci; \
  sudo install -m 755 /tmp/circleci/circleci /usr/local/bin/circleci; \
  rm -rf /tmp/circleci.tar.gz /tmp/circleci

# profiling: scripts/perf/alloc-profile and scripts/perf/cpu-profile both drive this
RUN dotnet tool install dotnet-trace --version 9.0.661903 -g

RUN mkdir -p \
  /home/dark/.vscode-server/extensions \
  /home/dark/.vscode-server-insiders/extensions \
  && chown -R dark \
  /home/dark/.vscode-server \
  /home/dark/.vscode-server-insiders

USER dark
