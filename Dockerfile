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
      && rm -rf /var/lib/apt/lists/* \
      # gcc's sanitizer runtimes, which come with it but which nothing here links.
      && rm -f /usr/lib/x86_64-linux-gnu/lib*san.so.* /usr/lib/aarch64-linux-gnu/lib*san.so.*

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

############################
# Node
# The vscode-extension gate builds the extension with npm, and CI runs the gates here.
############################

RUN \
  VERSION=v20.18.1 \
  && case ${TARGETARCH} in \
       arm64) ARCH=arm64; SHA256=44d1ffc5905c005ace4515ca6f8c090c4c7cfce3a9a67df0dba35c727590b8f6;; \
       amd64) ARCH=x64; SHA256=c6fa75c841cbffac851678a472f2a5bd612fff8308ef39236190e1f8dbb0e567;; \
       *) exit 1;; \
     esac \
  && FILENAME=node-$VERSION-linux-$ARCH.tar.xz \
  && wget -q -P /tmp https://nodejs.org/dist/$VERSION/$FILENAME \
  && echo "$SHA256 /tmp/$FILENAME" | sha256sum -c - \
  && sudo tar xJf /tmp/$FILENAME -C /usr/local --strip-components=1 \
       node-$VERSION-linux-$ARCH/bin node-$VERSION-linux-$ARCH/lib \
  && rm /tmp/$FILENAME

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
  --version=11.0.100 \
  --arm64-sha256=cfa248bb1377606e868602fb5b4d66fa16b7546156593937d1982f7f2142e6ded76d71c4a26efdab99f87c46ab40c3c672ab1b57ef7fa25449efe5b140a55af4 \
  --amd64-sha256=1bec9cca2ec689b1e80245b84f2b53c9441d1aa6daccf2ceb27490768bffa95954fa585bd275def31069d874d543d9ba9e6591fe9ade2dbd9f34ec5c0fd0050c \
  # Parts of the SDK nothing here runs: its bundled tools (`dotnet format`, `dotnet watch`,
  # the ASP.NET ones), the .NET Framework test host, which is Windows-only, `dotnet new`'s
  # templates, and the SDK's messages in other languages. Removed in the same layer, or they
  # would still be pulled.
  && sudo rm -rf \
    /usr/share/dotnet/sdk/*/DotnetTools \
    /usr/share/dotnet/sdk/*/TestHostNetFramework \
    /usr/share/dotnet/templates \
  && sudo find /usr/share/dotnet/sdk -type d \
    -regex '.*/\(cs\|de\|es\|fr\|it\|ja\|ko\|pl\|pt-BR\|ru\|tr\|zh-Hans\|zh-Hant\)' -prune \
    -exec rm -rf {} +

# formatting
RUN dotnet tool install fantomas --version 6.2.3 -g

ENV PATH="$PATH:/home/dark/bin:/home/dark/.dotnet/tools"

# without this, dotnet restore seems to fail, accessing the wrong path
ENV NUGET_SCRATCH=/tmp/NuGetScratch

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

# Deps:
# - net-tools for netstat
RUN DEBIAN_FRONTEND=noninteractive \
    apt update --allow-releaseinfo-change && \
    DEBIAN_FRONTEND=noninteractive \
    apt install \
      --no-install-recommends \
      -y \
      apt-transport-https \
      lsb-release \
      less \
      git-restore-mtime \
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

# Zig, for cross-compiling native libraries (e.g. SQLite) for the release CLIs. Not in dark-ci:
# CI only needs it when the SQLite archive isn't cached, and build-sqlite.sh fetches it then.
RUN /home/dark/install-zig
ENV PATH="$PATH:/home/dark/zig"

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
