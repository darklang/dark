#-------------------------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See https://go.microsoft.com/fwlink/?linkid=2090316 for license information.
#-------------------------------------------------------------------------------------------------------------

# Note: You can use any Debian/Ubuntu based image you want. 
FROM ubuntu:18.04@sha256:3235326357dfb65f1781dbc4df3b834546d8bf914e82cce58e6e6b676e23ce8f

# This Dockerfile adds a non-root user with sudo access. Use the "remoteUser"
# property in devcontainer.json to use it. On Linux, the container user's GID/UIDs
# will be updated to match your local UID/GID (when using the dockerFile property).
# See https://aka.ms/vscode-remote/containers/non-root-user for details.
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

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

RUN curl -sSL https://dl.google.com/linux/linux_signing_key.pub | apt-key add -
RUN curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
RUN echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

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
  libcurl4-gnutls-dev \
  libpq-dev \
  vim \
  unzip \
  docker-ce \
  fswatch \
  python3-pip \
  python3-setuptools \
  python3-dev \
  ssh

RUN pip3 install livereload