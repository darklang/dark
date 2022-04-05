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

FROM ubuntu:20.04@sha256:7cc0576c7c0ec2384de5cbf245f41567e922aab1b075f3e8ad565f508032df17 as dark-base

ENV FORCE_BUILD 3

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
RUN curl -sSL https://baltocdn.com/helm/signing.asc | apt-key add -


# We want postgres 9.6, but it is not in ubuntu 20.04
RUN echo "deb http://apt.postgresql.org/pub/repos/apt/ `lsb_release -cs`-pgdg main" >> /etc/apt/sources.list.d/pgdg.list

RUN echo "deb [arch=amd64] https://dl.google.com/linux/chrome/deb/ stable main" > /etc/apt/sources.list.d/google-chrome.list

RUN echo "deb https://nginx.org/packages/ubuntu/ bionic nginx" > /etc/apt/sources.list.d/nginx.list

RUN echo "deb https://deb.nodesource.com/node_14.x focal main" > /etc/apt/sources.list.d/nodesource.list
RUN echo "deb-src https://deb.nodesource.com/node_14.x focal main" >> /etc/apt/sources.list.d/nodesource.list

RUN echo "deb http://packages.cloud.google.com/apt cloud-sdk main" > /etc/apt/sources.list.d/google-cloud-sdk.list
RUN echo "deb [arch=amd64] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

RUN echo "deb https://baltocdn.com/helm/stable/debian/ all main" > /etc/apt/sources.list.d/helm-stable-debian.list

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
      helm \
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
      htop \
      net-tools \
      nginx=1.16.1-1~bionic \
      bash-completion \
      texinfo \
      openssh-server \
      dnsutils \
      # .NET dependencies - https://github.com/dotnet/dotnet-docker/blob/master/src/runtime-deps/3.1/bionic/amd64/Dockerfile
      libc6 \
      libgcc1 \
      libgssapi-krb5-2 \
      libicu66 \
      libssl1.1 \
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
RUN sudo npm install -g prettier@2.5.1

# Esy is currently a nightmare. Upgrading to esy 6.6 is stalled because:
# - esy 6.6 copies from ~/.esy to _esy, and in our container, that copy is
#   cross-volume, so it fails with an EXDEV error.
#
# - this is solved by adding `{ prefixPath: "/home/dark/app/_esy/.esy" }` to
#   .esyrc. However, this changes where esy keeps esy.lock, breaking the delicate
#   balance of versions we have installed, and breaks in different ways involving
#   dune-configurator, alcotest, and others.


# This violates our "dark is the user" rule but we'll be getting rid of this
# soon so don't spend the time making it work right
USER root
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
# Kubernetes
############################
RUN sudo kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl > /dev/null


############################
# Google cloud
############################
# New authentication for docker - not supported via apt
RUN sudo wget https://dl.google.com/cloudsql/cloud_sql_proxy.linux.amd64 \
        -O /usr/bin/cloud_sql_proxy \
  && sudo chmod +x /usr/bin/cloud_sql_proxy

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
RUN sudo pip3 install --no-cache-dir yq yamllint
ENV PATH "$PATH:/home/dark/.local/bin"

RUN pip3 install git+https://github.com/pbiggar/watchgod.git@b74cd7ec064ebc7b4263dc532c7c97e046002bef
# Formatting

RUN pip3 install yapf==0.32.0

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
  && FILENAME=shellcheck-$VERSION.linux.x86_64.tar.xz  \
  && wget -P tmp_install_folder/ https://github.com/koalaman/shellcheck/releases/download/$VERSION/$FILENAME \
  && tar xvf tmp_install_folder/$FILENAME -C tmp_install_folder \
  && sudo cp tmp_install_folder/shellcheck-$VERSION/shellcheck /usr/bin/shellcheck \
  && rm -Rf tmp_install_folder

############################
# Kubeconform - for linting k8s files
############################
RUN \
  VERSION=v0.4.12 \
  && wget -P tmp_install_folder/ https://github.com/yannh/kubeconform/releases/download/$VERSION/kubeconform-linux-amd64.tar.gz \
  && tar xvf tmp_install_folder/kubeconform-linux-amd64.tar.gz -C  tmp_install_folder \
  && sudo cp tmp_install_folder/kubeconform /usr/bin/ \
  && rm -Rf tmp_install_folder

####################################
# Honeytail and honeymarker installs
####################################
RUN wget -q https://honeycomb.io/download/honeytail/v1.6.1/honeytail_1.6.1_amd64.deb && \
      echo 'd099dd50b8446926be7a011eb4b98ed5bf07e5e7a4f9fce8015fe2147492833c  honeytail_1.6.1_amd64.deb' | sha256sum -c && \
      sudo dpkg -i honeytail_1.6.1_amd64.deb && \
      rm honeytail_1.6.1_amd64.deb

RUN wget -q https://honeycomb.io/download/honeymarker/linux/honeymarker_1.9_amd64.deb && \
      echo '5aa10dd42f4f369c9463a8c8a361e46058339e6273055600ddad50e1bcdf2149  honeymarker_1.9_amd64.deb' | sha256sum -c && \
      sudo dpkg -i honeymarker_1.9_amd64.deb && \
      rm honeymarker_1.9_amd64.deb


####################################
# dotnet / F#
####################################

# This section was created copying the commands from the dotnet dockerfiles.
# Note that the Dockerfiles are split among 3 different dockerfile
# (runtime-deps, runtime, and sdk), see
# https://github.com/dotnet/dotnet-docker/blob/master/src

ENV DOTNET_SDK_VERSION=6.0.201 \
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

RUN curl -SL --output dotnet.tar.gz https://dotnetcli.azureedge.net/dotnet/Sdk/$DOTNET_SDK_VERSION/dotnet-sdk-$DOTNET_SDK_VERSION-linux-x64.tar.gz \
    && dotnet_sha512='a4d96b6ca2abb7d71cc2c64282f9bd07cedc52c03d8d6668346ae0cd33a9a670d7185ab0037c8f0ecd6c212141038ed9ea9b19a188d1df2aae10b2683ce818ce' \
    && echo "$dotnet_sha512 dotnet.tar.gz" | sha512sum -c - \
    && sudo mkdir -p /usr/share/dotnet \
    && sudo tar -C /usr/share/dotnet -oxzf dotnet.tar.gz . \
    && sudo rm dotnet.tar.gz \
    # Trigger first run experience by running arbitrary cmd
    && sudo ln -s /usr/share/dotnet/dotnet /usr/bin/dotnet \
    && dotnet help

RUN sudo dotnet workload install wasm-tools
RUN dotnet tool install -g dotnet-sos
# TODO: is this the right directory?
RUN echo "plugin load /home/dark/.dotnet/tools/.store/dotnet-sos/5.0.160202/dotnet-sos/5.0.160202/tools/netcoreapp2.1/any/linux-x64/libsosplugin.so" > ~/.lldbinit

# formatting
RUN dotnet tool install fantomas-tool --version 4.6.3 -g
RUN curl https://raw.githubusercontent.com/darklang/build-files/main/ocamlformat --output ~/bin/ocamlformat && chmod +x ~/bin/ocamlformat
ENV PATH "$PATH:/home/dark/bin:/home/dark/.dotnet/tools"

#############
# tunnel user
#############
RUN sudo adduser --disabled-password --gecos '' --gid ${gid} tunnel

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
RUN mkdir -p app/containers/stroller/target
RUN mkdir -p app/containers/queue-scheduler/target
RUN mkdir -p app/fsharp-backend/Build
RUN mkdir -p .cargo

RUN mkdir -p \
      /home/dark/.vscode-server/extensions \
      /home/dark/.vscode-server-insiders/extensions \
    && chown -R dark \
      /home/dark/.vscode-server \
      /home/dark/.vscode-server-insiders

USER dark


########################
# Install Rust toolchain
# This is in a separate container to save time in CI
########################
FROM dark-base as dark-rust
# We use root here because we're eventually going to remove rust and so it's not worth solving
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
RUN cargo install cargo-cache --version 0.6.3 --no-default-features --features ci-autoclean
USER dark

# Once we have cargo and things installed in /usr/local/cargo and that added to PATH,
# reset CARGO_HOME so that we can use it as a project cache directory like normal.
ENV CARGO_HOME=/home/dark/.cargo

