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
      gnupg \
      nodejs=13.5.0-1nodesource1 \
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
      python3-setuptools \
      libsodium-dev \
      gcc \
      pgcli \
      xvfb \
      ffmpeg \
      tmux \
      libssl-dev \
      libssl-ocaml-dev \
      zlib1g-dev \
      pv \
      net-tools \
      nginx=1.16.1-1~bionic \
      bash-completion \
      texinfo \
      openssh-server \
      x11-utils \
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

RUN npm install -g yarn@1.21.1

RUN npm install -g esy@0.5.7 --unsafe-perm=true

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
# Nginx
############################
# We could mount these files into the container, but then we'd also want to make
# scripts/support/compile restart runserver if either of the nginx files
# changed, and I'd rather not add to the complexity of that file right now.
# nginx changes should be infrequent, making users restart scripts/builder is
# fine
RUN sudo rm /etc/nginx/conf.d/default.conf \
  && sudo ln -s /home/dark/app/scripts/support/nginx.conf /etc/nginx/conf.d/nginx.conf
RUN sudo rm -r /etc/nginx/nginx.conf && sudo ln -s /home/dark/app/scripts/support/base-nginx.conf /etc/nginx/nginx.conf
RUN sudo chown -R dark:dark /var/log/nginx

############################
# Kubernetes
############################
RUN sudo kubectl completion bash | sudo tee /etc/bash_completion.d/kubectl > /dev/null


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
RUN pip3 install -U --no-cache-dir -U crcmod

############################
# Pip packages
############################
RUN pip3 install yq && echo 'PATH=~/.local/bin:$PATH' >> ~/.bashrc

############################
# Ocaml
############################
USER dark
ENV FORCE_OCAML_BUILD 5
ENV OPAMJOBS 4

ENV OCAML_SWITCH ocaml-base-compiler.4.06.1
# env vars below here replace `eval $(opam env)` in dotfiles; by doing it here,
# we avoid having to source .bashrc before every command
ENV PATH "/home/dark/.opam/${OCAML_SWITCH}/bin:$PATH"
ENV CAML_LD_LIBRARY_PATH "/home/dark/.opam/${OCAML_SWITCH}/lib/stublibs"
ENV MANPATH "/home/dark/.opam/${OCAML_SWITCH}/man:"
ENV PERL5LIB "/home/dark/.opam/${OCAML_SWITCH}/lib/perl5"
ENV OCAML_TOPLEVEL_PATH "/home/dark/.opam/${OCAML_SWITCH}/lib/toplevel"
ENV FORCE_OCAML_UPDATE 5

# disabling sandboxing as it breaks and isn't necessary cause Docker
RUN curl -sSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh | bash \
      && opam init --comp ${OCAML_SWITCH} --auto-setup --disable-sandboxing \
      && opam update \
      && opam install -y \
           ppx_deriving.4.4 \
           dune.2.0.1 \
           merlin.3.3.3 \
           ocp-indent.1.8.1 \
           ocamlformat.0.12 \
      && opam clean --all-switches --download-cache --repo-cache


ENV ESY__PROJECT=/home/dark/app/backend


############################
# Shellcheck
# Ubuntu has a very old version
############################

RUN \
  VERSION=v0.7.0 \
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
    RUST_VERSION=1.38.0

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y --default-toolchain $RUST_VERSION \
  && rustup --version \
  && cargo --version \
  && rustc --version

# install Rust dev tools
RUN rustup component add clippy-preview rustfmt-preview
RUN cargo install cargo-cache

# Once we have cargo and things installed in /usr/local/cargo and that added to PATH,
# reset CARGO_HOME so that we can use it as a project cache directory like normal.
ENV CARGO_HOME=/home/dark/.cargo

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
