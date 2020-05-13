#!/bin/bash

set -euo pipefail

apt update
apt install -y curl vim procps less

curl -L -o honeytail https://honeycomb.io/download/honeytail/linux/1.762 && \
      echo '00e24441316c7ae24665b1aaea4cbb77e2ee52c83397bf67d70f3ffe14a1e341  honeytail' | sha256sum -c && \
      chmod 755 ./honeytail && \
      mv ./honeytail /usr/bin
