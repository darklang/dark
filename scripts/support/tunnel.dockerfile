FROM ubuntu:18.04

USER root

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get install \
      -y \
      --no-install-recommends \
      curl \
      ssh

RUN mkdir /run/sshd
RUN adduser --disabled-password --gecos '' tunnel

COPY tunnel-daemon /usr/local/bin/tunnel-daemon

EXPOSE 1080/tcp
CMD /usr/local/bin/tunnel-daemon
