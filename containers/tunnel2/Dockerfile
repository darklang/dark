FROM ubuntu:20.04

# CLEANUP don't run as root
USER root

RUN DEBIAN_FRONTEND=noninteractive \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get install \
      -y \
      --no-install-recommends \
      curl \
      ssh \
      ca-certificates

RUN mkdir /run/sshd
RUN adduser --disabled-password --gecos '' tunnel

USER tunnel

COPY containers/tunnel/tunnel-daemon /usr/local/bin/tunnel-daemon

EXPOSE 1080/tcp
CMD /usr/local/bin/tunnel-daemon
