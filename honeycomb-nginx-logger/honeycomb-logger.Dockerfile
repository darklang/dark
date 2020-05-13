FROM nginx:1.16.1

RUN DEBIAN_FRONTEND=noninteractive\
  apt update --allow-releaseinfo-change && \
  apt install \
    -y \
    --no-install-recommends \
    nginx-extras \
    lua-zlib \
    vim \
    procps \
    && apt clean \
    && rm -rf /var/lib/apt/lists/*

ADD nginx.conf /etc/nginx/conf.d/nginx.conf
ADD inflate_body.lua /usr/share/nginx/inflate_body.lua
ADD docker-runner.sh /docker-runner.sh
RUN chmod +x docker-runner.sh

EXPOSE 8010
CMD bash -c "/docker-runner.sh"
