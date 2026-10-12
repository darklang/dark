# The published site, and nothing else. Build context: deploy.sh stages `site/` from
# rundir/wasm-repl/wwwroot. brotli_static serves the publish's own quality-11 .br beside each
# file, gzip_static the .gz for a client without brotli. Alpine's nginx rather than the stock
# image, because only Alpine packages the brotli module; without it the .br went unused and
# Fly's edge re-compressed the gzip on the fly, 12 to 13 MB for a wasm that is 7.4 MB at
# quality 11. Everything unfingerprinted is no-cache: a cached cli.html outliving a deploy ran
# an old page against a new runtime.
FROM alpine:3.20
RUN apk add --no-cache nginx nginx-mod-http-brotli
RUN cat > /etc/nginx/nginx.conf <<'NGINX'
include /etc/nginx/modules/*.conf;
worker_processes 1;
events { worker_connections 256; }
http {
  include /etc/nginx/mime.types;
  types { application/wasm wasm; application/octet-stream db br dat blat; application/json json; }
  default_type application/octet-stream;
  sendfile on;
  brotli_static on;
  gzip_static on;
  gzip_vary on;
  server {
    listen 8080;
    root /site;
    index index.html;
    location / { try_files $uri $uri/ =404; add_header Cache-Control "no-cache"; }
    location /_framework/ { add_header Cache-Control "public, max-age=31536000, immutable"; }
  }
}
NGINX
COPY site /site
EXPOSE 8080
CMD ["nginx", "-g", "daemon off;"]
