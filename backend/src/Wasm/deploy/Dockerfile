# The published site, and nothing else. Build context: deploy.sh stages `site/` from
# rundir/wasm-repl/wwwroot. gzip_static serves the .gz beside each file (the publish writes
# .br too, but stock nginx has no brotli module). Everything unfingerprinted is no-cache: a
# cached cli.html outliving a deploy ran an old page against a new runtime.
FROM nginx:1.27-alpine
RUN cat > /etc/nginx/nginx.conf <<'NGINX'
worker_processes 1;
events { worker_connections 256; }
http {
  include /etc/nginx/mime.types;
  types { application/wasm wasm; application/octet-stream db br dat blat; application/json json; }
  default_type application/octet-stream;
  sendfile on;
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
