# The published site, and nothing else. Build context is rundir/wasm-repl/wwwroot after
# `dotnet publish` + `make-store.sh`; see backend/src/Wasm/README.md.
FROM nginx:1.27-alpine
COPY nginx.conf /etc/nginx/nginx.conf
COPY site /site
EXPOSE 8080
