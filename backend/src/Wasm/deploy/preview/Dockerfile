# nginx over a volume of published sites. The image holds no site: host.sh puts them in
# /data/<path>/ and regen-index rewrites the directory page there.
FROM alpine:3.20
# Alpine's nginx, because only Alpine packages the brotli module the store is served with.
RUN apk add --no-cache nginx nginx-mod-http-brotli
COPY nginx.conf /etc/nginx/nginx.conf
COPY regen-index /usr/local/bin/regen-index
COPY gone.html preview.css /usr/share/preview/
EXPOSE 8080
CMD ["nginx", "-g", "daemon off;"]
