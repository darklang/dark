# nginx over a volume of published sites. The image holds no site: host.sh puts them in
# /data/<path>/ and regen-index rewrites the directory page there.
FROM nginx:1.27-alpine
COPY nginx.conf /etc/nginx/nginx.conf
COPY regen-index /usr/local/bin/regen-index
COPY gone.html preview.css /usr/share/preview/
EXPOSE 8080
