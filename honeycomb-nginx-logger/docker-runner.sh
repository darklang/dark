#!/bin/bash

if [ -z "${DATASETS_TO_LOG:-}" ]; then
    echo "You must set a value for \$DATASETS_TO_LOG."
    echo "It can be either a |-delimited list - 'sandbox|sandbox2' - or"
    echo "'.*' to log all datasets"
    exit 1
fi

sed -i'' "s/DATASETS_TO_LOG/${DATASETS_TO_LOG}/" /etc/nginx/conf.d/nginx.conf

exec nginx -g "daemon off;"
