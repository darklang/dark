#!/usr/bin/env bash

# Not using assert-in-container because this runs a docker container - the nginx
# container - and so doesn't run inside the dark builder container.

set -euo pipefail

if [ -v CI ]; then
  # see "copy in data" in .circleci/config.yml
  MOUNTS="--volumes-from vols"
else
  MOUNTS="-v $(pwd)/scripts/support/base-nginx.conf:/etc/nginx/nginx.conf "
  MOUNTS+="-v $(pwd)/scripts/support/nginx.conf:/etc/nginx/conf.d/nginx.conf"
fi

# Mount base-nginx.conf and nginx.conf, then test the configuration (nginx -t),
# using the same docker image we use in prod.
docker run \
  $MOUNTS \
  -t nginx:1.15.3 \
  nginx -t

