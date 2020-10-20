#!/bin/bash

set -euo pipefail

cd Build/out/wwwroot
python3 -m http.server 2345
