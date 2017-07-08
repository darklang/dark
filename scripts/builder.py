#!/usr/bin/env python3

import sys
from subprocess import call

for f in sys.argv:
  if False:
    pass
  # General stuff
  elif ".git" in f:
    pass

  # Builder
  elif "scripts/builder.py" in f:
    pass
  elif "scripts/.#builder.py" in f:
    pass

  # Ocaml
  elif "ocamlserver/_build/" in f:
    pass
  elif "_oasis" in f:
    call("cd ocamlserver && oasis setup -setup-update=weak", shell=True)
  elif ".ml" in f:
    call("cd ocamlserver && make", shell=True)
  else:
    print("unknown file: " + f)
