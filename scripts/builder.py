#!/usr/bin/env python3

import sys
import datetime
import fileinput
import subprocess

def anyof(_dir, _list, x):
  for l in _list:
    if (_dir + "/" + l) in x:
      return True
  return False

def call(bash):
  print("calling: " + bash)
  subprocess.call(bash, shell=True)

def reload_server():
  call("scripts/runserver")
  pass

def reload_browser():
  call("osascript scripts/chrome-reload")

print ("Starting " + (datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")))

for f in sys.stdin:
  if False:
    pass

  # General stuff
  elif ".git" in f:
    pass

  # Ignore scripts
  elif "scripts/" in f:
    pass

  # Ignore logs
  elif "logs/" in f:
    pass

  # Ocaml
  elif "ocamlserver/_build/" in f:
    pass
  elif anyof("ocamlserver", ["setup.log", "main.native"], f):
    pass
  elif "_oasis" in f:
    call("cd ocamlserver && oasis setup -setup-update=weak && make")
    reload_server()
    reload_browser();
  elif ".ml" in f:
    call("cd ocamlserver && make")
    reload_server()
    reload_browser();
  else:
    print("unknown file: " + f)

print ("Done " + (datetime.datetime.now().strftime("%I:%M%p on %B %d, %Y")))
