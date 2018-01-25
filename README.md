# How to build and run

- Install Docker for Mac
- Run `brew install bash`
- Run `brew install fswatch`
- Run `scripts/builder --compile --watch`
- Wait til the terminal says "Starting" - this means the build server is ready
- Open your browser to http://localhost:8000/admin/ui
   - the usename is dark, password is eapnsdc (mnemonic: ellen and pauls new startup dot com)
- Edit code normally - on each save, the app will be rebuilt and the browser will reload as necessary

# Running conduit

- Run `./scripts/run-conduit-frontend.sh`
- Go to `http://localhost:8001`
- This will use http://conduit.localhost:8000 as its server, so use http://conduiit.localhost:8000/admin/ui to create the UI.

# How to profile
- make compilation and run use OCAML_LANDMARKS="auto,format=json,output=profile.json,allocation", in scripts/support/compile (enable the `profile` global)
- rm server/_build
- run the builder
- refresh the page 10 times, then go to localhost:8000/admin/api/shutdown (shuts
  down the server and saves the profiling info)
  - if the time is all spent in twitter, delete server/profile.json and
    try again
- upload the profile.json to https://lexifi.github.io/landmarks/viewer.html
- look in "Source Tree Time"


