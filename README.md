# Dark

## Getting Started

### Requirements

To build and run the server you must have the following installed (and running):

- Mac only: Homebrew for Mac (https://brew.sh/)
- Mac only: Docker for Mac (https://docs.docker.com/docker-for-mac/install/)
- Mac only: The latest version of bash `brew install bash`
- fswatch `brew install fswatch` / `apt install fswatch`
- PIP `brew install python` / `apt install python3-pip`
- live reload `pip3 install livereload`

### First time setup

#### Docker
The app and its dependencies are all held within the container. While code is edited on your machine, the application is compiled and run inside of the container.

Ensure that docker:
- set to use 4 CPUs, 4.0 GiB of Memory, and 4.0 GiB of Swap (under the Advanced preferences tab).
- more than that significantly increases build time

Ignore the other tabs (for example you don't need to enable Kubernetes).

#### Dnsmasq

A local DNS server is needed to access the application via a `.localhost` TLD. The following is a quick start, adapted from [this guide]( https://passingcuriosity.com/2013/dnsmasq-dev-osx/).

Install dnsmasq:

```
brew install dnsmasq / apt install dnsmasq
```

Follow brew's post-install instructions:
```
brew info dnsmasq
```
(probably `sudo brew services start dnsmasq`)

Add the following to `(brew --prefix)/etc/dnsmasq.conf`
```
address=/localhost/127.0.0.1
```

Restart dnsmasq:
```
sudo brew services restart dnsmasq / sudo /etc/init.d/dnsmasq restart
```

Configure OSX to use dnsmasq (not needed on linux):
```
sudo mkdir -p /etc/resolver
sudo tee /etc/resolver/localhost >/dev/null <<EOF
nameserver 127.0.0.1
EOF
```

Test it:
```
# Make sure you haven't broken your DNS.
ping -c 1 www.google.com
# Check that .localhost names work
dig testing.builtwithdark.localhost @127.0.0.1
```

#### Building and running for the first time

- Run `scripts/builder --compile --watch --test`
- Wait until the terminal says "Finished initial compile" - this means the build server is ready
- If you see "initial compile failed", it may be a memory issue. Ensure you
  have docker configured to provide 4GB of memory.
- Open your browser to http://darklang.localhost:8000/a/dark/, username "dark", password "what"
- Edit code normally - on each save to your filesystem, the app will be rebuilt and the browser will reload as necessary


## Other ways to run the dev container

### Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

### (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker invocations, including say ocamlmerlin, use the old+running container as opposed to attempting to use the container
that has an in progress build.

### Preserving battery life

The're a poll during building that is a great drain on battery life. To
reduce the frequency of the poll, run script/builder using
POLL_FREQUENCY, which is the number of times per second to check.

- `POLL_FREQUENCY=0.1 scripts/builder --etc`

You can also disable the polling (and consequently the building):

- `scripts/builder --compile --serve`


## Testing

To run unit tests:

- `scripts/builder --compile --test`

Integration tests:

- `scripts/run-in-docker ./integration-tests/run.sh`

You can also run them on your machine, which gives you some debugging ability, and typically runs faster:

- `./integration-tests/run.sh`

There are good debugging options for integration testing. See integration-tests/README.

## Running unix commands in the container

- `scripts/run-in-docker bash`

## Accessing the local db

- `scripts/run-in-docker psql -d devdb`

## Config files

Config files are in config/. Simple rule: anything that runs inside the
container must use a DARK_CONFIG value set in config/, and cannot use
any other env var.

## Setting up your editor

### Merlin

Merlin is an OCaml language server that provides things like autocompletion,
type information, and go-to-definition functionality. Because all OCaml
libraries are installed inside the container and not on your local machine,
merlin needs to be running inside the container. Unfortunately, this install is
not completely isolated and we still need some supporting things installed on
your host machine.

The way this works is that you'll need merlin on your host machine (to get the editor
support files) and then you'll point those at the `scripts/ocamlmerlin` wrapper
to execute the actual process inside the container. This means that you need
those scripts first in your `$PATH`.

- `export $PATH=[path to dark]/scripts:$PATH` in your shell config
- Install merlin:
  - `brew install opam`
  - `opam init -c 4.06.1`
    - grep the `Dockerfile` for `OCAML_SWITCH` to ensure the version matches
    - copy snippet to your bashrc/shell config. DO NOT DO THIS FOR zsh. SEE BELOW.
  - `opam install merlin.3.2.2`
- Install editor integration:
  - vim+plug: `Plug '~/.opam/default/share/merlin', { 'for': ['ocaml', 'merlin' ], 'rtp': 'vim' }`
  - others: ??

CAVEAT: If you install the zsh config that merlin recommends, it will
automatically execute the equivalent of `eval $(opam env)` after every command.
This command mucks with your `$PATH`, which means that you will never execute
the `scripts/` wrappers you want. Do not use the shell integration. Instead
call `eval $(opam env)` and then `export $PATH=[path to dark]/scripts:$PATH` (put
this in an alias or something).

### Formatting

You will also want to support formatting in your client. Dark uses Prettier for Js/Html/CSS, and OCamlformat for OCaml and Bucklescript. The script `script/format` can be used to format or check formatting, and there is a pre-commit hook you can use to run it automatically.

For emacs, see [the
readme](https://github.com/ocaml-ppx/ocamlformat#emacs-setup). For vim:
- install [ALE](https://github.com/w0rp/ale)
- Add to your `.vimrc` (with an appropriate path-to-dark replacement):
```
set rtp+=~/[path to dark]/dark/scripts/ocamlformat
let g:ale_javascript_prettier_executable= '~/path to dark]/dark/scripts/prettier'
let g:ale_fixers =
\ {'rust': ['rustfmt'],
\  'ocaml':['ocamlformat'],
\  'javascript': ['prettier'],
\  'js': ['prettier'],
\  'html': ['prettier'],
\  'css': ['prettier'],
\  'scss': ['prettier']}
```

### Pre-commit hook

You probably also want to install a pre-commit hook that the formatters for
you.
`cp scripts/pre-commit-hook.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit`


## Debugging the client

You can enable the FluidDebugger by mousing over the Gear in the
left-sidebar. There is also "Enable debugger" which enables a legacy
debugger that nobody uses and doesn't work well.

If you're using Chrome, enable Custom Formatters to see OCaml values in
Chrome Dev Tools instead of their JS representation. From within Chrome
Dev Tools, click "⠇", "Settings", "Preferences", "Enable Custom
Formatters".

## Running the client against production

If you want to run your local client against the production server, we have a way to do that using ngrok:

- Join our ngrok account by asking Paul
- install ngrok: `brew cask install ngrok` on OS X, or follow instructions at
  https://ngrok.com/download for Linux. (The snap installer is broken.)
- authorize your ngrok client: `ngrok authtoken YOURTOKEN` (found at
  https://dashboard.ngrok.com/auth)
- run your tunnel: `ngrok http 8000 -hostname=darklang-<username>.ngrok.io`
- You can simplify this to `ngrok start darklang` by adding to your
  `~/.ngrok2/ngrok.yml`:
```yaml
tunnels:
  darklang:
    proto: http
    addr: 8000
    subdomain: darklang-<username>
```

Use the queryparam "localhost-assets=<username>" to load static assets from darklang-<username>.ngrok.io instead of static.darklang.com.

You can check if it's going through via the ngrok console (which logs requests), and by tailing the server logs: `tail -f rundir/logs/server.log`.


## Editing other BS libraries

We sometimes have to edit other bs libraries in tandem with our codebase, which
is a little challenging. Here are the steps to make it work:

In client/package.json
```
-    "bucklescript-tea": "darklang/bucklescript-tea#master",
+    "bucklescript-tea": "file:../../bucklescript-tea",
```

In scripts/builder:
```
+  MOUNTS="$MOUNTS --mount type=bind,src=$PWD/../bucklescript-tea,dst=/home/dark/bucklescript-tea"
```

## Debugging ppx stuff

PPX is an ocaml preprocessor we use. The ppx libraries are all pretty
opaque. To read the preprocessed output, run dune with the --verbose
flag to see the commands run. You'll see something that looks like this:

```
./.ppx/landmarks.ppx+lwt.ppx+ppx_bin_prot+ppx_deriving.std+ppx_deriving_yojson+ppx_fields_conv+ppx_sexp_conv+ppx_pipebang/ppx.exe --dump-ast --cookie 'library-name="dark"' -o lib/types.pp.ml --impl lib/types.ml
```

Run that without the --dump-ast and then look at the .pp.ml file to find
the preprocessed version.

## Get a Dark account for yourself 

(note: this appears to be broken at the moment)

To add your account to local dev for yourself, run:
```
scripts/run-in-docker _build/default/backend/bin/add_admin.exe --prompt-for-password
```
which will prompt you for your password, username, email, and name.

This will output
```
  upsert_admin_exn
      { username = "YOURNAME"
      ; password =
          Password.from_hash
            "..."
      ; email = "developer@example.com"
      ; name = "Ada Lovelace"};
```

Dark employees should open a PR adding this account data to `account.ml` in the
`upsert_admins` function.

Note: this password is _not_ used in production; it may be different
from your password on darklang.com.



## Other important docs:

- [docs/oplist-serialization.md](docs/oplist-serialization.md)
- [docs/bs-tea.md](docs/bs-tea.md)
- [docs/writing-docstrings.md](docs/writing-docstrings.md)



