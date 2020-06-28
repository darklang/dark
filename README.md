# Dark

## Contributing

We have a [set of contributor
docs](https://darklang.github.io/docs/contributing/getting-started) to guide
you through your first PR, find good projects to contribute to, and learn about
the code base, available on our docs site.

## Getting Started

### Install dependencies

To build and run the server you must have the following installed (and running):

- Mac only: Homebrew for Mac (https://brew.sh/)
- Mac only: Docker for Mac (https://docs.docker.com/docker-for-mac/install/)
- Mac only: The latest version of bash `brew install bash`
- fswatch `brew install fswatch` / `apt install fswatch`
- PIP `brew install python` / `apt install python3-pip`
- live reload `pip3 install livereload`

### Docker
The app and its dependencies are all held within the container. While code is edited on your machine, the application is compiled and run inside of the container.

Ensure that docker:
- set to use 4 CPUs, 4.0 GiB of Memory, and 4.0 GiB of Swap (under the Advanced preferences tab).

Ignore the other tabs (for example you don't need to enable Kubernetes).

### Dnsmasq

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

### Building and running for the first time

- Run `scripts/builder --compile --watch --test`
- Wait until the terminal says "Finished initial compile" - this means the
  build server is ready. The `builder` script will sit open, waiting for file
  changes in order to recompile
- If you see "initial compile failed", it may be a memory issue. Ensure you
  have docker configured to provide 4GB or more of memory, then rerun the builder
  script. (Sometimes just rerunning will work, too).
- Open your browser to http://darklang.localhost:8000/a/dark/, username "dark",
  password "what"
- Edit code normally - on each save to your filesystem, the app will be rebuilt
  and the browser will reload as necessary

## Read the contributor docs

If you've gotten this far, you're now ready to [contribute your first PR](https://darklang.github.io/docs/contributing/getting-started#first-contribution).


## Testing

Unit tests run when you specify `--test` to `scripts/builder`. You can run them as a once off using:

- `scripts/runtests` # client
- `scripts/run-backend-tests`
- `scripts/run-rust-tests`

Integration tests:

- `scripts/run-in-docker ./integration-tests/run.sh`

You can also run integration tests on your (host) machine, which gives you some debugging ability, and typically runs faster:

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

## Debugging the client

You can enable the FluidDebugger by mousing over the Gear in the
left-sidebar. There is also "Enable debugger" which enables a legacy
debugger that nobody uses and doesn't work well.

If you're using Chrome, enable Custom Formatters to see OCaml values in
Chrome Dev Tools instead of their JS representation. From within Chrome
Dev Tools, click "⠇", "Settings", "Preferences", "Enable Custom
Formatters".


## Other important docs:

- [Contributor docs](https://darklang.github.io/docs/contributing/getting-started)
- [Other ways to run the dev container](docs/builder-options.md)
- [Setting up your editor](docs/editor-setup.md)

### Less important docs
- [Running the client against production (ngrok)](docs/running-against-production.md)
- [Oplist serialization](docs/oplist-serialization.md)
- [Intricacies of Bucklescript-tea](docs/bs-tea.md)
- [Writing Stdlib docstrings](docs/writing-docstrings.md)
- [Debugging PPXes](docs/debugging-ppxes.md)
- [Editing other BS libraries](docs/modifying-libraries.md)
- [Add an account for yourself](docs/add-account.md)
