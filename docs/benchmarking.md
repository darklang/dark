# Some notes on benchmarking

## General

- Benchmarking in a VS Code container doesn't work - VS Code seems to do something to each request
- the F# server will happily spread out to lots of cores and it's hard to siturate it. At `--cpus 2` it's easier to watch
- ensure you optimize by passing the `--optimize` flag to `scripts/builder` or `scripts/build/_build-server`

## Profiling .NET

- dotnet tool install dotnet-trace
- dotnet trace ps # get id of BwdServer
- dotnet trace collect --format SpeedScope -p ID
- upload the file to https://speedscope.app for a flame graph

## Using ab

- sudo apt install apache2-utils # install
- ab -n 2000 -c 50 URL # good settings
