# Honeycomb Client Logger

## Goals

We'd like to be able to send the logs that go to Honeycomb elsewhere (for
longer-term storage); we want this split to happen _after_ the Honeycomb client
(whether it's honeytail or honeycomb-kubernetes-agent or a beeline) does any
processing.

## Use

We provide Dockerfile containing an nginx proxy that does this.

You must provide the container with a `DATASETS_TO_LOG` env var. It may be
either a pipe-delimited list of datasets:
`dataset1|dataset2|dataset3`
or a match-all:
`.*`

Then, override your honeycomb client's `API Host` to point at the nginx proxy,
on port 8010; depending on your client, this might be an env var, a key in yaml
or INI config files, or a flag. (`honeytail` uses `--api_host`, for instance.)

Having done that:
- data will be proxied to `https://api.honeycomb.io:443`
- events sent to honeycomb (either as single events to /1/events/:dataset or in
  a batch to /1/batch/:dataset) will be logged, and prefixed with the name of
  the dataset

For example (see below), posting 4 events to a dataset
called 'sandbox', stdout from the container looks like:
```
sandbox [{"data":{"bar":3},"time":"2020-05-15T18:27:53.167828126Z"},{"data":{"alpha":"a","beta":"b"},"time":"2020-05-15T18:27:53.167971161Z"},{"data":{"foo":1},"time":"2020-05-15T18:27:53.167979722Z"},{"data":{"last":"done"},"time":"2020-05-15T18:27:53.168155905Z"}]
```

## Development & testing
1. To build: `docker build -t honeycomb-logger -f honeycomb-logger.Dockerfile .`

2. Run the container using `dev-stuff/runner.sh`

3. Find the container name using `docker ps`, and exec into it:
   `docker exec -it <container_name> bash`

4. Once in the container, run `./installer.sh`.

5. You can now run `./honeytail-runner.sh` to send the contents of `sample-data.json` to honeycomb. You will need to use your api key for `--writekey`, and you may wish to change which `--dataset` you send to.

6. Expected output from the container is shown above; `honeytail-runner.sh` also
   has some output logs, including a line indicating that you successfully sent
   4 events: `number sent by response status code=map[202:4] total attempted sends=4`.
