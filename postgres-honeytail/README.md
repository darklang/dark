CloudSQL to Honeycomb
=====================

This is a docker image, to be run in a k8s cluster, to ingest Postgres logs from
[CloudSQL](https://cloud.google.com/sql/docs/postgres/) and send them off to
[honeycomb](https://www.honeycomb.io/).

The basic infrastructure to accomplish this requires using Cloud Pub/Sub to
send messages between CloudSQL and this program, which sends them to Honeycomb.

That is:

CloudSQL -> Stackdriver Logs -> Cloud Pub/Sub ->
cloudsqltail (this) -> honeytail -> honeycomb

Running the default command of the provided docker image will run a pipeline of
both cloudsqltail and honeytail, with the effect of reading from a specified
Pub/Sub Subscription and writing the resulting events into Honeycomb.

## Implementation

`cloudsqltail` is a small golang program that reads log messages from a Pub/Sub
Subscription, attempting to correctly order them and write them to STDOUT in a
format consumable by `honeytail`. The two details to note here are the
reordering and reformatting.

Reordering: messages in a Subscription are not guaranteed to be delivered in
any order, which is problematic because Postgres query logs can span multiple
lines.  Honeytail expects a strict ordering of log lines to be able to make
sense of the queries. `cloudsqltail` attempts to best-effort order the log
messages it pulls from the Subscription by buffering messages in memory for
some amount of time, sorting them by their nanosecond-resolution timestamp,
then outputing the sorted list. This is not perfect, but is generally good
enough that the mangled or missing query noise becomes statistically
insignificant. See the `-flush-interval` flag of `cloudsqltail` for fine tuning
the time between buffer sort/flush.

Reformatting: `honeytail` requires a timestamp for each logged query for
accurate event time, which is normally accomplished by modifying the Postgres
`log_line_prefix` configuration to add a timestamp. However, CloudSQL does not
allow configuration of this parameter. Instead, `cloudsqltail` will prepend the
timestamp associated with the Pub/Sub message to the appropriate postgres log
lines, just as if it had been added by Postgres.

## CloudSQL configuration

You'll want to turn on query logging. Assuming no custom database flags are set
on your cloudsql instance, you can run:

```
gcloud sql instances patch <instance_name> --database-flags log_min_duration_statement=0
```

Caveat 1: this requires restarting your cloudsql instance, which gcloud will do for
you.

To see what flags are set: `gcloud sql instances describe <instance_name>` and
look for the top-level key `databaseFlags`. (You'll want to include existing
flags in your `gcloud sql instances patch` command, if any are set.)

## Stackdriver Configuration

Ingestion is done via [Cloud Pub/Sub](https://cloud.google.com/pubsub/).

To use this, [set up a Stackdriver
sink](https://cloud.google.com/logging/docs/export/configure_export_v2) of your
Postgres logs to a Pub/Sub topic.  You will also need to create a
[subscription](https://cloud.google.com/pubsub/docs/subscriber) to that topic.
Note the subscription name you provide, as it is necessary to configure
cloudsqltail.

## Docker

```
docker build -t postgres-honeytail .
```

Then push to your preferred docker registry.

By default, the container will run a wrapper script `run.sh`, which takes the
output of `cloudsqltail` and sends it along to honeycomb via `honeytail`.

## Config and deploy

### Required environment variables

- `GOOGLE_APPLICATION_CREDENTIALS_JSON` is used to auth to gcloud; the service
  account must have read access to the pubsub subscription.
- `PROJECT_ID` the gcloud project the pubsub subscription is in
- `SUBSCRIPTION_NAME` the pubsub subscription name
- `HONEYCOMB_WRITEKEY` (not required if `DEBUG` is set, see below)

### Optional environment variables

- `DEBUG` runs `honeytail` with the flags `--debug` (setting the log level to
  DEBUG) and `--debug_stdout` (writing events to stdout instead of sending to
  honeycomb).  The latter flag also means that `HONEYCOMB_WRITEKEY` is not
  verified, so it can be left unset.
- `DATASET` the honeycomb dataset to write to; defaults to `postgres`

## Development

### Go

The `cloudsqltail` command uses go modules to track it's depedencies, so you'll
need at least Go 1.11 or higher, with module mode enabled via `GO111MODULE=on`.
With >=1.12, `make` should work out of the box with no additional configuration
necessary.

### Running in Docker

`cloudtailsql` needs Google Cloud access credentials (to fetch the Pub/Sub
Subscription). To get this locally, run docker with

```
-v $HOME/.config/gcloud:/root/.config/gcloud
```

