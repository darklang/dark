# Stroller - push proxy

Stroller runs alongside the OCaml backend processes to offload I/O-intensive
operations onto. Currently the only such operation supported is sending events
in realtime ("push") to the editor. OCaml doesn't offer great support for
concurrency, and we don't want the server process to block other requests while
performing I/O.

Stroller is implemented in Rust to provide low latency and efficient
concurrency.

## Design

Stroller has very little business logic, to ensure that most uses just require
changing OCaml code and not also changing Rust code. The idea is that it should
be as transparent as possible.

When used to send an event to Pusher, stroller just passes through the JSON
bytes that it received in the POST request. This means that the schema of JSON
events can be defined in OCaml code (client and backend), without needing to
also implement any JSON parsing or generation in Rust.

## Running stroller

In dev, stroller runs as a background process in the dev container. Like the
other processes, changing the stroller code will rebuild stroller and restart
the running process automatically.

In prod, stroller runs as a sidecar container alongside server.exe and
queue_worker.exe, so that those processes can connect via loopback to minimise
latency.

## Push details

Stroller uses [Pusher](https://pusher.com/) to push events to the editor.
Stroller itself maintains a semi-persistent HTTP connection (via HTTP
keep-alive) to Pusher, while the editor opens a persistent connection to Pusher
on initial page load.

Eventually we plan to replace Pusher with a direct websocket connection between
the editor and stroller. We use Pusher for now to defer the work of setting up
HTTP routing down to stroller, ensuring that our layers of HTTP proxying handle
persistent connections appropriately, and having to manage the list of open
websocket connections.
