# Logging, observability, and telemetry

We track errors and problems in a number of ways:

- rollbar
- honeycomb
- logging

Logging is only used as a last ditch attempt to write something down, in the event
that rollbar and/or honeycomb are having problems. We do not expect to see much in
stdout.

Honeycomb is where we track all interesting events, whether they are successful or
not. This is all about understanding the context of how the service is run, and only
incidentally for checking errors.

Rollbar is where we track exceptions, along with stack-traces and whatever context we
can muster. We also report some "notices", such as admin actions, deploys, etc.

As such:

- if an error occurs, notify rollbar and honeycomb, also print a log cause why not
- if there's an action of note, for example using ProdExec, it should get a rollbar notification
- web requests, executions, DB calls, etc, should all honeycomb

We use honeycomb via the OpenTelemetry tracing in .NET.
