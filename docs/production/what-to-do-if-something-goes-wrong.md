## The darklang incident playbook

Something is down, going wrong, alerts are firing. What to do.

## Incident response

- Comment in the #status room on slack. Explain what the user visible impact
  is, and that you are working on it. If you know how to solve it, say so. If
  you are investigating, say so.

- As you work to resolve the problem, document what you are doing and learning
  in #operations.

- Update the #status room often, probably every 30 minutes, even if there is no
  new information.

## What you're looking for

## What could be wrong

Typically, you have some evidence of something going wrong. You need to turn that
into a cause, and then find a way to undo that.

The most likely candidates are:

- bad/broken code got deployed
- bad/broken k8s configuration was set
- a user, canvas, or handler that's going wild or hitting a slow part of our code
- a denial-of-service attack
  - however, it's much more likely an accident by a user
- something is going wrong with the DB
  - a bad migration or manual DB query
  - perhaps it's our quarterly DB maintenace: is it Tuesday or Wednesday around 11am
    EST?
- a server (kubernetes deployment) won't restart

Less likely candidates:

- google cloud is having an outage
- one of the `ops` canvases isn't working
- GKE has upgraded kubernetes from beneath our feet again
- something unexpected happened during CI deploy
- a cert ran out

## Starting points

Start with whatever external signs you have:

- pagerduty is triggered by something - follow that to get an indicator

- if the DB is down, check the Cloud SQL dashboard. Also check for long running
  queries, especially if a migration is running.
- If the server isn't responding to requests, check kubernetes workloads to see if
  it's up or being restarted
- if you can't connect over HTTP, check the load balancers
- did something just deploy? Probably that
- did something just change? Usually that
- HTTP requests have an `x-darklang-execution-id` header: you can search for this in honeycomb

Can you pinpoint a deploy? A service? A user? A canvas? A handler?

If you have no specific indicator of a problem, some starting points:

- look at exceptions in rollbar
- look at the CPU graph in Cloud SQL
- look at the boards in honeycomb
- check the kubernetes console: perhaps something is down or constantly restarting
- search honeycomb kubernetes-bwd-ocaml
  - group by `service.name` and `name`. Or if you know what's down, use the right service:
    - Crons: `service.name = CronChecker`
    - Queues and crons: `service.name = QueueWorker`
    - builtwithdark or custom domains: `service.name = BwdServer`
    - editor: `service.name = ApiServer`
  - search exceptions with `exception = true`
  - use `duration_ms`
    - `heatmap(duration_ms)` then "bubble up"
    - `sum(duration_ms)` can often be really interesting
  - you're trying to find a pattern of something in the graphs. A spike or a
    drop is usually a strong indicator that something changed, especially after
    a deploy (the vertical lines in Honeycomb graphs).
  - Can you pinpoint a deploy? A service? A user? A canvas? A handler?

## Detailed information

### Rollbar

Rollbar is useful when there are lots of exceptions firing. If not, you won't find
anything useful here.

The exceptions

- has stacktraces
- often has extra attributes
- has a link to a trace in honeycomb which may have more

### Cloud SQL

Our DB is "dark-west" in Google Cloud SQL. If you go there, usually the CPU
graph tells you how bad things are. If the CPU looks OK, it's usually not the
DB.

You can also check long running queries in the dashboard, in the postgres
dataset in honeycomb (there's some useful boards here), by searching honeycomb
kubernetes-bwd-ocaml dataset for `name = postgres`, group by the SQL query.

### Kubernetes

Kubernetes has lots of information on why something might be going wrong

- if the service is restarting or won't start, try the logs:
  - eg `kubectl logs -n darklang deploy/apiserver-deployment -c apiserver-ctr`
- the GKE dashboard shows you CPU and memory use. Look at the 7 day chart to
  see what you expect them to be and compare them to right now.
- the GKE workloads show you if things are restarting. Sometimes clicking through
  will tell you more, or highlight an error
- if the service is down, the GKE ingress/service dashboard shows you healthchecks
- google's monitoring can help you find logs that often don't exist elsewhere (eg
  ingress requests). However, they can be challenging to find.

### Where are the logs?

- typically, the "logs" are in honeycomb as structures and searchable events.
  If you want to follow one machine's "logs", group by on `server.machine.host`
  and then view only the one you're interested in.
- if something is really wrong, the logs won't be sent and you might see an
  exception in the container. Use `kubectl logs` to see it.

### Pub/Sub

Pub/Sub has a dashboard.

## What to do when you find it?

### Bad deploys

These can be rolled back: `kubectl rollout undo ...` TODO

### Bad kubernetes config

- fetch it: `kubectl get -n darklang cm/myconfig -o yaml > myconfig.yaml`
- edit `myconfig.yaml`
- the devcontainer has linters (make sure the k8s version matches the current
  production version, which upgrades dynamically)
- if it's a bad deploy or k8s config, roll it back

### DB issues:

#### In just one service?

- you can pause queues or crons at a coarse level by scaling the service to zero

#### Handling a runaway query

- log into gcp psql
- use the famous pg query to find the issue
- kill the query

### Handling accidentally DOS by a customer

- manually disable it:
  - For queues, pause the pause button
  - for any handler:
  - delete part of the header to make it stop running
  - disable a handler by putting the current code inside an `if false` expression
  - For http queries:
    - are we making them too? Find the handler and disable it
- tell the user: add a
  `let _ = "Change made by YOURNAME to prevent operational issues, please do not reenable"` comment
- Send the user and DM or email (get their email from `darklang.com/a/ops-adduser`)

### Deliberate DOS or cannot stop the other way

- You can prevent HTTP traffic to darklang.com, builtwithdark.com, or
  static.darklang.com or darksa.com by adding AppArmor rules to disable the
  traffic.
- You can prevent HTTP traffic to custom domains by adding config to the
  nginx-controller's nginx.conf
- You can disable queues by adding a scheduling rule (LibDarkInternal). Solves crons
  too.
- You can safely add launchdarkly flags to anything to be able to selectively disable
  them
  - you may need to add a new LaunchDarkly flag and redeploy to be able to
    selectively disable
- How do we disable users? Probably something we can do by a) removing all their
  sessions and b) preventing login via auth0.

## Other services:

- user signup happen on the ops-adduser canvas (accounts.darklang.com)
- user signin happens via login.darklang.com
- presence notifcations are on the ops-presence canvas (presence.darklang.com)
