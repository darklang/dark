# Events V2 (updated May 7, 2022)

## Design of Events V2

Goals:
- do not DB queries to find events to run (scheduling outside DB)
- support existing queue features (paused/blocked)
- preferably do not run a scheduling service

This is accomplished by using Google Cloud Pub/Sub to schedule events, and use the DB
as the actual source of truth for the event.

## What is an Event?

Events are generated any time a handler calls `emit` or automatically by a cron
handler on a set interval. They have a specific destination handler and a
lifecycle of processing. Events are stored mutably in Postgres, in the `eventsV2`
table. Completed events are deleted.

The scheduling of events is handled by a Pub/Sub subscription, which strictly only
handles the notification of the worker to try running an event. The presence of an
event in the subscription does not mean an event, since Pub/Sub does not handle
pausing.

### Scheduling related metadata

- `delayUntil` is used to delay processing of an event until a specified time
- `retries` counts the number of times this event has been retried on error
- `lockedAt` a worker will claim a lock before processing. DB locks are bad for this
  purpose since we're doing DB work at the same time. This "lock" is simply a timestamp to stake a claim.
- `enqueuedAt`: set when first enqueued

### Metadata

- `canvas_id`, `name` together specify the exact handler (`name`) on which
  canvas to execute when this event is processed.
- `value` is the emitted value (`DObj`) that was emitted to the handler

## DB schema

### Events

```
      Column       |            Type             |                      Modifiers
-------------------+-----------------------------+-----------------------
 id                | integer                     | not null default nextval ('events_id_seq'::regclass)
 canvas_id         | uuid                        | not null
 module            | text                        | not null
 name              | text                        | not null
 modifier          | text                        | not null
 value             | text                        | not null
 retries           | integer                     | not null default 0
 delay_until       | timestampz | not null default now()
 enqueued_at       | timestampz | not null default now()
 locked_at         | timestampz
```

### Scheduling rules

Code in `EventQueue.fs`. Allows a user to pause a queue, or allows an admin to lock a  queue for operational purposes.

```
 id           | integer                     | not null default nextval('scheduling_rules_id_seq'::regclass)
 rule_type    | scheduling_rule_type        | not null
 canvas_id    | uuid                        | not null
 handler_name | text                        | not null
 event_space  | text                        | not null
 created_at   | timestamp without time zone | not null default now()
```

#### Scheduling_rules_type

```
enumlabel
----------------------
 pause
 block
```

## Emit

Done in `LibEvent` via `emit`, or automatically via `CronChecker`. Calls
`EventQueue2.enqueue`. This adds a new value to the events table with:

- `retries = 0`
- `locked_at = NULL`
- `delay_until = CURRENT_TIMESTAMP`
- `enqueued_at = CURRENT_TIMESTAMP`

Note that `CronChecker` does not use event table information to schedule, it uses
`cron_records`.

Emit also adds an event to the PubSub topic. This will be delivered to a worker to
tell it to try fetching and running an event. The PubSub worker has:

- `retries not set`
- `delay_until not set`

## Execution

`EventQueue2.dequeue` fetches a notification from PubSub and runs the process to
execute it. First it will check if it should run it, looking at retries, whether
another worker has set `locked_at` and holds the lock, whether scheduling rules tell
us not to run it, whether it's not time to run it yet, or if the event is missing.

In most cases it put the event back in PubSub (except for the situations where that
doesn't make sense). Because the event is just a notifcation, it's basically almost
always fine to put it back in so long as the record exists (which means it's done).


```mermaid
  %% Happy path
  Queue[Queue] -->|Receive notification| EventLoad
  EventLoad --> |EventPresent| PauseCheck
  PauseCheck -->|PauseNone| DelayCheck
  DelayCheck -->|DelayNone| LockCheck
  LockCheck -->|LockNone| LockClaim
  LockCheck -->|LockExpired| LockClaim
  LockClaim --> |Claimed| Process
  Process --> |ExecutesToCompletion| Delete
  Delete --> End

  %% Error checking

  %% Done, so don't requeue
  EventLoad -->|EventMissing| End

  %% Might need to try again if the locked item fails
  LockCheck -->|IsLocked| Queue

  %% We got this too early
  DelayCheck -->|DelayNotYet| Queue

  %% Drop the notification, we'll reload it upon unpause
  PauseCheck -->|Paused| End
  PauseCheck -->|Blocked| End

  %% Probably another event beside us, no harm requeuing
  LockClaim --> |LockClaimFailed| Queue

  %% Allow retries
  Process --> |Exception| RetryCheck
  RetryCheck --> |RetryAllowed: increment, delay| Queue

  %% That's enough, we're done
  RetryCheck --> |RetryTooMany| Delete
  %% Will probably be dropped in EventMissing
  RetryCheck --> |Exception| Queue
```
