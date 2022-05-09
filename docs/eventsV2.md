# Events V2 (updated May 8, 2022)

## Design of Events V2

Goals:

- do not do DB queries to find events to run (that is, move scheduling outside DB)
- support existing queue features (paused/blocked, retries, delays)
- do not run a scheduling service

## High level description

The queue implements the following features:

- events are added by calling `emit`
- events are run in `WORKER` handlers on the canvas with the same name that the event was emitted to
- events are run asynchronously on separate worker machines
- workers can be paused
- workers can be blocked by Dark admins to prevent operational issues
- the UI shows the number of items in the queue
- at least once execution (events which experience issues after partial execution are retried)
- completed events are never retried (even if they complete with an error value,
  including `incomplete` or a type error)
- errors during execution cause events to be retried 5 minutes later (at most twice,
  after which they are dropped)

The queue also has the following accidental features:

- events do not have an execution time-limit
- events may be cancelled if the machine they are running on turns off. This should
  only happen to events that run longer than 28 seconds.

### Design of the queue

The Queue is made of 3 parts:

- `events_v2` table: the backing store and source of truth for all events
- `QueueWorker`: workers that execute events
- `Google PubSub` scheduler: sends notifications to workers to potentially an Event

When an event is first `emit`ted, we save it in the `events_v2` table, and add a
**Notification** in PubSub. The QueueWorkers pull messages from PubSub when they
have capacity. They then fetch the event from the `events_v2`, and perform some logic
to verify it should be run (eg, is this handler paused, is something else already
running this events, etc), possibly updating the stored event. It may decide to put
the notification back into PubSub if it's not time to run it yet, or it may decide
that it should not be run and to drop the PubSub notification. Completed events are
deleted.

**Important**: The scheduling of events is handled by a Pub/Sub subscription, which
**only** handles notifying the worker to try running an event. The worker may know that it should not The presence of
an event in the subscription does not mean an event.

### Design decisions

Why use PubSub?

- we spent over 50% of our DB managing worker scheduling

Why not use PubSub for everything?

- PubSub deletes messages after a week, so we'd have to implement extra (and difficult logic) to handle pausing
- Similarly, emiting events in the far future would be harder to implement
- PubSub does not support pausing
- Queues can be paused and unpaused in quick succession by the user, meaning that
  there can be multiple PubSub items for the same event in the queue.
- PubSub cannot be used to count the length of the queue, and that's a really cool feature

## DB schema

### Events

The `events_v2` table has event data

- `canvas_id`, `module`, `name`, `modifier` together specify the exact handler on
  which canvas to execute when this event is processed.
- `value` is the emitted value (`DObj`) that was emitted to the handler

It also holds scheduling metadata for an event:

- `id` an id for the event. Not the same any PubSub id.
- `delayUntil` is used to delay processing of an event until a specified time
- `retries` counts the number of times this event has been retried on error
- `lockedAt` a worker will claim a lock before processing. DB locks are bad for this
  purpose since we're doing DB work at the same time. This "lock" is simply a timestamp
  to stake a claim.
- `enqueuedAt`: set when first enqueued

```
      Column       |            Type             |                      Modifiers
-------------------+-----------------------------+-----------------------
 id                | uuid                        | not null default nextval ('events_id_seq'::regclass)
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

#### Scheduling rules

Code in `EventQueue.fs`. Allows a user to pause a queue, or allows an admin to lock a queue for operational purposes.

```
 id           | integer                     | not null default nextval('scheduling_rules_id_seq'::regclass)
 rule_type    | scheduling_rule_type        | not null
 canvas_id    | uuid                        | not null
 handler_name | text                        | not null
 event_space  | text                        | not null
 created_at   | timestamp without time zone | not null default now()
```

`scheduling_rules_type` values can be `pause` or `block`.

## How features are implemented

### Basic operation

`emit` saved the event in the DB, and sends a notification to PubSub. QueueWorkers
fetch notifications from PubSub, load the event, execute it, then delete the Event
from PubSub and the DB.

### Pausing/Unpausing

When the notification is delivered from PubSub to a QueueWorker, the QueueWorker
checks the scheduling rules for the handler. If it is blocked by an admin or paused
by the user, the PubSub notification is dropped, but the event remains in the DB.
When the handler is unpaused, notifications are added the PubSub to schedule them.

If the user pauses and unpauses in quick succession, there could be multiple
notifications in PubSub for the same event. As a result, there is per-event locking
using the `lockedAt` column.

### Errors

If a handler completes, then the Event is completed regardless of whether the outcome
is an error.

If there was an operational error, the QueueWorker will retry the event by increasing
the retry count, and adding a delay. It will put the Notification back into PubSub to
retry later. After 2 retries, it fails entirely and deletes the event.

### Emit

Done in `LibEvent` via `emit`, or automatically via `CronChecker`. Calls
`EventQueue2.enqueue`. This adds a new value to the events table with:

- `retries = 0`
- `locked_at = NULL`
- `delay_until = CURRENT_TIMESTAMP`
- `enqueued_at = CURRENT_TIMESTAMP`

Note that `CronChecker` does not use `events_v2` table information for Cron
scheduling, just for running the event after it creates it. It uses `cron_records`
for Cron scheduling.

Emit also adds an event to the PubSub topic. This will be delivered to a worker to
tell it to try fetching and running an event. The PubSub worker has:

- `retries` not set
- `delay_until` not set

### QueueWorker Execution

`EventQueue2.dequeue` fetches a notification from PubSub and runs the process to
execute it. First it will check if it should run it, looking at retries, whether
another worker has set `locked_at` and holds the lock, whether scheduling rules tell
us not to run it, whether it's not time to run it yet, or if the event is missing.

In most cases it put the event back in PubSub (except for the situations where that
doesn't make sense). Because the event is just a notifcation, it's basically almost
always fine to put it back in so long as the record exists (which means it's done).

```mermaid
graph LR
  %% # Happy path
  %% Load
  Queue -->|Receive notification| EventLoad
  EventLoad --> |EventPresent| DelayCheck

  %% Checks
  %% LockCheck, DelayCheck and RuleCheck are all required before we do a LockClaim.
  %% It's not essential that they are run in any particular order: since LockClaim does
  %% not happen in the same transaction of LockCheck, we can't rely on it anyway. So we
  %% just do these in the most convenient order based on when the information is
  %% loaded.
  DelayCheck -->|DelayNone| LockCheck
  LockCheck -->|LockNone| RuleCheck
  LockCheck -->|LockExpired| RuleCheck
  RuleCheck -->|RuleNone| LockClaim

  %% Running
  LockClaim --> |LockClaimed| Process
  Process --> |ExecutesToCompletion| Delete
  Delete --> End

  %% Error checking

  %% This means the event was completed, so don't requeue
  EventLoad -->|EventMissing| End

  %% Presumably someone else is running this, but let's reenqueue this to be safe.
  LockCheck -->|IsLocked| Queue

  %% We got this too early, so reenqueue it.
  DelayCheck -->|DelayNotYet| Queue

  %% Drop the notification, we'll reload it upon unpause/unblock
  RuleCheck -->|RulePaused| End
  RuleCheck -->|RuleBlocked| End

  %% There's another besides us, no harm requeuing
  LockClaim --> |LockClaimFailed| Queue

  %% Retries are allowed, so requ
  Process --> |Exception| RetryCheck
  RetryCheck --> |RetryAllowed: increment, delay| Queue

  %% That's enough retries, we're done
  RetryCheck --> |RetryTooMany| Delete

  %% Don't know what's wrong, must be another worker running it. This will probably
  %% be dropped on the next iteration.
  RetryCheck --> |Exception| Queue
```
