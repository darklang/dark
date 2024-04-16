# QueueWorker

Supports user "Workers"/Queues -- users emit events (using `emit`),
and this pulls those events from the queue and runs them.

The event algorithm in described in [eventsv2.md](/docs/eventsV2.md)
