import time
# pip install google-cloud-pubsub
from google.cloud import pubsub_v1
import json
import re

# TODO: could be config values
project_id = "balmy-ground-195100"
subscription_name = "postgres-logs-dark-west-honeycomb"

subscriber = pubsub_v1.SubscriberClient()
# The `subscription_path` method creates a fully qualified identifier
# in the form `projects/{project_id}/subscriptions/{subscription_name}`
subscription_path = subscriber.subscription_path(
    project_id, subscription_name)

LOG_LINE_PREFIX_RE = re.compile(
    "^\\[[0-9]*\\]: \\[[0-9]*-1\\] db=[^,]*,user=[^ ]* ")


def callback(message):
    j = json.loads(message.data)

    # log_line_prefix can't be changed in CloudSQL, so we're going to add our
    # own timestamps and fake it. *facepalm*
    #
    # We know CloudSQL's log_line_prefix is `[%p]: [%l-1] db=%d,user=%u`, so
    # lines we want to replace will match:
    # /^\[[0-9]*\]: \[[0-9]*-1\] db=[^,]*,user=[^ ]* /
    # (because a postgres log entry spans multiple lines, stackdriver treats
    # each line as an individual entry, and so only some lines have
    # log_line_prefix)
    #
    # The simplest thing to do is prefix "[{}]:".format(timestamp) to these
    # lines, where timestamp is what postgres would generate for %t
    # ('2019-04-12 21:35:16 UTC'), and then configure the postgres processor
    # with log_line_prefix=[%t]: [%p]: [%l-1] db=%d,user=%u

    # Note: the below conditional seems faster - 350-450 lines/s vs 250-300 -
    # than using re.sub(..., count=1). I have not checked more than
    # superficially, using `pv --line-mode --rate`
    if re.search(LOG_LINE_PREFIX_RE, j["textPayload"]):
        # timestamp comes in as "2019-04-13T22:43:15.095958Z", but honeytail
        # wants it to match
        # "\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}[.0-9]* [A-Z]+)"
        timestamp = re.sub('T', ' ', j["timestamp"], count=1)
        timestamp = re.sub('Z', ' UTC', timestamp, count=1)

        logline = "[{}]: {}".format(timestamp, j["textPayload"])
    else:
        logline = j["textPayload"]

    print(logline)
    message.ack()


subscriber.subscribe(subscription_path, callback=callback)

# The subscriber is non-blocking. We must keep the main thread from
# exiting to allow it to process messages asynchronously in the background.
print('Listening for messages on {}'.format(subscription_path))
while True:
    time.sleep(10)
