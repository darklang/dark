import time
# pip install google-cloud-pubsub
from google.cloud import pubsub_v1
import json
import re
import sys
import os

LOG_LINE_PREFIX_RE = re.compile(
    "^\\[[0-9]*\\]: \\[[0-9]*-1\\] db=[^,]*,user=[^ ]* ")


def callback(message):
    print(process_message(message.data))
    message.ack()


def process_message(data):
    # If data is empty, we can't process it, don't try
    if not data or data == "":
        return ""

    try:
        j = json.loads(data)
    except json.decoder.JSONDecodeError as e:
        msg = "Error, could not load json from message: '{}'".format(data)
        raise msg from e
        return ""

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

    return logline


if __name__ == "__main__":
    # Get google project_id from env if set, fall back to gcloud config if not
    project_id = os.getenv("PROJECT_ID")
    if not project_id:
        print("ERROR: project_id couldn't be found")
        sys.exit(1)

    subscription_name = os.getenv("SUBSCRIPTION_NAME")
    if not subscription_name:
        print("ERROR: SUBSCRIPTION_NAME not set")
        sys.exit(1)

    subscriber = pubsub_v1.SubscriberClient()
    # The `subscription_path` method creates a fully qualified identifier
    # in the form `projects/{project_id}/subscriptions/{subscription_name}`
    subscription_path = subscriber.subscription_path(
        project_id, subscription_name)
    print('Subscribing to messages on {}'.format(subscription_path))
    subscriber.subscribe(subscription_path, callback=callback)

    # The subscriber is non-blocking. We must keep the main thread from
    # exiting to allow it to process messages asynchronously in the background.
    print('Listening for messages on {}'.format(subscription_path))
    while True:
        time.sleep(10)
