# PubSub Topics

resource "google_pubsub_topic" "topic_queue" {
  name    = "topic-queue"
  project = local.project_name
}

resource "google_pubsub_topic" "topic_queue_deadletter" {
  name    = "topic-queue-deadletter"
  project = local.project_name
}

# PubSub Subscriptions

resource "google_pubsub_subscription" "topic_queue_sub" {
  ack_deadline_seconds = 60
  expiration_policy {
    ttl = ""
  }
  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.topic_queue_deadletter.id
    max_delivery_attempts = 5
  }
  enable_exactly_once_delivery = true
  message_retention_duration   = "604800s"
  name                         = "topic-queue-sub"
  project                      = local.project_name
  topic                        = google_pubsub_topic.topic_queue.name
}

resource "google_pubsub_subscription" "topic_queue_deadletter_sub" {
  ack_deadline_seconds       = 10
  message_retention_duration = "604800s"
  name                       = "topic-queue-deadletter-sub"
  project                    = local.project_name
  topic                      = google_pubsub_topic.topic_queue_deadletter.name
}
