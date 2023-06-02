
################
# PubSub Topics
################
resource "google_pubsub_topic" "queueworker_deadletter_1" {
  name    = "queueworker-deadletter-1"
  project = local.project_name
}

resource "google_pubsub_topic" "queueworker_deadletter_2" {
  name    = "queueworker-deadletter-2"
  project = local.project_name
}

resource "google_pubsub_topic" "topic_queueworker_1" {
  name    = "topic-queueworker-1"
  project = local.project_name
}

resource "google_pubsub_topic" "topic_queueworker_2" {
  name    = "topic-queueworker-2"
  project = local.project_name
}

#####################
# PubSub Subscriptions
#####################

### Darklang classic

resource "google_pubsub_subscription" "subscription_queueworker_1" {
  ack_deadline_seconds = 60
  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.queueworker_deadletter_1.id
    max_delivery_attempts = 5
  }
  message_retention_duration = "604800s"
  name                       = "subscription-queueworker-1"
  project                    = local.project_name
  topic                      = google_pubsub_topic.topic_queueworker_1.name
}

resource "google_pubsub_subscription" "deadletter_subscription_1" {
  ack_deadline_seconds       = 10
  message_retention_duration = "604800s"
  name                       = "deadletter-subscription-1"
  project                    = local.project_name
  topic                      = google_pubsub_topic.queueworker_deadletter_1.name
}

### Darklang AI

resource "google_pubsub_subscription" "topic_queueworker_2_sub" {
  ack_deadline_seconds = 60
  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.queueworker_deadletter_2.id
    max_delivery_attempts = 5
  }
  enable_exactly_once_delivery = true
  message_retention_duration   = "604800s"
  name                         = "topic-queueworker-2-sub"
  project                      = local.project_name
  topic                        = google_pubsub_topic.topic_queueworker_2.name
}

resource "google_pubsub_subscription" "queueworker_deadletter_2_sub" {
  ack_deadline_seconds       = 10
  message_retention_duration = "604800s"
  name                       = "queueworker-deadletter-2-sub"
  project                    = local.project_name
  topic                      = google_pubsub_topic.queueworker_deadletter_2.name
}
