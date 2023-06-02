#####################
# Darklang classic
#####################

# PubSub Topics

resource "google_pubsub_topic" "queueworker_deadletter_1" {
  name    = "queueworker-deadletter-1"
  project = local.project_name
}

resource "google_pubsub_topic" "topic_queueworker_1" {
  name    = "topic-queueworker-1"
  project = local.project_name
}

# PubSub Subscriptions

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

#####################
### Darklang AI
#####################

# PubSub Topics

resource "google_pubsub_topic" "topic_queue_ai" {
  name    = "topic-queue-ai"
  project = local.project_name
}

resource "google_pubsub_topic" "topic_queue_ai_deadletter" {
  name    = "topic-queue-ai-deadletter"
  project = local.project_name
}

# PubSub Subscriptions

resource "google_pubsub_subscription" "topic_queue_ai_sub" {
  ack_deadline_seconds = 60
  dead_letter_policy {
    dead_letter_topic     = google_pubsub_topic.topic_queue_ai_deadletter.id
    max_delivery_attempts = 5
  }
  enable_exactly_once_delivery = true
  message_retention_duration   = "604800s"
  name                         = "topic-queue-ai-sub"
  project                      = local.project_name
  topic                        = google_pubsub_topic.topic_queue_ai.name
}

resource "google_pubsub_subscription" "topic_queue_ai_deadletter_sub" {
  ack_deadline_seconds       = 10
  message_retention_duration = "604800s"
  name                       = "topic-queue-ai-deadletter-sub"
  project                    = local.project_name
  topic                      = google_pubsub_topic.topic_queue_ai_deadletter.name
}
