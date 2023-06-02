# This file manages service accounts and their iam permissions.

# We use non-authoritative resources as theu allow us ensure that certain service
# accouints have the right level of access without damaging other access that's
# managed outside terraform.


###################
# General admin
###################
resource "google_service_account" "circleci_deployer" {
  account_id   = "circleci-deployer"
  display_name = "CircleCI-deployer"
  project      = local.project_name
}

###################
# Darklang AI
###################
resource "google_service_account" "cloud_run_runner" {
  account_id   = "cloud-run-runner"
  description  = "For running darklang apps in cloud run"
  display_name = "cloud-run-runner"
  project      = local.project_name
}

resource "google_service_account" "queue_pubsub_access_ai" {
  account_id   = "queue-pubsub-access-ai"
  description  = "Access to pubsub for running queues on the AI deployment"
  display_name = "queue-pubsub-access-ai"
  project      = local.project_name
}

resource "google_project_iam_member" "queue_pubsub_access_ai_member_pubsub_publisher" {
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.queue_pubsub_access_ai.email}"
  project = local.project_id

  condition {
    title       = "Limit queue"
    description = "Only allow access to ai deployment queue"
    expression  = "resource.name == \"projects/balmy-ground-195100/subscriptions/topic-queueworker-2-sub\""
  }
}

resource "google_project_iam_member" "queue_pubsub_access_ai_member_pubsub_subscriber" {
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.queue_pubsub_access_ai.email}"
  project = local.project_id

  condition {
    title       = "Limit queue"
    description = "Only allow access to ai deployment queue"
    expression  = "resource.name == \"projects/balmy-ground-195100/subscriptions/topic-queueworker-2-sub\""
  }
}




resource "google_service_account" "traces_storage_ai" {
  account_id   = "traces-storage-ai"
  description  = "Access to traces storage buckets for the AI deployment"
  display_name = "traces-storage-ai"
  project      = local.project_name
}



###################
# Darklang classic
###################
resource "google_service_account" "dark_static_assets_tmp" {
  account_id   = "dark-static-assets-tmp"
  display_name = "dark-static-assets"
  project      = "balmy-ground-195100"
}


#####
# Darklang classic queue pubsub access
#####
resource "google_service_account" "queue_pubsub_access" {
  account_id   = "queue-pubsub-access"
  description  = "Credentials for accessing PubSub for the queue"
  display_name = "queue-pubsub-access"
  project      = local.project_name
}

resource "google_project_iam_member" "queue_pubsub_access_member_pubsub_publisher" {
  role    = "roles/pubsub.publisher"
  member  = "serviceAccount:${google_service_account.queue_pubsub_access.email}"
  project = local.project_id
}

resource "google_project_iam_member" "queue_pubsub_access_member_pubsub_subscriber" {
  role    = "roles/pubsub.subscriber"
  member  = "serviceAccount:${google_service_account.queue_pubsub_access.email}"
  project = local.project_id
}

# condition {
#   title       = "expires_after_2019_12_31"
#   description = "Expiring at midnight of 2019-12-31"
#   expression  = "request.time < timestamp(\"2020-01-01T00:00:00Z\")"
# }




#####
# Darklang classic trace storage
#####
resource "google_service_account" "traces_storage" {
  account_id   = "traces-storage"
  description  = "Cloud Storage permissions for traces"
  display_name = "Traces Storage"
  project      = local.project_name
}

resource "google_project_iam_member" "traces_storage_member_object_creator" {
  role    = "roles/storage.objectCreator"
  member  = "serviceAccount:${google_service_account.traces_storage.email}"
  project = local.project_id
}

resource "google_project_iam_member" "traces_storage_member_object_viewer" {
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.traces_storage.email}"
  project = local.project_id

  # condition {
  #   title       = "expires_after_2019_12_31"
  #   description = "Expiring at midnight of 2019-12-31"
  #   expression  = "request.time < timestamp(\"2020-01-01T00:00:00Z\")"
  # }
}


