# This file manages service accounts and their iam permissions.

# We use non-authoritative resources as theu allow us ensure that certain service
# accouints have the right level of access without damaging other access that's
# managed outside terraform.


###################
# General admin
###################

###################
# Darklang AI
###################
# CLEANUP: we've manually granted the `circleci-ai` Workload Identity Pool
# access to this (`circleci_deployer-ai`) service account,
# but haven't yet figured out how to put this into terraform.
resource "google_service_account" "circleci_deployer-ai" {
  account_id   = "circleci-deployer-ai"
  display_name = "CircleCI deployer AI"
  project      = local.project_name
}
resource "google_project_iam_member" "circleci_deployer-ai_member_object_viewer" {
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.circleci_deployer-ai.email}"
  project = local.project_id

  condition {
    title       = "Limit to bucket"
    description = "Only allow access to darklang downloads bucket"
    # This expression was hard to find, you can't use '=='
    expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.darklang_downloads.id}\")"
  }
}

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

##########
# PubSub
# PubSub have service-specific roles, instead of project-wide roles
##########
resource "google_pubsub_topic_iam_member" "queue_pubsub_access_ai_member_pubsub_publisher" {
  topic  = google_pubsub_topic.topic_queue_ai.id
  member = "serviceAccount:${google_service_account.queue_pubsub_access_ai.email}"
  role   = "roles/pubsub.publisher"
}


resource "google_pubsub_subscription_iam_member" "queue_pubsub_access_ai_member_pubsub_subscriber" {
  subscription = google_pubsub_subscription.topic_queue_ai_sub.id
  role         = "roles/pubsub.subscriber"
  member       = "serviceAccount:${google_service_account.queue_pubsub_access_ai.email}"
}


##########
# Cloud Storage
##########
resource "google_service_account" "traces_storage_ai" {
  account_id   = "traces-storage-ai"
  description  = "Access to traces storage buckets for the AI deployment"
  display_name = "traces-storage-ai"
  project      = local.project_name
}
resource "google_project_iam_member" "traces_storage_ai_member_object_creator" {
  role    = "roles/storage.objectCreator"
  member  = "serviceAccount:${google_service_account.traces_storage_ai.email}"
  project = local.project_id

  condition {
    title       = "Limit to bucket"
    description = "Only allow access to ai traces bucket"
    # This expression was hard to find, you can't use '=='
    expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.dark_traces_ai.id}\")"
  }
}

resource "google_project_iam_member" "traces_storage_ai_member_object_viewer" {
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.traces_storage_ai.email}"
  project = local.project_id

  condition {
    title       = "Limit to bucket"
    description = "Only allow access to ai traces bucket"
    # This expression was hard to find, you can't use '=='
    expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.dark_traces_ai.id}\")"
  }
}



###################
# Darklang classic
###################
resource "google_service_account" "circleci_deployer" {
  account_id   = "circleci-deployer"
  display_name = "CircleCI-deployer"
  project      = local.project_name
}

resource "google_service_account" "dark_static_assets_tmp" {
  account_id   = "dark-static-assets-tmp"
  display_name = "dark-static-assets"
  project      = local.project_name
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
}


