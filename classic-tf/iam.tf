# This file manages service accounts and their iam permissions.

# We use non-authoritative resources as theu allow us ensure that certain service
# accouints have the right level of access without damaging other access that's
# managed outside terraform.

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


