###################
# General admin
###################
resource "google_service_account" "circleci_deployer" {
  account_id   = "circleci-deployer"
  display_name = "CircleCI-deployer"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "dns01_solver" {
  account_id   = "dns01-solver"
  display_name = "dns01-solver"
  project      = "balmy-ground-195100"
}

###################
# Darklang AI
###################
resource "google_service_account" "cloud_run_runner" {
  account_id   = "cloud-run-runner"
  description  = "For running darklang apps in cloud run"
  display_name = "cloud-run-runner"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "queue_pubsub_access_ai" {
  account_id   = "queue-pubsub-access-ai"
  description  = "Access to pubsub for running queues on the AI deployment"
  display_name = "queue-pubsub-access-ai"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "traces_storage_ai" {
  account_id   = "traces-storage-ai"
  description  = "Access to traces storage buckets for the AI deployment"
  display_name = "traces-storage-ai"
  project      = "balmy-ground-195100"
}

###################
# Darklang classic
###################
resource "google_service_account" "dark_static_assets_tmp" {
  account_id   = "dark-static-assets-tmp"
  display_name = "dark-static-assets"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "queue_pubsub_access" {
  account_id   = "queue-pubsub-access"
  description  = "Credentials for accessing PubSub for the queue"
  display_name = "queue-pubsub-access"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "traces_storage" {
  account_id   = "traces-storage"
  description  = "Cloud Storage permissions for traces"
  display_name = "Traces Storage"
  project      = "balmy-ground-195100"
}
