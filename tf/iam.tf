resource "google_service_account" "cloud_run_runner" {
  account_id   = "cloud-run-runner"
  description  = "For running darklang apps in cloud run"
  display_name = "cloud-run-runner"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "traces_storage" {
  account_id   = "traces-storage"
  description  = "Cloud Storage permissions for traces"
  display_name = "Traces Storage"
  project      = "balmy-ground-195100"
}

resource "google_service_account" "circleci_deployer" {
  account_id   = "circleci-deployer"
  display_name = "CircleCI-deployer"
  project      = "balmy-ground-195100"
}
