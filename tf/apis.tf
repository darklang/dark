# Cloud Run
resource "google_project_service" "cloud_run_api" {
  provider           = google
  service            = "run.googleapis.com"
  disable_on_destroy = false
}
resource "google_project_service" "secret_manager_api" {
  provider           = google
  service            = "secretmanager.googleapis.com"
  disable_on_destroy = false
}

