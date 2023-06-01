# Cloud Run
resource "google_project_service" "cloud_run_api" {
  provider           = google
  service            = "run.googleapis.com"
  disable_on_destroy = false
}
