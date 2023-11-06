variable "google_cloud_services" {
  type = set(string)
  default = [
    "compute.googleapis.com",
    "secretmanager.googleapis.com",
    "run.googleapis.com"
  ]
}

resource "google_project_service" "apis" {
  for_each           = toset(var.google_cloud_services)
  provider           = google
  service            = each.value
  disable_on_destroy = false
}
