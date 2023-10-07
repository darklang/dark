resource "google_artifact_registry_repository" "production-containers" {
  location      = "us-central1"
  repository_id = "production-containers"
  format        = "DOCKER"
  timeouts {}
}
import {
  to = google_artifact_registry_repository.production-containers
  id = "projects/darklang-next/locations/us-central1/repositories/production-containers"
}
