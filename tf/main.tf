terraform {
  required_version = ">= 0.14"

  required_providers {
    google = "4.47.0"
  }
}

provider "google" {
  project = "balmy-ground-195100"
  region  = "us-west1"
}

# main.tf

# Enables the Cloud Run API
# resource "google_project_service" "run_api" {
#   service = "run.googleapis.com"

#   disable_on_destroy = true
# }


resource "google_cloud_run_service" "run_service" {
  name     = "bwdserver"
  location = "us-west1"

  template {

    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale" : "1"
        "autoscaling.knative.dev/maxScale" : "10"
        "run.googleapis.com/cloudsql-instances" : "balmy-ground-195100:us-west1:dark-west"
        "run.googleapis.com/startup-cpu-boost" : "true"
      }
    }

    spec {
      container_concurrency = 0
      timeout_seconds       = 300
      service_account_name  = "cloud-run-runner@balmy-ground-195100.iam.gserviceaccount.com"
      containers {
        image = "gcr.io/balmy-ground-195100/gcp-fsharp-bwdserver@sha256:7e91876469db2b8a21a176308e54d8a61147976ab83251d896fee4fb2d63ba77"
        ports {
          name           = "http1"
          container_port = 11001
        }
        resources {
          limits = {
            cpu    = "2.0"
            memory = "6000Mi"
          }
        }
        # startup_probe {
        #   initial_delay_seconds = 0
        #   timeout_seconds = 1
        #   period_seconds = 3
        #   failure_threshold = 1
        #   tcp_socket {
        #     port = 8080
        #   }
        # }
        # liveness_probe {
        #   http_get {
        #     path = "/"
        #   }
        # }

        # secrets
        dynamic "env" {
          for_each = var.service_secrets
          content {
            name = env.key
            value_from {
              secret_key_ref {
                name = env.value
                key  = "latest"
              }
            }
          }
        }

        # Env vars
        dynamic "env" {
          for_each = var.service_env_vars
          content {
            name  = env.key
            value = env.value
          }
        }
      }
    }
  }

  traffic {
    percent         = 100
    latest_revision = true
  }

  # Waits for the Cloud Run API to be enabled
  # depends_on = [google_project_service.run_api]
}

output "service_url" {
  value = google_cloud_run_service.run_service.status[0].url
}
