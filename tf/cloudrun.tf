
resource "google_cloud_run_service" "bwdserver" {
  name     = "bwdserver"
  location = "us-west1"

  template {

    metadata {
      annotations = {
        "autoscaling.knative.dev/minScale" : "0"
        "autoscaling.knative.dev/maxScale" : "10"
        "run.googleapis.com/cloudsql-instances" : "balmy-ground-195100:us-west1:dark-west"
        "run.googleapis.com/startup-cpu-boost" : "true"
        "run.googleapis.com/cpu-throttling" : "true"
        "run.googleapis.com/execution-environment" : "gen2"
      }
    }

    spec {
      timeout_seconds      = 300
      service_account_name = "cloud-run-runner@balmy-ground-195100.iam.gserviceaccount.com"
      containers {
        image = "gcr.io/balmy-ground-195100/gcp-fsharp-bwdserver@sha256:ec7bbcd9e38d8965b76d5718359fac1242fefdcb11ae7250ed918415ecd829a3"
        ports {
          name           = "http1"
          container_port = 11001
        }
        resources {
          requests = {
            "cpu"    = "2.0"
            "memory" = "4000Mi"
          }
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

  depends_on = [google_project_service.cloud_run_api]
}
