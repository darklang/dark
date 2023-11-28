resource "google_cloud_run_v2_service" "bwdserver" {
  lifecycle {
    ignore_changes = [
      template[0].containers[0].image, client, client_version

    ]
  }
  name     = "bwdserver"
  location = "us-central1"

  traffic {
    percent = 100
    type    = "TRAFFIC_TARGET_ALLOCATION_TYPE_LATEST"
  }

  timeouts {
  }

  template {

    execution_environment = "EXECUTION_ENVIRONMENT_GEN2"
    service_account       = google_service_account.cloud_run_runner.email
    timeout               = "300s"

    scaling {
      min_instance_count = 0
      max_instance_count = 1
    }


    vpc_access {
      connector = google_vpc_access_connector.serverless_connector_1.id
      egress    = "PRIVATE_RANGES_ONLY"
    }

    containers {
      name  = "bwdserver-1"
      image = "us-central1-docker.pkg.dev/darklang-next/production-containers/bwdserver:latest"
      ports {
        name           = "http1"
        container_port = 11001
      }
      resources {
        # requests = {
        #   "cpu"    = "2.0"
        #   "memory" = "4000Mi"
        # }
        limits = {
          cpu    = "2.0"
          memory = "6000Mi"
        }
        cpu_idle          = true
        startup_cpu_boost = true
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


      # env vars are either secrets or values. If there is a value key, it adds a
      # value and no value_source block. If there's a secret key, it adds a
      # value_source block and a null value.
      # We keep env vars and secrets together and sort them so that the order is the
      # same on each update. Otherwise, it's hard to read the changes in the
      # terraform plan output.
      dynamic "env" {
        for_each = sort(keys(var.service_env_vars))
        content {
          name = env.value # name of the environment variable, since keys returns a list

          value = try(var.service_env_vars[env.value].value, null)


          dynamic "value_source" {
            for_each = var.service_env_vars[env.value].secret == null ? [] : [1]
            content {
              secret_key_ref {
                secret  = var.service_env_vars[env.value].secret
                version = "latest"
              }
            }
          }
        }
      }



    }
  }

  depends_on = [google_project_service.apis["run.googleapis.com"]]
}
