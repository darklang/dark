#################
# Darklang AI
# (Darklang classic secrets are in kubernetes)
#################

resource "google_secret_manager_secret" "cloudsql_password" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "cloudsql-password"
}

resource "google_secret_manager_secret" "cloudsql_username" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "cloudsql-username"
}

resource "google_secret_manager_secret" "honeycomb_api_key" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "honeycomb-api-key"
}

resource "google_secret_manager_secret" "launchdarkly_sdk_api_key" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "launchdarkly-sdk-api-key"
}

resource "google_secret_manager_secret" "pusher_app_id" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "pusher-app-id"
}

resource "google_secret_manager_secret" "pusher_cluster" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "pusher-cluster"
}

resource "google_secret_manager_secret" "pusher_key" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "pusher-key"
}

resource "google_secret_manager_secret" "pusher_secret" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "pusher-secret"
}

resource "google_secret_manager_secret" "queue_pubsub_credentials" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "queue-pubsub-credentials"
}

resource "google_secret_manager_secret" "rollbar_post_token" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "rollbar-post-token"
}

resource "google_secret_manager_secret" "traces_cloud_storage_credentials" {
  project = local.project_id
  replication {
    automatic = true
  }
  secret_id = "traces-cloud-storage-credentials"
}
