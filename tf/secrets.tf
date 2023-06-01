#################
# Darklang AI
# (Darklang classic secrets are in kubernetes)
#################

resource "google_secret_manager_secret" "cloudsql_password" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "cloudsql-password"
}

resource "google_secret_manager_secret" "cloudsql_username" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "cloudsql-username"
}

resource "google_secret_manager_secret" "honeycomb_api_key" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "honeycomb-api-key"
}

resource "google_secret_manager_secret" "launchdarkly_sdk_api_key" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "launchdarkly-sdk-api-key"
}

resource "google_secret_manager_secret" "pusher_app_id" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "pusher-app-id"
}

resource "google_secret_manager_secret" "pusher_cluster" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "pusher-cluster"
}

resource "google_secret_manager_secret" "pusher_key" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "pusher-key"
}

resource "google_secret_manager_secret" "pusher_secret" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "pusher-secret"
}

resource "google_secret_manager_secret" "queue_pubsub_credentials" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "queue-pubsub-credentials"
}

resource "google_secret_manager_secret" "rollbar_post_token" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "rollbar-post-token"
}

resource "google_secret_manager_secret" "traces_cloud_storage_credentials" {
  project = "118633020131"
  replication {
    automatic = true
  }
  secret_id = "traces-cloud-storage-credentials"
}
