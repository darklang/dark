variable "secret_names" {
  default = [
    "db-password",
    "db-username",
    "honeycomb-api-key",
    "launchdarkly-sdk-api-key",
    "pusher-app-id",
    "pusher-cluster",
    "pusher-key",
    "pusher-secret",
    "queue-pubsub-credentials",
    "rollbar-post-token",
    "traces-cloud-storage-credentials"
  ]
}

resource "google_secret_manager_secret" "secrets" {
  for_each = toset(var.secret_names)
  project  = local.project_id
  replication {
    auto {}
  }
  secret_id = each.key
}

# resource "google_secret_manager_secret_iam_binding" "db_password_cloud_run_runner" {
#   project   = local.project_id
#   secret_id = google_secret_manager_secret.db_password.secret_id
#   role      = "roles/secretmanager.secretAccessor"
#   members = [
#     "serviceAccount:${google_service_account.cloud_run_runner.email}",
#   ]
# }
