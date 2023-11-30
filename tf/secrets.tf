// to add more secrets, add them here and apply, then add a version via the UI or CLI
variable "secret_names" {
  type = list(string)
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
    "traces-cloud-storage-credentials",
    "allowed-dark-internal-canvas-ids",
    "prodexec-chisel-username",
    "prodexec-chisel-password",
    "prodexec-ssh-password"
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

resource "google_secret_manager_secret_iam_binding" "secret_cloud_run_access" {
  for_each  = toset(var.secret_names)
  project   = local.project_id
  secret_id = google_secret_manager_secret.secrets["${each.key}"].id
  role      = "roles/secretmanager.secretAccessor"
  members = [
    "serviceAccount:${google_service_account.cloud_run_runner.email}",
  ]
}
