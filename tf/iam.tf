# This file manages service accounts and their iam permissions.

# We use non-authoritative resources as theu allow us ensure that certain service
# accouints have the right level of access without damaging other access that's
# managed outside terraform.


###################
# General admin
###################

resource "google_service_account" "circleci_deployer" {
  account_id   = "circleci-deployer"
  display_name = "CircleCI deployer"
  project      = local.project_name
}

resource "google_project_iam_custom_role" "circleci-deployment-role" {
  description = "CircleCI deployment role"
  permissions = ["resourcemanager.projects.get"] // Just give it a very small permission
  project     = "darklang-next"
  role_id     = "circleciDeploymentRole"
  title       = "CircleCI deployment role"
}

# resource "google_project_iam_member" "circleci_deployer_member_object_viewer" {
#   role    = "roles/storage.objectViewer"
#   member  = "serviceAccount:${google_service_account.circleci_deployer.email}"
#   project = local.project_id

#   condition {
#     title       = "Limit to bucket"
#     description = "Only allow access to darklang downloads bucket"
#     # This expression was hard to find, you can't use '=='
#     expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.downloads.id}\")"
#   }
# }


##########
# Cloud Run
##########
# resource "google_service_account" "cloud_run_runner" {
#   account_id   = "cloud-run-runner"
#   description  = "For running darklang apps in cloud run"
#   display_name = "cloud-run-runner"
#   project      = local.project_name
# }


##########
# PubSub
#
# PubSub have service-specific roles, instead of project-wide roles. That means the
# role is connected to the topic and subscription, and is not visible in the IAM page
# in the console.
##########

resource "google_service_account" "queue_pubsub_access" {
  account_id   = "queue-pubsub-access"
  description  = "Access to pubsub for running queues"
  display_name = "queue-pubsub-access"
  project      = local.project_name
}

resource "google_project_iam_custom_role" "worker-queue-access" {
  description = "Access to queues via LibCloud, reading/writing events from pubsub"
  permissions = ["pubsub.subscriptions.consume", "pubsub.topics.publish"]
  project     = "darklang-next"
  role_id     = "worker_queue_access"
  title       = "Worker Queue access"
}

// Role access, restricted to the specific topic
resource "google_pubsub_topic_iam_member" "queue_pubsub_access_member_pubsub_publisher" {
  topic  = google_pubsub_topic.topic_queue.id
  role   = google_project_iam_custom_role.worker-queue-access.id
  member = "serviceAccount:${google_service_account.queue_pubsub_access.email}"
}

// Role access, restricted to the specific subscription
resource "google_pubsub_subscription_iam_member" "queue_pubsub_access_member_worker_queue_access" {
  subscription = google_pubsub_subscription.topic_queue_sub.id
  role         = google_project_iam_custom_role.worker-queue-access.id
  member       = "serviceAccount:${google_service_account.queue_pubsub_access.email}"
}


##########
# Cloud Storage
##########

resource "google_service_account" "traces_storage" {
  account_id   = "traces-storage"
  description  = "Access to traces storage buckets"
  display_name = "traces-storage"
  project      = local.project_name
}

resource "google_project_iam_member" "traces_storage_member_object_creator" {
  role    = "roles/storage.objectCreator"
  member  = "serviceAccount:${google_service_account.traces_storage.email}"
  project = local.project_id

  condition {
    title       = "Limit to bucket"
    description = "Only allow access to traces bucket"
    # This expression was hard to find, you can't use '=='
    expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.traces.id}\")"
  }
}

resource "google_project_iam_member" "traces_storage_member_object_viewer" {
  role    = "roles/storage.objectViewer"
  member  = "serviceAccount:${google_service_account.traces_storage.email}"
  project = local.project_id

  condition {
    title       = "Limit to bucket"
    description = "Only allow access to traces bucket"
    # This expression was hard to find, you can't use '=='
    expression = "resource.name.startsWith(\"projects/_/buckets/${google_storage_bucket.traces.id}\")"
  }
}
