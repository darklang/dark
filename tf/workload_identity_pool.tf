
resource "google_iam_workload_identity_pool" "circleci" {
  workload_identity_pool_id = "circleci"
  display_name              = "CircleCI"
  description               = "Access for CircleCI deployments"
  disabled                  = false
}

resource "google_iam_workload_identity_pool_provider" "circleci" {
  workload_identity_pool_id          = "circleci"
  workload_identity_pool_provider_id = "circleci"
  display_name                       = "CircleCI"
  disabled                           = false
  // Only darklang/dark project
  attribute_condition = "attribute.project==\"1f60315c-0228-42dd-9205-ed25beb24371\""

  attribute_mapping = {
    "attribute.project" = "assertion['oidc.circleci.com/project-id']"
    "attribute.org_id"  = "assertion.aud"
    "google.subject"    = "assertion.sub"
  }
  oidc {
    // Publicly available org id
    issuer_uri        = "https://oidc.circleci.com/org/a5d9bde7-7922-4277-90ff-dbec5d9c200e"
    allowed_audiences = ["a5d9bde7-7922-4277-90ff-dbec5d9c200e"]
  }
}
