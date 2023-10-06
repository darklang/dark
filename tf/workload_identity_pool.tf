###################
// CircleCI ids:
// darklang org: a5d9bde7-7922-4277-90ff-dbec5d9c200e
//
// project ids:
// dark:         1f60315c-0228-42dd-9205-ed25beb24371
// darklang.com: ee84bf88-8af8-47a3-ad05-48a1ec815474
// dockerfile:   9573c70d-4695-4135-b913-4bde88847f75
// docs:         a7f703ca-2adb-406f-b4c1-faa2b80c8468
###################


# resource "google_iam_workload_identity_pool" "circleci" {
#   workload_identity_pool_id = "circleci"
#   display_name              = "CircleCI"
#   description               = "Access for CircleCI deployments"
#   disabled                  = false
# }

# resource "google_iam_workload_identity_pool_provider" "circleci" {
#   workload_identity_pool_id          = "circleci"
#   workload_identity_pool_provider_id = "circleci"
#   display_name                       = "CircleCI"

#   disabled = false

#   attribute_condition = <<-EOT
#     attribute.project=="1f60315c-0228-42dd-9205-ed25beb24371"
#     && attribute.vcs_origin=="github.com/darklang/dark"
#     && attribute.vcs_ref=="refs/heads/main"
#     EOT

#   timeouts {}

#   attribute_mapping = {
#     "attribute.project"    = "assertion['oidc.circleci.com/project-id']"
#     "attribute.vcs_origin" = "assertion['oidc.circleci.com/vcs-origin']"
#     "attribute.vcs_ref"    = "assertion['oidc.circleci.com/vcs-ref']"
#     "attribute.org_id"     = "assertion.aud"
#     "google.subject"       = "assertion.sub"
#   }

#   oidc {
#     // https://app.circleci.com/settings/organization/github/darklang/overview
#     issuer_uri        = "https://oidc.circleci.com/org/a5d9bde7-7922-4277-90ff-dbec5d9c200e"
#     allowed_audiences = ["a5d9bde7-7922-4277-90ff-dbec5d9c200e"]
#   }
# }
