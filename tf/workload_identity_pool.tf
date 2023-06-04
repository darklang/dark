
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

  // CircleCI ids:
  // classic-dark: 7bc34e71-a1cd-4e3e-9144-741acb7b5bf1
  // cli:          63b57329-8cec-4980-ab2b-2e490c6f94b7
  // dark-ocaml:   438d6e6e-095c-4857-bd17-f88b31d7892a
  // dark:         1f60315c-0228-42dd-9205-ed25beb24371
  // darklang.com: ee84bf88-8af8-47a3-ad05-48a1ec815474
  // dockerfile:   9573c70d-4695-4135-b913-4bde88847f75
  // docs:         a7f703ca-2adb-406f-b4c1-faa2b80c8468

  attribute_condition = <<-EOT
    attribute.project=="1f60315c-0228-42dd-9205-ed25beb24371" ||
    attribute.project=="a9e00368-7791-41f2-98fe-99e5ca56ae0e" ||
    attribute.project=="7bc34e71-a1cd-4e3e-9144-741acb7b5bf1"
    EOT

  timeouts {}

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
