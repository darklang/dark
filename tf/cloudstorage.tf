##########################
# Darklang AI deployment
##########################

# Downloads for the new cli(s)
resource "google_storage_bucket" "downloads" {
  force_destroy               = false
  location                    = "US"
  name                        = "darklang-downloads"
  project                     = local.project_name
  public_access_prevention    = "inherited"
  storage_class               = "STANDARD"
  uniform_bucket_level_access = true
  autoclass {
    enabled = true
  }
}

# Bucket for storing customer traces
resource "google_storage_bucket" "traces" {
  force_destroy               = false
  location                    = "US-CENTRAL1"
  name                        = "darklang-traces"
  project                     = local.project_name
  public_access_prevention    = "enforced"
  storage_class               = "STANDARD"
  uniform_bucket_level_access = true
}
