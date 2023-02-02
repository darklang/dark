
resource "google_storage_bucket" "dark_traces" {
  force_destroy               = false
  location                    = "US-WEST1"
  name                        = "dark-traces"
  project                     = "balmy-ground-195100"
  public_access_prevention    = "enforced"
  storage_class               = "STANDARD"
  uniform_bucket_level_access = true
}
