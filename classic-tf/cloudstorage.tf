##########################
# Used by container registry
##########################

resource "google_storage_bucket" "artifacts_balmy_ground_195100_appspot_com" {
  force_destroy            = false
  location                 = "US"
  name                     = "artifacts.balmy-ground-195100.appspot.com"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
}
resource "google_storage_bucket" "us_artifacts_balmy_ground_195100_appspot_com" {
  force_destroy            = false
  location                 = "US"
  name                     = "us.artifacts.balmy-ground-195100.appspot.com"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
}

##########################
# Darklang classic
##########################

# Bucket to download the rust-based static assets cli
resource "google_storage_bucket" "dark_cli" {
  force_destroy            = false
  location                 = "US"
  name                     = "dark-cli"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
  website {
    main_page_suffix = "index.html"
    not_found_page   = "404.html"
  }
}

# For assets used as part of the osx cross-compilation used by the rust-based cli
resource "google_storage_bucket" "dark_osxcross_files" {
  force_destroy            = false
  location                 = "US"
  name                     = "dark-osxcross-files"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
}

# Bucket for storing customer static assets on darklang-classic
resource "google_storage_bucket" "dark_static_assets" {
  cors {
    max_age_seconds = 3600
    method          = ["GET"]
    origin          = ["*"]
    response_header = ["Content-Type"]
  }
  force_destroy            = false
  location                 = "US"
  name                     = "dark-static-assets"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
  website {
    main_page_suffix = "index.html"
    not_found_page   = "404.html"
  }
}

# Dev-environment bucket for testing static assets on darklang-classic
resource "google_storage_bucket" "dark_static_assets_dev" {
  force_destroy            = false
  location                 = "US"
  name                     = "dark-static-assets-dev"
  project                  = local.project_name
  public_access_prevention = "inherited"
  storage_class            = "STANDARD"
  website {
    not_found_page = "404.html"
  }
}

# Bucket for storing customer traces on darklang-classic
resource "google_storage_bucket" "dark_traces" {
  force_destroy               = false
  location                    = "US-WEST1"
  name                        = "dark-traces"
  project                     = local.project_name
  public_access_prevention    = "enforced"
  storage_class               = "STANDARD"
  uniform_bucket_level_access = true
}

# Bucket for static assets for the old editor (darklang-classic)
resource "google_storage_bucket" "darklang_static_assets" {
  cors {
    max_age_seconds = 3600
    method          = ["GET"]
    origin          = ["https://*.darklang.com", "https://darklang.com"]
    response_header = ["Content-Type"]
  }
  force_destroy               = false
  location                    = "US"
  name                        = "darklang-static-assets"
  project                     = local.project_name
  public_access_prevention    = "inherited"
  storage_class               = "STANDARD"
  uniform_bucket_level_access = true
  website {
    not_found_page = "404.html"
  }
}
