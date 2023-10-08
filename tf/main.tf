terraform {
  required_version = ">= 1.6.0"

  required_providers {
    google = "4.84.0"
  }
}

provider "google" {
  project = local.project_name
  region  = "us-central1"
}

terraform {
  cloud {
    organization = "darklang"

    workspaces {
      name = "darklang"
    }
  }
}
