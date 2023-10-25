terraform {
  required_version = ">= 1.6.0"

  required_providers {
    google = "5.3.0"
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
