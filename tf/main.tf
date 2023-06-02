terraform {
  required_version = ">= 1.4.6"

  required_providers {
    google = "4.67.0"
  }
}

provider "google" {
  project = local.project_name
  region  = "us-west1"
}

terraform {
  cloud {
    organization = "darklang"

    workspaces {
      name = "darklang-main"
    }
  }
}
