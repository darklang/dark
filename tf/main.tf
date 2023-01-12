terraform {
  required_version = ">= 0.14"

  required_providers {
    google = "4.47.0"
  }
}

provider "google" {
  project = "balmy-ground-195100"
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
