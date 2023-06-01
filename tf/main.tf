terraform {
  required_version = ">= 1.4.6"

  required_providers {
    google = "4.67.0"
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
