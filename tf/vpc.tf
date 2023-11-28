
resource "google_compute_network" "default" {
  name                    = "default"
  description             = "Default network for the project"
  auto_create_subnetworks = "true"
}

resource "google_compute_network_peering" "yugabyte" {
  name         = "yugabyte"
  network      = google_compute_network.default.id
  peer_network = "projects/yugabyte-cloud-prod/global/networks/puoo57iiz2fbaszkvdyi5w75vs4u"
  stack_type   = "IPV4_ONLY"
}

resource "google_vpc_access_connector" "serverless_connector_1" {
  name           = "serverless-connector-1"
  network        = google_compute_network.default.id
  region         = "us-central1"
  ip_cidr_range  = "10.8.100.0/28"
  max_instances  = 10
  min_instances  = 2
  max_throughput = 1000
  machine_type   = "e2-micro"
}

