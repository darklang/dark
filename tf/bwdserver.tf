resource "google_compute_global_address" "bwdserver_ip_address" {
  name         = "bwdserver"
  address      = "34.117.116.96"
  address_type = "EXTERNAL"
  ip_version   = "IPV4"
}

########################
# Certificate Map
########################
resource "google_certificate_manager_certificate_map" "bwdserver" {
  name        = "bwdserver-map"
  description = ""
}

########################
# Load balancer
# This was tough to figure out. I followed the docs below, but used a certificate map
# instead of the --ssl-certificates flag, then imported them to terraform.
# https://cloud.google.com/load-balancing/docs/https/setup-global-ext-https-serverless#creating_the_load_balancer
########################

resource "google_compute_region_network_endpoint_group" "bwdserver" {
  name                  = "bwdserver-neg"
  network_endpoint_type = "SERVERLESS"
  region                = google_cloud_run_v2_service.bwdserver.location
  cloud_run {
    service = google_cloud_run_v2_service.bwdserver.name
  }
}

resource "google_compute_backend_service" "bwdserver" {
  name = "bwdserver-backend"

  load_balancing_scheme           = "EXTERNAL_MANAGED"
  protocol                        = "HTTP"
  port_name                       = "http"
  timeout_sec                     = 30
  connection_draining_timeout_sec = 0

  backend {
    group = google_compute_region_network_endpoint_group.bwdserver.id
  }
}

resource "google_compute_url_map" "bwdserver_url_map" {
  name            = "bwdserver-url-map"
  default_service = google_compute_backend_service.bwdserver.id
}

resource "google_compute_target_https_proxy" "bwdserver_target_proxy" {
  name    = "bwdserver-target-proxy"
  url_map = google_compute_url_map.bwdserver_url_map.id
  // self_link isn't a member here
  certificate_map = "https://certificatemanager.googleapis.com/v1/${google_certificate_manager_certificate_map.bwdserver.id}"
}

resource "google_compute_global_forwarding_rule" "bwdserver" {
  name = "forward-bwdserver-all"

  load_balancing_scheme = "EXTERNAL_MANAGED"
  target                = google_compute_target_https_proxy.bwdserver_target_proxy.self_link
  port_range            = "443"
  ip_address            = google_compute_global_address.bwdserver_ip_address.address
}
