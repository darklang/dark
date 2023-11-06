########################
# DNS setup for darklang.io
# NOTE: darklang.com is in darklang-classic project
########################

resource "google_dns_managed_zone" "darklangio" {
  name        = "darklang-io"
  dns_name    = "darklang.io."
  description = "DNS zone for darklang.io"

  dnssec_config {
    kind          = "dns#managedZoneDnsSecConfig"
    non_existence = "nsec3"
    state         = "on"

    default_key_specs {
      algorithm  = "rsasha256"
      key_length = 2048
      key_type   = "keySigning"
      kind       = "dns#dnsKeySpec"
    }
    default_key_specs {
      algorithm  = "rsasha256"
      key_length = 1024
      key_type   = "zoneSigning"
      kind       = "dns#dnsKeySpec"
    }
  }
}

#########################
# DNS setup for *.darklang.io
#########################

resource "google_compute_global_address" "darklangio_ip_address" {
  name         = "darklangio"
  address      = "34.111.152.115"
  address_type = "EXTERNAL"
  ip_version   = "IPV4"
}

resource "google_dns_record_set" "star_darklangio_a" {
  name         = "*.darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.darklangio_ip_address.address]
}

resource "google_dns_record_set" "darklangio_a" {
  name         = "darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.darklangio_ip_address.address]
}


// add darklang.io to public suffix list
resource "google_dns_record_set" "psl_darklangio" {
  name         = "_psl.darklang.io."
  type         = "TXT"
  ttl          = 5
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = ["https://github.com/publicsuffix/list/pull/1880"]
}



########################
# Certificate Manager
########################

// Enable DNS authorization for Certificate Manager, which allows domains we control
// have certs managed by Google
// https://cloud.google.com/certificate-manager/docs/dns-authorizations#auth-create
resource "google_certificate_manager_dns_authorization" "darklangio" {
  name        = "star-darklang-io-dnsauth-ksu48h2"
  description = "DNS auth for darklang.io"
  domain      = "darklang.io"
}

// DNS entry for Certificate Manager to verify we control the domain
// https://cloud.google.com/certificate-manager/docs/dns-authorizations#cname-record
resource "google_dns_record_set" "darklangio_dns_authorization_cname" {
  name         = google_certificate_manager_dns_authorization.darklangio.dns_resource_record[0].name
  managed_zone = google_dns_managed_zone.darklangio.name
  type         = google_certificate_manager_dns_authorization.darklangio.dns_resource_record[0].type
  ttl          = 5
  rrdatas      = [google_certificate_manager_dns_authorization.darklangio.dns_resource_record[0].data]
}

// Certificate Manager certificate request for darklang.io
// https://cloud.google.com/certificate-manager/docs/certificates#cert-dns-auth
resource "google_certificate_manager_certificate" "root_cert" {
  name        = "darklangio-rootcert-jsd83hs"
  description = "The wildcard cert"
  managed {
    domains = ["darklang.io", "*.darklang.io"]
    dns_authorizations = [
      google_certificate_manager_dns_authorization.darklangio.id
    ]
  }
}

resource "google_certificate_manager_certificate_map" "bwdserver" {
  name        = "bwdserver-map"
  description = ""
}

resource "google_certificate_manager_certificate_map_entry" "star_darklangio" {
  name         = "star-darklangio-entry"
  description  = ""
  map          = google_certificate_manager_certificate_map.bwdserver.name
  certificates = [google_certificate_manager_certificate.root_cert.id]
  hostname     = "*.darklang.io"
}

resource "google_certificate_manager_certificate_map_entry" "darklangio" {
  name         = "darklangio-entry"
  description  = ""
  map          = google_certificate_manager_certificate_map.bwdserver.name
  certificates = [google_certificate_manager_certificate.root_cert.id]
  hostname     = "darklang.io"
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
  region                = google_cloud_run_service.bwdserver.location
  cloud_run {
    service = google_cloud_run_service.bwdserver.name
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

resource "google_compute_url_map" "darklangio_url_map" {
  name            = "darklangio-url-map"
  default_service = google_compute_backend_service.bwdserver.id
}

resource "google_compute_target_https_proxy" "darklangio_target_proxy" {
  name    = "darklangio-target-proxy"
  url_map = google_compute_url_map.darklangio_url_map.id
  // self_link isn't a member here
  certificate_map = "https://certificatemanager.googleapis.com/v1/${google_certificate_manager_certificate_map.bwdserver.id}"
}

resource "google_compute_global_forwarding_rule" "bwdserver" {
  name = "forward-darklangio-all"

  load_balancing_scheme = "EXTERNAL_MANAGED"
  target                = google_compute_target_https_proxy.darklangio_target_proxy.self_link
  port_range            = "443"
  ip_address            = google_compute_global_address.darklangio_ip_address.address
}
