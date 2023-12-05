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

resource "google_dns_record_set" "star_darklangio_a" {
  name         = "*.darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.bwdserver_ip_address.address]
}

resource "google_dns_record_set" "darklangio_a" {
  name         = "darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.bwdserver_ip_address.address]
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
# Certs
########################

// Enable DNS authorization for Certificate Manager, which allows domains we control
// have certs managed by Google
// https://cloud.google.com/certificate-manager/docs/dns-authorizations#auth-create
resource "google_certificate_manager_dns_authorization" "darklang-io" {
  name   = "star-darklang-io"
  domain = "darklang.io"
}

// DNS entry for Certificate Manager to verify we control the domain
// https://cloud.google.com/certificate-manager/docs/dns-authorizations#cname-record
resource "google_dns_record_set" "darklangio_dns_authorization_cname" {
  name         = google_certificate_manager_dns_authorization.darklang-io.dns_resource_record[0].name
  managed_zone = google_dns_managed_zone.darklangio.name
  type         = google_certificate_manager_dns_authorization.darklang-io.dns_resource_record[0].type
  ttl          = 5
  rrdatas      = [google_certificate_manager_dns_authorization.darklang-io.dns_resource_record[0].data]
}

// Certificate Manager certificate request for darklang.io
// https://cloud.google.com/certificate-manager/docs/certificates#cert-dns-auth
resource "google_certificate_manager_certificate" "darklang-io" {
  name = "darklang-io"
  managed {
    domains = ["darklang.io", "*.darklang.io"]
    dns_authorizations = [
      google_certificate_manager_dns_authorization.darklang-io.id
    ]
  }
}

resource "google_certificate_manager_certificate_map_entry" "star-darklang-io" {
  name         = "star-darklangio-entry"
  map          = google_certificate_manager_certificate_map.bwdserver.name
  certificates = [google_certificate_manager_certificate.darklang-io.id]
  hostname     = "*.darklang.io"
}

resource "google_certificate_manager_certificate_map_entry" "darklang-io" {
  name         = "darklangio-entry"
  map          = google_certificate_manager_certificate_map.bwdserver.name
  certificates = [google_certificate_manager_certificate.darklang-io.id]
  hostname     = "darklang.io"
}



