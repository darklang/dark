
resource "google_compute_global_address" "darklangio_ip_address" {
  name         = "darklangio"
  address      = "34.111.152.115"
  address_type = "EXTERNAL"
  ip_version   = "IPV4"
}

# dns
# darklang.com is in darklang-classic project
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

resource "google_dns_record_set" "star_darklangio_a" {
  name         = "*.darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.darklangio_ip_address.address]
}

resource "google_dns_record_set" "psl_darklangio" {
  name         = "_psl.darklang.io."
  type         = "TXT"
  ttl          = 5
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = ["https://github.com/publicsuffix/list/pull/1880"]
}


# resource "google_certificate_manager_dns_authorization" "star_darklangio" {
#   name        = "star-darklang-io-dnsauth-ksu48h2"
#   description = "The default dns auth"
#   domain      = "*.darklang.io"
# }
# resource "google_dns_record_set" "star_darklangio_dns_authorization_cname" {
#   name         = google_certificate_manager_dns_authorization.star_darklangio.dns_resource_record[0].name
#   managed_zone = google_dns_managed_zone.darklangio.name
#   type         = google_certificate_manager_dns_authorization.star_darklangio.dns_resource_record[0].type
#   ttl          = 5
#   rrdatas      = [google_certificate_manager_dns_authorization.star_darklangio.dns_resource_record[0].data]
# }

# TODO dnssec
