
resource "google_compute_global_address" "darklangio_ip_address" {
  name         = "darklangio"
  address      = "34.111.152.115"
  address_type = "EXTERNAL"
  ip_version   = "IPV4"
}

# dns
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

resource "google_dns_record_set" "darklangio" {
  name         = "*.darklang.io."
  type         = "A"
  ttl          = 300
  managed_zone = google_dns_managed_zone.darklangio.name
  rrdatas      = [google_compute_global_address.darklangio_ip_address.address]
}

import {
  id = "darklang-io/*.darklang.io./A"
  to = google_dns_record_set.darklangio
}

# dnssec
# darklang.com is in darklang-classic project
# cert manager
