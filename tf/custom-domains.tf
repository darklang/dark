
resource "google_certificate_manager_certificate_map_entry" "packages-darklang-com" {
  name         = "packages-darklang-com-entry"
  description  = ""
  map          = google_certificate_manager_certificate_map.bwdserver.name
  certificates = [google_certificate_manager_certificate.packages-darklang-com.id]
  hostname     = "packages.darklang.com"
}

resource "google_certificate_manager_certificate" "packages-darklang-com" {
  name        = "packages-darklang-com"
  description = ""
  managed {
    domains = ["packages.darklang.com"]
  }
}

