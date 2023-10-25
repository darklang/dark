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

########################
# Load balancer
# This was tough to figure out. I followed the docs below, but used a certificate map
# instead of the --ssl-certificates flag.
# https://cloud.google.com/load-balancing/docs/https/setup-global-ext-https-serverless#creating_the_load_balancer
########################

# gcloud compute network-endpoint-groups create bwdserver-neg
#   --region=us-central1
#   --network-endpoint-type=serverless
#   --cloud-run-service=bwdserver
# Created [https://www.googleapis.com/compute/v1/projects/darklang-next/regions/us-central1/networkEndpointGroups/bwdserver-neg].
# Created network endpoint group [bwdserver-neg].

# resource "google_compute_network_endpoint_group" "bwdserver-neg" {
#   name                  = "bwdserver-neg"
#   network_endpoint_type = "SERVERLESS"
# }

# gcloud compute backend-services create bwdserver-backend
#   --load-balancing-scheme=EXTERNAL_MANAGED
#   --global
# Created [https://www.googleapis.com/compute/v1/projects/darklang-next/global/backendServices/bwdserver-backend].
# NAME               BACKENDS  PROTOCOL
# bwdserver-backend            HTTP

# resource "google_compute_backend_service" "bwdserver-backend" {
#   name                  = "bwdserver-backend"
#   load_balancing_scheme = "EXTERNAL_MANAGED"
#   protocol              = "HTTP"
#   backends {
#     group = google_compute_network_endpoint_group.bwdserver-neg.self_link
#   }
# }

# gcloud compute backend-services add-backend bwdserver-backend \
#   --global \
#   --network-endpoint-group=bwdserver-neg \
#   --network-endpoint-group-region=us-central1
# Updated [https://www.googleapis.com/compute/v1/projects/darklang-next/global/backendServices/bwdserver-backend].

# resource "google_compute_backend_service" "bwdserver-backend" {
#   name                  = "bwdserver-backend"
#   load_balancing_scheme = "EXTERNAL_MANAGED"
#   protocol              = "HTTP"
#   backends {
#     group = google_compute_network_endpoint_group.bwdserver-neg.self_link
#   }
# }

# gcloud compute url-maps create darklangio-url-map \
#   --default-service bwdserver-backend
# Created [https://www.googleapis.com/compute/v1/projects/darklang-next/global/urlMaps/darklangio-url-map].
# NAME                DEFAULT_SERVICE
# darklangio-url-map  backendServices/bwdserver-backend

# resource "google_compute_url_map" "darklangio-url-map" {
#   name            = "darklangio-url-map"
#   default_service = google_compute_backend_service.bwdserver-backend.self_link
# }

# gcloud certificate-manager maps create bwdserver-map
# Waiting for 'operation-1698248064352-6088c309dc017-012f58c6-d94beee1' to
# complete...done.
# Created certificate map [bwdserver-map].

# resource "google_compute_ssl_certificate" "bwdserver-map" {
#   name        = "bwdserver-map"
#   certificate = google_certificate_manager_certificate.root_cert.certificate_raw_data
#   private_key = google_certificate_manager_certificate.root_cert.private_key
# }

# gcloud compute target-https-proxies create darklangio-target-proxy
#   --url-map=darklangio-url-map
#   --certificate-map=bwdserver-map
# Created [https://www.googleapis.com/compute/v1/projects/darklang-next/global/targetHttpsProxies/darklangio-target-proxy].
# NAME                     SSL_CERTIFICATES  URL_MAP             REGION  CERTIFICATE_MAP
# darklangio-target-proxy                    darklangio-url-map          bwdserver-map

# resource "google_compute_target_https_proxy" "darklangio-target-proxy" {
#   name               = "darklangio-target-proxy"
#   url_map            = google_compute_url_map.darklangio-url-map.self_link
#   ssl_certificates   = [google_compute_ssl_certificate.bwdserver-map.self_link]
#   certificate_mapper = "bwdserver-map"
# }

# gcloud certificate-manager maps entries create star-darklangio-entry --map=bwdserver-map --certificates=darklangio-rootcert-jsd83hs --hostname=*.darklang.io
# Waiting for 'operation-1698248068224-6088c30d8d7e4-8ee3798c-a7278f61' to
# complete...done.
# Created certificate map entry [star-darklangio-entry].

# resource "google_compute_ssl_certificate" "star-darklangio-entry" {
#   name        = "star-darklangio-entry"
#   certificate = google_certificate_manager_certificate.root_cert.certificate_raw_data
#   private_key = google_certificate_manager_certificate.root_cert.private_key
# }

# gcloud certificate-manager maps entries create darklangio-entry --map=bwdserver-map --certificates=darklangio-rootcert-jsd83
# hs --hostname=darklang.io
# Waiting for 'operation-1698248085009-6088c31d8f4ca-0ff61263-1c70ec6f' to
# complete...done.
# Created certificate map entry [darklangio-entry].

# resource "google_compute_ssl_certificate" "darklangio-entry" {
#   name        = "darklangio-entry"
#   certificate = google_certificate_manager_certificate.root_cert.certificate_raw_data
#   private_key = google_certificate_manager_certificate.root_cert.private_key
# }

# gcloud compute forwarding-rules create forward-darklangio-all \
#   --load-balancing-scheme=EXTERNAL_MANAGED \
#   --network-tier=PREMIUM \
#   --address=darklangio \
#   --target-https-proxy=darklangio-target-proxy \
#   --global \
#   --ports=443
# Created [https://www.googleapis.com/compute/v1/projects/darklang-next/global/forwardingRules/forward-darklangio-all].

