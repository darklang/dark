# TLS Configuration, last updated: Mar 25, 2023

Dark has many SSL certs and an elaborate history of using different cert providers
over the years. This file should contain the important information about our current
SSL setup: see the git history for anything else.

Today, we have the following certs, most of which are handled in cert-manager.

- \*.builtwithdark.com, builtwithdark.com: Cert-manager, automatically renewed.
- static.darklang.com uses Google managed domains (it sits in front of a Cloud
  Storage bucket, so we can't use cert manager).
- darklang.com and it's many subdomains (except static.darklang.com): Cert-manager,
  automatically renewed.
- customer certs: Cert-manager (using k8s, see darkcustomdomain, other docs
  mentioning custom-domain). Some customer certs previously used Google-managed SSL,
  but no longer do. In the future we how to use Google Certificate Manager
- darksa.com, \*.darksa.com, darkstaticassets.com, \*.darkstaticassets.com:
  Automatically managed by Google Certificate Manager (not the same as cert-manager,
  or as Google Managed Certificates)

# Cert-manager

Most certificates are created by Cert-manager. This is a service we run ourselves in
k8s, and it manages renewals via Lets Encrypt. We configure customer certs
dynamically, adding them to the darkcustomdomain load balancer. We configure most of
our certs statically, but cert-manager and Lets Encrypt automatically renew them and
update the certs in the appropriate load balancers.
