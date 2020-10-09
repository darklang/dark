Custom Domains (with certs via Let's Encrypt)
========================

Our former constraint of <= 15 certs is no longer applicable! Yay.

## Customer requirements
- You need to set up a CNAME from your desired domain to
  `<canvas>.builtwithdark.com`.
  - Note: this cannot be an apex (`foo.com`); using `www.foo.com` is the usual
    way, though you could use `app` or `api` or another subdomain instead of
    `www`. Why? Netlify has a post about this
    (https://www.netlify.com/blog/2017/02/28/to-www-or-not-www/), but tl;dr apex
    CNAMEs aren't supported by the DNS spec, and A records remove some of our
    options for providing stable and resilient infrastructure.
- You can, if you wish, also set up an A record pointing the apex (`foo.com`) to
  `35.227.208.117`; we cannot currently provide an SSL cert for that, but we can
  redirect to your main subdomain (usually `www`). If your DNS provider supports
  ALIAS records (like CNAME, but permitted on an apex), that also works - though
  again, for redirects, not SSL certs.

## Dark ops instructions
- Make sure you have run `scripts/gcp-authorize-kubectl` (if you have not, you might get `The connection to the server localhost:8080 was refused - did you specify the right host or port?` in the next step)
- Run `scripts/add-custom-domain` and provide the domain (eg `api.example.com`); we'll get the canvas
  name from the CNAME, which also verifies that the CNAME DNS record is in
place.
  Make sure to supply the DOMAIN without the `https://` prefix.
- The CNAME must exist before the below is done because Let's Encrypt uses an
  HTTP request to verify that "we" (the user) control the domain before issuing
a cert.
- There are no longer any manual steps to run, nor deploys needed, to provision
  a custom domain.

## Implementation details
See `scripts/add-custom-domain.md` for a high level overview.

If you're wondering about the `cert-manager-*` yamls in
`services/cert-manager/`, see the [cert-manager
docs](https://cert-manager.io/docs/); these files were taken directly from the
[cert-manager Kubernetes installation
instructions](https://cert-manager.io/docs/installation/kubernetes/), see
"installing with regular manifests" since we don't use Helm. The [cert-manager
Concepts doc](https://cert-manager.io/docs/concepts/) may also be useful.

tl;dr: adding a tls host to the `darkcustomdomain-l4-ingress` resource causes
Cert Manager to request a cert from Let's Encrypt and launch a pod to respond to
[Let's Encrypt/ACME's HTTP-01 challenge](https://letsencrypt.org/docs/challenge-types/).

If you're interested in `darkcustomdomain-ingress.yaml`,
`nginx-ingress-controller.yaml`, or `darkcustomdomain-ip-svc.yaml`, those are
derived from the [kubernetes/ingress-nginx static-ip
example](https://github.com/kubernetes/ingress-nginx/tree/master/docs/examples/static-ip).
