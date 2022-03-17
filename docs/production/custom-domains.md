# Custom Domains (with certs via Let's Encrypt)

## Customer requirements

- You need to set up a CNAME from your desired domain to
  `<canvas>.darkcutsomdomain.com`.
  - Note: this cannot be an apex (`foo.com`); using `www.foo.com` is the usual
    way, though you could use `app` or `api` or another subdomain instead of
    `www`. Why? Netlify has a post about this
    (https://www.netlify.com/blog/2017/02/28/to-www-or-not-www/), but tl;dr apex
    CNAMEs aren't supported by the DNS spec, and A records remove some of our
    options for providing stable and resilient infrastructure.

## Dark ops instructions

- Run `scripts/custom-domains/add` and provide the domain (eg `api.example.com`); we'll get the canvas
  name from the CNAME, which also verifies that the CNAME DNS record is in
  place.
  Make sure to supply the DOMAIN without the `https://` prefix.
- The CNAME must exist before the below is done because Let's Encrypt uses an
  HTTP request to verify that "we" (the user) control the domain before issuing
  a cert.
- There are no manual steps to run, nor deploys needed, to provision
  a custom domain.

## Deleting custom domains

Deleting custom domains is inherently lossy cause k8s sucks.

Domains are stored in three places: in our custom_domains table in the main DB,
and also in the `darkcustomdomain-l4-ingress` in `.spec.tls[]` and also in
`.spec.rules[]`. The latter is to enable SSL, the former is to connect the
request to the appropriate canvas.

Removing from the DB is straightforward with SQL. Removing from
`darkcustomdomain-l4-ingress` is not. They are lists, and there is no safe way
to remove a single entry from a list in k8s (it does not have a "remove the
array element with this value" command).

These actions are scripted, split into two parts:

- scripts/custom-domains/delete-from-cert-manager
- scripts/custom-domains/delete-from-db

You need to do both and they're interactive (reflecting the risky nature).

## Implementation details

See `scripts/custom-domains/add` for a high level overview.

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
