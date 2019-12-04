Managed SSL Certificates
========================

GCP can do most of the hard work of setting up a Let's Encrypt cert for you.
CAVEAT: a GCP load balancer can have no more than 15 certs. We currently have
two for us (bwd-tls, darklang-tls - note that bwd-ingress serves both bwd.com
and darklang.com, I believe) and one for birb (www.hellobirb.com-tls) that are
_not_ managed certificates.

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
- Run `scripts/add-custom-domain`; it will generate the necessary code changes.
- It will also instruct you in the necessary manual steps to run _after_ the
  CNAME is ready, but _before_ merging the script's changes.
  - The CNAME must exist before the below is done because Let's Encrypt (and
    thus Google) uses DNS to verify that "we" (the user) control the domain
    before issuing a cert.
  - (Provisioning a cert also requires that the ingress have the
    `kubernetes.io/ingress.global-static-ip-name` annotation set; that's already
    in there for `www.kiksht.com-cert`, so this is no longer necessary for the
    bwd-ingress.)
