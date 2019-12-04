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
    way.
- You can, if you wish, also set up an A record pointing the apex (again,
  `foo.com`) to `35.227.208.117`; we cannot currently provide an SSL cert for
  that, but we can redirect to the `www` domain.

## Dark ops instructions
- The CNAME must exist before the below is done; Let's Encrypt (and thus Google)
  uses DNS to verify that "we" (the user) control the domain before issuing a
  cert, so having DNS resolve is a blocker. Confirm that `dig www.foo.com`
  resolves to `CNAME <canvas>.builtwithdark.com` before proceeding.
- First,  deploy a `ManagedCertificate`; see
  `scripts/support/kubernetes/certs/www.kiksht.com.yaml` for an example of this.
  You will also need to tell the ingress that this `ManagedCertificate` is there,
  so it can handle provisioning - this is done in `bwd-ingress.yaml`'s
  `metadata.annotations.networking.gke.io/managed-certificates`, which takes a
  comma-separated list of cert names.

  I recommend deploying this manually (`kubectl apply -f
scripts/support/kubernetes/certs/<www.foo.com>.yaml`) before merging.

(Provisioning also requires that the ingress have the
`kubernetes.io/ingress.global-static-ip-name` annotation set; that's already in
there for `www.kiksht.com-cert`.)

- Once this is deployed, you can run `kubectl get managedcertificate`, followed
  by `kubectl describe managedcertificate $cert_name`. Look at
`Status."Certificate Status"` - it should go from `Provisioning` to `Active` in
no more than 15 minutes.

- You'll need to edit `webserver.ml` and `nginx.conf` to do a few things - see
  https://github.com/darklang/dark/pull/1492 for an example:
  - tell the server which canvas to route `www.foo.com` to
  - to force SSL (incoming reqs to `http://www.foo.com` redirect to
    `https://...`)
  - redirect from the apex to the www (`foo.com` -> `www.foo.com`)
