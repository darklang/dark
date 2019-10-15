Managed SSL Certificates
========================

GCP can do most of the hard work of setting up a Let's Encrypt cert for you.
CAVEAT: a GCP load balancer can have no more than 15 certs. We currently have
two for us (bwd-tls, darklang-tls - note that bwd-ingress serves both bwd.com
and darklang.com, I believe) and one for birb (www.hellobirb.com-tls) that are
_not_ managed certificates.

To do this, first deploy a `ManagedCertificate`; see
`scripts/support/kubernetes/certs/www.kiksht.com.yaml` for an example of this.
You will also need to tell the ingress that this `ManagedCertificate` is there,
so it can handle provisioning - this is done in `bwd-ingress.yaml`'s
`metadata.annotations.networking.gke.io/managed-certificates`, which takes a
comma-separated list of cert names.

(Provisioning also requires that the ingress have the
`kubernetes.io/ingress.global-static-ip-name` annotation set; that's already in
there for `www.kiksht.com-cert`.)

Once done, you can run `kubectl get managedcertificate`, followed by `kubectl describe managedcertificate $cert_name`. Look at `Status."Certificate Status"` - it should go from `Provisioning` to `Active` in no more than 15 minutes.

Once that's done, you're all set for SSL. `curl https://www.kiksht.com` (for
instance) will still get you `Hello internal overlord`, because we haven't told
`webserver.ml` to route that hostname to an appropriate canvas. You'll probably
also want to set up an apex->www redirect, and force ssl.  This all involves two
edits in `webserver.ml` and one in `nginx.conf`; see
https://github.com/darklang/dark/pull/1492 for an example of this.

Status: Provisioning
> Wait for the managed certificate to be provisioned. This may take up to 15 minute s
