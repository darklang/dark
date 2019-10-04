Custom Domains
==============

So. A Dark user wants to point a custom domain at Dark! COOL. This is not just
CNAME mycooldomain.io user-canvas.builtwithdark.com, action is required by Dark
ops.

User steps:
- Create said CNAME in your DNS setup of choice. (Can be done before or after
  Dark steps.)
- Provide us with an SSL cert for your host. (Must happen before we process this
  request.)

Dark steps (for bwd):
- Edit `backend/libbackend/webserver.ml` - add an entry to the `route_host`
  function under `Customers` - examples of this include `*.dabblefox.com` and
`hellobirb.com`.
- Create a kubernetes secret with the user's SSL cert. See
  `www.hellobirb.com-tls` for an example; it will contain both `tls.crt` and
`tls.key` files.
  - This is approximately ```kubectl create secret tls ${domain}-tls \
  --cert=tls.crt \
  --key=tls.key```
- add the secret to `scripts/support/kubernetes/builtwithdark/bwd-ingress.yaml`;
  there is a list of certs under `spec.tls`.

Dark steps (for static assets):
- [ ] TBD

TBD:
- [ ] we should provide users with a secure way of giving us their certs. At least
as an option, if not mandatory yet. GPG? (Keybase on our end makes this easier
to support, though for bus factor reasons we might want something like Samir's
Secret Sharing - I'm so sorry, nevermind, we just want a secure form where they
can upload certs to us.)
- [ ] k8s secrets don't have history/rollback, so maybe we want to keep copies of certs elsewhere in case of operator error? And also disaster recovery.
- [ ] can we validate certs coming in from users, maybe via a script and openssl?
Just, yes, this key+crt are a pair, and they specify the cname you said you
wanted. Could maybe also create the k8s secret with this script
- [ ] can we have some monitor on all certs installed in this way to alert us
ahead of expiration? (And then we can tell users "hey, your cert's gonna expire
in 30 days, pls renew".) Only as a stopgap until we can automate this.
- [ ] Let's Encrypt! TLS termination inside k8s, at least for hostnames that are
not ours.
- [ ] We can only have 15 certs in an LB, as of 2019-10-04 we're using 3 (bwd, darklang, LE solves this, in addition to reducing the ops time needed to service these reqs.
hellobirb)
