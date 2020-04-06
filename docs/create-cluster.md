Create cluster
=============

Occasionally, you may wish to spin up a separate k8s cluster for testing, say,
infra stuff.

To do this:
```
# assuming you want to use the existing DB
./scripts/gke-create-cluster --with-new-database=false
```

If the hardcoded cluster version (of gke) is no longer available, that script
will tell you what versions _are_ currently supported.

Once this is done, you can build containers locally and deploy them to the
cluster:
```
./scripts/gcp-build-containers
./scripts/gcp-push-images-to-gcr
./scripts/gke-deploy --cluster=darkcluster1570566003 --skip-rollbar
```

Note: the cluster argument can be had by running `kubectl config get-contexts`;
the value you want is `darkcluster<timestamp>` (the current prod cluster is
`darkcluster`). You can also get this cluster name from
https://console.cloud.google.com/kubernetes/list?project=balmy-ground-195100.

You may also want to edit
`scripts/support/kubernetes/builtwithdark/bwd-deployment.yaml.template` and
`scripts/support/kubernetes/builtwithdark/editor-deployment.yaml.template` and
set `replicas: 1`, or something _slightly_ less ridiculous than 72. (Deploys
will be faster, and you likely won't have prod-level traffic.)


Use
=====
If all has gone well, you now have a cluster ... on a different load balancer.
Get the IP address (`kubectl get ingress`), and then use curl with `--resolve`
to make certs DTRT: `curl --resolve darklang.com:443:ip https://darklang.com/`
(hostname, obviously, may be other things depending what you are testing).


Teardown
========
When done, go to
https://console.cloud.google.com/kubernetes/list?project=balmy-ground-195100 and
delete your new cluster manually.
