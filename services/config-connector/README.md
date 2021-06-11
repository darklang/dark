Config-connector is a google project that allows us to manage google cloud
services using k8s manifests. It was originally added so that cert-manager
certs could be added to load balancers that front the Dark Static Assets
buckets.

This was added using the instructions at
https://cloud.google.com/config-connector/docs/how-to/install-upgrade-uninstall#gcloud_1

The namespace is called `config-connector`

# Config-connector experiment

The goal of the config-connector experiment is to set up automatically rotating certs for darksa.com and darkstaticassets.com. The intent was:

1) Add darksa.com and darkstaticassets.com to cert-manager, using a secret
2) Create a new ingress fronting the dark-static-assets bucket
3) Add that secret as a TLS cert for that ingress

This relied on some new features of k8s and GKE:

- in kubernetes 1.19, you can have an ingress point to a Resource instead of a
  Service. The specific use case envisioned is a StorageBucket, as you can see
  in the docs. See
  https://kubernetes.io/docs/concepts/services-networking/ingress/#resource-backend.

- GKE has a feature called config-connector. It's like terraform in a way; in
  that it lets you configure cloud services using declarative yaml files.
  There's an agent running in our cluster that reads these configs, and then
  applies them to GCP services. See
  https://cloud.google.com/config-connector/docs/overview and
  https://cloud.google.com/config-connector/docs/reference/resource-docs/storage/storagebucket

Obviously, the combination of the two services is how we hook this up. You can see the examples in config-connector-experiment-\*.yaml to demonstrate. We create a k8s name for the bucket in config-connector-experiment-storagebucket.yaml, and then put an ingress in front of it in config-connector-experiment-ingress.yaml.

Alas this does not work. It seems this is because the GCE ingress' do not yet
support resources. This issue tracks it's implementation: https://issuetracker.google.com/182815525

# Future work

The process we want to follow when this is supported (note: do it on a test bucket
first, ensuring there is data in the bucket and checking that nothing happens
to that data):

- choose a bucket with data in it:
	- write the yaml for the bucket
	- add the storagebucket resource via kubectl apply
	- check it works by updating the bucket in some minor way via kubectl apply
	- make a new k8s ingress that fronts the StorageBucket
	- add certs to it via cert-manager
	- check that renewing the certs creates a new SSL cert for it
  - change the DNS from the old ingress to the new one



