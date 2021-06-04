Config-connector is a google project that allows us to manage google cloud
services using k8s manifests. It was originally added so that cert-manager
certs could be added to load balancers that front the Dark Static Analysis
buckets.

This was added using the instructions at
https://cloud.google.com/config-connector/docs/how-to/install-upgrade-uninstall#gcloud_1
