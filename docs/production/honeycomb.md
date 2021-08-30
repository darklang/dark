# Honeycomb notes

Write key is stored in a k8s secret: honeycomb-writekey: https://ui.honeycomb.io/teams/dark/dataset_instructions (See also scripts/production/gke-create-cluster)

Note: changes to honeycomb.yaml that only affect the configmap will require that
you restart the honeycomb-agent pods. Fortunately, this is a one-liner:
`kubectl delete pod --selector k8s-app=honeycomb-agent`
