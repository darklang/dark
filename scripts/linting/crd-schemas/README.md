# Schemas for CRDs

These can be generated using

```
kubectl get crds -o yaml > crds_from_cluster.yaml
python3 openapi2jsonschema.py crds_from_cluster.yaml
```

with [https://github.com/yannh/kubeconform/blob/d536a659bdb20ee6d06ab55886b348cd1c0fa21b/scripts/openapi2jsonschema.py](openapi2jsonschema.py)
