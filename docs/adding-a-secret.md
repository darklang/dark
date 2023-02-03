# Add a secret

## Add it to Kubernetes

Create a yaml file with the secret in it (`username` and `password` below):

```
apiVersion: v1
kind: Secret
metadata:
  name: segment-account-credentials
  namespace: darklang
type: Opaque
data:
  username: YWRtaW4=
  password: MWYyZDFlMmU2N2Rm
```

Note that the secret must be base64 encoded, a k8s requirement
(https://kubernetes.io/docs/concepts/configuration/secret/#creating-a-secret-manually).

Add it to Kubernetes with `kubectl apply -f secret.yaml`

## Keep it somewhere safe

If it's not available from the vendor's console, consider adding it to the Dark
admin 1password vault, so that we can recover it if we lose the cluster. Ask
Paul.

## Make it accessible to containers

To add the secret to a container, add an `env` stanza to the kubernetes
templates in services/\*\*/\*.yaml.

For example:

```
            - name: DARK_CONFIG_SEGMENT_WRITE_KEY
              valueFrom:
                secretKeyRef:
                  name: segment-account-credentials
                  key: segment-key
```

Each template has multiple containers so make sure it ends up in all
the containers that need it.

## Make it accessible to the app

Getting the production configuration into the F# app is handled in
`Config.fs` or `config.rs`. It typically makes it into the JS app
via `ui.html`, which has values filled in from `Ui.fs`.

## Non-secret configuration

Non-secret config should be stored in `config/gke-builtwithdark` (substitude
dev, etc, for gke-builtwithdark), and passed from there into production. You
should leave placeholders for the values loaded from kubernetes (the dynamic
configs are loaded last, and will overwrite the config-file config).
