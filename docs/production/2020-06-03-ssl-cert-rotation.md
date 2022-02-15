TO ROTATE SECRET:
=================
<name> is a secret's name.

# This is your backup
kubectl get secret <name> -o yaml > old-<name>.yaml

You can, if you wish, use `yq` (installed in the container, or in pip if you
want to run it outside of docker) to get the `.data` and any keys; remember that
these values are base-b64 armored, so you may want to pipe the results through
`base64 -d`.

Now create the new one. In this example, I want to replace `.data."tls.crt"`with
new contents from a file.

I did this by running
`cat <new file> | base64 | xsel -ib`, and then manually editing a copy of
old-<name>.yaml that I called new-<name>.yaml.

I then confirmed that it looks right:
`yq -r '.data."tls.crt"' new-<name>.yaml | base64 -d` looks like a buncha cets.

Now to deploy!:
`kubectl apply -f new-<name>.yaml`

If you need to rollback, `kubectl apply -f old-<name>.yaml`.

Post deploy, confirm that the new md5sum is as expected:
```
kubectl get secret www.hellobirb.com-tls -o yaml |  yq -r '.data."tls.crt"' |
base64 -d | md5sum
```

It won't _take effect_ until the pods all restart - a deploy is the easiest way
to do this.
