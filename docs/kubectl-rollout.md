kubernetes rollouts
===================

## history
Use `kubectl rollout history <deployment>` to get
the history:

```
dark@dark-dev:~/app$ kubectl rollout history deployment/bwd-deployment
deployment.extensions/bwd-deployment 
REVISION  CHANGE-CAUSE
701       'circle=https://circleci.com/gh/darklang/dark/2276 ; orig-time: Wed Oct 31 23:01:15 UTC 2018'
703       'circle=https://circleci.com/gh/darklang/dark/2279 ; orig-time: Thu Nov  1 18:33:05 UTC 2018'
705       'circle=https://circleci.com/gh/darklang/dark/2280 ; orig-time: Thu Nov  1 21:03:19 UTC 2018'
707       'circle=https://circleci.com/gh/darklang/dark/2290 ; orig-time: Fri Nov  2 18:07:32 UTC 2018'
[etc]
```
Note here that #2 and #3 are missing, because we rolled back to them (rev 6 is a
rollback to 3, rev 7 is a rollback to 2).

`CHANGE-CAUSE` in the above example is `<none>`, but we're setting that value
now in `scripts/gke-deploy` using `kubectl annotate`, to include both a
timestamp (of the original deploy, rollbacks won't change the timestamp) and a
link to the circleci build.

## undo
`kubectl rollout undo <deployment> --to-revision=N`
Note that, as above, rolling back to revision N will create a new revision,
_and_ remove N from the revision history.

If you do this, you'll likely also want to use `kubectl rollout pause` until
CI/CD is fixed.

## pause and resume
`kubectl rollout pause <deployment>` will prevent any further changes to the
spec from taking effect (that is, `kubectl apply` and `kubectl set image` will
still run successfully, but they will not scale up).

`kubectl rollout resume <deployment>` will resume updates; note that this
doesn't just allow new images to be deployed; it also applies the latest
(paused) update.
