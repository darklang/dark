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

NOTE: DO NOT DO THIS WHILE A ROLLBACK IS IN PROGRESS. `kubectl rollout undo ...`
followed by `kubectl rollout pause ...` will pause the rollback in progress.
Wait until the rollback is complete before pausing.

`kubectl rollout resume <deployment>` will resume updates; note that this
doesn't just allow new images to be deployed; it also applies the latest
(paused) update.
