# Incident Response

hello friendly neighbourhood L3 responder! the engineering team is extremely sorry
for L1+L2 response being AWOL.

### Things to check:

- Is google cloud down? https://status.cloud.google.com/
  - If so, there's not much we can do unfortunately. Restarting our services will not help
- The pingdom service which determines if we're up or not hits https://dabblefox-shelf.builtwithdark.com/ping, so you can check this yourself to determine if we're totally down, or only partially down.

### Resolving Issues:

#### Google Cloud Shell

Google helpfully have a web-based shell that you can use anywhere for incident response.

- Go to https://console.cloud.google.com/home/dashboard?project=balmy-ground-195100
- Click "Active Google Cloud Shell" button in top right (it's this button: https://www.dropbox.com/s/q042csiq1ebgzqs/Screenshot%202018-07-17%2016.20.11.png?dl=0)
- Wait for the shell to boot
- Run the following command to finish setup

```
$ gcloud container clusters get-credentials "$(< current-cluster)" --zone=us-west1 --project=balmy-ground-195100
```

#### Checking on the Kubernetes pods:

Run the following command at the cloud shell prompt:

```
$ kubectl get pods
```

You should see something like:

https://www.dropbox.com/s/nt9vd579eiyq3kh/Screenshot%202018-07-17%2016.28.36.png?dl=0

##### Interpreting the 'Status' column

- If everything is mostly 'Running' then Kubernetes thinks the system is healthy. If we're still down, you should try to restart the pods via the section below
- Anything else like Terminating, CrashLoopBackOff etc. indicates that kubernetes knows that the system is unhealthy and is working to fix it

#### Restarting Kubernetes pods:

Run the following command:

`scripts/production/force-system-restart`
