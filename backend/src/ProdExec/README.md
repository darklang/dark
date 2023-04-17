# Prod Execution Host

Based on
https://andrewlock.net/deploying-asp-net-core-applications-to-kubernetes-part-10-creating-an-exec-host-deployment-for-running-one-off-commands/,
this is an executable which runs on a container in production, that can be used
for one off tasks. To start with, the one-off tasks are:

- run migrations
- emergency login if auth provider is down
