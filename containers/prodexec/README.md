# ProdExec container

The prodexec container runs as service in Cloud Run. It's purpose is to ssh into it
so we can run commands like DB migrations.

Since Cloud Run doesn't allow ssh access, we tunnel it through http using chisel. For
security, it should only be accessible via google cloud, and never on the public
internet.

To connect, run ./scripts/production/connect-to-prod-exec, which will ssh you in if you have
permission to do so.