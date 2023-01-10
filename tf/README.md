# Terraform config

This is our production terraform. We have only started using it recently, and
so we are slowly migrating it over, as we make changes to config.

I made a deliberate choice not to just migrate everything over to reduce the
risk of breaking something. Fortunately, if it's not imported into the state
then terraform won't try to manage it (and hence won't try to delete it).
