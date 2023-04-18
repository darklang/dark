# DB credentials rotation

Postgres has a reasonably challenging way to rotate credentials. See:

- http://davidhollenberger.com/2017/03/16/postgres-credential-rotation/

## Setup

_This has already been setup in the production DB._

Our default user is the `postgres` user - this user/role owns all the tables, so we
need the other roles to be able to act as the `postgres` user, so we'll add two roles
that we can rotate between.

- Add two users who can't login, `user1` and `user2`:

```
create role user1 with nologin in role postgres;
create role user2 with nologin in role postgres;
```

- Set their default login role as `postgres`

```
alter role user1 set role postgres;
alter role user2 set role postgres;
```

## Overview of pw rotation

- existing infra will be using either `user1` or `user2`. Find out which one.
- enable the other user, with a new password
- change the secrets in k8s to the other user
- bounce every pod
- check that everything is using the same user
- disable logins on the old user

## Enable a user/password

- Figure out which user is being used at the moment by

```
\du
SELECT DISTINCT usename FROM pg_stat_activity # presumably user1 or user2
```

- Enable the unused user `userNEW`

```
alter role userNEW login password 'NEW PASSWORD';
```

- Log in with the user and validate that it's working:

```
create table check_its_working(id int);
select * from pg_tables where tableowner = 'postgres'; # should include `check_its_working`
drop table check_its_working;
```

- Update all uses (we update in place because it's a much bigger hassle not to)

  - update the `default` namespace

    - backup secret: `kubectl get secrets -n default cloudsql-db-credentials -o yaml > old-db-secret.yaml`
    - update the password in the secret
      - get the base64 for the new password: echo -n "NEW PASSWORD" | base64
      - get the base64 for the new user: echo -n "userNEW" | base64
      - edit the secret: `kubectl edit secrets cloudsql-db-credentials -n default`
    - restart each service (least risky first):
      - `kubectl rollout restart deployment editor-deployment -n default`
      - `kubectl rollout restart deployment garbagecollector-deployment -n default`
      - `kubectl rollout restart deployment cronchecker-deployment -n default`
    - check each starts up
    - In case of error
      - restore old secret: - `kubectl apply -f old-db-secret.yaml`
      - bounce again

  - update the `darklang` namespace
    - backup secret: `kubectl get secrets -n darklang cloudsql-db-credentials -o yaml > old-db-secret.yaml`
    - update the password in the secret
      - get the base64 for the new password: echo -n "NEW PASSWORD" | base64
      - get the base64 for the new user: echo -n "userNEW" | base64
      - edit the secret: `kubectl edit secrets cloudsql-db-credentials -n darklang`
    - restart each service (least risky first):
      - `kubectl rollout restart deployment prodexec-deployment -n darklang`
      - `kubectl rollout restart deployment queueworker-deployment -n darklang`
      - `kubectl rollout restart deployment bwdserver-deployment -n darklang`
    - check each starts up
    - In case of error
      - restore old secret: - `kubectl apply -f old-db-secret.yaml`
      - bounce again

## Disable the old user/password

- Validate there's no users logged in with this user

```
SELECT DISTINCT usename FROM pg_stat_activity # presumably user1 or user2
```

More details

```
select datname, usename, count(usename)
from pg_stat_activity
group by datname, usename
order by datname;
```

- match expectation of users who can't log in: `\du`

- Disable the unused user `userOLD`

```
alter role userOLD nologin password 'SOME GIBBERISH';
```
