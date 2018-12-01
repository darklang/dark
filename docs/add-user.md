# How to add users

`$ scripts/run-in-docker backend/_build/default/bin/add_user.exe`

Fill in username, email, name.

This will print out something like this:

```
  (* This user's password is as follows: asdfghjklzxcvbnm
     Insert everything after this into backend/libbackend/account.ml *)

  upsert_account
    { username = "ausername"
    ; password = "qwertyuiopasdfghjklzxcvbnmqwertyuioiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcvbnmqwertyuiopasdfghjklzxcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="
    ; email = "theiremail@gmail.com"
    ; name = "FirstName Lastname"};

```

Take a note of the user password, and send it to the user. It will not be available again.

Go to backend/libbackend/account.ml, and add the object to the bottom.

Make a PR, then merge it. As soon as it goes live, the password will work.

