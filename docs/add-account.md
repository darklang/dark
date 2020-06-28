## Get a Dark account for yourself

(note: this appears to be broken at the moment)

Most contributors can use the `dark` account (password `what`) instead of this.

To add your account to local dev for yourself, run:
```
scripts/run-in-docker _build/default/backend/bin/add_admin.exe --prompt-for-password
```
which will prompt you for your password, username, email, and name.

This will output
```
  upsert_admin_exn
      { username = "YOURNAME"
      ; password =
          Password.from_hash
            "..."
      ; email = "developer@example.com"
      ; name = "Ada Lovelace"};
```

Dark employees should open a PR adding this account data to `account.ml` in the
`upsert_admins` function.

Note: this password is _not_ used in production; it may be different
from your password on darklang.com.


