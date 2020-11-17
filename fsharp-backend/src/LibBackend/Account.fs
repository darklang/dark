module LibBackend.Account

let bannedUsernames : List<string> =
  // originally from https://ldpreload.com/blog/names-to-reserve
  // we allow www, because we have a canvas there
  [ "abuse"
    "admin"
    "administrator"
    "autoconfig"
    "broadcasthost"
    "ftp"
    "hostmaster"
    "imap"
    "info"
    "is"
    "isatap"
    "it"
    "localdomain"
    "localhost"
    "mail"
    "mailer-daemon"
    "marketing"
    "mis"
    "news"
    "nobody"
    "noc"
    "noreply"
    "no-reply"
    "pop"
    "pop3"
    "postmaster"
    "root"
    "sales"
    "security"
    "smtp"
    "ssladmin"
    "ssladministrator"
    "sslwebmaster"
    "support"
    "sysadmin"
    "usenet"
    "uucp"
    "webmaster"
    "wpad"
    // original to us from here
    "billing"
    "dev"

    // alpha, but not beta, because user beta already exists (with ownership
    // transferred to us)
    "alpha" ]
