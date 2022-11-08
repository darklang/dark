module ClientTypes2BackendTypes.Authorization

module Auth = LibBackend.Authorization
module CTAuth = ClientTypes.Authorization

module Permission =
  let toCT
    (p : LibBackend.Authorization.Permission)
    : ClientTypes.Authorization.Permission =
    match p with
    | Auth.Read -> CTAuth.Read
    | Auth.ReadWrite -> CTAuth.ReadWrite
