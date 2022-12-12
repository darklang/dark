module ClientTypes.Authorization

type Permission =
  | Read
  | ReadWrite

type UserInfo = { username : string; name : string; email : string }
