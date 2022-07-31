module Permission = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | Read
    | ReadWrite

  let decode = j => {
    open Json_decode_extended
    variants(list{("Read", variant0(Read)), ("ReadWrite", variant0(ReadWrite))}, j)
  }
  let encode = (str: t): Js.Json.t => {
    open Json_encode_extended
    let ev = variant
    switch str {
    | Read => ev("Read", list{})
    | ReadWrite => ev("ReadWrite", list{})
    }
  }
}

module Account = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    name: string,
    email: string,
    username: string,
  }

  let decode = (j): t => {
    open Json.Decode
    {
      name: field("name", string, j),
      email: field("email", string, j),
      username: field("username", string, j),
    }
  }
  let encode = (a: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{
      ("name", string(a.name)),
      ("email", string(a.email)),
      ("username", string(a.username)),
    })
  }

  let default: t = {name: "", email: "", username: ""}
}
