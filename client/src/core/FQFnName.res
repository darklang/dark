module StdlibFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {module_: string, function: string, version: int}

  let toString = (std: t) => {
    let name = if std.module_ == "" {
      std.function
    } else {
      `${std.module_}::${std.function}`
    }
    if std.version == 0 {
      name
    } else {
      `${name}_v${Belt.Int.toString(std.version)}`
    }
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      // Note: module has no underscore in the API
      module_: field("module_", string, j),
      function: field("function_", string, j),
      version: field("version", int, j),
    }
  }

  let encode = (n: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("module_", string(n.module_)),
      ("function_", string(n.function)),
      ("version", int(n.version)),
    })
  }
}

module UserFnName = {
  @ppx.deriving(show({with_path: false})) type rec t = string
  let decode = Json_decode_extended.string
  let encode = Json_encode_extended.string
}

module PackageFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    owner: string,
    package: string,
    module_: string,
    function: string,
    version: int,
  }

  let toString = (pkg: t): string =>
    `${pkg.owner}/${pkg.package}/${pkg.module_}::${pkg.function}_v${Belt.Int.toString(pkg.version)}`

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      owner: field("owner", string, j),
      package: field("package", string, j),
      module_: field("module_", string, j),
      function: field("function_", string, j),
      version: field("version", int, j),
    }
  }
  let encode = (n: t): Js.Json.t => {
    open Json.Encode
    object_(list{
      ("owner", string(n.owner)),
      ("package", string(n.package)),
      ("module_", string(n.module_)),
      ("function_", string(n.function)),
      ("version", int(n.version)),
    })
  }
}

@ppx.deriving(show({with_path: false}))
type rec t =
  | User(UserFnName.t)
  | Stdlib(StdlibFnName.t)
  | Package(PackageFnName.t)

let toString = (fqfnName: t): string =>
  switch fqfnName {
  | User(name) => name
  | Stdlib(std) => StdlibFnName.toString(std)
  | Package(pkg) => PackageFnName.toString(pkg)
  }

let encode = (n: t): Js.Json.t => {
  open Json_encode_extended
  let ev = variant
  switch n {
  | User(name) => ev("User", list{string(name)})
  | Stdlib(name) => ev("Stdlib", list{StdlibFnName.encode(name)})
  | Package(name) => ev("Package", list{PackageFnName.encode(name)})
  }
}

let decode = (j: Js.Json.t): t => {
  open Json_decode_extended
  variants(
    list{
      ("User", variant1(name => User(name), UserFnName.decode)),
      ("Stdlib", variant1(name => Stdlib(name), StdlibFnName.decode)),
      ("Package", variant1(name => Package(name), PackageFnName.decode)),
    },
    j,
  )
}

let stdlib = (m: string, f: string, v: int): t => Stdlib({
  module_: m,
  function: f,
  version: v,
})

let package = (o: string, p: string, m: string, f: string, v: int): t => Package({
  owner: o,
  package: p,
  module_: m,
  function: f,
  version: v,
})
