module BuiltInFn = RuntimeTypes.BuiltInFn

@ppx.deriving(show({with_path: false}))
type rec origin =
  | UserFunction
  | PackageManager
  | Builtin

// A superset of other function types, but basically matches a RuntimeTypes.BuiltInFn.t
@ppx.deriving(show({with_path: false}))
type rec t = {
  name: FQFnName.t,
  parameters: list<BuiltInFn.Param.t>,
  description: string,
  returnType: DType.t,
  previewable: BuiltInFn.Previewable.t,
  deprecation: BuiltInFn.Deprecation.t,
  isInfix: bool,
  sqlSpec: BuiltInFn.SqlSpec.t,
  // This is a client-side only field to be able to give different UX to
  // different functions
  origin: origin,
}

let fromUserFn = (f: ProgramTypes.UserFunction.t): option<t> => {
  let ufpToP = (ufp: ProgramTypes.UserFunction.Parameter.t): option<BuiltInFn.Param.t> =>
    switch (ufp.name, ufp.typ) {
    | ("", _) => None
    | (_, None) => None
    | (name, Some(typ)) =>
      {
        BuiltInFn.Param.name: name,
        typ: typ,
        args: list{},
        description: ufp.description,
      } |> (x => Some(x))
    }
  let ps = Tc.List.filterMap(~f=ufpToP, f.parameters)
  let sameLength = List.length(ps) == List.length(f.parameters)
  if sameLength && f.name != "" {
    Some({
      name: User(f.name),
      parameters: ps,
      description: f.description,
      returnType: f.returnType,
      isInfix: false,
      previewable: Impure,
      deprecation: NotDeprecated,
      sqlSpec: NotQueryable,
      origin: UserFunction,
    })
  } else {
    None
  }
}

let fromPkgFn = (pkgFn: ProgramTypes.Package.Fn.t): t => {
  let paramOfPkgFnParam = (pkgFnParam: ProgramTypes.Package.Parameter.t): BuiltInFn.Param.t => {
    name: pkgFnParam.name,
    typ: pkgFnParam.typ,
    description: pkgFnParam.description,
    args: list{},
  }
  {
    name: Package(pkgFn.name),
    parameters: pkgFn.parameters |> Tc.List.map(~f=paramOfPkgFnParam),
    description: pkgFn.description,
    returnType: pkgFn.returnType,
    previewable: Impure,
    deprecation: if pkgFn.deprecated {
      DeprecatedBecause("") // TODO: we don't know why at this point
    } else {
      NotDeprecated
    },
    isInfix: false,
    sqlSpec: NotQueryable,
    origin: PackageManager,
  }
}

let fromBuiltinFn = (fn: BuiltInFn.t): t => {
  {
    name: Stdlib(fn.name),
    parameters: fn.parameters,
    description: fn.description,
    returnType: fn.returnType,
    previewable: fn.previewable,
    deprecation: fn.deprecated,
    isInfix: fn.isInfix,
    sqlSpec: fn.sqlSpec,
    origin: Builtin,
  }
}
