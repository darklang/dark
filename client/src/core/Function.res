// TODO combine with RuntimeTypes.BuiltinFn

@ppx.deriving(show({with_path: false}))
type rec origin =
  | UserFunction
  | PackageManager
  | Builtin

@ppx.deriving(show({with_path: false}))
type rec t = {
  fnName: FQFnName.t,
  parameters: list<RuntimeTypes.BuiltInFn.Param.t>,
  description: string,
  returnType: DType.t,
  previewable: RuntimeTypes.BuiltInFn.Previewable.t,
  fnDeprecated: RuntimeTypes.BuiltInFn.Deprecation.t,
  infix: bool,
  fnIsSupportedInQuery: bool,
  // This is a client-side only field to be able to give different UX to
  // different functions
  origin: origin,
}

let fromUserFn = (f: ProgramTypes.UserFunction.t): option<t> => {
  let ufpToP = (ufp: ProgramTypes.UserFunction.Parameter.t): option<
    RuntimeTypes.BuiltInFn.Param.t,
  > =>
    switch (ufp.name, ufp.typ) {
    | ("", _) => None
    | (_, None) => None
    | (name, Some(typ)) =>
      {
        RuntimeTypes.BuiltInFn.Param.name: name,
        typ: typ,
        args: list{},
        description: ufp.description,
      } |> (x => Some(x))
    }
  let ps = Tc.List.filterMap(~f=ufpToP, f.parameters)
  let sameLength = List.length(ps) == List.length(f.parameters)
  if sameLength && f.name != "" {
    Some({
      fnName: User(f.name),
      parameters: ps,
      description: f.description,
      returnType: f.returnType,
      infix: false,
      previewable: Impure,
      fnDeprecated: NotDeprecated,
      fnIsSupportedInQuery: false,
      origin: UserFunction,
    })
  } else {
    None
  }
}

let fromPkgFn = (pkgFn: ProgramTypes.Package.Fn.t): t => {
  let paramOfPkgFnParam = (
    pkgFnParam: ProgramTypes.Package.Parameter.t,
  ): RuntimeTypes.BuiltInFn.Param.t => {
    name: pkgFnParam.name,
    typ: pkgFnParam.tipe,
    description: pkgFnParam.description,
    args: list{},
  }

  {
    fnName: Package(pkgFn.name),
    parameters: pkgFn.parameters |> Tc.List.map(~f=paramOfPkgFnParam),
    description: pkgFn.description,
    returnType: pkgFn.returnType,
    previewable: Impure,
    fnDeprecated: DeprecatedBecause(""), // TODO: we don't know why at this point
    infix: false,
    fnIsSupportedInQuery: false,
    origin: PackageManager,
  }
}

let fromBuiltinFn = (fn: RuntimeTypes.BuiltInFn.t): t => {
  let toParam = (p: RuntimeTypes.BuiltInFn.Param.t): RuntimeTypes.BuiltInFn.Param.t => {
    name: p.name,
    typ: p.typ,
    description: p.description,
    args: p.args,
  }
  {
    fnName: Stdlib({
      module_: fn.name.module_,
      function: fn.name.function,
      version: fn.name.version,
    }),
    parameters: fn.parameters |> Tc.List.map(~f=toParam),
    description: fn.description,
    returnType: fn.returnType,
    previewable: fn.previewable,
    fnDeprecated: fn.deprecated,
    infix: fn.isInfix,
    fnIsSupportedInQuery: RuntimeTypes.BuiltInFn.SqlSpec.isQueryable(fn.sqlSpec),
    origin: Builtin,
  }
}
