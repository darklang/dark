// TODO combine with RuntimeTypes.BuiltinFn

@ppx.deriving(show({with_path: false}))
type rec parameter = {
  paramName: string,
  typ: DType.t,
  args: list<string>,
  description: string,
}

@ppx.deriving(show({with_path: false}))
type rec previewSafety =
  | Safe
  | Unsafe

@ppx.deriving(show({with_path: false}))
type rec fnOrigin =
  | UserFunction
  | PackageManager
  | Builtin

@ppx.deriving(show({with_path: false}))
type rec t = {
  fnName: FQFnName.t,
  fnParameters: list<parameter>,
  fnDescription: string,
  fnReturnTipe: DType.t,
  fnPreviewSafety: previewSafety,
  fnDeprecated: bool,
  fnInfix: bool,
  fnIsSupportedInQuery: bool,
  // This is a client-side only field to be able to give different UX to
  // different functions
  fnOrigin: fnOrigin,
}

let fromUserFn = (f: ProgramTypes.UserFunction.t): option<t> => {
  let ufpToP = (ufp: ProgramTypes.UserFunction.Parameter.t): option<parameter> =>
    switch (ufp.name, ufp.typ) {
    | ("", _) => None
    | (_, None) => None
    | (name, Some(typ)) =>
      {
        paramName: name,
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
      fnParameters: ps,
      fnDescription: f.description,
      fnReturnTipe: f.returnType,
      fnInfix: false,
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnIsSupportedInQuery: false,
      fnOrigin: UserFunction,
    })
  } else {
    None
  }
}

let fromPkgFn = (pkgFn: ProgramTypes.Package.Fn.t): t => {
  let paramOfPkgFnParam = (pkgFnParam: ProgramTypes.Package.Parameter.t): parameter => {
    paramName: pkgFnParam.name,
    typ: pkgFnParam.tipe,
    description: pkgFnParam.description,
    args: list{},
  }

  {
    fnName: Package(pkgFn.name),
    fnParameters: pkgFn.parameters |> Tc.List.map(~f=paramOfPkgFnParam),
    fnDescription: pkgFn.description,
    fnReturnTipe: pkgFn.returnType,
    fnPreviewSafety: Unsafe,
    fnDeprecated: pkgFn.deprecated,
    fnInfix: false,
    fnIsSupportedInQuery: false,
    fnOrigin: PackageManager,
  }
}

let fromBuiltinFn = (fn: RuntimeTypes.BuiltInFn.t): t => {
  let toParam = (p: RuntimeTypes.BuiltInFn.Param.t): parameter => {
    paramName: p.name,
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
    fnParameters: fn.parameters |> Tc.List.map(~f=toParam),
    fnDescription: fn.description,
    fnReturnTipe: fn.returnType,
    fnPreviewSafety: switch fn.previewable {
    | Pure => Safe
    | Impure => Unsafe
    | ImpurePreviewable => Unsafe
    },
    fnDeprecated: fn.deprecated != NotDeprecated,
    fnInfix: fn.isInfix,
    fnIsSupportedInQuery: RuntimeTypes.BuiltInFn.SqlSpec.isQueryable(fn.sqlSpec),
    fnOrigin: Builtin,
  }
}
