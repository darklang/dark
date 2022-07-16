open Prelude

let pmParamsToUserFnParams = (p: packageFnParameter): PT.UserFunction.Parameter.t => {
  name: BlankOr.newF(p.name),
  typ: BlankOr.newF(p.tipe),
  args: list{},
  optional: false,
  description: p.description,
}

let paramData = (pfp: packageFnParameter): list<blankOrData> => {
  let paramName = BlankOr.newF(pfp.name)
  let paramTipe = BlankOr.newF(pfp.tipe)
  list{PParamName(paramName), PParamTipe(paramTipe)}
}

let allParamData = (pmf: packageFn): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, pmf.parameters))

let blankOrData = (pmf: packageFn): list<blankOrData> => {
  let fnname = BlankOr.newF(pmf.fnname)
  list{PFnName(fnname), ...allParamData(pmf)}
}

let extendedName = (pkgFn: packageFn): string =>
  Printf.sprintf(
    "%s/%s/%s::%s_v%d",
    pkgFn.user,
    pkgFn.package,
    pkgFn.module_,
    pkgFn.fnname,
    pkgFn.version,
  )

let fn_of_packageFn = (pkgFn: packageFn): function_ => {
  let paramOfPkgFnParam = (pkgFnParam: packageFnParameter): parameter => {
    paramName: pkgFnParam.name,
    paramTipe: pkgFnParam.tipe,
    paramDescription: pkgFnParam.description,
    paramBlock_args: list{},
    paramOptional: false,
  }

  {
    fnName: Package({
      owner: pkgFn.user,
      package: pkgFn.package,
      module_: pkgFn.module_,
      function: pkgFn.module_,
      version: pkgFn.version,
    }),
    fnParameters: pkgFn.parameters |> List.map(~f=paramOfPkgFnParam),
    fnDescription: pkgFn.description,
    fnReturnTipe: pkgFn.return_type,
    fnPreviewSafety: Unsafe,
    fnDeprecated: pkgFn.deprecated,
    fnInfix: false,
    fnIsSupportedInQuery: false,
    fnOrigin: PackageManager,
  }
}
