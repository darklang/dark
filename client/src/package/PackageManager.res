open Prelude

let pmParamsToUserFnParams = (p: PT.Package.Parameter.t): PT.UserFunction.Parameter.t => {
  name: "",
  nameID: ID.generate(),
  typ: None,
  typeID: ID.generate(),
  description: p.description,
}

let paramData = (pfp: PT.Package.Parameter.t): list<blankOrData> => {
  let paramName = BlankOr.newF(pfp.name)
  let paramTipe = BlankOr.newF(pfp.tipe)
  list{PParamName(paramName), PParamTipe(paramTipe)}
}

let allParamData = (pmf: PT.Package.Fn.t): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, pmf.parameters))

let blankOrData = (pmf: PT.Package.Fn.t): list<blankOrData> => {
  let fnname = BlankOr.newF(FQFnName.PackageFnName.toString(pmf.name))
  list{PFnName(fnname), ...allParamData(pmf)}
}
