open Prelude

let pmParamsToUserFnParams = (p: PT.Package.Parameter.t): PT.UserFunction.Parameter.t => {
  name: p.name,
  nameID: ID.generate(),
  typ: Some(p.typ),
  typeID: ID.generate(),
  description: p.description,
}

let paramData = (pfp: PT.Package.Parameter.t): list<blankOrData> => {
  let paramName = BlankOr.newF(pfp.name)
  let paramType = BlankOr.newF(pfp.typ)
  list{PParamName(paramName), PParamType(paramType)}
}

let allParamData = (pmf: PT.Package.Fn.t): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, pmf.parameters))

let blankOrData = (pmf: PT.Package.Fn.t): list<blankOrData> => {
  let fnname = BlankOr.newF(FQFnName.PackageFnName.toString(pmf.name))
  list{PFnName(fnname), ...allParamData(pmf)}
}
