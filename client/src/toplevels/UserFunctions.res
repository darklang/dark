open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict
module RT = RuntimeTypes

type model = AppTypes.model

let allNames = (fns: TLID.Dict.t<PT.UserFunction.t>): list<string> =>
  fns
  |> Map.values
  |> List.map(~f=(fn: PT.UserFunction.t) => fn.name)
  |> List.filter(~f=(name: string) => name != "")

let toID = (uf: PT.UserFunction.t): TLID.t => uf.tlid

let upsert = (m: model, userFunction: PT.UserFunction.t): model => {
  ...m,
  userFunctions: Map.add(~key=userFunction.tlid, ~value=userFunction, m.userFunctions),
}

let update = (m: model, ~tlid: TLID.t, ~f: PT.UserFunction.t => PT.UserFunction.t): model => {
  ...m,
  userFunctions: Map.updateIfPresent(~key=tlid, ~f, m.userFunctions),
}

let remove = (m: model, userFunction: PT.UserFunction.t): model => {
  ...m,
  userFunctions: Map.remove(~key=userFunction.tlid, m.userFunctions),
}

let fromList = (ufs: list<PT.UserFunction.t>): TLID.Dict.t<PT.UserFunction.t> =>
  ufs |> List.map(~f=(uf: PT.UserFunction.t) => (uf.tlid, uf)) |> TLID.Dict.fromList

let sameName = (name: string, uf: PT.UserFunction.t): bool => uf.name == name

let paramData = (ufp: PT.UserFunction.Parameter.t): list<blankOrData> => list{
  PParamName(BlankOr.fromStringID(ufp.name, ufp.nameID)),
  PParamType(BlankOr.fromOptionID(ufp.typ, ufp.typeID)),
}

let allParamData = (uf: PT.UserFunction.t): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, uf.parameters))

let blankOrData = (uf: PT.UserFunction.t): list<blankOrData> => list{
  PFnName(BlankOr.fromStringID(uf.name, uf.nameID)),
  PFnReturnType(F(uf.returnTypeID, uf.returnType)),
  ...allParamData(uf),
}

let replaceFnReturn = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let sId = P.toID(search)
  if uf.returnTypeID == sId {
    switch replacement {
    | PFnReturnType(new_) => {
        let (typ, id) = B.toOptionID(new_)
        {
          ...uf,
          returnType: Option.unwrap(typ, ~default=DType.any),
          returnTypeID: id,
        }
      }
    | _ => uf
    }
  } else {
    uf
  }
}

let replaceFnName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let sId = P.toID(search)
  if uf.nameID == sId {
    switch replacement {
    | PFnName(new_) => {
        let (name, id) = B.toStringID(new_)
        {...uf, name: name, nameID: id}
      }
    | _ => uf
    }
  } else {
    uf
  }
}

let replaceParamName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let sId = P.toID(search)
  let paramNames =
    uf
    |> allParamData
    |> List.filterMap(~f=p =>
      switch p {
      | PParamName(n) => Some(n)
      | _ => None
      }
    )

  if List.any(~f=p => B.toID(p) == sId, paramNames) {
    let newParameters = switch replacement {
    | PParamName(new_) =>
      uf.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) => {
        if sId == p.nameID {
          let (name, id) = B.toStringID(new_)
          {...p, name: name, nameID: id}
        } else {
          p
        }
      })

    | _ => uf.parameters
    }

    let newBody = switch (search, replacement) {
    | (PParamName(F(_, oldName)), PParamName(F(_, newName))) =>
      uf.body |> FluidAST.map(~f=FluidExpression.renameVariableUses(~oldName, ~newName))
    | _ => uf.body
    }

    {...uf, parameters: newParameters, body: newBody}
  } else {
    uf
  }
}

let replaceParamType = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let sId = P.toID(search)
  let paramTypes =
    uf
    |> allParamData
    |> List.filterMap(~f=p =>
      switch p {
      | PParamType(t) => Some(t)
      | _ => None
      }
    )

  if List.any(~f=p => B.toID(p) == sId, paramTypes) {
    let newParameters = switch replacement {
    | PParamType(new_) =>
      uf.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) => {
        if sId == p.typeID {
          let (typ, id) = B.toOptionID(new_)
          {...p, typ: typ, typeID: id}
        } else {
          p
        }
      })

    | _ => uf.parameters
    }
    {...uf, parameters: newParameters}
  } else {
    uf
  }
}

let usesOfType = (typename: string, version: int, uf: PT.UserFunction.t): list<blankOrData> =>
  uf
  |> allParamData
  |> List.filterMap(~f=p =>
    switch p {
    | PParamType(F(_, TUserType(n, v))) as pd if n == typename && v == version => Some(pd)
    | _ => None
    }
  )

let replaceMetadataField = (
  old: blankOrData,
  new_: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t =>
  uf
  |> replaceFnName(old, new_)
  |> replaceFnReturn(old, new_)
  |> replaceParamName(old, new_)
  |> replaceParamType(old, new_)

let extend = (uf: PT.UserFunction.t): PT.UserFunction.t => {
  let newParam = {
    PT.UserFunction.Parameter.name: "",
    nameID: gid(),
    typ: None,
    typeID: gid(),
    description: "",
  }

  {
    ...uf,
    parameters: Belt.List.concat(uf.parameters, list{newParam}),
  }
}

let removeParameter = (
  uf: PT.UserFunction.t,
  ufp: PT.UserFunction.Parameter.t,
): PT.UserFunction.t => {
  let params = List.filter(~f=p => p != ufp, uf.parameters)
  {...uf, parameters: params}
}

let idOfLastBlankor = (f: PT.UserFunction.t): id =>
  List.last(f.parameters)
  |> Option.andThen(~f=(p: PT.UserFunction.Parameter.t) => Some(p.typeID))
  |> Option.unwrap(~default=f.nameID)

// Converts AnalysisTypes.InputValueDict.t to executeFunctionAPIParams.args
let inputToArgs = (f: PT.UserFunction.t, input: AnalysisTypes.InputValueDict.t): list<
  RT.Dval.t,
> => {
  let default = RT.Dval.DIncomplete(SourceNone)
  f.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) =>
    Belt.Map.String.get(input, p.name) |> Option.unwrap(~default)
  )
}

let canDelete = (usedInRefs: list<toplevel>, tlid: TLID.t): bool =>
  /* Allow deletion if the only callers are itself or there are no references at all.
    List.all returns true if the list is empty.
 */
  usedInRefs |> List.all(~f=x =>
    switch x {
    | TLFunc(f) if f.tlid == tlid => true
    | _ => false
    }
  )
