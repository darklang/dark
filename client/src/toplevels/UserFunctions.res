open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict
module RT = RuntimeTypes

type model = AppTypes.model

let allNames = (fns: TLID.Dict.t<PT.UserFunction.t>): list<string> =>
  fns |> Map.filterMapValues(~f=(fn: PT.UserFunction.t) => B.toOption(fn.metadata.name))

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

let ufpToP = (ufp: PT.UserFunction.Parameter.t): option<parameter> =>
  switch (ufp.name, ufp.typ) {
  | (F(_, name), F(_, tipe)) =>
    {
      paramName: name,
      paramTipe: tipe,
      paramBlock_args: ufp.args,
      paramOptional: ufp.optional,
      paramDescription: ufp.description,
    } |> (x => Some(x))
  | _ => None
  }

let ufmToF = (ufm: PT.UserFunction.Metadata.t): option<function_> => {
  let ps = List.filterMap(~f=ufpToP, ufm.parameters)
  let sameLength = List.length(ps) == List.length(ufm.parameters)
  switch (ufm.name, ufm.returnType, sameLength) {
  | (F(_, name), F(_, tipe), true) =>
    {
      fnName: User(name),
      fnParameters: ps,
      fnDescription: ufm.description,
      fnReturnTipe: tipe,
      fnInfix: ufm.infix,
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnIsSupportedInQuery: false,
      fnOrigin: UserFunction,
    } |> (x => Some(x))
  | _ => None
  }
}

let sameName = (name: string, uf: PT.UserFunction.t): bool =>
  switch uf.metadata.name {
  | F(_, n) => n == name
  | _ => false
  }

let paramData = (ufp: PT.UserFunction.Parameter.t): list<blankOrData> => list{
  PParamName(ufp.name),
  PParamTipe(ufp.typ),
}

let allParamData = (uf: PT.UserFunction.t): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, uf.metadata.parameters))

let blankOrData = (uf: PT.UserFunction.t): list<blankOrData> => list{
  PFnName(uf.metadata.name),
  PFnReturnTipe(uf.metadata.returnType),
  ...allParamData(uf),
}

let replaceFnReturn = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let metadata = uf.metadata
  let sId = P.toID(search)
  if B.toID(metadata.returnType) == sId {
    let newMetadata = switch replacement {
    | PFnReturnTipe(new_) => {
        ...metadata,
        returnType: B.replace(sId, new_, metadata.returnType),
      }
    | _ => metadata
    }

    {...uf, metadata: newMetadata}
  } else {
    uf
  }
}

let replaceFnName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let metadata = uf.metadata
  let sId = P.toID(search)
  if B.toID(metadata.name) == sId {
    let newMetadata = switch replacement {
    | PFnName(new_) => {...metadata, name: B.replace(sId, new_, metadata.name)}
    | _ => metadata
    }

    {...uf, metadata: newMetadata}
  } else {
    uf
  }
}

let replaceParamName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let metadata = uf.metadata
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
    let newMetadata = switch replacement {
    | PParamName(new_) =>
      let newP = metadata.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) => {
        ...p,
        name: B.replace(sId, new_, p.name),
      })

      {...metadata, parameters: newP}
    | _ => metadata
    }

    let newBody = switch (search, replacement) {
    | (PParamName(F(_, oldName)), PParamName(F(_, newName))) =>
      uf.ast |> FluidAST.map(~f=FluidExpression.renameVariableUses(~oldName, ~newName))
    | _ => uf.ast
    }

    {...uf, metadata: newMetadata, ast: newBody}
  } else {
    uf
  }
}

let replaceParamTipe = (
  search: blankOrData,
  replacement: blankOrData,
  uf: PT.UserFunction.t,
): PT.UserFunction.t => {
  let metadata = uf.metadata
  let sId = P.toID(search)
  let paramTipes =
    uf
    |> allParamData
    |> List.filterMap(~f=p =>
      switch p {
      | PParamTipe(t) => Some(t)
      | _ => None
      }
    )

  if List.any(~f=p => B.toID(p) == sId, paramTipes) {
    let newMetadata = switch replacement {
    | PParamTipe(new_) =>
      let newP = metadata.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) => {
        ...p,
        typ: B.replace(sId, new_, p.typ),
      })

      {...metadata, parameters: newP}
    | _ => metadata
    }

    {...uf, metadata: newMetadata}
  } else {
    uf
  }
}

let usesOfTipe = (tipename: string, version: int, uf: PT.UserFunction.t): list<blankOrData> =>
  uf
  |> allParamData
  |> List.filterMap(~f=p =>
    switch p {
    | PParamTipe(F(_, TUserType(n, v))) as pd if n == tipename && v == version => Some(pd)
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
  |> replaceParamTipe(old, new_)

let extend = (uf: PT.UserFunction.t): PT.UserFunction.t => {
  let newParam = {
    PT.UserFunction.Parameter.name: B.new_(),
    typ: B.new_(),
    args: list{},
    optional: false,
    description: "",
  }

  let metadata = uf.metadata
  let newMetadata = {
    ...metadata,
    parameters: Belt.List.concat(uf.metadata.parameters, list{newParam}),
  }

  {...uf, metadata: newMetadata}
}

let removeParameter = (
  uf: PT.UserFunction.t,
  ufp: PT.UserFunction.Parameter.t,
): PT.UserFunction.t => {
  let metadata = uf.metadata
  let params = List.filter(~f=p => p != ufp, metadata.parameters)
  let newM = {...metadata, parameters: params}
  {...uf, metadata: newM}
}

let idOfLastBlankor = (f: PT.UserFunction.t): id =>
  List.last(f.metadata.parameters)
  |> Option.andThen(~f=(p: PT.UserFunction.Parameter.t) => Some(B.toID(p.typ)))
  |> Option.unwrap(~default=B.toID(f.metadata.name))

// Converts AnalysisTypes.InputValueDict.t to executeFunctionAPIParams.args
let inputToArgs = (f: PT.UserFunction.t, input: AnalysisTypes.InputValueDict.t): list<
  RT.Dval.t,
> => {
  let default = RT.Dval.DIncomplete(SourceNone)
  f.metadata.parameters |> List.map(~f=(p: PT.UserFunction.Parameter.t) =>
    switch p.name {
    | F(_, name) => Belt.Map.String.get(input, name) |> Option.unwrap(~default)
    | _ => default
    }
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
