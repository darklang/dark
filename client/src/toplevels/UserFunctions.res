open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let allNames = (fns: TLIDDict.t<userFunction>): list<string> =>
  fns |> Map.filterMapValues(~f=fn => B.toOption(fn.ufMetadata.ufmName))

let toID = (uf: userFunction): TLID.t => uf.ufTLID

let upsert = (m: model, userFunction: userFunction): model => {
  ...m,
  userFunctions: Map.add(~key=userFunction.ufTLID, ~value=userFunction, m.userFunctions),
}

let update = (m: model, ~tlid: TLID.t, ~f: userFunction => userFunction): model => {
  ...m,
  userFunctions: Map.updateIfPresent(~key=tlid, ~f, m.userFunctions),
}

let remove = (m: model, userFunction: userFunction): model => {
  ...m,
  userFunctions: Map.remove(~key=userFunction.ufTLID, m.userFunctions),
}

let fromList = (ufs: list<userFunction>): TLIDDict.t<userFunction> =>
  ufs |> List.map(~f=uf => (uf.ufTLID, uf)) |> TLIDDict.fromList

let ufpToP = (ufp: userFunctionParameter): option<parameter> =>
  switch (ufp.ufpName, ufp.ufpTipe) {
  | (F(_, name), F(_, tipe)) =>
    {
      paramName: name,
      paramTipe: tipe,
      paramBlock_args: ufp.ufpBlock_args,
      paramOptional: ufp.ufpOptional,
      paramDescription: ufp.ufpDescription,
    } |> (x => Some(x))
  | _ => None
  }

let ufmToF = (ufm: userFunctionMetadata): option<function_> => {
  let ps = List.filterMap(~f=ufpToP, ufm.ufmParameters)
  let sameLength = List.length(ps) == List.length(ufm.ufmParameters)
  switch (ufm.ufmName, ufm.ufmReturnTipe, sameLength) {
  | (F(_, name), F(_, tipe), true) =>
    {
      fnName: name,
      fnParameters: ps,
      fnDescription: ufm.ufmDescription,
      fnReturnTipe: tipe,
      fnInfix: ufm.ufmInfix,
      fnPreviewSafety: Unsafe,
      fnDeprecated: false,
      fnIsSupportedInQuery: false,
      fnOrigin: UserFunction,
    } |> (x => Some(x))
  | _ => None
  }
}

let sameName = (name: string, uf: userFunction): bool =>
  switch uf.ufMetadata.ufmName {
  | F(_, n) => n == name
  | _ => false
  }

let paramData = (ufp: userFunctionParameter): list<blankOrData> => list{
  PParamName(ufp.ufpName),
  PParamTipe(ufp.ufpTipe),
}

let allParamData = (uf: userFunction): list<blankOrData> =>
  List.flatten(List.map(~f=paramData, uf.ufMetadata.ufmParameters))

let blankOrData = (uf: userFunction): list<blankOrData> => list{
  PFnName(uf.ufMetadata.ufmName),
  PFnReturnTipe(uf.ufMetadata.ufmReturnTipe),
  ...allParamData(uf),
}

let replaceFnReturn = (
  search: blankOrData,
  replacement: blankOrData,
  uf: userFunction,
): userFunction => {
  let metadata = uf.ufMetadata
  let sId = P.toID(search)
  if B.toID(metadata.ufmReturnTipe) == sId {
    let newMetadata = switch replacement {
    | PFnReturnTipe(new_) => {
        ...metadata,
        ufmReturnTipe: B.replace(sId, new_, metadata.ufmReturnTipe),
      }
    | _ => metadata
    }

    {...uf, ufMetadata: newMetadata}
  } else {
    uf
  }
}

let replaceFnName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: userFunction,
): userFunction => {
  let metadata = uf.ufMetadata
  let sId = P.toID(search)
  if B.toID(metadata.ufmName) == sId {
    let newMetadata = switch replacement {
    | PFnName(new_) => {...metadata, ufmName: B.replace(sId, new_, metadata.ufmName)}
    | _ => metadata
    }

    {...uf, ufMetadata: newMetadata}
  } else {
    uf
  }
}

let replaceParamName = (
  search: blankOrData,
  replacement: blankOrData,
  uf: userFunction,
): userFunction => {
  let metadata = uf.ufMetadata
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
      let newP =
        metadata.ufmParameters |> List.map(~f=p => {...p, ufpName: B.replace(sId, new_, p.ufpName)})

      {...metadata, ufmParameters: newP}
    | _ => metadata
    }

    let newBody = switch (search, replacement) {
    | (PParamName(F(_, oldName)), PParamName(F(_, newName))) =>
      uf.ufAST |> FluidAST.map(~f=FluidExpression.renameVariableUses(~oldName, ~newName))
    | _ => uf.ufAST
    }

    {...uf, ufMetadata: newMetadata, ufAST: newBody}
  } else {
    uf
  }
}

let replaceParamTipe = (
  search: blankOrData,
  replacement: blankOrData,
  uf: userFunction,
): userFunction => {
  let metadata = uf.ufMetadata
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
      let newP =
        metadata.ufmParameters |> List.map(~f=p => {...p, ufpTipe: B.replace(sId, new_, p.ufpTipe)})

      {...metadata, ufmParameters: newP}
    | _ => metadata
    }

    {...uf, ufMetadata: newMetadata}
  } else {
    uf
  }
}

let usesOfTipe = (tipename: string, version: int, uf: userFunction): list<blankOrData> =>
  uf
  |> allParamData
  |> List.filterMap(~f=p =>
    switch p {
    | PParamTipe(F(_, TUserType(n, v))) as pd if n == tipename && v == version => Some(pd)
    | _ => None
    }
  )

let replaceMetadataField = (old: blankOrData, new_: blankOrData, uf: userFunction): userFunction =>
  uf
  |> replaceFnName(old, new_)
  |> replaceFnReturn(old, new_)
  |> replaceParamName(old, new_)
  |> replaceParamTipe(old, new_)

let extend = (uf: userFunction): userFunction => {
  let newParam = {
    ufpName: B.new_(),
    ufpTipe: B.new_(),
    ufpBlock_args: list{},
    ufpOptional: false,
    ufpDescription: "",
  }

  let metadata = uf.ufMetadata
  let newMetadata = {
    ...metadata,
    ufmParameters: Belt.List.concat(uf.ufMetadata.ufmParameters, list{newParam}),
  }

  {...uf, ufMetadata: newMetadata}
}

let removeParameter = (uf: userFunction, ufp: userFunctionParameter): userFunction => {
  let metadata = uf.ufMetadata
  let params = List.filter(~f=p => p != ufp, metadata.ufmParameters)
  let newM = {...metadata, ufmParameters: params}
  {...uf, ufMetadata: newM}
}

let idOfLastBlankor = (f: userFunction): id =>
  List.last(f.ufMetadata.ufmParameters)
  |> Option.andThen(~f=p => Some(B.toID(p.ufpTipe)))
  |> Option.unwrap(~default=B.toID(f.ufMetadata.ufmName))

// Converts inputValueDict to executeFunctionAPIParams.efpArgs
let inputToArgs = (f: userFunction, input: inputValueDict): list<dval> => {
  let default = DIncomplete(SourceNone)
  f.ufMetadata.ufmParameters |> List.map(~f=p =>
    switch p.ufpName {
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
    | TLFunc(f) if f.ufTLID == tlid => true
    | _ => false
    }
  )
