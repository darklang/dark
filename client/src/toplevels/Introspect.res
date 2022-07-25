open Prelude
module B = BlankOr
module TL = Toplevel
module TD = TLID.Dict

let keyForHandlerSpec = (space: string, name: string): string => space ++ (":" ++ name)

let keyForTipe = (name: string, version: int): string => name ++ (":" ++ Int.toString(version))

let dbsByName = (dbs: TD.t<PT.DB.t>): Map.String.t<TLID.t> =>
  dbs
  |> Map.filterMapValues(~f=(db: PT.DB.t) =>
    db.name |> B.toOption |> Option.map(~f=name => (name, db.tlid))
  )
  |> Map.String.fromList

let handlersByName = (hs: TD.t<PT.Handler.t>): Map.String.t<TLID.t> => {
  module S = PT.Handler.Spec
  hs
  |> Map.mapValues(~f=(h: PT.Handler.t) => {
    let space = S.space(h.spec)->Belt.Option.getWithDefault("")
    let name = S.name(h.spec)
    let key = keyForHandlerSpec(space, name)
    (key, h.tlid)
  })
  |> Map.String.fromList
}

let functionsByName = (fns: TD.t<PT.UserFunction.t>): Map.String.t<TLID.t> =>
  fns
  |> Map.filterMapValues(~f=(uf: PT.UserFunction.t) =>
    uf.metadata.name |> B.toOption |> Option.map(~f=name => (name, uf.tlid))
  )
  |> Map.String.fromList

let packageFunctionsByName = (fns: TD.t<PT.Package.Fn.t>): Map.String.t<TLID.t> =>
  fns
  |> Map.mapValues(~f=(fn: PT.Package.Fn.t) => (
    PT.FQFnName.PackageFnName.toString(fn.name),
    fn.tlid,
  ))
  |> Map.String.fromList

let tipesByName = (uts: TD.t<PT.UserType.t>): Map.String.t<TLID.t> =>
  uts
  |> Map.mapValues(~f=(ut: PT.UserType.t) => {
    let name =
      ut.name
      |> B.toOption
      |> // Shouldn't happen: all tipes have a default name
      recoverOpt("tipes should have default names", ~default="_")

    let version = ut.version
    let key = keyForTipe(name, version)
    (key, ut.tlid)
  })
  |> Map.String.fromList

let tlidsToUpdateUsage = (ops: list<PT.Op.t>): list<TLID.t> =>
  ops
  |> List.filterMap(~f=op =>
    switch op {
    | PT.Op.SetHandler(tlid, _, _) | SetExpr(tlid, _, _) => Some(tlid)
    | SetFunction(f) => Some(f.tlid)
    | CreateDB(_)
    | DeleteTL(_)
    | MoveTL(_)
    | TLSavepoint(_)
    | UndoTL(_)
    | RedoTL(_)
    | DeleteFunction(_)
    | ChangeDBColName(_)
    | ChangeDBColType(_)
    | CreateDBWithBlankOr(_)
    | SetType(_)
    | DeleteType(_)
    | AddDBCol(_)
    | SetDBColType(_)
    | DeleteDBCol(_)
    | RenameDBname(_)
    | SetDBColName(_) =>
      None
    }
  )
  |> List.uniqueBy(~f=TLID.toString)

let rec updateAssocList = (~key: 'k, ~f: option<'v> => option<'v>, assoc: list<('k, 'v)>): list<(
  'k,
  'v,
)> =>
  switch assoc {
  | list{(k, v), ...xs} =>
    if key == k {
      switch f(Some(v)) {
      | Some(nw) => list{(key, nw), ...xs}
      | None => xs
      }
    } else {
      list{(k, v), ...updateAssocList(~key, ~f, xs)}
    }
  | list{} =>
    switch f(None) {
    | Some(nw) => list{(key, nw)}
    | None => list{}
    }
  }

let allRefersTo = (tlid: TLID.t, m: AppTypes.model): list<(toplevel, list<id>)> =>
  m.tlRefersTo
  |> Map.get(~key=tlid)
  |> Option.unwrap(~default=list{})
  |> List.fold(~initial=list{}, ~f=(assoc, (tlid, id)): list<(TLID.t, list<id>)> =>
    updateAssocList(~key=tlid, assoc, ~f=x =>
      switch x {
      | None => Some(list{id})
      | Some(lst) => Some(Belt.List.concat(lst, list{id}))
      }
    )
  )
  |> List.filterMap(~f=((tlid, ids)) => TL.get(m, tlid) |> Option.map(~f=tl => (tl, ids)))

let allUsedIn = (tlid: TLID.t, m: AppTypes.model): list<toplevel> =>
  m.tlUsedIn
  |> Map.get(~key=tlid)
  |> Option.unwrap(~default=TLID.Set.empty)
  |> Set.toList
  |> List.filterMap(~f=tlid => TL.get(m, tlid))

let findUsagesInAST = (
  tlid: TLID.t,
  ~datastores: Map.String.t<TLID.t>,
  ~handlers: Map.String.t<TLID.t>,
  ~functions: Map.String.t<TLID.t>,
  ~packageFunctions: Map.String.t<TLID.t>,
  ast: FluidAST.t,
): list<usage> =>
  FluidAST.toExpr(ast)
  |> FluidExpression.filterMap(~f=e =>
    switch e {
    | EVariable(id, name) => Map.get(~key=name, datastores) |> Option.map(~f=tlid => (tlid, id))
    | EFnCall(
        id,
        Stdlib({module_: "", function: "emit", version: 0}),
        list{_, EString(_, space_), EString(_, name_)},
        _,
      ) =>
      let name = Util.removeQuotes(name_)
      let space = Util.removeQuotes(space_)
      let key = keyForHandlerSpec(space, name)
      Map.get(~key, handlers) |> Option.map(~f=fnTLID => (fnTLID, id))
    | EFnCall(
        id,
        Stdlib({module_: "", function: "emit", version: 1}),
        list{_, EString(_, name_)},
        _,
      ) =>
      let name = Util.removeQuotes(name_)
      let space = "WORKER"
      let key = keyForHandlerSpec(space, name)
      Map.get(~key, handlers) |> Option.map(~f=fnTLID => (fnTLID, id))
    | EFnCall(id, name, _, _) =>
      Option.orElse(
        Map.get(~key=PT.FQFnName.toString(name), functions) |> Option.map(~f=fnTLID => (
          fnTLID,
          id,
        )),
        Map.get(~key=PT.FQFnName.toString(name), packageFunctions) |> Option.map(~f=fnTLID => (
          fnTLID,
          id,
        )),
      )
    | _ => None
    }
  )
  |> List.map(~f=((usedIn, id)) => {refersTo: tlid, usedIn: usedIn, id: id})

let findUsagesInFunctionParams = (tipes: Map.String.t<TLID.t>, fn: PT.UserFunction.t): list<
  usage,
> => {
  // Versions are slightly aspirational, and we don't have them in most of
  // the places we use tipes, including here
  let version = 0
  fn.metadata.parameters
  |> List.filterMap(~f=(p: PT.UserFunction.Parameter.t) =>
    p.typ
    |> B.toOption
    |> Option.map(~f=Runtime.tipe2str)
    |> Option.map(~f=t => keyForTipe(t, version))
    |> Option.andThen(~f=key => Map.get(~key, tipes))
    |> Option.thenAlso(~f=_ => Some(B.toID(p.typ)))
  )
  |> List.map(~f=((usedIn, id)) => {refersTo: fn.tlid, usedIn: usedIn, id: id})
}

let getUsageFor = (
  tl: toplevel,
  ~datastores: Map.String.t<TLID.t>,
  ~handlers: Map.String.t<TLID.t>,
  ~functions: Map.String.t<TLID.t>,
  ~packageFunctions: Map.String.t<TLID.t>,
  ~tipes: Map.String.t<TLID.t>,
): list<usage> => {
  let astUsages =
    TL.getAST(tl)
    |> Option.map(
      ~f=findUsagesInAST(TL.id(tl), ~datastores, ~handlers, ~functions, ~packageFunctions),
    )
    |> Option.unwrap(~default=list{})

  let fnUsages =
    TL.asUserFunction(tl)
    |> Option.map(~f=findUsagesInFunctionParams(tipes))
    |> Option.unwrap(~default=list{})

  // TODO: tipes in other tipes
  Belt.List.concat(astUsages, fnUsages)
}

let refreshUsages = (m: AppTypes.model, tlids: list<TLID.t>): AppTypes.model => {
  let datastores = dbsByName(m.dbs)
  let handlers = handlersByName(m.handlers)
  let functions = functionsByName(m.userFunctions)
  let packageFunctions = packageFunctionsByName(m.functions.packageFunctions)
  let tipes = tipesByName(m.userTipes)
  /* We need to overwrite the already-stored results for the passed-in TLIDs.
   * So we clear tlRefers for these tlids, and remove them from the inner set
   * of tlUsedIn. */
  let tlRefersToDict = Map.removeMany(~keys=tlids, m.tlRefersTo)
  let tlUsedInDict = Map.map(m.tlUsedIn, ~f=tlidsReferedTo =>
    Set.removeMany(tlidsReferedTo, ~values=tlids)
  )

  let (newTlUsedIn, newTlRefersTo) =
    tlids
    |> List.filterMap(~f=tlid => {
      let tl = TL.get(m, tlid)
      Option.map(tl, ~f=tl =>
        getUsageFor(tl, ~datastores, ~handlers, ~functions, ~packageFunctions, ~tipes)
      )
    })
    |> List.flatten
    |> List.fold(~initial=(tlUsedInDict, tlRefersToDict), ~f=((usedIn, refersTo), usage) => {
      let newRefersTo = Map.add(
        ~key=usage.refersTo,
        ~value=Belt.List.concat(
          Map.get(~key=usage.refersTo, refersTo) |> Option.unwrap(~default=list{}),
          list{(usage.usedIn, usage.id)},
        ),
        refersTo,
      )

      let newUsedIn =
        Map.get(~key=usage.usedIn, usedIn)
        |> Option.unwrap(~default=TLID.Set.empty)
        |> Set.add(~value=usage.refersTo)
        |> (value => Map.add(~key=usage.usedIn, ~value, usedIn))

      (newUsedIn, newRefersTo)
    })

  {...m, tlUsedIn: newTlUsedIn, tlRefersTo: newTlRefersTo}
}

let setHoveringReferences = (tlid: TLID.t, ids: list<id>): AppTypes.modification => {
  let new_props = x =>
    switch x {
    | None => Some({...AppTypes.HandlerProperty.default, hoveringReferences: ids})
    | Some(v) => Some({...v, hoveringReferences: ids})
    }

  ReplaceAllModificationsWithThisOne(
    m => ({...m, handlerProps: Map.update(~key=tlid, ~f=new_props, m.handlerProps)}, Tea.Cmd.none),
  )
}
