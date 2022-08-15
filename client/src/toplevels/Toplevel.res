open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict

module Mod = AppTypes.Modification
type model = AppTypes.model

// -------------------------
// Toplevel manipulation
// -------------------------
let name = (tl: toplevel): string =>
  switch tl {
  | TLHandler(h) =>
    "H: " ++ PT.Handler.Spec.name(h.spec)->B.toOption->Belt.Option.getWithDefault("Undefined")
  | TLDB(db) => "DB: " ++ db.name
  | TLPmFunc(fn) => "Package Manager Func: " ++ PT.FQFnName.PackageFnName.toString(fn.name)
  | TLFunc(f) => "Func: " ++ f.name
  | TLTipe(t) => "Type: " ++ t.name
  }

let sortkey = (tl: toplevel): string =>
  switch tl {
  | TLHandler(h) =>
    PT.Handler.Spec.space(h.spec)->B.toOption->Belt.Option.getWithDefault("Undefined") ++
    PT.Handler.Spec.name(h.spec)->B.toOption->Belt.Option.getWithDefault("Undefined") ++
    PT.Handler.Spec.modifier(h.spec)
    ->Option.andThen(~f=B.toOption)
    ->Belt.Option.getWithDefault("Undefined")
  | TLDB(db) => db.name
  | TLPmFunc(f) => PT.FQFnName.PackageFnName.toString(f.name)
  | TLFunc(f) => f.name
  | TLTipe(t) => t.name
  }

let id = tl =>
  switch tl {
  | TLHandler(h) => h.tlid
  | TLDB(db) => db.tlid
  | TLFunc(f) => f.tlid
  | TLPmFunc(f) => f.tlid
  | TLTipe(t) => t.tlid
  }

let pos = tl =>
  switch tl {
  | TLHandler(h) => h.pos
  | TLDB(db) => db.pos
  | TLPmFunc(f) => recover("no pos in a func", ~debug=f.tlid, ({x: 0, y: 0}: Pos.t))
  | TLFunc(f) => recover("no pos in a func", ~debug=f.tlid, ({x: 0, y: 0}: Pos.t))
  | TLTipe(t) => recover("no pos in a tipe", ~debug=t.tlid, ({x: 0, y: 0}: Pos.t))
  }

let remove = (m: model, tl: toplevel): model => {
  let m = {...m, cursorState: Deselected, currentPage: Architecture}
  switch tl {
  | TLHandler(h) => Handlers.remove(m, h)
  | TLDB(db) => DB.remove(m, db)
  | TLFunc(f) => UserFunctions.remove(m, f)
  | TLTipe(ut) => UserTypes.remove(m, ut)
  | TLPmFunc(_) => // Cannot remove a package manager function
    m
  }
}

let fromList = (tls: list<toplevel>): TLID.Dict.t<toplevel> =>
  tls |> List.map(~f=tl => (id(tl), tl)) |> TD.fromList

let move = (tlid: TLID.t, xOffset: int, yOffset: int, m: model): model => {
  let newPos = (p: Pos.t) => {Pos.x: p.x + xOffset, y: p.y + yOffset}
  {
    ...m,
    handlers: Map.updateIfPresent(m.handlers, ~key=tlid, ~f=(h: PT.Handler.t) => {
      ...h,
      pos: newPos(h.pos),
    }),
    dbs: Map.updateIfPresent(m.dbs, ~key=tlid, ~f=(db: PT.DB.t) => {...db, pos: newPos(db.pos)}),
  }
}

let ufToTL = (uf: PT.UserFunction.t): toplevel => TLFunc(uf)

let pmfToTL = (pmf: PT.Package.Fn.t): toplevel => TLPmFunc(pmf)

let utToTL = (ut: PT.UserType.t): toplevel => TLTipe(ut)

let asUserFunction = (tl: toplevel): option<PT.UserFunction.t> =>
  switch tl {
  | TLFunc(f) => Some(f)
  | _ => None
  }

let asUserTipe = (tl: toplevel): option<PT.UserType.t> =>
  switch tl {
  | TLTipe(t) => Some(t)
  | _ => None
  }

let isUserFunction = (tl: toplevel): bool =>
  switch tl {
  | TLFunc(_) => true
  | _ => false
  }

let isUserTipe = (tl: toplevel): bool =>
  switch tl {
  | TLTipe(_) => true
  | _ => false
  }

let asHandler = (tl: toplevel): option<PT.Handler.t> =>
  switch tl {
  | TLHandler(h) => Some(h)
  | _ => None
  }

let asDB = (tl: toplevel): option<PT.DB.t> =>
  switch tl {
  | TLDB(h) => Some(h)
  | _ => None
  }

let isDB = (tl: toplevel): bool =>
  switch tl {
  | TLDB(_) => true
  | _ => false
  }

let isHandler = (tl: toplevel): bool =>
  switch tl {
  | TLHandler(_) => true
  | _ => false
  }

let handlers = (tls: list<toplevel>): list<PT.Handler.t> => List.filterMap(~f=asHandler, tls)

let dbs = (tls: TD.t<toplevel>): list<PT.DB.t> => tls |> Map.filterMapValues(~f=asDB)

let spaceOfHandler = (h: PT.Handler.t): handlerSpace => SpecHeaders.spaceOf(h.spec)

let spaceOf = (tl: toplevel): option<handlerSpace> =>
  tl |> asHandler |> Option.map(~f=spaceOfHandler)

let isHTTPHandler = (tl: toplevel): bool =>
  switch asHandler(tl) {
  | Some({spec: PT.Handler.Spec.HTTP(_), _}) => true
  | _ => false
  }

let isReplHandler = (tl: toplevel): bool =>
  switch asHandler(tl) {
  | Some({spec: PT.Handler.Spec.REPL(_), _}) => true
  | _ => false
  }

let isCronHandler = (tl: toplevel): bool =>
  switch asHandler(tl) {
  | Some({spec: PT.Handler.Spec.Cron(_), _}) => true
  | _ => false
  }
  
let isWorkerHandler = (tl: toplevel): bool =>
  switch asHandler(tl) {
  | Some({spec: PT.Handler.Spec.Worker(_), _}) => true
  | _ => false
  }

let isDeprecatedCustomHandler = (tl: toplevel): bool =>
  switch asHandler(tl) {
  | Some(h) =>
    switch h.spec {
    | PT.Handler.Spec.OldWorker(_) => true
    | PT.Handler.Spec.UnknownHandler(_) => true
    | _ => false
    }
  | None => false
  }

let toOp = (tl: toplevel): list<PT.Op.t> =>
  switch tl {
  | TLHandler(h) => list{SetHandler(h.tlid, h.pos, h)}
  | TLFunc(fn) => list{SetFunction(fn)}
  | TLTipe(t) => list{SetType(t)}
  | TLPmFunc(_) => recover("Package Manager functions are not editable", ~debug=id(tl), list{})
  | TLDB(_) => recover("This isn't how datastore ops work", ~debug=id(tl), list{})
  }

// -------------------------
// Generic
// -------------------------
let blankOrData = (tl: toplevel): list<blankOrData> =>
  switch tl {
  | TLHandler(h) => SpecHeaders.blankOrData(h.spec)
  | TLDB(db) => DB.blankOrData(db)
  | TLPmFunc(f) => PackageManager.blankOrData(f)
  | TLFunc(f) => UserFunctions.blankOrData(f)
  | TLTipe(t) => UserTypes.blankOrData(t)
  }

let isValidBlankOrID = (tl: toplevel, id: id): bool =>
  List.member(~value=id, tl |> blankOrData |> List.map(~f=P.toID))

// -------------------------
// ASTs
// -------------------------

let getAST = (tl: toplevel): option<FluidAST.t> =>
  switch tl {
  | TLHandler(h) => Some(h.ast)
  | TLFunc(f) => Some(f.body)
  | TLPmFunc(fn) => Some(FluidAST.ofExpr(fn.body))
  | _ => None
  }

let setAST = (tl: toplevel, newAST: FluidAST.t): toplevel =>
  switch tl {
  | TLHandler(h) => TLHandler({...h, ast: newAST})
  | TLFunc(uf) => TLFunc({...uf, body: newAST})
  | TLDB(_) | TLTipe(_) | TLPmFunc(_) => tl
  }

let withAST = (m: model, tlid: TLID.t, ast: FluidAST.t): model => {
  ...m,
  handlers: Map.updateIfPresent(m.handlers, ~key=tlid, ~f=(h: PT.Handler.t) => {...h, ast: ast}),
  userFunctions: Map.updateIfPresent(m.userFunctions, ~key=tlid, ~f=(uf: PT.UserFunction.t) => {
    ...uf,
    body: ast,
  }),
}

/* Create the modification to set the AST in this toplevel. `ops` is optional
 * other ops to include in this modification. Does not change the model. */
let setASTMod = (~ops=list{}, tl: toplevel, ast: FluidAST.t): AppTypes.modification =>
  switch tl {
  | TLHandler(h) =>
    if h.ast == ast {
      NoChange
    } else {
      AddOps(
        Belt.List.concat(ops, list{PT.Op.SetHandler(id(tl), h.pos, {...h, ast: ast})}),
        FocusNoChange,
      )
    }
  | TLFunc(f) =>
    if f.body == ast {
      NoChange
    } else {
      AddOps(Belt.List.concat(ops, list{SetFunction({...f, body: ast})}), FocusNoChange)
    }
  | TLPmFunc(_) => recover("cannot change ast in package manager", ~debug=tl, Mod.NoChange)
  | TLTipe(_) => recover("no ast in Tipes", ~debug=tl, Mod.NoChange)
  | TLDB(_) => recover("no ast in DBs", ~debug=tl, Mod.NoChange)
  }

@ocaml.doc(" modifyASTMod is a combination of getAST and setASTMod. It fetches the AST
  * for [tl] and passes it to [f], which should return a modified version of the
  * AST. An AddOps modification is returned, which updates the AST accordingly. ")
let modifyASTMod = (tl: toplevel, ~f: FluidAST.t => FluidAST.t): AppTypes.modification =>
  getAST(tl)->Option.map(~f=ast => ast->f |> setASTMod(tl))->Option.unwrap(~default=Mod.NoChange)

let replace = (p: blankOrData, replacement: blankOrData, tl: toplevel): toplevel => {
  let id = P.toID(p)
  switch replacement {
  | PEventName(bo) | PEventModifier(bo) | PEventSpace(bo) =>
    switch asHandler(tl) {
    | Some(h) =>
      let newSpec = SpecHeaders.replace(id, bo, h.spec)
      TLHandler({...h, spec: newSpec})
    | _ => recover("Changing handler metadata on non-handler", ~debug=replacement, tl)
    }
  | PDBName(_) | PDBColType(_) | PDBColName(_) => tl
  | PFnName(_) | PFnReturnTipe(_) | PParamName(_) | PParamTipe(_) =>
    switch asUserFunction(tl) {
    | Some(fn) =>
      let newFn = UserFunctions.replaceMetadataField(p, replacement, fn)
      TLFunc(newFn)
    | _ => recover("Changing fn metadata on non-fn", ~debug=replacement, tl)
    }
  | PTypeName(_) | PTypeFieldName(_) | PTypeFieldTipe(_) =>
    switch asUserTipe(tl) {
    | Some(tipe) =>
      let newTL = UserTypes.replace(p, replacement, tipe)
      TLTipe(newTL)
    | _ => recover("Changing tipe metadata on non-tipe", ~debug=replacement, tl)
    }
  }
}

let combine = (
  handlers: TD.t<PT.Handler.t>,
  dbs: TD.t<PT.DB.t>,
  userFunctions: TD.t<PT.UserFunction.t>,
  packageFn: TD.t<PT.Package.Fn.t>,
  userTypes: TD.t<PT.UserType.t>,
): TD.t<toplevel> =>
  Map.map(~f=h => TLHandler(h), handlers)
  |> Map.mergeLeft(Map.map(~f=db => TLDB(db), dbs))
  |> Map.mergeLeft(Map.map(~f=ufToTL, userFunctions))
  |> Map.mergeLeft(Map.map(~f=pmfToTL, packageFn))
  |> Map.mergeLeft(Map.map(~f=utToTL, userTypes))

let all = (m: model): TD.t<toplevel> =>
  combine(m.handlers, m.dbs, m.userFunctions, m.functions.packageFunctions, m.userTypes)

let structural = (m: model): TD.t<toplevel> =>
  Map.map(~f=h => TLHandler(h), m.handlers) |> Map.mergeLeft(Map.map(~f=db => TLDB(db), m.dbs))

let get = (m: model, tlid: TLID.t): option<toplevel> => Map.get(~key=tlid, all(m))

let find = (tl: toplevel, id_: id): option<blankOrData> =>
  blankOrData(tl)
  |> List.filter(~f=d => id_ == P.toID(d))
  |> assertFn("cant find pd for id", ~debug=(id(tl), id), ~f=r => List.length(r) <= 1)
  |> // guard against dups
  List.head

let getPD = (m: model, tlid: TLID.t, id: id): option<blankOrData> =>
  get(m, tlid) |> Option.andThen(~f=tl => find(tl, id))

let getTLAndPD = (m: model, tlid: TLID.t, id: id): option<(toplevel, option<blankOrData>)> =>
  get(m, tlid) |> Option.map(~f=tl => (tl, find(tl, id)))

let allDBNames = (dbs: TD.t<PT.DB.t>): list<string> =>
  dbs |> Map.filterMapValues(~f=(db: PT.DB.t) =>
    if db.name == "" {
      None
    } else {
      Some(db.name)
    }
  )

let allGloballyScopedVarnames = (dbs: TD.t<PT.DB.t>): list<string> => allDBNames(dbs)

let asPage = (tl: toplevel, center: bool): AppTypes.Page.t =>
  switch tl {
  | TLHandler(_) => FocusedHandler(id(tl), None, center)
  | TLDB(_) => FocusedDB(id(tl), center)
  | TLPmFunc(_) | TLFunc(_) => FocusedFn(id(tl), None)
  | TLTipe(_) => FocusedType(id(tl))
  }

let selected = (m: model): option<toplevel> =>
  m.cursorState |> CursorState.tlidOf |> Option.andThen(~f=get(m))

let selectedAST = (m: model): option<FluidAST.t> => selected(m) |> Option.andThen(~f=getAST)

let setSelectedAST = (m: model, ast: FluidAST.t): AppTypes.modification =>
  switch selected(m) {
  | None => NoChange
  | Some(tl) => setASTMod(tl, ast)
  }

// -------------------------
// Blanks
// -------------------------

type predecessor = option<id>

type successor = option<id>

let allBlanks = (tl: toplevel): list<id> =>
  Belt.List.concat(
    tl |> blankOrData |> List.filter(~f=P.isBlank) |> List.map(~f=P.toID),
    tl
    |> getAST
    |> Option.map(~f=FluidAST.blanks)
    |> Option.unwrap(~default=list{})
    |> List.map(~f=FluidExpression.toID),
  )

let allIDs = (tl: toplevel): list<id> =>
  Belt.List.concat(
    tl |> blankOrData |> List.map(~f=P.toID),
    tl |> getAST |> Option.map(~f=FluidAST.ids) |> Option.unwrap(~default=list{}),
  )

let firstBlank = (tl: toplevel): successor => tl |> allBlanks |> List.head

let lastBlank = (tl: toplevel): successor => tl |> allBlanks |> List.last

let getNextBlank = (tl: toplevel, id: id): successor => {
  let all = allIDs(tl)
  let index = List.elemIndex(~value=id, all) |> Option.unwrap(~default=-1)
  let blanks = allBlanks(tl) |> List.map(~f=ID.toString) |> Set.String.fromList
  all
  |> List.drop(~count=index + 1)
  |> List.find(~f=id => Set.member(blanks, ~value=ID.toString(id)))
  |> Option.orElse(firstBlank(tl))
}

let getPrevBlank = (tl: toplevel, id: id): predecessor => {
  let all = allIDs(tl)
  let index = List.elemIndex(~value=id, all) |> Option.unwrap(~default=List.length(all))

  let blanks = allBlanks(tl) |> List.map(~f=ID.toString) |> Set.String.fromList
  all
  |> List.take(~count=index)
  |> List.reverse
  |> List.find(~f=id => Set.member(blanks, ~value=ID.toString(id)))
  |> Option.orElse(lastBlank(tl))
}
