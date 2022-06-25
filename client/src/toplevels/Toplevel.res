open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLIDDict

// -------------------------
// Toplevel manipulation
// -------------------------
let name = (tl: toplevel): string =>
  switch tl {
  | TLHandler(h) => "H: " ++ (h.spec.name |> B.toOption |> Option.unwrap(~default=""))
  | TLDB(db) => "DB: " ++ (db.dbName |> B.toOption |> Option.unwrap(~default=""))
  | TLPmFunc(f) => "Package Manager Func: " ++ f.fnname
  | TLFunc(f) => "Func: " ++ (f.ufMetadata.ufmName |> B.toOption |> Option.unwrap(~default=""))
  | TLTipe(t) => "Type: " ++ (t.utName |> B.toOption |> Option.unwrap(~default=""))
  }

let sortkey = (tl: toplevel): string =>
  switch tl {
  | TLHandler(h) =>
    (h.spec.space |> B.toOption |> Option.unwrap(~default="Undefined")) ++
      ((h.spec.name |> B.toOption |> Option.unwrap(~default="Undefined")) ++
      (h.spec.modifier |> B.toOption |> Option.unwrap(~default="")))
  | TLDB(db) => db.dbName |> B.toOption |> Option.unwrap(~default="Undefined")
  | TLPmFunc(f) => f.fnname
  | TLFunc(f) => f.ufMetadata.ufmName |> B.toOption |> Option.unwrap(~default="")
  | TLTipe(t) => t.utName |> B.toOption |> Option.unwrap(~default="")
  }

let id = tl =>
  switch tl {
  | TLHandler(h) => h.hTLID
  | TLDB(db) => db.dbTLID
  | TLFunc(f) => f.ufTLID
  | TLPmFunc(f) => f.pfTLID
  | TLTipe(t) => t.utTLID
  }

let pos = tl =>
  switch tl {
  | TLHandler(h) => h.pos
  | TLDB(db) => db.pos
  | TLPmFunc(f) => recover("no pos in a func", ~debug=f.pfTLID, {x: 0, y: 0})
  | TLFunc(f) => recover("no pos in a func", ~debug=f.ufTLID, {x: 0, y: 0})
  | TLTipe(t) => recover("no pos in a tipe", ~debug=t.utTLID, {x: 0, y: 0})
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

let fromList = (tls: list<toplevel>): TLIDDict.t<toplevel> =>
  tls |> List.map(~f=tl => (id(tl), tl)) |> TD.fromList

let move = (tlid: TLID.t, xOffset: int, yOffset: int, m: model): model => {
  let newPos = p => {x: p.x + xOffset, y: p.y + yOffset}
  {
    ...m,
    handlers: Map.updateIfPresent(m.handlers, ~key=tlid, ~f=(h: handler) => {
      ...h,
      pos: newPos(h.pos),
    }),
    dbs: Map.updateIfPresent(m.dbs, ~key=tlid, ~f=(db: db) => {...db, pos: newPos(db.pos)}),
  }
}

let ufToTL = (uf: userFunction): toplevel => TLFunc(uf)

let pmfToTL = (pmf: packageFn): toplevel => TLPmFunc(pmf)

let utToTL = (ut: userTipe): toplevel => TLTipe(ut)

let asUserFunction = (tl: toplevel): option<userFunction> =>
  switch tl {
  | TLFunc(f) => Some(f)
  | _ => None
  }

let asUserTipe = (tl: toplevel): option<userTipe> =>
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

let asHandler = (tl: toplevel): option<handler> =>
  switch tl {
  | TLHandler(h) => Some(h)
  | _ => None
  }

let asDB = (tl: toplevel): option<db> =>
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

let handlers = (tls: list<toplevel>): list<handler> => List.filterMap(~f=asHandler, tls)

let dbs = (tls: TD.t<toplevel>): list<db> => tls |> Map.filterMapValues(~f=asDB)

let spaceOfHandler = (h: handler): handlerSpace => SpecHeaders.spaceOf(h.spec)

let spaceOf = (tl: toplevel): option<handlerSpace> =>
  tl |> asHandler |> Option.map(~f=spaceOfHandler)

let isHTTPHandler = (tl: toplevel): bool => tl |> spaceOf |> \"="(Some(HSHTTP))

let isCronHandler = (tl: toplevel): bool => tl |> spaceOf |> \"="(Some(HSCron))

let isWorkerHandler = (tl: toplevel): bool => tl |> spaceOf |> \"="(Some(HSWorker))

let isReplHandler = (tl: toplevel): bool => tl |> spaceOf |> \"="(Some(HSRepl))

let isDeprecatedCustomHandler = (tl: toplevel): bool =>
  tl |> spaceOf |> \"="(Some(HSDeprecatedOther))

let toOp = (tl: toplevel): list<op> =>
  switch tl {
  | TLHandler(h) => list{SetHandler(h.hTLID, h.pos, h)}
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
  | TLFunc(f) => Some(f.ufAST)
  | TLPmFunc(fn) => Some(FluidAST.ofExpr(fn.body))
  | _ => None
  }

let setAST = (tl: toplevel, newAST: FluidAST.t): toplevel =>
  switch tl {
  | TLHandler(h) => TLHandler({...h, ast: newAST})
  | TLFunc(uf) => TLFunc({...uf, ufAST: newAST})
  | TLDB(_) | TLTipe(_) | TLPmFunc(_) => tl
  }

let withAST = (m: model, tlid: TLID.t, ast: FluidAST.t): model => {
  ...m,
  handlers: Map.updateIfPresent(m.handlers, ~key=tlid, ~f=h => {...h, ast: ast}),
  userFunctions: Map.updateIfPresent(m.userFunctions, ~key=tlid, ~f=uf => {...uf, ufAST: ast}),
}

/* Create the modification to set the AST in this toplevel. `ops` is optional
 * other ops to include in this modification. Does not change the model. */
let setASTMod = (~ops=list{}, tl: toplevel, ast: FluidAST.t): modification =>
  switch tl {
  | TLHandler(h) =>
    if h.ast == ast {
      NoChange
    } else {
      AddOps(
        Belt.List.concat(ops, list{SetHandler(id(tl), h.pos, {...h, ast: ast})}),
        FocusNoChange,
      )
    }
  | TLFunc(f) =>
    if f.ufAST == ast {
      NoChange
    } else {
      AddOps(Belt.List.concat(ops, list{SetFunction({...f, ufAST: ast})}), FocusNoChange)
    }
  | TLPmFunc(_) => recover("cannot change ast in package manager", ~debug=tl, NoChange)
  | TLTipe(_) => recover("no ast in Tipes", ~debug=tl, NoChange)
  | TLDB(_) => recover("no ast in DBs", ~debug=tl, NoChange)
  }

@ocaml.doc(" modifyASTMod is a combination of getAST and setASTMod. It fetches the AST
  * for [tl] and passes it to [f], which should return a modified version of the
  * AST. An AddOps modification is returned, which updates the AST accordingly. ")
let modifyASTMod = (tl: toplevel, ~f: FluidAST.t => FluidAST.t): modification =>
  getAST(tl) |> Option.map(~f=\">>"(f, setASTMod(tl))) |> Option.unwrap(~default=NoChange)

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
  handlers: TD.t<handler>,
  dbs: TD.t<db>,
  userFunctions: TD.t<userFunction>,
  packageFn: TD.t<packageFn>,
  userTipes: TD.t<userTipe>,
): TD.t<toplevel> =>
  Map.map(~f=h => TLHandler(h), handlers)
  |> Map.mergeLeft(Map.map(~f=db => TLDB(db), dbs))
  |> Map.mergeLeft(Map.map(~f=ufToTL, userFunctions))
  |> Map.mergeLeft(Map.map(~f=pmfToTL, packageFn))
  |> Map.mergeLeft(Map.map(~f=utToTL, userTipes))

let all = (m: model): TD.t<toplevel> =>
  combine(m.handlers, m.dbs, m.userFunctions, m.functions.packageFunctions, m.userTipes)

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

let allDBNames = (dbs: TD.t<db>): list<string> =>
  dbs |> Map.filterMapValues(~f=db =>
    switch db.dbName {
    | F(_, name) => Some(name)
    | Blank(_) => None
    }
  )

let allGloballyScopedVarnames = (dbs: TD.t<db>): list<string> => allDBNames(dbs)

let asPage = (tl: toplevel, center: bool): page =>
  switch tl {
  | TLHandler(_) => FocusedHandler(id(tl), None, center)
  | TLDB(_) => FocusedDB(id(tl), center)
  | TLPmFunc(_) | TLFunc(_) => FocusedFn(id(tl), None)
  | TLTipe(_) => FocusedType(id(tl))
  }

let selected = (m: model): option<toplevel> =>
  m.cursorState |> CursorState.tlidOf |> Option.andThen(~f=get(m))

let selectedAST = (m: model): option<FluidAST.t> => selected(m) |> Option.andThen(~f=getAST)

let setSelectedAST = (m: model, ast: FluidAST.t): modification =>
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
