open Prelude

// Dark
module B = BlankOr
module TL = Toplevel
module TD = TLID.Dict
module E = ProgramTypes.Expr
module P = ProgramTypes.Pattern

type model = AppTypes.model
module Mod = AppTypes.Modification
type modification = AppTypes.modification

let generateFnName = (_: unit): string => "fn_" ++ (() |> Util.random |> string_of_int)

let generateTipeName = (): string => "Type_" ++ (() |> Util.random |> string_of_int)

let convertTipe = (tipe: DType.t): DType.t =>
  switch tipe {
  | TIncomplete => TAny
  | TError => TAny
  | _ => tipe
  }

// Call f on calls to uf across the whole AST
let transformFnCalls = (
  m: model,
  uf: PT.UserFunction.t,
  f: FluidExpression.t => FluidExpression.t,
): list<PT.Op.t> => {
  let transformCallsInAst = (ast: FluidAST.t) => {
    let rec run = e =>
      switch e {
      | E.EFnCall(_, User(name), _, _) if Some(name) == BlankOr.toOption(uf.metadata.name) => f(e)
      | other => FluidExpression.deprecatedWalk(~f=run, other)
      }

    FluidAST.map(ast, ~f=run)
  }

  let newHandlers = m.handlers |> Map.filterMapValues(~f=(h: PT.Handler.t) => {
    let newAst = h.ast |> transformCallsInAst
    if newAst != h.ast {
      Some(PT.Op.SetHandler(h.tlid, h.pos, {...h, ast: newAst}))
    } else {
      None
    }
  })

  let newFunctions = m.userFunctions |> Map.filterMapValues(~f=(uf_: PT.UserFunction.t) => {
    let newAst = uf_.ast |> transformCallsInAst
    if newAst != uf_.ast {
      Some(PT.Op.SetFunction({...uf_, ast: newAst}))
    } else {
      None
    }
  })

  Belt.List.concat(newHandlers, newFunctions)
}

type wrapLoc =
  | WLetRHS
  | WLetBody
  | WIfCond
  | WIfThen
  | WIfElse
  | WMatchExpr
  | WMatchArm

let wrap = (wl: wrapLoc, _: model, tl: toplevel, id: id): AppTypes.modification => {
  let replacement = (e): FluidExpression.t => {
    let newB = FluidExpression.newB
    switch wl {
    | WLetRHS => ELet(gid(), "", e, newB())
    | WLetBody => ELet(gid(), "", newB(), e)
    | WIfCond => EIf(gid(), e, newB(), newB())
    | WIfThen => EIf(gid(), newB(), e, newB())
    | WIfElse => EIf(gid(), newB(), newB(), e)
    | WMatchExpr =>
      /* e becomes
       * match e
       * _ -> _ */
      let mid = gid()
      EMatch(mid, e, list{(P.PBlank(gid()), newB())})
    | WMatchArm =>
      /* e becomes
       * match _
       * _ ->  e
       * _ -> _
       *
       * (the empty line is b/c it's not always possible to add a new pattern
       * at the end of a match, but it's always possible to delete a pattern)
       * */
      let mid = gid()
      EMatch(mid, newB(), list{(P.PBlank(gid()), e), (P.PBlank(gid()), newB())})
    }
  }

  TL.getAST(tl)
  |> Option.map(~f=\">>"(FluidAST.update(~f=replacement, id), TL.setASTMod(tl)))
  |> Option.unwrap(~default=Mod.NoChange)
}

let takeOffRail = (_m: model, tl: toplevel, id: id): modification =>
  TL.getAST(tl)
  |> Option.map(~f=ast =>
    ast
    |> FluidAST.update(id, ~f=x =>
      switch x {
      | EFnCall(_, name, exprs, Rail) => EFnCall(id, name, exprs, NoRail)
      | e => recover("incorrect id in takeoffRail", e)
      }
    )
    |> TL.setASTMod(tl)
  )
  |> Option.unwrap(~default=Mod.NoChange)

let isRailable = (m: model, name: PT.FQFnName.t) =>
  m.functions
  |> Functions.find(name)
  |> Option.map(~f=fn => fn.fnReturnTipe == TOption || fn.fnReturnTipe == TResult)
  |> Option.unwrap(~default=false)

let putOnRail = (m: model, tl: toplevel, id: id): modification =>
  // Only toggle onto rail iff. return tipe is TOption or TResult
  TL.modifyASTMod(tl, ~f=ast =>
    FluidAST.update(id, ast, ~f=x =>
      switch x {
      | EFnCall(_, name, exprs, NoRail) if isRailable(m, name) => EFnCall(id, name, exprs, Rail)
      | e => e
      }
    )
  )

let extractVarInAst = (
  m: model,
  tl: toplevel,
  id: id,
  varname: string,
  ast: FluidAST.t,
): FluidAST.t => {
  let traceID = Analysis.getSelectedTraceID(m, TL.id(tl))
  switch FluidAST.find(id, ast) {
  | Some(e) =>
    let lastPlaceWithSameVarsAndValues = {
      let ancestors = FluidAST.ancestors(id, ast)
      let freeVariables = AST.freeVariables(e) |> List.map(~f=Tuple2.second) |> Set.String.fromList

      list{e, ...ancestors}
      |> List.takeWhile(~f=elem => {
        let id = FluidExpression.toID(elem)
        let availableVars =
          Option.map(traceID, ~f=Analysis.getAvailableVarnames(m, tl, id))
          |> Option.unwrap(~default=list{})
          |> List.map(~f=((varname, _)) => varname)
          |> Set.String.fromList

        let allRequiredVariablesAvailable =
          Set.difference(freeVariables, availableVars) |> Set.isEmpty

        let noVariablesAreRedefined =
          freeVariables |> Set.toList |> List.all(~f=\"<<"(not, v => AST.isDefinitionOf(v, elem)))

        allRequiredVariablesAvailable && noVariablesAreRedefined
      })
      |> List.last
    }

    switch lastPlaceWithSameVarsAndValues {
    | Some(last) =>
      ast
      |> FluidAST.update(FluidExpression.toID(last), ~f=x =>
        switch x {
        | last => ELet(gid(), varname, FluidExpression.clone(e), last)
        }
      )
      |> FluidAST.replace(FluidExpression.toID(e), ~replacement=EVariable(gid(), varname))
    | None => ast
    }
  | None => ast
  }
}

let extractVariable = (m: model, tl: toplevel, id: id): modification => {
  let varname = "var" ++ string_of_int(Util.random())
  TL.modifyASTMod(tl, ~f=extractVarInAst(m, tl, id, varname))
}

let extractFunction = (m: model, tl: toplevel, id: id): modification => {
  let tlid = TL.id(tl)
  let ast = TL.getAST(tl)
  switch (ast, Option.andThen(ast, ~f=FluidAST.find(id))) {
  | (Some(ast), Some(body)) =>
    let name = generateFnName()
    let glob = TL.allGloballyScopedVarnames(m.dbs)
    let freeVars =
      AST.freeVariables(body) |> List.filter(~f=((_, v)) => !List.member(~value=v, glob))

    let paramExprs = List.map(~f=((_, name_)) => E.EVariable(gid(), name_), freeVars)

    let replacement = E.EFnCall(gid(), User(name), paramExprs, NoRail)
    let newAST = FluidAST.replace(~replacement, id, ast)
    let astOp = TL.setASTMod(tl, newAST)
    let params = List.map(freeVars, ~f=((id, name_)) => {
      let tipe =
        Analysis.getSelectedTraceID(m, tlid)
        |> Option.andThen(~f=Analysis.getTipeOf(m, id))
        |> Option.unwrap(~default=DType.TAny)
        |> convertTipe

      {
        PT.UserFunction.Parameter.name: F(gid(), name_),
        typ: F(gid(), tipe),
        args: list{},
        optional: false,
        description: "",
      }
    })

    let metadata = {
      PT.UserFunction.Metadata.name: F(gid(), name),
      parameters: params,
      description: "",
      returnType: F(gid(), TAny),
      infix: false,
    }

    let newF = {
      PT.UserFunction.tlid: gtlid(),
      metadata: metadata,
      ast: FluidExpression.clone(body) |> FluidAST.ofExpr,
    }

    Many(list{
      AddOps(list{SetFunction(newF)}, FocusExact(tlid, FluidExpression.toID(replacement))),
      astOp,
    })
  | _ => NoChange
  }
}

let renameFunction = (m: model, uf: PT.UserFunction.t, newName: PT.FQFnName.t): list<PT.Op.t> => {
  open ProgramTypes.Expr
  let fn = e =>
    switch e {
    | EFnCall(id, _, params, r) => EFnCall(id, newName, params, r)
    | _ => e
    }

  transformFnCalls(m, uf, fn)
}

let renameUserTipe = (m: model, old: PT.UserType.t, new_: PT.UserType.t): list<PT.Op.t> => {
  let renameUserTipeInFnParameters = (fn, oldTipe: PT.UserType.t, newTipe: PT.UserType.t) => {
    let transformUse = (newName_, oldUse) =>
      switch oldUse {
      | PParamTipe(F(id, TUserType(_, v))) => PParamTipe(F(id, TUserType(newName_, v)))
      | _ => oldUse
      }

    let (origName, uses) = switch oldTipe.name {
    | Blank(_) => (None, list{})
    | F(_, n) => (Some(n), UserFunctions.usesOfTipe(n, oldTipe.version, fn))
    }

    let newName = switch newTipe.name {
    | Blank(_) => None
    | F(_, n) => Some(n)
    }

    switch (origName, newName) {
    | (Some(_), Some(newName)) =>
      List.foldRight(
        ~f=(accfn, use) => UserFunctions.replaceParamTipe(use, transformUse(newName, use), accfn),
        ~initial=fn,
        uses,
      )
    | _ => fn
    }
  }

  let newFunctions = m.userFunctions |> Map.filterMapValues(~f=uf => {
    let newFn = renameUserTipeInFnParameters(uf, old, new_)
    if newFn != uf {
      Some(PT.Op.SetFunction(newFn))
    } else {
      None
    }
  })

  newFunctions
}

let fnUseCount = (m: model, name: string): int =>
  Map.get(m.usedFns, ~key=name) |> Option.unwrap(~default=0)

let usedFn = (m: model, name: string): bool => fnUseCount(m, name) != 0

let tipeUseCount = (m: model, name: string): int =>
  Map.get(m.usedTipes, ~key=name) |> Option.unwrap(~default=0)

let usedTipe = (m: model, name: string): bool => tipeUseCount(m, name) != 0

let dbUseCount = (m: model, name: string): int =>
  Map.get(m.usedDBs, ~key=name) |> Option.unwrap(~default=0)

let updateUsageCounts = (m: model): model => {
  open ProgramTypes.Expr
  let countFromList = names =>
    List.fold(names, ~initial=Map.String.empty, ~f=(dict, name) =>
      Map.update(dict, ~key=name, ~f=x =>
        switch x {
        | Some(count) => Some(count + 1)
        | None => Some(1)
        }
      )
    )

  let asts =
    m |> TL.all |> Map.mapValues(~f=TL.getAST) |> List.filterMap(~f=Option.map(~f=FluidAST.toExpr))

  // Pretend it's one big AST
  let bigAst = EList(gid(), asts)
  let usedFns =
    bigAst
    |> FluidExpression.filterMap(~f=x =>
      switch x {
      | EFnCall(_, name, _, _) => Some(PT.FQFnName.toString(name))
      | EBinOp(_, name, _, _, _) => Some(PT.FQFnName.InfixStdlibFnName.toString(name))
      | _ => None
      }
    )
    |> countFromList

  let usedDBs =
    bigAst
    |> FluidExpression.filterMap(~f=x =>
      switch x {
      | EVariable(_, name) if String.isCapitalized(name) => Some(name)
      | _ => None
      }
    )
    |> countFromList

  let usedTipes =
    m.userFunctions
    |> Map.mapValues(~f=UserFunctions.allParamData)
    |> List.flatten
    |> List.filterMap(~f=x =>
      // Note: this does _not_ currently handle multiple versions
      switch x {
      | PParamTipe(F(_, TUserType(name, _))) => Some(name)
      | _ => None
      }
    )
    |> countFromList

  {...m, usedDBs: usedDBs, usedFns: usedFns, usedTipes: usedTipes}
}

let removeFunctionParameter = (
  m: model,
  uf: PT.UserFunction.t,
  ufp: PT.UserFunction.Parameter.t,
): list<PT.Op.t> => {
  open ProgramTypes.Expr
  let indexInList =
    List.findIndex(~f=(_, p) => p == ufp, uf.metadata.parameters)
    |> Option.map(~f=Tuple2.first)
    |> recoverOpt("removing invalid fnparam", ~default=-1)

  let fn = e =>
    switch e {
    | EFnCall(id, name, params, r) =>
      EFnCall(id, name, List.removeAt(~index=indexInList, params), r)
    | _ => e
    }

  transformFnCalls(m, uf, fn)
}

let addFunctionParameter = (m: model, f: PT.UserFunction.t, currentBlankId: id): modification => {
  open ProgramTypes.Expr
  let transformOp = old => {
    let fn = e =>
      switch e {
      | EFnCall(id, name, params, r) =>
        EFnCall(id, name, Belt.List.concat(params, list{FluidExpression.newB()}), r)
      | _ => e
      }

    transformFnCalls(m, old, fn)
  }

  let replacement = UserFunctions.extend(f)
  let newCalls = transformOp(f)
  AddOps(list{SetFunction(replacement), ...newCalls}, FocusNext(f.tlid, Some(currentBlankId)))
}

let generateEmptyFunction = (_: unit): PT.UserFunction.t => {
  let funcName = generateFnName()
  let tlid = gtlid()
  let metadata: PT.UserFunction.Metadata.t = {
    name: F(gid(), funcName),
    parameters: list{},
    description: "",
    returnType: F(gid(), TAny),
    infix: false,
  }

  {
    tlid: tlid,
    metadata: metadata,
    ast: FluidAST.ofExpr(EBlank(gid())),
  }
}

let generateEmptyUserType = (): PT.UserType.t => {
  let tipeName = generateTipeName()
  let tlid = gtlid()
  let definition = PT.UserType.Definition.UTRecord(list{
    {name: "", nameID: gid(), typ: None, typeID: gid()},
  })
  {
    tlid: tlid,
    name: F(gid(), tipeName),
    version: 0,
    definition: definition,
  }
}

let generateUserType = (dv: option<RT.Dval.t>): Result.t<PT.UserType.t, string> =>
  switch dv {
  | Some(DObj(dvalmap)) =>
    let userTipeDefinition =
      dvalmap
      |> Belt.Map.String.toList
      |> List.map(~f=((k, v)) => {
        let typ = Runtime.typeOf(v)
        /*
         * In the future, we may want to recognize stringified UUIDs and
         * Dates, but we decided that today is not that day. See
         * discussion at
         * https://dark-inc.slack.com/archives/C7MFHVDDW/p1562878578176700
         * let tipe = v |> coerceType in
         */
        {PT.UserType.RecordField.name: k, nameID: gid(), typ: Some(typ), typeID: gid()}
      })

    Ok({
      ...generateEmptyUserType(),
      definition: UTRecord(userTipeDefinition),
    })
  | Some(_) => Error("Live value is not an object.")
  | None => Error("No live value.")
  }

let renameDBReferences = (m: model, oldName: string, newName: string): list<PT.Op.t> =>
  m
  |> TL.all
  |> Map.filterMapValues(~f=tl =>
    switch tl {
    | TLHandler(h) =>
      let newAST = h.ast |> FluidAST.map(~f=FluidExpression.renameVariableUses(~oldName, ~newName))

      if newAST != h.ast {
        Some(PT.Op.SetHandler(h.tlid, h.pos, {...h, ast: newAST}))
      } else {
        None
      }
    | TLFunc(f) =>
      let newAST = f.ast |> FluidAST.map(~f=FluidExpression.renameVariableUses(~oldName, ~newName))

      if newAST != f.ast {
        Some(SetFunction({...f, ast: newAST}))
      } else {
        None
      }
    | TLPmFunc(_) => None
    | TLTipe(_) => None
    | TLDB(_) => None
    }
  )

let reorderFnCallArgs = (
  m: model,
  tlid: TLID.t,
  fnName: PT.FQFnName.t,
  oldPos: int,
  newPos: int,
): list<modification> =>
  Introspect.allUsedIn(tlid, m)
  |> List.filterMap(~f=tl =>
    switch TL.getAST(tl) {
    | Some(ast) => Some(tl, ast)
    | None => None
    }
  )
  |> List.map(~f=((tl, ast)) =>
    ast |> FluidAST.map(~f=AST.reorderFnCallArgs(fnName, oldPos, newPos)) |> TL.setASTMod(tl)
  )

let hasExistingFunctionNamed = (m: model, name: string): bool => {
  let fns = Introspect.functionsByName(m.userFunctions)
  Map.has(fns, ~key=name)
}

let createNewDB = (m: model, maybeName: option<string>, pos: pos): modification => {
  let name = maybeName |> Option.unwrap(~default=DB.generateDBName())
  if Autocomplete.assertValid(Autocomplete.dbNameValidator, name) != name {
    Model.updateErrorMod(
      Error.set("DB name must match " ++ (Autocomplete.dbNameValidator ++ " pattern")),
    )
  } else if List.member(~value=name, TL.allDBNames(m.dbs)) {
    Model.updateErrorMod(Error.set("There is already a DB named " ++ name))
  } else {
    let next = gid()
    let tlid = gtlid()
    let pageChanges = list{Mod.SetPage(FocusedDB(tlid, true))}
    let rpcCalls = list{
      PT.Op.CreateDBWithBlankOr(tlid, pos, Prelude.gid(), name),
      AddDBCol(tlid, next, Prelude.gid()),
    }

    Many(list{
      AppendUnlockedDBs(TLID.Set.singleton(tlid)),
      AddOps(rpcCalls, FocusExact(tlid, next)),
      ...pageChanges,
    })
  }
}

// Create a new function, update the server, and go to the new function
let createNewFunction = (m: model, newFnName: option<string>): modification => {
  let fn = generateEmptyFunction()
  let newFn = switch newFnName {
  | Some(name) => {...fn, metadata: {...fn.metadata, name: F(gid(), name)}}
  | None => fn
  }

  switch newFnName {
  | Some(name) if hasExistingFunctionNamed(m, name) =>
    Model.updateErrorMod(Error.set("Function named " ++ (name ++ " already exists")))
  | _ =>
    // We need to update both the model and the backend
    Many(list{
      ReplaceAllModificationsWithThisOne(m => (UserFunctions.upsert(m, newFn), Tea.Cmd.none)),
      // Both ops in a single transaction
      AddOps(list{SetFunction(newFn)}, FocusNothing),
      MakeCmd(Url.navigateTo(FocusedFn(newFn.tlid, None))),
    })
  }
}

/* Create a new function, update the expression (tlid, id) to call the new
 * function, update the server about both functions, and go to the new function */
let createAndInsertNewFunction = (
  m: model,
  tlid: TLID.t,
  partialID: id,
  newFnName: string,
): modification =>
  switch Toplevel.get(m, tlid) |> Option.thenAlso(~f=TL.getAST) {
  | Some(tl, ast) =>
    // Create the new function
    let fn = generateEmptyFunction()
    let newFn = {
      ...fn,
      metadata: {...fn.metadata, name: F(gid(), newFnName)},
    }

    let op = PT.Op.SetFunction(newFn)
    // Update the old ast
    let replacement = E.EFnCall(partialID, User(newFnName), list{}, NoRail)
    let newAST = FluidAST.replace(partialID, ast, ~replacement)
    // We need to update both the model and the backend
    let alreadyExists = hasExistingFunctionNamed(m, newFnName)
    if alreadyExists {
      Model.updateErrorMod(Error.set("Function named " ++ (newFnName ++ " already exists")))
    } else {
      let invalidMessage = Autocomplete.validateFunctionName(newFnName)
      switch invalidMessage {
      | Some(msg) => Model.updateErrorMod(Error.set(msg))
      | None =>
        Many(list{
          ReplaceAllModificationsWithThisOne(
            m => (
              TL.withAST(m, tlid, newAST) |> (m => UserFunctions.upsert(m, newFn)),
              Tea.Cmd.none,
            ),
          ),
          // Both ops in a single transaction
          TL.setASTMod(~ops=list{op}, tl, newAST),
          MakeCmd(Url.navigateTo(FocusedFn(newFn.tlid, None))),
        })
      }
    }
  | None => NoChange
  }
