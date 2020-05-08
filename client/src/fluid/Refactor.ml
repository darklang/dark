open Prelude

(* Dark *)
module B = BlankOr
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression
module P = FluidPattern
module M = Modifications

let generateFnName (_ : unit) : string =
  "fn_" ^ (() |> Util.random |> string_of_int)


let generateTipeName () : string = "Type_" ^ (() |> Util.random |> string_of_int)

let convertTipe (tipe : tipe) : tipe =
  match tipe with TIncomplete -> TAny | TError -> TAny | _ -> tipe


(* Call f on calls to uf across the whole AST *)
let transformFnCalls
    (m : model) (uf : userFunction) (f : FluidExpression.t -> FluidExpression.t)
    : op list =
  let transformCallsInAst (ast : FluidAST.t) =
    let rec run e =
      match e with
      | E.EFnCall (_, name, _, _)
        when Some name = BlankOr.toOption uf.ufMetadata.ufmName ->
          f e
      | other ->
          E.deprecatedWalk ~f:run other
    in
    FluidAST.map ast ~f:run
  in
  let newHandlers =
    m.handlers
    |> TD.filterMapValues ~f:(fun h ->
           let newAst = h.ast |> transformCallsInAst in
           if newAst <> h.ast
           then Some (SetHandler (h.hTLID, h.pos, {h with ast = newAst}))
           else None)
  in
  let newFunctions =
    m.userFunctions
    |> TD.filterMapValues ~f:(fun uf_ ->
           let newAst = uf_.ufAST |> transformCallsInAst in
           if newAst <> uf_.ufAST
           then Some (SetFunction {uf_ with ufAST = newAst})
           else None)
  in
  newHandlers @ newFunctions


let modASTWithID (tl : toplevel) (id : ID.t) ~(f : E.t -> E.t) : modification =
  TL.getAST tl
  |> Option.andThen ~f:(fun ast ->
         let newAST = FluidAST.update ~f id ast in
         if newAST <> ast then Some newAST else None)
  |> Option.map ~f:(M.fullstackASTUpdate tl)
  |> Option.withDefault ~default:NoChange


type wrapLoc =
  | WLetRHS
  | WLetBody
  | WIfCond
  | WIfThen
  | WIfElse
  | WMatchExpr
  | WMatchArm

let wrap (wl : wrapLoc) (_ : model) (tl : toplevel) (id : ID.t) : modification =
  let replacement e : FluidExpression.t =
    let newBlankPattern mid = P.FPBlank (mid, gid ()) in
    match wl with
    | WLetRHS ->
        ELet (gid (), "", e, E.newB ())
    | WLetBody ->
        ELet (gid (), "", E.newB (), e)
    | WIfCond ->
        EIf (gid (), e, E.newB (), E.newB ())
    | WIfThen ->
        EIf (gid (), E.newB (), e, E.newB ())
    | WIfElse ->
        EIf (gid (), E.newB (), E.newB (), e)
    | WMatchExpr ->
        (* e becomes
         * match e
         * _ -> _ *)
        let mid = gid () in
        EMatch (mid, e, [(newBlankPattern mid, E.newB ())])
    | WMatchArm ->
        (* e becomes
         * match _
         * _ ->  e
         * _ -> _
         *
         * (the empty line is b/c it's not always possible to add a new pattern
         * at the end of a match, but it's always possible to delete a pattern)
         * *)
        let mid = gid () in
        EMatch
          ( mid
          , E.newB ()
          , [(newBlankPattern mid, e); (newBlankPattern mid, E.newB ())] )
  in
  modASTWithID tl id ~f:replacement


let takeOffRail (_m : model) (tl : toplevel) (id : ID.t) : modification =
  modASTWithID tl id ~f:(function
      | EFnCall (_, name, exprs, Rail) ->
          EFnCall (id, name, exprs, NoRail)
      | e ->
          recover "incorrect id in takeoffRail" e)


let isRailable (m : model) (name : string) =
  m.functions
  |> Functions.find name
  |> Option.map ~f:(fun fn ->
         fn.fnReturnTipe = TOption || fn.fnReturnTipe = TResult)
  |> Option.withDefault ~default:false


let putOnRail (m : model) (tl : toplevel) (id : ID.t) : modification =
  (* Only toggle onto rail iff. return tipe is TOption or TResult *)
  modASTWithID tl id ~f:(function
      | EFnCall (_, name, exprs, NoRail) when isRailable m name ->
          EFnCall (id, name, exprs, Rail)
      | e ->
          e)


let extractVarInAst
    (m : model)
    (tl : toplevel)
    (id : ID.t)
    (varname : string)
    (ast : FluidAST.t) : FluidAST.t =
  let traceID = Analysis.getSelectedTraceID m (TL.id tl) in
  match FluidAST.find id ast with
  | Some e ->
      let lastPlaceWithSameVarsAndValues =
        let ancestors = FluidAST.ancestors id ast in
        let freeVariables =
          AST.freeVariables e |> List.map ~f:Tuple2.second |> StrSet.fromList
        in
        e :: ancestors
        |> List.takeWhile ~f:(fun elem ->
               let id = E.toID elem in
               let availableVars =
                 Option.map traceID ~f:(Analysis.getAvailableVarnames m tl id)
                 |> Option.withDefault ~default:[]
                 |> List.map ~f:(fun (varname, _) -> varname)
                 |> StrSet.fromList
               in
               let allRequiredVariablesAvailable =
                 StrSet.diff freeVariables availableVars |> StrSet.isEmpty
               in
               let noVariablesAreRedefined =
                 freeVariables
                 |> StrSet.toList
                 |> List.all ~f:(not << fun v -> AST.isDefinitionOf v elem)
               in
               allRequiredVariablesAvailable && noVariablesAreRedefined)
        |> List.last
      in
      ( match lastPlaceWithSameVarsAndValues with
      | Some last ->
          ast
          |> FluidAST.update (E.toID last) ~f:(function last ->
                 ELet (gid (), varname, E.clone e, last))
          |> FluidAST.replace
               (E.toID e)
               ~replacement:(EVariable (gid (), varname))
      | None ->
          ast )
  | None ->
      ast


let extractVariable (m : model) (tl : toplevel) (id : ID.t) : modification =
  let varname = "var" ^ string_of_int (Util.random ()) in
  TL.getAST tl
  |> Option.map ~f:(extractVarInAst m tl id varname)
  |> Option.map ~f:(M.fullstackASTUpdate tl)
  |> Option.withDefault ~default:NoChange


let extractFunction (m : model) (tl : toplevel) (id : ID.t) : modification =
  let tlid = TL.id tl in
  let ast = TL.getAST tl in
  match (ast, Option.andThen ast ~f:(FluidAST.find id)) with
  | Some ast, Some body ->
      let name = generateFnName () in
      let glob = TL.allGloballyScopedVarnames m.dbs in
      let freeVars =
        AST.freeVariables body
        |> List.filter ~f:(fun (_, v) -> not (List.member ~value:v glob))
      in
      let paramExprs =
        List.map ~f:(fun (_, name_) -> E.EVariable (gid (), name_)) freeVars
      in
      let replacement = E.EFnCall (gid (), name, paramExprs, NoRail) in
      let newAST = FluidAST.replace ~replacement id ast in
      let astUpdate = M.fullstackASTUpdate tl newAST in
      let params =
        List.map freeVars ~f:(fun (id, name_) ->
            let tipe =
              Analysis.getSelectedTraceID m tlid
              |> Option.andThen ~f:(Analysis.getTipeOf m id)
              |> Option.withDefault ~default:TAny
              |> convertTipe
            in
            { ufpName = F (gid (), name_)
            ; ufpTipe = F (gid (), tipe)
            ; ufpBlock_args = []
            ; ufpOptional = false
            ; ufpDescription = "" })
      in
      let metadata =
        { ufmName = F (gid (), name)
        ; ufmParameters = params
        ; ufmDescription = ""
        ; ufmReturnTipe = F (gid (), TAny)
        ; ufmInfix = false }
      in
      let newF =
        { ufTLID = gtlid ()
        ; ufMetadata = metadata
        ; ufAST = E.clone body |> FluidAST.ofExpr }
      in
      Many
        [ AddOps ([SetFunction newF], FocusExact (tlid, E.toID replacement))
        ; astUpdate ]
  | _ ->
      NoChange


let renameFunction (m : model) (uf : userFunction) (newName : string) : op list
    =
  let open FluidExpression in
  let fn e =
    match e with
    | EFnCall (id, _, params, r) ->
        EFnCall (id, newName, params, r)
    | _ ->
        e
  in
  transformFnCalls m uf fn


let renameUserTipe (m : model) (old : userTipe) (new_ : userTipe) : op list =
  let renameUserTipeInFnParameters fn oldTipe newTipe =
    let transformUse newName_ oldUse =
      match oldUse with
      | PParamTipe (F (id, TUserType (_, v))) ->
          PParamTipe (F (id, TUserType (newName_, v)))
      | _ ->
          oldUse
    in
    let origName, uses =
      match oldTipe.utName with
      | Blank _ ->
          (None, [])
      | F (_, n) ->
          (Some n, UserFunctions.usesOfTipe n oldTipe.utVersion fn)
    in
    let newName =
      match newTipe.utName with Blank _ -> None | F (_, n) -> Some n
    in
    match (origName, newName) with
    | Some _, Some newName ->
        List.foldr
          ~f:(fun use accfn ->
            UserFunctions.replaceParamTipe use (transformUse newName use) accfn)
          ~init:fn
          uses
    | _ ->
        fn
  in
  let newFunctions =
    m.userFunctions
    |> TD.filterMapValues ~f:(fun uf ->
           let newFn = renameUserTipeInFnParameters uf old new_ in
           if newFn <> uf then Some (SetFunction newFn) else None)
  in
  newFunctions


let fnUseCount (m : model) (name : string) : int =
  StrDict.get m.usedFns ~key:name |> Option.withDefault ~default:0


let usedFn (m : model) (name : string) : bool = fnUseCount m name <> 0

let tipeUseCount (m : model) (name : string) : int =
  StrDict.get m.usedTipes ~key:name |> Option.withDefault ~default:0


let usedTipe (m : model) (name : string) : bool = tipeUseCount m name <> 0

let dbUseCount (m : model) (name : string) : int =
  StrDict.get m.usedDBs ~key:name |> Option.withDefault ~default:0


let updateUsageCounts (m : model) : model =
  let open FluidExpression in
  let countFromList names =
    List.foldl names ~init:StrDict.empty ~f:(fun name dict ->
        StrDict.update dict ~key:name ~f:(function
            | Some count ->
                Some (count + 1)
            | None ->
                Some 1))
  in
  let asts =
    m
    |> TL.all
    |> TD.mapValues ~f:TL.getAST
    |> List.filterMap ~f:(Option.map ~f:FluidAST.toExpr)
  in
  (* Pretend it's one big AST *)
  let bigAst = EList (gid (), asts) in
  let usedFns =
    bigAst
    |> E.filterMap ~f:(function
           | EFnCall (_, name, _, _) | EBinOp (_, name, _, _, _) ->
               Some name
           | _ ->
               None)
    |> countFromList
  in
  let usedDBs =
    bigAst
    |> E.filterMap ~f:(function
           | EVariable (_, name) when String.isCapitalized name ->
               Some name
           | _ ->
               None)
    |> countFromList
  in
  let usedTipes =
    m.userFunctions
    |> TD.mapValues ~f:UserFunctions.allParamData
    |> List.concat
    |> List.filterMap ~f:(function
           (* Note: this does _not_ currently handle multiple versions *)
           | PParamTipe (F (_, TUserType (name, _))) ->
               Some name
           | _ ->
               None)
    |> countFromList
  in
  {m with usedDBs; usedFns; usedTipes}


let removeFunctionParameter
    (m : model) (uf : userFunction) (ufp : userFunctionParameter) : op list =
  let open FluidExpression in
  let indexInList =
    List.findIndex ~f:(fun p -> p = ufp) uf.ufMetadata.ufmParameters
    |> recoverOpt "removing invalid fnparam" ~default:(-1)
  in
  let fn e =
    match e with
    | EFnCall (id, name, params, r) ->
        EFnCall (id, name, List.removeAt ~index:indexInList params, r)
    | _ ->
        e
  in
  transformFnCalls m uf fn


let addFunctionParameter (m : model) (f : userFunction) (currentBlankId : ID.t)
    : modification =
  let open FluidExpression in
  let transformOp old =
    let fn e =
      match e with
      | EFnCall (id, name, params, r) ->
          EFnCall (id, name, params @ [FluidExpression.newB ()], r)
      | _ ->
          e
    in
    transformFnCalls m old fn
  in
  let replacement = UserFunctions.extend f in
  let newCalls = transformOp f in
  AddOps
    ( SetFunction replacement :: newCalls
    , FocusNext (f.ufTLID, Some currentBlankId) )


let generateEmptyFunction (_ : unit) : userFunction =
  let funcName = generateFnName () in
  let tlid = gtlid () in
  let metadata =
    { ufmName = F (gid (), funcName)
    ; ufmParameters = []
    ; ufmDescription = ""
    ; ufmReturnTipe = F (gid (), TAny)
    ; ufmInfix = false }
  in
  { ufTLID = tlid
  ; ufMetadata = metadata
  ; ufAST = FluidAST.ofExpr (EBlank (gid ())) }


let generateEmptyUserType () : userTipe =
  let tipeName = generateTipeName () in
  let tlid = gtlid () in
  let definition = UTRecord [{urfName = B.new_ (); urfTipe = B.new_ ()}] in
  { utTLID = tlid
  ; utName = F (gid (), tipeName)
  ; utVersion = 0
  ; utDefinition = definition }


let generateUserType (dv : dval option) : (string, userTipe) Result.t =
  match dv with
  | Some (DObj dvalmap) ->
      let userTipeDefinition =
        dvalmap
        |> StrDict.toList
        |> List.map ~f:(fun (k, v) ->
               let tipe = v |> Runtime.typeOf in
               (*
                * In the future, we may want to recognize stringified UUIDs and
                * Dates, but we decided that today is not that day. See
                * discussion at
                * https://dark-inc.slack.com/archives/C7MFHVDDW/p1562878578176700
                * let tipe = v |> coerceType in
                *)
               {urfName = k |> BlankOr.newF; urfTipe = tipe |> BlankOr.newF})
      in
      Ok
        { (generateEmptyUserType ()) with
          utDefinition = UTRecord userTipeDefinition }
  | Some _ ->
      Error "Live value is not an object."
  | None ->
      Error "No live value."


let renameDBReferences (m : model) (oldName : dbName) (newName : dbName) :
    op list =
  m
  |> TL.all
  |> TD.filterMapValues ~f:(fun tl ->
         match tl with
         | TLHandler h ->
             let newAST =
               h.ast |> FluidAST.map ~f:(E.renameVariableUses ~oldName ~newName)
             in
             if newAST <> h.ast
             then Some (SetHandler (h.hTLID, h.pos, {h with ast = newAST}))
             else None
         | TLFunc f ->
             let newAST =
               f.ufAST
               |> FluidAST.map ~f:(E.renameVariableUses ~oldName ~newName)
             in
             if newAST <> f.ufAST
             then Some (SetFunction {f with ufAST = newAST})
             else None
         | TLPmFunc _ ->
             None
         | TLTipe _ ->
             None
         | TLDB _ ->
             None
         | TLGroup _ ->
             None)


let reorderFnCallArgs
    (m : model) (tlid : TLID.t) (fnName : string) (oldPos : int) (newPos : int)
    : modification list =
  Introspect.allUsedIn tlid m
  |> List.filterMap ~f:(fun tl ->
         match TL.getAST tl with Some ast -> Some (tl, ast) | None -> None)
  |> List.map ~f:(fun (tl, ast) ->
         ast
         |> FluidAST.map ~f:(AST.reorderFnCallArgs fnName oldPos newPos)
         |> TL.setASTOpMod tl)


let hasExistingFunctionNamed (m : model) (name : string) : bool =
  let fns = Introspect.functionsByName m.userFunctions in
  StrDict.has fns ~key:name


let createNewDB (m : model) (maybeName : dbName option) (pos : pos) :
    modification =
  let name = maybeName |> Option.withDefault ~default:(DB.generateDBName ()) in
  if Autocomplete.assertValid Autocomplete.dbNameValidator name <> name
  then
    Model.updateErrorMod
      (Error.set
         ("DB name must match " ^ Autocomplete.dbNameValidator ^ " pattern"))
  else if List.member ~value:name (TL.allDBNames m.dbs)
  then Model.updateErrorMod (Error.set ("There is already a DB named " ^ name))
  else
    let next = gid () in
    let tlid = gtlid () in
    let pageChanges = [SetPage (FocusedDB (tlid, true))] in
    let rpcCalls =
      [ CreateDBWithBlankOr (tlid, pos, Prelude.gid (), name)
      ; AddDBCol (tlid, next, Prelude.gid ()) ]
    in
    Many
      ( AppendUnlockedDBs (StrSet.fromList [TLID.toString tlid])
      :: AddOps (rpcCalls, FocusExact (tlid, next))
      :: pageChanges )


(* Create a new function, update the server, and go to the new function *)
let createNewFunction (m : model) (newFnName : string option) : modification =
  let fn = generateEmptyFunction () in
  let newFn =
    match newFnName with
    | Some name ->
        {fn with ufMetadata = {fn.ufMetadata with ufmName = F (gid (), name)}}
    | None ->
        fn
  in
  match newFnName with
  | Some name when hasExistingFunctionNamed m name ->
      Model.updateErrorMod
        (Error.set ("Function named " ^ name ^ " already exists"))
  | _ ->
      (* We need to update both the model and the backend *)
      Many
        [ ReplaceAllModificationsWithThisOne
            (fun m -> (UserFunctions.upsert m newFn, Tea.Cmd.none))
        ; (* Both ops in a single transaction *)
          AddOps ([SetFunction newFn], FocusNothing)
        ; MakeCmd (Url.navigateTo (FocusedFn (newFn.ufTLID, None))) ]


(* Create a new function, update the expression (tlid, id) to call the new
 * function, update the server about both functions, and go to the new function *)
let createAndInsertNewFunction
    (m : model) (tlid : TLID.t) (partialID : ID.t) (newFnName : string) :
    modification =
  match Toplevel.get m tlid |> Option.thenAlso ~f:TL.getAST with
  | Some (tl, ast) ->
      (* Create the new function *)
      let fn = generateEmptyFunction () in
      let newFn =
        { fn with
          ufMetadata = {fn.ufMetadata with ufmName = F (gid (), newFnName)} }
      in
      let op = SetFunction newFn in
      (* Update the old ast *)
      let replacement = E.EFnCall (partialID, newFnName, [], NoRail) in
      let newAST = FluidAST.replace partialID ast ~replacement in
      (* We need to update both the model and the backend *)
      let alreadyExists = hasExistingFunctionNamed m newFnName in
      if alreadyExists
      then
        Model.updateErrorMod
          (Error.set ("Function named " ^ newFnName ^ " already exists"))
      else
        let invalidMessage = Autocomplete.validateFunctionName newFnName in
        ( match invalidMessage with
        | Some msg ->
            Model.updateErrorMod (Error.set msg)
        | None ->
            Many
              [ ReplaceAllModificationsWithThisOne
                  (fun m ->
                    ( ( TL.updateModelWithAST m tlid newAST
                      |> fun m -> UserFunctions.upsert m newFn )
                    , Tea.Cmd.none ))
              ; (* Both ops in a single transaction *)
                TL.setASTOpMod ~ops:[op] tl newAST
              ; MakeCmd (Url.navigateTo (FocusedFn (newFn.ufTLID, None))) ] )
  | None ->
      NoChange
