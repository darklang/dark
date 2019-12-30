open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel
module TD = TLIDDict
module E = FluidExpression

let generateFnName (_ : unit) : string =
  "fn_" ^ (() |> Util.random |> string_of_int)


let generateTipeName () : string = "Type_" ^ (() |> Util.random |> string_of_int)

let convertTipe (tipe : tipe) : tipe =
  match tipe with TIncomplete -> TAny | TError -> TAny | _ -> tipe


(* Call f on calls to uf across the whole AST *)
let transformFnCalls
    (m : model) (uf : userFunction) (f : fluidExpr -> fluidExpr) : op list =
  let transformCallsInAst ast =
    let rec run e =
      match e with
      | EFnCall (_, name, _, _)
        when Some name = Blank.toOption uf.ufMetadata.ufmName ->
          f e
      | other ->
          E.walk ~f:run other
    in
    run ast
  in
  let newHandlers =
    m.handlers
    |> TD.filterMapValues ~f:(fun h ->
           let newAst = transformCallsInAst h.ast in
           if newAst <> h.ast
           then Some (SetHandler (h.hTLID, h.pos, {h with ast = newAst}))
           else None)
  in
  let newFunctions =
    m.userFunctions
    |> TD.filterMapValues ~f:(fun uf_ ->
           let newAst = transformCallsInAst uf_.ufAST in
           if newAst <> uf_.ufAST
           then Some (SetFunction {uf_ with ufAST = newAst})
           else None)
  in
  newHandlers @ newFunctions


type wrapLoc =
  | WLetRHS
  | WLetBody
  | WIfCond
  | WIfThen
  | WIfElse

let wrap (wl : wrapLoc) (_ : model) (tl : toplevel) (id : id) : modification =
  let module E = FluidExpression in
  let replacement e =
    match wl with
    | WLetRHS ->
        ELet (gid (), gid (), "", e, E.newB ())
    | WLetBody ->
        ELet (gid (), gid (), "", E.newB (), e)
    | WIfCond ->
        EIf (gid (), e, E.newB (), E.newB ())
    | WIfThen ->
        EIf (gid (), E.newB (), e, E.newB ())
    | WIfElse ->
        EIf (gid (), E.newB (), E.newB (), e)
  in
  TL.getAST tl
  |> Option.map ~f:(E.update ~f:replacement id)
  |> Option.map ~f:(TL.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let takeOffRail (_m : model) (tl : toplevel) (id : id) : modification =
  TL.getAST tl
  |> Option.map ~f:(fun ast ->
         FluidExpression.update id ast ~f:(function
             | EFnCall (_, name, exprs, Rail) ->
                 EFnCall (id, name, exprs, NoRail)
             | e ->
                 recover "incorrect id in takeoffRail" e))
  |> Option.map ~f:(Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let putOnRail (m : model) (tl : toplevel) (id : id) : modification =
  (* Only toggle onto rail iff. return tipe is TOption or TResult *)
  let isRailable name =
    (* We don't want to use m.complete.functions as the autocomplete
     * filters out deprecated functions *)
    let allFunctions =
      let ufs =
        m.userFunctions
        |> TD.mapValues ~f:(fun uf -> uf.ufMetadata)
        |> List.filterMap ~f:Functions.ufmToF
      in
      m.builtInFunctions @ ufs
    in
    List.find ~f:(fun fn -> fn.fnName = name) allFunctions
    |> Option.map ~f:(fun fn -> fn.fnReturnTipe)
    |> Option.map ~f:(fun t -> t = TOption || t = TResult)
    |> Option.withDefault ~default:false
  in
  TL.getAST tl
  |> Option.map ~f:(fun ast ->
         FluidExpression.update id ast ~f:(function
             | EFnCall (_, name, exprs, NoRail) when isRailable name ->
                 EFnCall (id, name, exprs, Rail)
             | e ->
                 e))
  |> Option.map ~f:(Toplevel.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let extractVarInAst
    (m : model) (tl : toplevel) (id : id) (varname : string) (ast : fluidExpr) :
    fluidExpr =
  let module E = FluidExpression in
  let traceID = Analysis.getSelectedTraceID m (TL.id tl) in
  match E.find id ast with
  | Some e ->
      let lastPlaceWithSameVarsAndValues =
        let ancestors = AST.ancestors id ast in
        let freeVariables =
          AST.freeVariables e |> List.map ~f:Tuple2.second |> StrSet.fromList
        in
        e :: ancestors
        |> List.takeWhile ~f:(fun elem ->
               let id = E.id elem in
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
          |> E.update (E.id last) ~f:(function last ->
                 ELet (gid (), gid (), varname, E.clone e, last))
          |> E.replace (E.id e) ~replacement:(EVariable (gid (), varname))
      | None ->
          ast )
  | None ->
      ast


let extractVariable (m : model) (tl : toplevel) (id : id) : modification =
  let varname = "var" ^ string_of_int (Util.random ()) in
  TL.getAST tl
  |> Option.map ~f:(extractVarInAst m tl id varname)
  |> Option.map ~f:(TL.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let extractFunction (m : model) (tl : toplevel) (id : id) : modification =
  let module E = FluidExpression in
  let tlid = TL.id tl in
  let ast = TL.getAST tl in
  match (ast, Option.andThen ast ~f:(E.find id)) with
  | Some ast, Some body ->
      let name = generateFnName () in
      let glob = TL.allGloballyScopedVarnames m.dbs in
      let freeVars =
        AST.freeVariables body
        |> List.filter ~f:(fun (_, v) -> not (List.member ~value:v glob))
      in
      let paramExprs =
        List.map ~f:(fun (_, name_) -> EVariable (gid (), name_)) freeVars
      in
      let replacement = EFnCall (gid (), name, paramExprs, NoRail) in
      let newAST = E.replace ~replacement id ast in
      let astOp = TL.setASTMod tl newAST in
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
        ; ufAST = FluidExpression.clone body }
      in
      Many [RPC ([SetFunction newF], FocusExact (tlid, E.id replacement)); astOp]
  | _ ->
      NoChange


let renameFunction (m : model) (uf : userFunction) (newName : string) : op list
    =
  let fn e =
    match e with
    | EFnCall (id, _, params, r) ->
        EFnCall (id, newName, params, r)
    | _ ->
        e
  in
  transformFnCalls m uf fn


let rec isFunctionInExpr (fnName : string) (expr : expr) : bool =
  let maybeNExpr = B.asF expr in
  match maybeNExpr with
  | None ->
      false
  | Some nExpr ->
    ( match nExpr with
    | FnCall (F (_, name), list, _) ->
        if name = fnName
        then true
        else List.any ~f:(isFunctionInExpr fnName) list
    | FnCall (Blank _, _, _) ->
        recover "blank in fncall" ~debug:nExpr false
    | Constructor (_, args) ->
        List.any ~f:(isFunctionInExpr fnName) args
    | If (ifExpr, thenExpr, elseExpr) ->
        List.any ~f:(isFunctionInExpr fnName) [ifExpr; thenExpr; elseExpr]
    | Variable _ ->
        false
    | Let (_, a, b) ->
        List.any ~f:(isFunctionInExpr fnName) [a; b]
    | Lambda (_, ex) ->
        isFunctionInExpr fnName ex
    | Value _ ->
        false
    | ObjectLiteral li ->
        let valuesMap = List.map ~f:Tuple2.second li in
        List.any ~f:(isFunctionInExpr fnName) valuesMap
    | ListLiteral li ->
        List.any ~f:(isFunctionInExpr fnName) li
    | Thread li ->
        List.any ~f:(isFunctionInExpr fnName) li
    | FieldAccess (ex, _) ->
        isFunctionInExpr fnName ex
    | FeatureFlag (_, cond, a, b) ->
        isFunctionInExpr fnName cond
        || isFunctionInExpr fnName a
        || isFunctionInExpr fnName b
    | Match (matchExpr, cases) ->
        isFunctionInExpr fnName matchExpr
        || List.any
             ~f:(isFunctionInExpr fnName)
             (List.map ~f:Tuple2.second cases)
    | FluidPartial (_, oldExpr) ->
        isFunctionInExpr fnName oldExpr
    | FluidRightPartial (_, oldExpr) ->
        isFunctionInExpr fnName oldExpr )


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
          (Some n, Functions.usesOfTipe n oldTipe.utVersion fn)
    in
    let newName =
      match newTipe.utName with Blank _ -> None | F (_, n) -> Some n
    in
    match (origName, newName) with
    | Some _, Some newName ->
        List.foldr
          ~f:(fun use accfn ->
            Functions.replaceParamTipe use (transformUse newName use) accfn)
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
  let tldata = m |> TL.all |> TD.mapValues ~f:TL.allData in
  let fndata =
    m.userFunctions |> TD.mapValues ~f:(fun fn -> AST.allData fn.ufAST)
  in
  let all = List.concat (fndata @ tldata) in
  let countFromList names =
    List.foldl names ~init:StrDict.empty ~f:(fun name dict ->
        StrDict.update dict ~key:name ~f:(function
            | Some count ->
                Some (count + 1)
            | None ->
                Some 1))
  in
  let usedFns =
    all
    |> List.filterMap ~f:(function
           | PFnCallName (_, name) ->
               Some name
           | _ ->
               None)
    |> countFromList
  in
  let usedDBs =
    all
    |> List.filterMap ~f:(function
           | PExpr (EVariable (_, name)) when String.isCapitalized name ->
               Some name
           | _ ->
               None)
    |> countFromList
  in
  let usedTipes =
    m.userFunctions
    |> TD.mapValues ~f:Functions.allParamData
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


let addFunctionParameter (m : model) (f : userFunction) (currentBlankId : id) :
    modification =
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
  let replacement = Functions.extend f in
  let newCalls = transformOp f in
  RPC
    ( SetFunction replacement :: newCalls
    , FocusNext (f.ufTLID, Some currentBlankId) )


let generateEmptyFunction (_ : unit) : userFunction =
  let funcName = generateFnName () in
  let tlid = gtlid () in
  let params =
    [ { ufpName = F (gid (), "var")
      ; ufpTipe = F (gid (), TAny)
      ; ufpBlock_args = []
      ; ufpOptional = true
      ; ufpDescription = "" } ]
  in
  let metadata =
    { ufmName = F (gid (), funcName)
    ; ufmParameters = params
    ; ufmDescription = ""
    ; ufmReturnTipe = F (gid (), TAny)
    ; ufmInfix = false }
  in
  {ufTLID = tlid; ufMetadata = metadata; ufAST = EBlank (gid ())}


let generateEmptyUserType () : userTipe =
  let tipeName = generateTipeName () in
  let tlid = gtlid () in
  let definition = UTRecord [{urfName = B.new_ (); urfTipe = B.new_ ()}] in
  { utTLID = tlid
  ; utName = F (gid (), tipeName)
  ; utVersion = 0
  ; utDefinition = definition }


let coerceTypes (v : dval) : tipe =
  let isUuid (dstr : dval) : bool =
    match dstr with
    | DStr s ->
        Util.Regex.exactly
          ~re:
            "[a-zA-Z0-9]{8}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{4}-[a-zA-Z0-9]{12}"
          s
    | _ ->
        false
  in
  let isDate (dstr : dval) : bool =
    match dstr with
    | DStr s ->
        let parsedDate = Js.Date.fromString s in
        ( try
            (* toISOString will raise Invalid Date if date
                          * is invalid; bucklescript doesn't expose this
                          * to us otherwise *)
            ignore (Js.Date.toISOString parsedDate) ;
            true
          with _ -> false )
    | _ ->
        false
  in
  let tipe = v |> Runtime.typeOf in
  match tipe with
  | TStr ->
      if isUuid v then TUuid else if isDate v then TDate else TStr
  | _ ->
      tipe


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
               {urfName = k |> Blank.newF; urfTipe = tipe |> Blank.newF})
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
               h.ast |> FluidExpression.renameVariableUses ~oldName ~newName
             in
             if newAST <> h.ast
             then Some (SetHandler (h.hTLID, h.pos, {h with ast = newAST}))
             else None
         | TLFunc f ->
             let newAST =
               f.ufAST |> FluidExpression.renameVariableUses ~oldName ~newName
             in
             if newAST <> f.ufAST
             then Some (SetFunction {f with ufAST = newAST})
             else None
         | TLTipe _ ->
             None
         | TLDB _ ->
             None
         | TLGroup _ ->
             None)
