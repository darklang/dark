open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel

let generateFnName (_ : unit) : string =
  "fn_" ^ (() |> Util.random |> string_of_int)


let generateTipeName () : string =
  "Type_" ^ (() |> Util.random |> string_of_int)


let convertTipe (tipe : tipe) : tipe =
  match tipe with TIncomplete -> TAny | TError -> TAny | _ -> tipe


type wrapLoc =
  | WLetRHS
  | WLetBody
  | WIfCond
  | WIfThen
  | WIfElse

let wrap (wl : wrapLoc) (_ : model) (tl : toplevel) (p : pointerData) :
    modification =
  let wrapAst e ast wl_ =
    let replacement, focus =
      match wl_ with
      | WLetRHS ->
          let lhs = B.new_ () in
          let replacement_ = PExpr (B.newF (Let (lhs, e, B.new_ ()))) in
          (replacement_, FocusExact (tl.id, B.toID lhs))
      | WLetBody ->
          let lhs = B.new_ () in
          let replacement_ = PExpr (B.newF (Let (lhs, B.new_ (), e))) in
          (replacement_, FocusExact (tl.id, B.toID lhs))
      | WIfCond ->
          let thenBlank = B.new_ () in
          let replacement_ = PExpr (B.newF (If (e, thenBlank, B.new_ ()))) in
          (replacement_, FocusExact (tl.id, B.toID thenBlank))
      | WIfThen ->
          let condBlank = B.new_ () in
          let replacement_ = PExpr (B.newF (If (condBlank, e, B.new_ ()))) in
          (replacement_, FocusExact (tl.id, B.toID condBlank))
      | WIfElse ->
          let condBlank = B.new_ () in
          let replacement_ = PExpr (B.newF (If (condBlank, B.new_ (), e))) in
          (replacement_, FocusExact (tl.id, B.toID condBlank))
    in
    (AST.replace (PExpr e) replacement ast, focus)
  in
  match (p, tl.data) with
  | PExpr e, TLHandler h ->
      let newAst, focus = wrapAst e h.ast wl in
      let newH = {h with ast = newAst} in
      RPC ([SetHandler (tl.id, tl.pos, newH)], focus)
  | PExpr e, TLFunc f ->
      let newAst, focus = wrapAst e f.ufAST wl in
      let newF = {f with ufAST = newAst} in
      RPC ([SetFunction newF], focus)
  | _ ->
      NoChange


let takeOffRail (_m : model) (tl : toplevel) (p : pointerData) : modification =
  let new_ =
    match p with
    | PExpr (F (id, FnCall (name, exprs, Rail))) ->
        PExpr (F (id, FnCall (name, exprs, NoRail)))
    | _ ->
        p
  in
  if p = new_
  then NoChange
  else
    let newtl = TL.replace p new_ tl in
    RPC (TL.toOp newtl, FocusSame)


let putOnRail (m : model) (tl : toplevel) (p : pointerData) : modification =
  let new_ =
    match p with
    | PExpr (F (id, FnCall (F (nid, name), exprs, NoRail))) ->
        (* We don't want to use m.complete.functions as the autocomplete
          * filters out deprecated functions *)
        let allFunctions =
          let ufs =
            m.userFunctions
            |> List.map ~f:(fun uf -> uf.ufMetadata)
            |> List.filterMap ~f:Functions.ufmToF
          in
          m.builtInFunctions @ ufs
        in
        (* Only toggle onto rail iff. return tipe is TOption or TResult *)
        List.find ~f:(fun fn -> fn.fnName = name) allFunctions
        |> Option.map ~f:(fun fn -> fn.fnReturnTipe)
        |> Option.map ~f:(fun t ->
               if t = TOption || t = TResult
               then PExpr (F (id, FnCall (F (nid, name), exprs, Rail)))
               else p )
        |> Option.withDefault ~default:p
    | _ ->
        p
  in
  if p = new_
  then NoChange
  else
    let newtl = TL.replace p new_ tl in
    RPC (TL.toOp newtl, FocusSame)


let extractVariable (m : model) (tl : toplevel) (p : pointerData) :
    modification =
  let extractVarInAst e ast =
    let varname = "var" ^ string_of_int (Util.random ()) in
    let freeVariables =
      AST.freeVariables e |> List.map ~f:Tuple2.second |> StrSet.fromList
    in
    let ancestors = AST.ancestors (B.toID e) ast in
    let lastPlaceWithSameVarsAndValues =
      ancestors
      |> List.takeWhile ~f:(fun elem ->
             let id = B.toID elem in
             let availableVars =
               Analysis.getCurrentAvailableVarnames m tl id |> StrSet.fromList
             in
             let allRequiredVariablesAvailable =
               StrSet.diff freeVariables availableVars |> StrSet.isEmpty
             in
             let noVariablesAreRedefined =
               freeVariables
               |> StrSet.toList
               |> List.all ~f:(not << fun v -> AST.isDefinitionOf v elem)
             in
             allRequiredVariablesAvailable && noVariablesAreRedefined )
      |> List.last
    in
    let newVar = B.newF varname in
    match lastPlaceWithSameVarsAndValues with
    | Some p ->
        let nbody =
          AST.replace (PExpr e) (PExpr (B.newF (Variable varname))) p
        in
        let nlet = B.newF (Let (newVar, e, nbody)) in
        (AST.replace (PExpr p) (PExpr nlet) ast, B.toID newVar)
    | None ->
        (* something weird is happening because we couldn't find anywhere to *)
        (* extract to, we can just wrap the entire AST in a Let *)
        let newAST =
          AST.replace (PExpr e) (PExpr (B.newF (Variable varname))) ast
        in
        (B.newF (Let (newVar, e, newAST)), B.toID newVar)
  in
  match (p, tl.data) with
  | PExpr e, TLHandler h ->
      let newAst, enterTarget = extractVarInAst e h.ast in
      let newHandler = {h with ast = newAst} in
      Many
        [ RPC ([SetHandler (tl.id, tl.pos, newHandler)], FocusNoChange)
        ; Enter (Filling (tl.id, enterTarget)) ]
  | PExpr e, TLFunc f ->
      let newAst, enterTarget = extractVarInAst e f.ufAST in
      let newF = {f with ufAST = newAst} in
      Many
        [ RPC ([SetFunction newF], FocusNoChange)
        ; Enter (Filling (tl.id, enterTarget)) ]
  | _ ->
      NoChange


let extractFunction (m : model) (tl : toplevel) (p : pointerData) :
    modification =
  if not (TL.isValidID tl (P.toID p))
  then NoChange
  else
    match p with
    | PExpr body ->
        let name = generateFnName () in
        let glob = TL.allGloballyScopedVarnames m.toplevels in
        let freeVars =
          AST.freeVariables body
          |> List.filter ~f:(fun (_id, v) -> not (List.member ~value:v glob))
        in
        let paramExprs =
          List.map ~f:(fun (_, name_) -> F (gid (), Variable name_)) freeVars
        in
        let replacement =
          let (ID id) = gid () in
          let nameid = id ^ "_name" in
          PExpr (F (ID id, FnCall (F (ID nameid, name), paramExprs, NoRail)))
        in
        let astOp = TL.replaceOp p replacement tl in
        let params =
          List.map
            ~f:(fun (id, name_) ->
              let tipe =
                Analysis.getCurrentTipeOf m tl.id id
                |> Option.withDefault ~default:TAny
                |> convertTipe
              in
              { ufpName = F (gid (), name_)
              ; ufpTipe = F (gid (), tipe)
              ; ufpBlock_args = []
              ; ufpOptional = false
              ; ufpDescription = "" } )
            freeVars
        in
        let metadata =
          { ufmName = F (gid (), name)
          ; ufmParameters = params
          ; ufmDescription = ""
          ; ufmReturnTipe = F (gid (), TAny)
          ; ufmInfix = false }
        in
        let newF =
          {ufTLID = gtlid (); ufMetadata = metadata; ufAST = AST.clone body}
        in
        RPC ([SetFunction newF] @ astOp, FocusExact (tl.id, P.toID replacement))
    | _ ->
        NoChange


let renameFunction (m : model) (old : userFunction) (new_ : userFunction) :
    op list =
  let renameFnCalls ast old_ new_ =
    let transformCall newName_ oldCall =
      let transformExpr name oldExpr =
        match oldExpr with
        | F (ID id, FnCall (_, params, r)) ->
            F (ID id, FnCall (F (ID (id ^ "_name"), name), params, r))
        | _ ->
            oldExpr
      in
      match oldCall with
      | PExpr e ->
          PExpr (transformExpr newName_ e)
      | _ ->
          oldCall
    in
    let origName, calls =
      match old_.ufMetadata.ufmName with
      | Blank _ ->
          (None, [])
      | F (_, n) ->
          (Some n, AST.allCallsToFn n ast |> List.map ~f:(fun x -> PExpr x))
    in
    let newName =
      match new_.ufMetadata.ufmName with Blank _ -> None | F (_, n) -> Some n
    in
    match (origName, newName) with
    | Some _, Some r ->
        List.foldr
          ~f:(fun call acc -> AST.replace call (transformCall r call) acc)
          ~init:ast
          calls
    | _ ->
        ast
  in
  let newHandlers =
    m.toplevels
    |> List.filterMap ~f:(fun tl ->
           match TL.asHandler tl with
           | None ->
               None
           | Some h ->
               let newAst = renameFnCalls h.ast old new_ in
               if newAst <> h.ast
               then Some (SetHandler (tl.id, tl.pos, {h with ast = newAst}))
               else None )
  in
  let newFunctions =
    m.userFunctions
    |> List.filterMap ~f:(fun uf ->
           let newAst = renameFnCalls uf.ufAST old new_ in
           if newAst <> uf.ufAST
           then Some (SetFunction {uf with ufAST = newAst})
           else None )
  in
  newHandlers @ newFunctions


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
        Debug.crash "blank in fncall"
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
            Functions.replaceParamTipe use (transformUse newName use) accfn )
          ~init:fn
          uses
    | _ ->
        fn
  in
  let newFunctions =
    m.userFunctions
    |> List.filterMap ~f:(fun uf ->
           let newFn = renameUserTipeInFnParameters uf old new_ in
           if newFn <> uf then Some (SetFunction newFn) else None )
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
  let tldata = m.toplevels |> List.map ~f:TL.allData in
  let fndata =
    m.userFunctions |> List.map ~f:(fun fn -> AST.allData fn.ufAST)
  in
  let all = List.concat (fndata @ tldata) in
  let countFromList names =
    List.foldl names ~init:StrDict.empty ~f:(fun name dict ->
        StrDict.update dict ~key:name ~f:(function
            | Some count ->
                Some (count + 1)
            | None ->
                Some 1 ) )
  in
  let usedFns =
    all
    |> List.filterMap ~f:(function
           | PFnCallName (F (_, name)) ->
               Some name
           | _ ->
               None )
    |> countFromList
  in
  let usedDBs =
    all
    |> List.filterMap ~f:(function
           | PExpr (F (_, Variable name)) when String.isCapitalized name ->
               Some name
           | _ ->
               None )
    |> countFromList
  in
  let usedTipes =
    m.userFunctions
    |> List.map ~f:Functions.allParamData
    |> List.concat
    |> List.filterMap ~f:(function
           (* Note: this does _not_ currently handle multiple versions *)
           | PParamTipe (F (_, TUserType (name, _))) ->
               Some name
           | _ ->
               None )
    |> countFromList
  in
  {m with usedDBs; usedFns; usedTipes}


let transformFnCalls (m : model) (uf : userFunction) (f : nExpr -> nExpr) :
    op list =
  let transformCallsInAst f_ ast old =
    let transformCall old_ =
      let transformExpr oldExpr =
        match oldExpr with
        | F (id, FnCall (name, params, r)) ->
            F (id, f_ (FnCall (name, params, r)))
        | _ ->
            oldExpr
      in
      match old_ with PExpr e -> PExpr (transformExpr e) | _ -> old_
    in
    let origName, calls =
      match old.ufMetadata.ufmName with
      | Blank _ ->
          (None, [])
      | F (_, n) ->
          (Some n, AST.allCallsToFn n ast |> List.map ~f:(fun x -> PExpr x))
    in
    match origName with
    | Some _ ->
        List.foldr
          ~f:(fun call acc -> AST.replace call (transformCall call) acc)
          ~init:ast
          calls
    | _ ->
        ast
  in
  let newHandlers =
    m.toplevels
    |> List.filterMap ~f:(fun tl ->
           match TL.asHandler tl with
           | None ->
               None
           | Some h ->
               let newAst = transformCallsInAst f h.ast uf in
               if newAst <> h.ast
               then Some (SetHandler (tl.id, tl.pos, {h with ast = newAst}))
               else None )
  in
  let newFunctions =
    m.userFunctions
    |> List.filterMap ~f:(fun uf_ ->
           let newAst = transformCallsInAst f uf_.ufAST uf_ in
           if newAst <> uf_.ufAST
           then Some (SetFunction {uf_ with ufAST = newAst})
           else None )
  in
  newHandlers @ newFunctions


let addNewFunctionParameter (m : model) (old : userFunction) : op list =
  let fn e =
    match e with
    | FnCall (name, params, r) ->
        FnCall (name, params @ [B.new_ ()], r)
    | _ ->
        e
  in
  transformFnCalls m old fn


let removeFunctionParameter
    (m : model) (uf : userFunction) (ufp : userFunctionParameter) : op list =
  let indexInList =
    List.findIndex ~f:(fun p -> p = ufp) uf.ufMetadata.ufmParameters
    |> deOption "tried to remove parameter that does not exist in function"
  in
  let fn e =
    match e with
    | FnCall (name, params, r) ->
        FnCall (name, List.removeAt ~index:indexInList params, r)
    | _ ->
        e
  in
  transformFnCalls m uf fn


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
  {ufTLID = tlid; ufMetadata = metadata; ufAST = Blank (gid ())}


let generateEmptyUserType () : userTipe =
  let tipeName = generateTipeName () in
  let tlid = gtlid () in
  let definition = UTRecord [{urfName = B.new_ (); urfTipe = B.new_ ()}] in
  { utTLID = tlid
  ; utName = F (gid (), tipeName)
  ; utVersion = 0
  ; utDefinition = definition }


let renameDBReferences (m : model) (oldName : dBName) (newName : dBName) :
    op list =
  let newPd () = PExpr (B.newF (Variable newName)) in
  let transform ast =
    let usedInExprs = AST.uses oldName ast in
    List.foldr
      ~f:(fun pd newAST -> AST.replace (PExpr pd) (newPd ()) newAST)
      ~init:ast
      usedInExprs
  in
  let toplevels = m.toplevels @ List.map ~f:TL.ufToTL m.userFunctions in
  toplevels
  |> List.filterMap ~f:(fun tl ->
         match tl.data with
         | TLHandler h ->
             let newAST = transform h.ast in
             if newAST <> h.ast
             then Some (SetHandler (tl.id, tl.pos, {h with ast = newAST}))
             else None
         | TLFunc f ->
             let newAST = transform f.ufAST in
             if newAST <> f.ufAST
             then Some (SetFunction {f with ufAST = newAST})
             else None
         | TLTipe _ ->
             None
         | TLDB _ ->
             None )
