open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
module TL = Toplevel
open Types

let generateFnName (_ : unit) : string =
  "fn_" ^ (() |> Util.random |> string_of_int)

let convertTipe (tipe : tipe) : tipe =
  match tipe with TIncomplete -> TAny | TError -> TAny | _ -> tipe

type wrapLoc = WLetRHS | WLetBody | WIfCond | WIfThen | WIfElse

let wrap (wl : wrapLoc) (m : model) (tl : toplevel) (p : pointerData) :
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
      let newH = {h with ast= newAst} in
      RPC ([SetHandler (tl.id, tl.pos, newH)], focus)
  | PExpr e, TLFunc f ->
      let newAst, focus = wrapAst e f.ufAST wl in
      let newF = {f with ufAST= newAst} in
      RPC ([SetFunction newF], focus)
  | _ -> NoChange

let toggleOnRail (m : model) (tl : toplevel) (p : pointerData) : modification =
  let new_ =
    match p with
    | PExpr (F (id, FnCall (name, exprs, Rail))) ->
        PExpr (F (id, FnCall (name, exprs, NoRail)))
    | PExpr (F (id, FnCall (name, exprs, NoRail))) ->
        PExpr (F (id, FnCall (name, exprs, Rail)))
    | _ -> p
  in
  if p = new_ then NoChange
  else
    let newtl = TL.replace p new_ tl in
    RPC (TL.toOp newtl, FocusSame)

let extractVariable (m : model) (tl : toplevel) (p : pointerData) :
    modification =
  let extractVarInAst e ast =
    let varname = "var" ^ string_of_int (Util.random ()) in
    let freeVariables =
      AST.freeVariables e |> List.map Tuple.second |> StrSet.fromList
    in
    let ancestors = AST.ancestors (B.toID e) ast in
    let lastPlaceWithSameVarsAndValues =
      ancestors
      |> List.takeWhile (fun elem ->
             let id = B.toID elem in
             let availableVars =
               Analysis.getCurrentAvailableVarnames m tl.id id
               |> StrSet.fromList
             in
             let allRequiredVariablesAvailable =
               StrSet.diff freeVariables availableVars |> StrSet.isEmpty
             in
             let noVariablesAreRedefined =
               freeVariables |> StrSet.toList
               |> List.all (not << fun v -> AST.isDefinitionOf v elem)
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
        let newAST =
          AST.replace (PExpr e) (PExpr (B.newF (Variable varname))) ast
        in
        (B.newF (Let (newVar, e, newAST)), B.toID newVar)
  in
  match (p, tl.data) with
  | PExpr e, TLHandler h ->
      let newAst, enterTarget = extractVarInAst e h.ast in
      let newHandler = {h with ast= newAst} in
      Many
        [ RPC ([SetHandler (tl.id, tl.pos, newHandler)], FocusNoChange)
        ; Enter (Filling (tl.id, enterTarget)) ]
  | PExpr e, TLFunc f ->
      let newAst, enterTarget = extractVarInAst e f.ufAST in
      let newF = {f with ufAST= newAst} in
      Many
        [ RPC ([SetFunction newF], FocusNoChange)
        ; Enter (Filling (tl.id, enterTarget)) ]
  | _ -> NoChange

let extractFunction (m : model) (tl : toplevel) (p : pointerData) :
    modification =
  if not (TL.isValidID tl (P.toID p)) then NoChange
  else
    match p with
    | PExpr body ->
        let pred = TL.getPrevBlank tl (Some p) |> Option.map P.toID in
        let name = generateFnName () in
        let freeVars = AST.freeVariables body in
        let paramExprs =
          List.map (fun (_, name_) -> F (gid (), Variable name_)) freeVars
        in
        let replacement =
          PExpr (F (gid (), FnCall (name, paramExprs, NoRail)))
        in
        let h =
          deOption "PointerData is a PExpr and isValidID for this TL"
            (TL.asHandler tl)
        in
        let newAst = AST.replace p replacement h.ast in
        let newH = {h with ast= newAst} in
        let params =
          List.map
            (fun (id, name_) ->
              let tipe =
                Analysis.getCurrentTipeOf m tl.id id
                |> Option.withDefault TAny |> convertTipe
              in
              { ufpName= F (gid (), name_)
              ; ufpTipe= F (gid (), tipe)
              ; ufpBlock_args= []
              ; ufpOptional= false
              ; ufpDescription= "" } )
            freeVars
        in
        let metadata =
          { ufmName= F (gid (), name)
          ; ufmParameters= params
          ; ufmDescription= ""
          ; ufmReturnTipe= F (gid (), TAny)
          ; ufmInfix= false }
        in
        let newF =
          {ufTLID= gtlid (); ufMetadata= metadata; ufAST= AST.clone body}
        in
        RPC
          ( [SetFunction newF; SetHandler (tl.id, tl.pos, newH)]
          , FocusExact (tl.id, P.toID replacement) )
    | _ -> NoChange

let renameFunction (m : model) (old : userFunction) (new_ : userFunction) :
    op list =
  let renameFnCalls ast old_ new_ =
    let transformCall newName_ oldCall =
      let transformExpr name oldExpr =
        match oldExpr with
        | F (id, FnCall (_, params, r)) -> F (id, FnCall (name, params, r))
        | _ -> oldExpr
      in
      match oldCall with
      | PExpr e -> PExpr (transformExpr newName_ e)
      | _ -> oldCall
    in
    let origName, calls =
      match old_.ufMetadata.ufmName with
      | Blank _ -> (None, [])
      | F (_, n) ->
          (Some n, AST.allCallsToFn n ast |> List.map (fun x -> PExpr x))
    in
    let newName =
      match new_.ufMetadata.ufmName with Blank _ -> None | F (_, n) -> Some n
    in
    match (origName, newName) with
    | Some o, Some r ->
        List.foldr
          (fun call acc -> AST.replace call (transformCall r call) acc)
          ast calls
    | _ -> ast
  in
  let newHandlers =
    m.toplevels
    |> List.filterMap (fun tl ->
           match TL.asHandler tl with
           | None -> None
           | Some h ->
               let newAst = renameFnCalls h.ast old new_ in
               if newAst <> h.ast then
                 Some (SetHandler (tl.id, tl.pos, {h with ast= newAst}))
               else None )
  in
  let newFunctions =
    m.userFunctions
    |> List.filterMap (fun uf ->
           let newAst = renameFnCalls uf.ufAST old new_ in
           if newAst <> uf.ufAST then
             Some (SetFunction {uf with ufAST= newAst})
           else None )
  in
  newHandlers @ newFunctions

let rec isFunctionInExpr (fnName : string) (expr : expr) : bool =
  let maybeNExpr = B.asF expr in
  match maybeNExpr with
  | None -> false
  | Some nExpr -> (
    match nExpr with
    | FnCall (name, list, _) ->
        if name = fnName then true else List.any (isFunctionInExpr fnName) list
    | If (ifExpr, thenExpr, elseExpr) ->
        List.any (isFunctionInExpr fnName) [ifExpr; thenExpr; elseExpr]
    | Variable _ -> false
    | Let (_, a, b) -> List.any (isFunctionInExpr fnName) [a; b]
    | Lambda (_, ex) -> isFunctionInExpr fnName ex
    | Value _ -> false
    | ObjectLiteral li ->
        let valuesMap = List.map Tuple.second li in
        List.any (isFunctionInExpr fnName) valuesMap
    | ListLiteral li -> List.any (isFunctionInExpr fnName) li
    | Thread li -> List.any (isFunctionInExpr fnName) li
    | FieldAccess (ex, filed) -> isFunctionInExpr fnName ex
    | FeatureFlag (_, cond, a, b) ->
        isFunctionInExpr fnName cond
        || isFunctionInExpr fnName a || isFunctionInExpr fnName b )

let countFnUsage (m : model) (name : string) : int =
  let usedIn =
    TL.all m
    |> List.filter (fun tl ->
           match tl.data with
           | TLHandler h -> isFunctionInExpr name h.ast
           | TLDB _ -> false
           | TLFunc f -> isFunctionInExpr name f.ufAST )
  in
  List.length usedIn

let unusedDeprecatedFunctions (m : model) : StrSet.t =
  m.builtInFunctions
  |> List.filter (fun x -> x.fnDeprecated)
  |> List.map (fun x -> x.fnName)
  |> List.filter (fun n -> countFnUsage m n = 0)
  |> StrSet.fromList

let transformFnCalls (m : model) (uf : userFunction) (f : nExpr -> nExpr) :
    op list =
  let transformCallsInAst f_ ast old =
    let transformCall old_ =
      let transformExpr oldExpr =
        match oldExpr with
        | F (id, FnCall (name, params, r)) ->
            F (id, f_ (FnCall (name, params, r)))
        | _ -> oldExpr
      in
      match old_ with PExpr e -> PExpr (transformExpr e) | _ -> old_
    in
    let origName, calls =
      match old.ufMetadata.ufmName with
      | Blank _ -> (None, [])
      | F (_, n) ->
          (Some n, AST.allCallsToFn n ast |> List.map (fun x -> PExpr x))
    in
    match origName with
    | Some _ ->
        List.foldr
          (fun call acc -> AST.replace call (transformCall call) acc)
          ast calls
    | _ -> ast
  in
  let newHandlers =
    m.toplevels
    |> List.filterMap (fun tl ->
           match TL.asHandler tl with
           | None -> None
           | Some h ->
               let newAst = transformCallsInAst f h.ast uf in
               if newAst <> h.ast then
                 Some (SetHandler (tl.id, tl.pos, {h with ast= newAst}))
               else None )
  in
  let newFunctions =
    m.userFunctions
    |> List.filterMap (fun uf_ ->
           let newAst = transformCallsInAst f uf_.ufAST uf_ in
           if newAst <> uf_.ufAST then
             Some (SetFunction {uf_ with ufAST= newAst})
           else None )
  in
  newHandlers @ newFunctions

let addNewFunctionParameter (m : model) (old : userFunction) : op list =
  let fn e =
    match e with
    | FnCall (name, params, r) -> FnCall (name, params @ [B.new_ ()], r)
    | _ -> e
  in
  transformFnCalls m old fn

let removeFunctionParameter (m : model) (uf : userFunction)
    (ufp : userFunctionParameter) : op list =
  let indexInList =
    List.findIndex (fun p -> p = ufp) uf.ufMetadata.ufmParameters
    |> deOption "tried to remove parameter that does not exist in function"
  in
  let fn e =
    match e with
    | FnCall (name, params, r) ->
        FnCall (name, List.removeAt indexInList params, r)
    | _ -> e
  in
  transformFnCalls m uf fn

let generateEmptyFunction (_ : unit) : userFunction =
  let funcName = generateFnName () in
  let tlid = gtlid () in
  let params =
    [ { ufpName= F (gid (), "var")
      ; ufpTipe= F (gid (), TAny)
      ; ufpBlock_args= []
      ; ufpOptional= true
      ; ufpDescription= "" } ]
  in
  let metadata =
    { ufmName= F (gid (), funcName)
    ; ufmParameters= params
    ; ufmDescription= ""
    ; ufmReturnTipe= F (gid (), TAny)
    ; ufmInfix= false }
  in
  UserFunction (tlid, metadata, Blank (gid ()))
