open Tea
open! Porting
module B = Blank
module P = Pointer
open Prelude
open Types

let traverse fn expr =
  match expr with
  | Blank _ -> expr
  | F (id, nexpr) ->
      F
        ( id
        , match nexpr with
          | Value _ -> nexpr
          | Variable _ -> nexpr
          | Let (lhs, rhs, body) -> Let (lhs, fn rhs, fn body)
          | If (cond, ifbody, elsebody) -> If (fn cond, fn ifbody, fn elsebody)
          | FnCall (name, exprs, r) -> FnCall (name, List.map fn exprs, r)
          | Lambda (vars, lexpr) -> Lambda (vars, fn lexpr)
          | Thread exprs -> Thread (List.map fn exprs)
          | FieldAccess (obj, field) -> FieldAccess (fn obj, field)
          | ObjectLiteral pairs ->
              pairs |> List.map (fun (k, v) -> (k, fn v)) |> ObjectLiteral
          | ListLiteral elems -> ListLiteral (List.map fn elems)
          | FeatureFlag (msg, cond, a, b) ->
              FeatureFlag (msg, fn cond, fn a, fn b) )

let listThreadBlanks expr =
  let r = listThreadBlanks in
  let _ = "type annotation" in
  let rList exprs = exprs |> List.map listThreadBlanks |> List.concat in
  let rn nexpr =
    match nexpr with
    | Value v -> []
    | Variable name -> []
    | Let (lhs, rhs, body) -> r rhs ^ r body
    | FnCall (name, exprs, _) -> rList exprs
    | Lambda (vars, body) -> r body
    | FieldAccess (obj, _) -> r obj
    | If (cond, ifbody, elsebody) -> (r cond ^ r ifbody) ^ r elsebody
    | Thread exprs ->
        let blanks, filled = List.partition B.isBlank exprs in
        let blankids = List.map B.toID blanks in
        let subExprsBlankids = rList filled in
        blankids ^ subExprsBlankids
    | ObjectLiteral pairs -> pairs |> List.map Tuple.second |> rList
    | ListLiteral exprs -> rList exprs
    | FeatureFlag (_, cond, a, b) -> (r cond ^ r a) ^ r b
  in
  match expr with Blank _ -> [] | F (_, f) -> rn f

let closeThreads expr =
  match expr with
  | F (id, Thread exprs) -> (
      let addBlank =
        match exprs with
        | [] -> false
        | [e] -> false
        | [_; F (_, FnCall (_, _, _))] -> false
        | _ -> true
      in
      let newExprs = List.filter B.isF exprs |> List.map closeThreads in
      let adjusted =
        match newExprs with
        | [rest; F (id_, FnCall (name, args, r))] ->
            if addBlank then
              [F (id_, FnCall (name, B.new_ () :: args, r))] ^ rest
            else [F (id_, FnCall (name, args, r))] ^ rest
        | _ -> newExprs
      in
      match adjusted with
      | [] -> Blank id
      | [e] -> e
      | _ -> F (id, Thread adjusted) )
  | _ -> traverse closeThreads expr

let closeObjectLiterals expr =
  match expr with
  | F (id, ObjectLiteral pairs) ->
      pairs
      |> List.filterMap (fun (k, v) ->
             if B.isBlank k && B.isBlank v then None
             else Some (k, closeObjectLiterals v) )
      |> (fun l -> if l <> [] then l else [(B.new_ (), B.new_ ())])
      |> ObjectLiteral |> F id
  | _ -> traverse closeObjectLiterals expr

let closeListLiterals expr =
  match expr with
  | F (id, ListLiteral exprs) ->
      let exprs2 = List.map closeListLiterals exprs in
      let exprs3 = List.filter B.isF exprs2 in
      F (id, ListLiteral (exprs3 ^ [B.new_ ()]))
  | _ -> traverse closeObjectLiterals expr

let closeBlanks expr =
  expr |> closeThreads |> closeObjectLiterals |> closeListLiterals

let addThreadBlank id blank expr =
  let atb = addThreadBlank id blank in
  if id = B.toID expr then
    match expr with
    | F (tid, Thread exprs) -> F (tid, Thread (exprs ^ [blank]))
    | _ -> B.newF (Thread [expr; blank])
  else
    match expr with
    | F (tid, Thread exprs) ->
        let replaced = extendThreadChild id blank exprs in
        if replaced = exprs then traverse atb expr else F (tid, Thread replaced)
    | _ -> traverse atb expr

let addLambdaBlank id expr =
  match parentOf_ id expr with
  | Some (F (lid, Lambda (vars, body))) as old ->
      let r = F (lid, Lambda (vars ^ [B.new_ ()], body)) in
      replace (old |> Option.getExn "impossible" |> PExpr) (PExpr r) expr
  | _ -> expr

let addObjectLiteralBlanks id expr =
  match findExn id expr with
  | PKey key -> (
    match parentOf id expr with
    | F (olid, ObjectLiteral pairs) as old ->
        let newKey = B.new_ () in
        let newExpr = B.new_ () in
        let newPairs = pairs ^ [(newKey, newExpr)] in
        let new_ = F (olid, ObjectLiteral newPairs) in
        let replacement = replace (PExpr old) (PExpr new_) expr in
        (B.toID newKey, B.toID newExpr, replacement)
    | _ -> impossible ("key parent must be object", id, expr) )
  | _ -> impossible ("must add to key", id, expr)

let maybeExtendObjectLiteralAt pd expr =
  let id = P.toID pd in
  match pd with
  | PKey key -> (
    match parentOf id expr with
    | F (olid, ObjectLiteral pairs) ->
        if pairs |> List.last |> Option.map Tuple.first |> ( = ) (Some key)
        then
          let _, _, replacement = addObjectLiteralBlanks id expr in
          replacement
        else expr
    | _ -> expr )
  | _ -> expr

let addListLiteralBlanks id expr =
  let new1 = B.new_ () in
  let new2 = B.new_ () in
  let parent = parentOf id expr in
  match parent with
  | F (lid, ListLiteral exprs) ->
      let newExprs =
        exprs |> List.reverse |> List.dropWhile B.isBlank
        |> ( ^ ) [new1]
        |> List.reverse
      in
      replace (PExpr parent) (PExpr (F (lid, ListLiteral newExprs))) expr
  | _ -> expr

let maybeExtendListLiteralAt pd expr =
  let id = P.toID pd in
  match parentOf_ id expr with
  | Some (F (lid, ListLiteral exprs)) ->
      if exprs |> List.filter B.isBlank |> List.length |> fun l -> l <= 1 then
        let replacement = addListLiteralBlanks id expr in
        replacement
      else expr
  | _ -> expr

let wrapInThread id expr =
  if B.toID expr = id then
    match expr with
    | F (_, Thread _) -> expr
    | F (_, _) -> B.newF (Thread [expr; B.new_ ()])
    | Blank _ -> B.newF (Thread [expr])
  else traverse (wrapInThread id) expr

let extendThreadChild at blank threadExprs =
  List.foldr
    (fun e list -> if B.toID e = at then (e :: blank) :: list else e :: list)
    [] threadExprs

let maybeExtendThreadAt id blank expr =
  match expr with
  | F (tid, Thread exprs) ->
      let newExprs =
        extendThreadChild id blank exprs
        |> List.map (maybeExtendThreadAt id blank)
      in
      F (tid, Thread newExprs)
  | _ -> traverse (maybeExtendThreadAt id blank) expr

let isThreadBlank expr p = expr |> listThreadBlanks |> List.member p

let grandparentIsThread expr parent =
  parent
  |> Option.map (fun p ->
         match parentOf_ (B.toID p) expr with
         | Some (F (_, Thread ts)) ->
             ts |> List.head
             |> Option.map (( <> ) p)
             |> Option.withDefault true
         | _ -> false )
  |> Option.withDefault false

let getParamIndex expr id =
  let parent = parentOf_ id expr in
  let inThread = grandparentIsThread expr parent in
  match parent with
  | Some (F (_, FnCall (name, args, _))) ->
      args
      |> List.findIndex (fun a -> B.toID a = id)
      |> Option.map (fun i -> if inThread then (name, i + 1) else (name, i))
  | _ -> None

let threadPrevious id ast =
  let parent = parentOf_ id ast in
  match parent with
  | Some (F (_, Thread exprs)) ->
      exprs
      |> List.filter (fun e -> B.toID e = id)
      |> List.head
      |> Option.andThen (fun this -> Util.listPrevious this exprs)
  | _ -> None

let children expr =
  match expr with
  | Blank _ -> []
  | F (_, nexpr) -> (
    match nexpr with
    | Value _ -> []
    | Variable _ -> []
    | If (cond, ifbody, elsebody) -> [PExpr cond; PExpr ifbody; PExpr elsebody]
    | FnCall (name, exprs, _) -> List.map PExpr exprs
    | Lambda (vars, lexpr) -> List.map PVarBind vars ^ [PExpr lexpr]
    | Thread exprs -> List.map PExpr exprs
    | FieldAccess (obj, field) -> [PExpr obj; PField field]
    | Let (lhs, rhs, body) -> [PVarBind lhs; PExpr rhs; PExpr body]
    | ObjectLiteral pairs ->
        pairs |> List.map (fun (k, v) -> [PKey k; PExpr v]) |> List.concat
    | ListLiteral elems -> List.map PExpr elems
    | FeatureFlag (msg, cond, a, b) ->
        [PFFMsg msg; PExpr cond; PExpr a; PExpr b] )

let childrenOf pid expr =
  let co = childrenOf pid in
  if pid = B.toID expr then children expr
  else
    match expr with
    | Blank _ -> []
    | F (_, nexpr) -> (
      match nexpr with
      | Value _ -> []
      | Variable _ -> []
      | Let (lhs, rhs, body) -> co body ^ co rhs
      | If (cond, ifbody, elsebody) -> (co cond ^ co ifbody) ^ co elsebody
      | FnCall (name, exprs, _) -> List.map co exprs |> List.concat
      | Lambda (vars, lexpr) -> co lexpr
      | Thread exprs -> List.map co exprs |> List.concat
      | FieldAccess (obj, field) -> co obj
      | ObjectLiteral pairs ->
          pairs |> List.map Tuple.second |> List.map co |> List.concat
      | ListLiteral pairs -> pairs |> List.map co |> List.concat
      | FeatureFlag (msg, cond, a, b) -> (co cond ^ co a) ^ co b )

let uses var expr =
  let is_rebinding newbind =
    match newbind with
    | Blank _ -> false
    | F (_, potential) -> if potential = var then true else false
  in
  let u = uses var in
  match expr with
  | Blank _ -> []
  | F (_, nexpr) -> (
    match nexpr with
    | Value _ -> []
    | Variable potential -> if potential = var then [expr] else []
    | Let (lhs, rhs, body) ->
        if is_rebinding lhs then [] else List.concat [u rhs; u body]
    | If (cond, ifbody, elsebody) -> List.concat [u cond; u ifbody; u elsebody]
    | FnCall (name, exprs, _) -> exprs |> List.map u |> List.concat
    | Lambda (vars, lexpr) ->
        if List.any is_rebinding vars then [] else u lexpr
    | Thread exprs -> exprs |> List.map u |> List.concat
    | FieldAccess (obj, field) -> u obj
    | ListLiteral exprs -> exprs |> List.map u |> List.concat
    | ObjectLiteral pairs ->
        pairs |> List.map Tuple.second |> List.map u |> List.concat
    | FeatureFlag (msg, cond, a, b) -> List.concat [u cond; u a; u b] )

let allCallsToFn s e =
  e |> allData
  |> List.filterMap (fun pd ->
         match pd with
         | PExpr (F (id, FnCall (name, params, r))) ->
             if name = s then Some (F (id, FnCall (name, params, r))) else None
         | _ -> None )

let usesRail ast =
  List.any
    (fun e ->
      match e with PExpr (F (_, FnCall (_, _, Rail))) -> true | _ -> false )
    (allData ast)

let ancestors id expr =
  let _ = "type annotation" in
  let rec_ancestors tofind walk exp =
    let rec_ id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_) in
    let reclist id_ e_ walk_ exprs =
      exprs |> List.map (rec_ id_ e_ walk_) |> List.concat
    in
    if B.toID exp = tofind then walk
    else
      match exp with
      | Blank _ -> []
      | F (i, nexpr) -> (
        match nexpr with
        | Value _ -> []
        | Variable _ -> []
        | Let (lhs, rhs, body) -> reclist id exp walk [rhs; body]
        | If (cond, ifbody, elsebody) ->
            reclist id exp walk [cond; ifbody; elsebody]
        | FnCall (name, exprs, _) -> reclist id exp walk exprs
        | Lambda (vars, lexpr) -> rec_ id exp walk lexpr
        | Thread exprs -> reclist id exp walk exprs
        | FieldAccess (obj, field) -> rec_ id exp walk obj
        | ListLiteral exprs -> reclist id expr walk exprs
        | ObjectLiteral pairs ->
            pairs |> List.map Tuple.second |> reclist id expr walk
        | FeatureFlag (msg, cond, a, b) -> reclist id exp walk [cond; a; b] )
  in
  rec_ancestors id [] expr

let ancestorsWhere id expr fn = List.filter fn (ancestors id expr)

let threadAncestors id expr =
  ancestorsWhere id expr (fun e ->
      match e with F (_, Thread _) -> true | _ -> false )

let parentOf id ast = Option.getExn "parentOf" <| parentOf_ id ast

let parentOf_ eid expr =
  let po = parentOf_ eid in
  let _ = "comment" in
  let poList xs = xs |> List.map po |> List.filterMap identity |> List.head in
  if List.member eid (children expr |> List.map P.toID) then Some expr
  else
    match expr with
    | Blank _ -> None
    | F (id, nexpr) -> (
      match nexpr with
      | Value _ -> None
      | Variable _ -> None
      | Let (lhs, rhs, body) -> poList [rhs; body]
      | If (cond, ifbody, elsebody) -> poList [cond; ifbody; elsebody]
      | FnCall (name, exprs, _) -> poList exprs
      | Lambda (vars, lexpr) -> po lexpr
      | Thread exprs -> poList exprs
      | FieldAccess (obj, field) -> po obj
      | ListLiteral exprs -> poList exprs
      | ObjectLiteral pairs -> pairs |> List.map Tuple.second |> poList
      | FeatureFlag (msg, cond, a, b) -> poList [cond; a; b] )

let siblings p expr =
  match parentOf_ (P.toID p) expr with
  | None -> [p]
  | Some parent -> (
    match parent with
    | F (_, If (cond, ifbody, elsebody)) ->
        List.map PExpr [cond; ifbody; elsebody]
    | F (_, Let (lhs, rhs, body)) -> [PVarBind lhs; PExpr rhs; PExpr body]
    | F (_, FnCall (name, exprs, _)) -> List.map PExpr exprs
    | F (_, Lambda (vars, lexpr)) -> List.map PVarBind vars ^ [PExpr lexpr]
    | F (_, Thread exprs) -> List.map PExpr exprs
    | F (_, FieldAccess (obj, field)) -> [PExpr obj; PField field]
    | F (_, Value _) -> [p]
    | F (_, Variable _) -> [p]
    | F (_, ObjectLiteral pairs) ->
        pairs |> List.map (fun (k, v) -> [PKey k; PExpr v]) |> List.concat
    | F (_, ListLiteral exprs) -> List.map PExpr exprs
    | F (_, FeatureFlag (msg, cond, a, b)) ->
        [PFFMsg msg] ^ List.map PExpr [cond; a; b]
    | Blank _ -> [p] )

let getValueParent p expr =
  let parent = parentOf_ (P.toID p) expr in
  match (P.typeOf p, parent) with
  | Expr, Some (F (_, Thread exprs)) ->
      exprs |> List.map PExpr |> Util.listPrevious p
  | Field, Some (F (_, FieldAccess (obj, _))) -> Some <| PExpr obj
  | _ -> None

let allData expr =
  let e2ld e = PExpr e in
  let _ = "type annotation" in
  let rl exprs = exprs |> List.map allData |> List.concat in
  [e2ld expr]
  ^
  match expr with
  | Blank _ -> []
  | F (_, nexpr) -> (
    match nexpr with
    | Value v -> []
    | Variable name -> []
    | Let (lhs, rhs, body) -> [PVarBind lhs] ^ rl [rhs; body]
    | If (cond, ifbody, elsebody) -> rl [cond; ifbody; elsebody]
    | FnCall (name, exprs, _) -> rl exprs
    | Lambda (vars, body) -> List.map PVarBind vars ^ allData body
    | Thread exprs -> rl exprs
    | FieldAccess (obj, field) -> allData obj ^ [PField field]
    | ListLiteral exprs -> rl exprs
    | ObjectLiteral pairs ->
        pairs |> List.map (fun (k, v) -> PKey k :: allData v) |> List.concat
    | FeatureFlag (msg, cond, a, b) -> [PFFMsg msg] ^ rl [cond; a; b] )

let findExn id expr = expr |> find id |> Option.getExn "findExn"

let find id expr =
  expr |> allData
  |> List.filter (fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  |> List.head

let replace search replacement expr = replace_ search replacement None expr

let within e id =
  e |> F (ID (-1)) |> allData |> List.map P.toID |> List.member id

let replace_ search replacement parent expr =
  let r = replace_ search replacement (Some expr) in
  let _ = "comment" in
  let sId = P.toID search in
  if B.toID expr = sId then
    match replacement with
    | PExpr e ->
        let repl_ =
          match parent with
          | Some (F (_, Thread [_; first])) -> (
            match e with
            | F (id, FnCall (fn, ([rest; _] as args), r_)) ->
                if B.toID first = sId then F (id, FnCall (fn, args, r_))
                else F (id, FnCall (fn, rest, r_))
            | _ -> e )
          | _ -> e
        in
        B.replace sId repl_ expr
    | _ -> recoverable ("cannot occur", replacement) expr
  else
    match (expr, replacement) with
    | F (id, FeatureFlag (msg, cond, a, b)), PFFMsg newMsg ->
        if B.toID msg = sId then F (id, FeatureFlag (newMsg, cond, a, b))
        else traverse r expr
    | F (id, Let (lhs, rhs, body)), PVarBind replacement_ ->
        if B.toID lhs = sId then
          let replacementContent =
            match replacement_ with Blank _ -> None | F (_, var) -> Some var
          in
          let orig =
            match lhs with Blank _ -> None | F (_, var) -> Some var
          in
          let newBody =
            let usesOf =
              match orig with
              | Some var -> uses var body |> List.map PExpr
              | _ -> []
            in
            let transformUse replacementContent_ old =
              match old with
              | PExpr (F (_, _)) ->
                  PExpr (F (gid (), Variable replacementContent_))
              | _ -> impossible old
            in
            match (orig, replacementContent) with
            | Some o, Some r_ ->
                List.foldr
                  (fun use acc ->
                    replace_ use (transformUse r_ use) (Some expr) acc )
                  body usesOf
            | _ -> body
          in
          F (id, Let (B.replace sId replacement_ lhs, rhs, newBody))
        else traverse r expr
    | F (id, Lambda (vars, body)), PVarBind replacement_ -> (
      match List.findIndex (fun v -> B.toID v = sId) vars with
      | None -> traverse r expr
      | Some i ->
          let replacementContent =
            match replacement_ with Blank _ -> None | F (_, var) -> Some var
          in
          let orig =
            match List.getAt i vars |> Option.getExn "we somehow lost it?" with
            | Blank _ -> None
            | F (_, var) -> Some var
          in
          let newBody =
            let usesInBody =
              match orig with
              | Some v -> uses v body |> List.map PExpr
              | None -> []
            in
            let transformUse replacementContent_ old =
              match old with
              | PExpr (F (_, _)) ->
                  PExpr (F (gid (), Variable replacementContent_))
              | _ -> impossible old
            in
            match (orig, replacementContent) with
            | Some o, Some r_ ->
                List.foldr
                  (fun use acc ->
                    replace_ use (transformUse r_ use) (Some expr) acc )
                  body usesInBody
            | _ -> body
          in
          let newVars =
            List.updateAt i (fun old -> B.replace sId replacement_ old) vars
          in
          F (id, Lambda (newVars, newBody)) )
    | F (id, FieldAccess (obj, field)), PField replacement_ ->
        if B.toID field = sId then
          F (id, FieldAccess (obj, B.replace sId replacement_ field))
        else traverse r expr
    | F (id, ObjectLiteral pairs), PKey replacement_ ->
        pairs
        |> List.map (fun (k, v) ->
               let newK = if B.toID k = sId then replacement_ else k in
               (newK, r v) )
        |> ObjectLiteral |> F id
    | _ -> traverse r expr

let deleteExpr p expr id =
  let replacement = P.emptyD_ id (P.typeOf p) in
  replace p replacement expr

let clone expr =
  let nid = gid () in
  let c be = clone be in
  let cl bes = List.map c bes in
  let _ = "type annotation" in
  let cString = B.clone identity in
  let cNExpr nexpr =
    match nexpr with
    | Let (lhs, rhs, body) -> Let (cString lhs, c rhs, c body)
    | If (cond, ifbody, elsebody) -> If (c cond, c ifbody, c elsebody)
    | FnCall (name, exprs, r) -> FnCall (name, cl exprs, r)
    | Lambda (vars, body) -> Lambda (List.map cString vars, c body)
    | Thread exprs -> Thread (cl exprs)
    | FieldAccess (obj, field) -> FieldAccess (c obj, cString field)
    | Value v -> Value v
    | Variable name -> Variable name
    | ListLiteral exprs -> ListLiteral (cl exprs)
    | ObjectLiteral pairs ->
        ObjectLiteral (List.map (fun (k, v) -> (cString k, c v)) pairs)
    | FeatureFlag (msg, cond, a, b) ->
        FeatureFlag (cString msg, c cond, c a, c b)
  in
  B.clone cNExpr expr

let isDefinitionOf var exp =
  match exp with
  | Blank _ -> false
  | F (id, e) -> (
    match e with
    | Let (b, _, _) -> (
      match b with Blank _ -> false | F (_, vb) -> vb = var )
    | Lambda (vars, _) ->
        vars
        |> List.any (fun v ->
               match v with Blank _ -> false | F (_, vb) -> vb = var )
    | _ -> false )

let freeVariables ast =
  let definedAndUsed =
    ast |> allData
    |> List.filterMap (fun n ->
           match n with
           | PExpr boe -> (
             match boe with
             | Blank _ -> None
             | F (id, e) as expr -> (
               match e with
               | Let (F (_, lhs), rhs, body) -> Some (uses lhs body)
               | Lambda (vars, body) ->
                   vars |> List.filterMap B.toMaybe
                   |> List.map (fun v -> uses v body)
                   |> List.concat |> Some
               | _ -> None ) )
           | _ -> None )
    |> List.concat
    |> List.map (B.toID >> deID)
    |> Set.fromList
  in
  ast |> allData
  |> List.filterMap (fun n ->
         match n with
         | PExpr boe -> (
           match boe with
           | Blank _ -> None
           | F (id, e) -> (
             match e with
             | Variable name ->
                 if Set.member (deID id) definedAndUsed then None
                 else Some (id, name)
             | _ -> None ) )
         | _ -> None )
  |> List.uniqueBy (fun (_, name) -> name)
