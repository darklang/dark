open! Porting
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

let traverse (fn : expr -> expr) (expr : expr) : expr =
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
              pairs
              |> List.map (fun (k, v) -> (k, fn v))
              |> fun x -> ObjectLiteral x
          | ListLiteral elems -> ListLiteral (List.map fn elems)
          | FeatureFlag (msg, cond, a, b) ->
              FeatureFlag (msg, fn cond, fn a, fn b)
          | Match (matchExpr, cases) ->
            let traversedCases =
              cases
              |> List.map (fun (k, v) -> (k, fn v))
            in
            Match (fn matchExpr, traversedCases)
          )

let rec allData (expr : expr) : pointerData list =
  let e2ld e = PExpr e in
  let _ = "type annotation" in
  let rl exprs = exprs |> List.map allData |> List.concat in
  [e2ld expr]
  @
  match expr with
  | Blank _ -> []
  | F (_, nexpr) -> (
    match nexpr with
    | Value _ -> []
    | Variable _ -> []
    | Let (lhs, rhs, body) -> [PVarBind lhs] @ rl [rhs; body]
    | If (cond, ifbody, elsebody) -> rl [cond; ifbody; elsebody]
    | FnCall (_, exprs, _) -> rl exprs
    | Lambda (vars, body) -> List.map (fun x -> PVarBind x) vars @ allData body
    | Thread exprs -> rl exprs
    | FieldAccess (obj, field) -> allData obj @ [PField field]
    | ListLiteral exprs -> rl exprs
    | ObjectLiteral pairs ->
        pairs |> List.map (fun (k, v) -> PKey k :: allData v) |> List.concat
    | FeatureFlag (msg, cond, a, b) -> [PFFMsg msg] @ rl [cond; a; b]
    | Match (matchExpr, cases) ->
        let matchData = allData matchExpr in
        let caseData =
          cases
            |> List.map (fun (p, e) -> ( Pattern.allData p) @ rl [e])
            |> List.concat
        in
        matchData @ caseData
  )


let find (id : id) (expr : expr) : pointerData option =
  expr |> allData
  |> List.filter (fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  |> List.head

let findExn (id : id) (expr : expr) : pointerData =
  expr |> find id |> deOption "findExn"

let rec uses (var : varName) (expr : expr) : expr list =
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
    | FnCall (_, exprs, _) -> exprs |> List.map u |> List.concat
    | Lambda (vars, lexpr) ->
        if List.any is_rebinding vars then [] else u lexpr
    | Thread exprs -> exprs |> List.map u |> List.concat
    | FieldAccess (obj, _) -> u obj
    | ListLiteral exprs -> exprs |> List.map u |> List.concat
    | ObjectLiteral pairs ->
        pairs |> List.map Tuple.second |> List.map u |> List.concat
    | FeatureFlag (_, cond, a, b) -> List.concat [u cond; u a; u b]
    | Match (matchExpr, cases) ->
        let findReplacements (p, e) =
          match Pattern.extractVariableName p with
          | Some originalVar ->
              if originalVar = var then [] else u e
          | None -> u e
        in

        let replacements =
          cases
          |> List.map findReplacements
          |> List.concat
        in

        (u matchExpr) @ replacements
  )

let rec replace_ (search : pointerData) (replacement : pointerData)
    (parent : expr option) (expr : expr) : expr =
  let r = replace_ search replacement (Some expr) in (* expr is new parent *)
  let sId = P.toID search in
  if B.toID expr = sId then
    match replacement with
    | PExpr e ->
        let repl_ =
          match parent with
          (* if pasting it into a thread, make the shape fit *)
          | Some (F (_, Thread (first :: _))) -> (
            match e with
            | F (id, FnCall (fn, (_ :: rest as args), r_)) ->
                if B.toID first = sId then F (id, FnCall (fn, args, r_))
                else F (id, FnCall (fn, rest, r_))
            | _ -> e )
          | _ -> e
        in
        B.replace sId repl_ expr
    | _ -> recoverable ("cannot occur", replacement) expr
  else
    let renameVariable currentName newName target =
      let toPointer name = PExpr (F (gid (), Variable name)) in
      let replaceOccurrence use acc = replace_ use (toPointer newName) (Some expr) acc in

      (uses currentName target)
      |> List.map (fun x -> PExpr x)
      |> List.foldr replaceOccurrence target
    in

    let pairCurrentAndNew currentName newName =
      match currentName, newName with
      | Some (c), Some (n) -> Some (c, n)
      | _ -> None
    in

    match (expr, replacement) with
    | F (id, FeatureFlag (msg, cond, a, b)), PFFMsg newMsg ->
        if B.toID msg = sId then F (id, FeatureFlag (newMsg, cond, a, b))
        else traverse r expr
    | F (id, Let (lhs, rhs, body)), PVarBind newBinding ->
        if B.toID lhs = sId then
          let newBody =
            match pairCurrentAndNew (Blank.toMaybe lhs) (Blank.toMaybe newBinding) with
            | Some (c, n) -> renameVariable c n body
            | _ -> body
          in

          F (id, Let (B.replace sId newBinding lhs, rhs, newBody))
        else traverse r expr
    | F (id, Lambda (vars, body)), PVarBind newBinding -> (
      match List.findIndex (fun v -> B.toID v = sId) vars with
      | None -> traverse r expr
      | Some i ->
          let currentName = List.getAt i vars |> deOption "we somehow lost it?" in
          let newVars =
            List.updateAt i (fun old -> B.replace sId newBinding old) vars
          in
          let newBody =
            match pairCurrentAndNew (Blank.toMaybe currentName) (Blank.toMaybe newBinding) with
            | Some (c, n) -> renameVariable c n body
            | _ -> body
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
        |> (fun x -> ObjectLiteral x)
        |> fun e -> F (id, e)
    | F (id, Match (matchExpr, cases)), PPattern newPattern ->
        let newCases =
          cases
          |> List.map (fun (p, e) ->
            if Pattern.contains search p then
            begin
                let currentName = Pattern.extractVariableName p in
                let newName = Pattern.extractVariableName newPattern in
                let currentAndNew = pairCurrentAndNew currentName newName in
                let newBody =
                  match currentAndNew with
                  | Some (c, n) -> renameVariable c n e
                  | _ -> e
                in

                (Pattern.replace search replacement p, newBody)
            end
            else (p, r e)
          )
        in

        F (id, Match (r matchExpr, newCases))
    | _ -> traverse r expr

let replace (search : pointerData) (replacement : pointerData) (expr : expr) :
    expr =
  replace_ search replacement None expr

let children (expr : expr) : pointerData list =
  match expr with
  | Blank _ -> []
  | F (_, nexpr) -> (
    match nexpr with
    | Value _ -> []
    | Variable _ -> []
    | If (cond, ifbody, elsebody) -> [PExpr cond; PExpr ifbody; PExpr elsebody]
    | FnCall (_, exprs, _) -> List.map (fun e -> PExpr e) exprs
    | Lambda (vars, lexpr) ->
        List.map (fun vb -> PVarBind vb) vars @ [PExpr lexpr]
    | Thread exprs -> List.map (fun e -> PExpr e) exprs
    | FieldAccess (obj, field) -> [PExpr obj; PField field]
    | Let (lhs, rhs, body) -> [PVarBind lhs; PExpr rhs; PExpr body]
    | ObjectLiteral pairs ->
        pairs |> List.map (fun (k, v) -> [PKey k; PExpr v]) |> List.concat
    | ListLiteral elems -> List.map (fun e -> PExpr e) elems
    | FeatureFlag (msg, cond, a, b) ->
        [PFFMsg msg; PExpr cond; PExpr a; PExpr b]
    | Match (matchExpr, cases) ->
      (* We list all the descendents of the pattern here. This isn't ideal,
       * but it's challenging with the current setup to do otherwise, because
       * all of these things take exprs *)
      let casePointers =
        cases
        |> List.map
          (fun (p, e) ->
             let ps = Pattern.allData p in
             ps @ [PExpr e])
        |> List.concat
      in
      (PExpr matchExpr) :: casePointers)

let rec childrenOf (pid : id) (expr : expr) : pointerData list =
  let co = childrenOf pid in
  if pid = B.toID expr then children expr
  else
    match expr with
    | Blank _ -> []
    | F (_, nexpr) -> (
      match nexpr with
      | Value _ -> []
      | Variable _ -> []
      | Let (_, rhs, body) -> co body @ co rhs
      | If (cond, ifbody, elsebody) -> co cond @ co ifbody @ co elsebody
      | FnCall (_, exprs, _) -> List.map co exprs |> List.concat
      | Lambda (_, lexpr) -> co lexpr
      | Thread exprs -> List.map co exprs |> List.concat
      | FieldAccess (obj, _) -> co obj
      | ObjectLiteral pairs ->
          pairs |> List.map Tuple.second |> List.map co |> List.concat
      | ListLiteral pairs -> pairs |> List.map co |> List.concat
      | FeatureFlag (_, cond, a, b) -> co cond @ co a @ co b
      | Match (matchExpr, cases) ->
        let cCases =
          cases |> List.map Tuple.second |> List.map co |> List.concat
        in
        co matchExpr @ cCases
    )

let rec findParentOfWithin_ (eid : id) (haystack : expr) : expr option =
  let fpow = findParentOfWithin_ eid in
  (* the `or` of all items in the list *)
  let fpowList xs = xs |> List.map fpow |> List.filterMap identity |> List.head in
  if List.member eid (haystack |> children |> List.map P.toID)
  then Some haystack
  else
    match haystack with
    | Blank _ -> None
    | F (_, nexpr) -> (
      match nexpr with
      | Value _ -> None
      | Variable _ -> None
      | Let (_, rhs, body) -> fpowList [rhs; body]
      | If (cond, ifbody, elsebody) -> fpowList [cond; ifbody; elsebody]
      | FnCall (_, exprs, _) -> fpowList exprs
      | Lambda (_, lexpr) -> fpow lexpr
      | Thread exprs -> fpowList exprs
      | FieldAccess (obj, _) -> fpow obj
      | ListLiteral exprs -> fpowList exprs
      | ObjectLiteral pairs -> pairs |> List.map Tuple.second |> fpowList
      | FeatureFlag (_, cond, a, b) -> fpowList [cond; a; b]
      | Match (matchExpr, cases) ->
        fpowList (matchExpr :: (cases |> List.map Tuple.second))
    )

let findParentOfWithin (id : id) (haystack : expr) : expr =
  deOption "findParentOfWithin" <| findParentOfWithin_ id haystack

let rec listThreadBlanks (expr : expr) : id list =
  let r = listThreadBlanks in
  let _ = "type annotation" in
  let rList exprs = exprs |> List.map listThreadBlanks |> List.concat in
  let rn nexpr =
    match nexpr with
    | Value _ -> []
    | Variable _ -> []
    | Let (_, rhs, body) -> r rhs @ r body
    | FnCall (_, exprs, _) -> rList exprs
    | Lambda (_, body) -> r body
    | FieldAccess (obj, _) -> r obj
    | If (cond, ifbody, elsebody) -> r cond @ r ifbody @ r elsebody
    | Thread exprs ->
        let blanks, filled = List.partition B.isBlank exprs in
        let blankids = List.map B.toID blanks in
        let subExprsBlankids = rList filled in
        blankids @ subExprsBlankids
    | ObjectLiteral pairs -> pairs |> List.map Tuple.second |> rList
    | ListLiteral exprs -> rList exprs
    | FeatureFlag (_, cond, a, b) -> r cond @ r a @ r b
    | Match (matchExpr, cases) ->
        r matchExpr @ (cases |> List.map Tuple.second |> rList)
  in
  match expr with Blank _ -> [] | F (_, f) -> rn f

let rec closeThreads (expr : expr) : expr =
  match expr with
  | F (id, Thread exprs) -> (
      let addBlank =
        match exprs with
        | [] -> false
        | [_] -> false
        | F (_, FnCall (_, _, _)) :: _ -> false
        | _ -> true
      in
      let newExprs = List.filter B.isF exprs |> List.map closeThreads in
      let adjusted =
        match newExprs with
        | F (id_, FnCall (name, args, r)) :: rest ->
            if addBlank then
              [F (id_, FnCall (name, B.new_ () :: args, r))] @ rest
            else [F (id_, FnCall (name, args, r))] @ rest
        | _ -> newExprs
      in
      match adjusted with
      | [] -> Blank id
      | [e] -> e
      | _ -> F (id, Thread adjusted) )
  | _ -> traverse closeThreads expr

let rec closeObjectLiterals (expr : expr) : expr =
  match expr with
  | F (id, ObjectLiteral pairs) ->
      pairs
      |> List.filterMap (fun (k, v) ->
             if B.isBlank k && B.isBlank v then None
             else Some (k, closeObjectLiterals v) )
      |> (fun l -> if l <> [] then l else [(B.new_ (), B.new_ ())])
      |> (fun x -> ObjectLiteral x)
      |> fun x -> F (id, x)
  | _ -> traverse closeObjectLiterals expr

let rec closeListLiterals (expr : expr) : expr =
  match expr with
  | F (id, ListLiteral exprs) ->
      let exprs2 = List.map closeListLiterals exprs in
      let exprs3 = List.filter B.isF exprs2 in
      F (id, ListLiteral (exprs3 @ [B.new_ ()]))
  | _ -> traverse closeObjectLiterals expr

let rec closeMatchPatterns (expr : expr) : expr =
  match expr with
  | F (id, Match (cond, pairs)) ->
      pairs
      |> List.filterMap (fun (p, e) ->
             if B.isBlank p && B.isBlank e
             then None
             else Some (p, closeMatchPatterns e))
      |> (fun l -> if l <> [] then l else [(B.new_ (), B.new_ ())])
      |> (fun l -> Match (closeMatchPatterns cond, l))
      |> fun m -> F (id, m)
  | _ -> traverse closeMatchPatterns expr



let closeBlanks (expr : expr) : expr =
  expr
  |> closeThreads
  |> closeObjectLiterals
  |> closeListLiterals
  |> closeMatchPatterns

let extendThreadChild (at : id) (blank : expr) (threadExprs : expr list) :
    expr list =
  List.foldr
    (fun e list -> if B.toID e = at then e :: blank :: list else e :: list)
    [] threadExprs

let rec maybeExtendThreadAt (id : id) (blank : expr) (expr : expr) : expr =
  match expr with
  | F (tid, Thread exprs) ->
      let newExprs =
        extendThreadChild id blank exprs
        |> List.map (maybeExtendThreadAt id blank)
      in
      F (tid, Thread newExprs)
  | _ -> traverse (maybeExtendThreadAt id blank) expr

let rec addThreadBlank (id : id) (blank : expr) (expr : expr) : expr =
  let atb = addThreadBlank id blank in
  if id = B.toID expr then
    match expr with
    | F (tid, Thread exprs) -> F (tid, Thread (exprs @ [blank]))
    | _ -> B.newF (Thread [expr; blank])
  else
    match expr with
    | F (tid, Thread exprs) ->
        let replaced = extendThreadChild id blank exprs in
        if replaced = exprs then traverse atb expr else F (tid, Thread replaced)
    | _ -> traverse atb expr

let addLambdaBlank (id : id) (expr : expr) : expr =
  match findParentOfWithin_ id expr with
  | Some (F (lid, Lambda (vars, body))) as old ->
      let r = F (lid, Lambda (vars @ [B.new_ ()], body)) in
      replace (old |> deOption "impossible" |> fun x -> PExpr x) (PExpr r) expr
  | _ -> expr

let addObjectLiteralBlanks (id : id) (expr : expr) : id * id * expr =
  match findExn id expr with
  | PKey _ -> (
    match findParentOfWithin id expr with
    | F (olid, ObjectLiteral pairs) as old ->
        let newKey = B.new_ () in
        let newExpr = B.new_ () in
        let newPairs = pairs @ [(newKey, newExpr)] in
        let new_ = F (olid, ObjectLiteral newPairs) in
        let replacement = replace (PExpr old) (PExpr new_) expr in
        (B.toID newKey, B.toID newExpr, replacement)
    | _ -> impossible ("key parent must be object", id, expr) )
  | _ -> impossible ("must add to key", id, expr)

let maybeExtendObjectLiteralAt (pd : pointerData) (ast : expr) : expr =
  let id = P.toID pd in
  match pd with
  | PKey key -> (
    match findParentOfWithin id ast with
    | F (_, ObjectLiteral pairs) ->
        if pairs |> List.last |> Option.map Tuple.first |> ( = ) (Some key)
        then
          let _, _, replacement = addObjectLiteralBlanks id ast in
          replacement
        else ast
    | _ -> ast )
  | _ -> ast

let addListLiteralBlanks (id : id) (expr : expr) : expr =
  let new1 = B.new_ () in
  let parent = findParentOfWithin id expr in
  match parent with
  | F (lid, ListLiteral exprs) ->
      let newExprs =
        exprs |> List.reverse |> List.dropWhile B.isBlank
        |> ( @ ) [new1]
        |> List.reverse
      in
      replace (PExpr parent) (PExpr (F (lid, ListLiteral newExprs))) expr
  | _ -> expr

let maybeExtendListLiteralAt (pd : pointerData) (expr : expr) : expr =
  let id = P.toID pd in
  match findParentOfWithin_ id expr with
  | Some (F (_, ListLiteral exprs)) ->
      if exprs |> List.filter B.isBlank |> List.length |> fun l -> l <= 1 then
        let replacement = addListLiteralBlanks id expr in
        replacement
      else expr
  | _ -> expr

let addPatternBlanks (id : id) (expr : expr) : id * id * expr =
  match findExn id expr with
  | PPattern _ -> (
    match findParentOfWithin id expr with
    | F (olid, Match (cond, pairs)) as old ->
        let newPat = B.new_ () in
        let newExpr = B.new_ () in
        let pos = List.findIndex (fun (p2, _) -> B.toID p2 = id) pairs
                  |> Option.withDefault 0
        in
        let newPairs = List.insertAt (pos+1) pairs (newPat, newExpr) in
        let new_ = F (olid, Match (cond, newPairs)) in
        let replacement = replace (PExpr old) (PExpr new_) expr in
        (B.toID newPat, B.toID newExpr, replacement)
    | _ -> impossible ("pattern parent must be match", id, expr) )
  | _ -> impossible ("must add to pattern", id, expr)

let maybeExtendPatternAt (pd : pointerData) (ast : expr) : expr =
  let id = P.toID pd in
  match pd with
  | PPattern pat -> (
    match findParentOfWithin id ast with
    | F (_, Match (_, pairs)) ->
        if pairs
           |> List.last
           |> Option.map Tuple.first
           |> (=) (Some pat)
        then
          let _, _, replacement = addPatternBlanks id ast in
          replacement
        else ast
    | _ -> ast )
  | _ -> ast

let rec wrapInThread (id : id) (expr : expr) : expr =
  if B.toID expr = id then
    match expr with
    | F (_, Thread _) -> expr
    | F (_, _) -> B.newF (Thread [expr; B.new_ ()])
    | Blank _ -> B.newF (Thread [expr])
  else traverse (wrapInThread id) expr

let isThreadBlank (expr : expr) (p : id) : bool =
  expr |> listThreadBlanks |> List.member p

let grandparentIsThread (expr : expr) (parent : expr option) : bool =
  parent
  |> Option.map (fun p ->
         match findParentOfWithin_ (B.toID p) expr with
         | Some (F (_, Thread ts)) ->
             ts |> List.head
             |> Option.map (( <> ) p)
             |> Option.withDefault true
         | _ -> false )
  |> Option.withDefault false

let getParamIndex (expr : expr) (id : id) : (string * int) option =
  let parent = findParentOfWithin_ id expr in
  let inThread = grandparentIsThread expr parent in
  match parent with
  | Some (F (_, FnCall (name, args, _))) ->
      args
      |> List.findIndex (fun a -> B.toID a = id)
      |> Option.map (fun i -> if inThread then (name, i + 1) else (name, i))
  | _ -> None

let threadPrevious (id : id) (ast : expr) : expr option =
  let parent = findParentOfWithin_ id ast in
  match parent with
  | Some (F (_, Thread exprs)) ->
      exprs
      |> List.filter (fun e -> B.toID e = id)
      |> List.head
      |> Option.andThen (fun this -> Util.listPrevious this exprs)
  | _ -> None

let allCallsToFn (s : string) (e : expr) : expr list =
  e |> allData
  |> List.filterMap (fun pd ->
         match pd with
         | PExpr (F (id, FnCall (name, params, r))) ->
             if name = s then Some (F (id, FnCall (name, params, r))) else None
         | _ -> None )

let usesRail (ast : expr) : bool =
  List.any
    (fun e ->
      match e with PExpr (F (_, FnCall (_, _, Rail))) -> true | _ -> false )
    (allData ast)

let ancestors (id : id) (expr : expr) : expr list =
  let rec rec_ancestors (tofind : id) (walk : expr list) (exp : expr) =
    let rec_ id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_) in
    let reclist id_ e_ walk_ exprs =
      exprs |> List.map (rec_ id_ e_ walk_) |> List.concat
    in
    if B.toID exp = tofind then walk
    else
      match exp with
      | Blank _ -> []
      | F (_, nexpr) -> (
        match nexpr with
        | Value _ -> []
        | Variable _ -> []
        | Let (_, rhs, body) -> reclist id exp walk [rhs; body]
        | If (cond, ifbody, elsebody) ->
            reclist id exp walk [cond; ifbody; elsebody]
        | FnCall (_, exprs, _) -> reclist id exp walk exprs
        | Lambda (_, lexpr) -> rec_ id exp walk lexpr
        | Thread exprs -> reclist id exp walk exprs
        | FieldAccess (obj, _) -> rec_ id exp walk obj
        | ListLiteral exprs -> reclist id expr walk exprs
        | ObjectLiteral pairs ->
            pairs |> List.map Tuple.second |> reclist id expr walk
        | FeatureFlag (_, cond, a, b) -> reclist id exp walk [cond; a; b]
        | Match (matchExpr, cases) ->
          reclist id exp walk (matchExpr :: (List.map Tuple.second cases)))
  in
  rec_ancestors id [] expr

let ancestorsWhere (id : id) (expr : expr) (fn : expr -> bool) : expr list =
  List.filter fn (ancestors id expr)

let threadAncestors (id : id) (expr : expr) : expr list =
  ancestorsWhere id expr
    (fun e ->
      match e with
        | F (_, Thread _) -> true
        | _ -> false)

let siblings (p : pointerData) (expr : expr) : pointerData list =
  match findParentOfWithin_ (P.toID p) expr with
  | None -> [p]
  | Some parent -> (
    match parent with
    | F (_, If (cond, ifbody, elsebody)) ->
        [PExpr cond; PExpr ifbody; PExpr elsebody]
    | F (_, Let (lhs, rhs, body)) -> [PVarBind lhs; PExpr rhs; PExpr body]
    | F (_, FnCall (_, exprs, _)) -> List.map (fun e -> PExpr e) exprs
    | F (_, Lambda (vars, lexpr)) ->
        List.map (fun vb -> PVarBind vb) vars @ [PExpr lexpr]
    | F (_, Thread exprs) -> List.map (fun e -> PExpr e) exprs
    | F (_, FieldAccess (obj, field)) -> [PExpr obj; PField field]
    | F (_, Value _) -> [p]
    | F (_, Variable _) -> [p]
    | F (_, ObjectLiteral pairs) ->
        pairs |> List.map (fun (k, v) -> [PKey k; PExpr v]) |> List.concat
    | F (_, ListLiteral exprs) -> List.map (fun e -> PExpr e) exprs
    | F (_, FeatureFlag (msg, cond, a, b)) ->
        [PFFMsg msg; PExpr cond; PExpr a; PExpr b]
    | F (_, Match (matchExpr, cases)) ->
      (* TODO(match): patterns - no one uses siblings so it should really be deleted *)
      (PExpr matchExpr) :: (cases |> List.map (fun (_, v) -> [PExpr v]) |> List.concat)
    | Blank _ -> [p] )

let getValueParent (p : pointerData) (expr : expr) : pointerData option =
  let parent = findParentOfWithin_ (P.toID p) expr in
  match (P.typeOf p, parent) with
  | Expr, Some (F (_, Thread exprs)) ->
      exprs |> List.map (fun x -> PExpr x) |> Util.listPrevious p
  | Field, Some (F (_, FieldAccess (obj, _))) -> Some (PExpr obj)
  | _ -> None

let within (e : nExpr) (id : id) : bool =
  F (ID "invalidIDforASTwithin", e) |> allData |> List.map P.toID |> List.member id

let deleteExpr (p : pointerData) (expr : expr) (id : id) : expr =
  let replacement = P.emptyD_ id (P.typeOf p) in
  replace p replacement expr

let rec clonePattern (pattern : pattern) : pattern =
  let cNPattern npat =
    match npat with
    | PLiteral _
    | PVariable _ -> npat
    | PConstructor (name, args) ->
      PConstructor (name, List.map clonePattern args)
  in
  B.clone cNPattern pattern


let rec clone (expr : expr) : expr =
  let c be = clone be in
  let cl bes = List.map c bes in
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
    | Match (matchExpr, cases) ->
        Match (c matchExpr, List.map (fun (k, v) -> (clonePattern k, c v)) cases)
  in
  B.clone cNExpr expr

let isDefinitionOf (var : varName) (exp : expr) : bool =
  match exp with
  | Blank _ -> false
  | F (_, e) -> (
    match e with
    | Let (b, _, _) -> (
      match b with Blank _ -> false | F (_, vb) -> vb = var )
    | Lambda (vars, _) ->
        vars
        |> List.any (fun v ->
               match v with Blank _ -> false | F (_, vb) -> vb = var )
    | _ -> false )

let freeVariables (ast : expr) : (id * varName) list =
  let definedAndUsed =
    ast |> allData
    |> List.filterMap (fun n ->
           match n with
           | PExpr boe -> (
             match boe with
             | Blank _ -> None
             | F (_, e) -> (
               match e with
               | Let (F (_, lhs), _, body) -> Some (uses lhs body)
               | Lambda (vars, body) ->
                   vars |> List.filterMap B.toMaybe
                   |> List.map (fun v -> uses v body)
                   |> List.concat
                   |> fun x -> Some x
               | _ -> None ) )
           | _ -> None )
    |> List.concat
    |> List.map (B.toID >> deID)
    |> StrSet.fromList
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
                 if StrSet.member (deID id) definedAndUsed then None
                 else Some (id, name)
             | _ -> None ) )
         | _ -> None )
  |> List.uniqueBy (fun (_, name) -> name)

module SymSet = Porting.StrSet
module IDTable = Belt.MutableMap.String
type sym_set = SymSet.t
type sym_store = sym_set IDTable.t

let rec sym_exec
    ~(trace: (expr -> sym_set -> unit))
    (st: sym_set)
    (expr: expr)
  : unit =
  let sexe = sym_exec ~trace in
  ignore
    ((match expr with
        | Blank _ -> ()
        | F (_, Value _) -> ()
        | F (_, Variable _) -> ()

        | F (_, Let (lhs, rhs, body)) ->
          let bound = match lhs with
            | F (_, name) ->
              sexe st rhs;
              SymSet.add st name
            | Blank _ -> st
          in sexe bound body

        | F (_, FnCall (_, exprs, _)) ->
          List.iter ~f:(sexe st) exprs

        | F (_, If (cond, ifbody, elsebody))
        | F (_, FeatureFlag(_, cond, elsebody, ifbody)) ->
          sexe st cond;
          sexe st ifbody;
          sexe st elsebody;

        | F (_, Lambda (vars, body)) ->
          let new_st =
            vars
            |> List.filterMap Blank.toMaybe
            |> SymSet.ofList
            |> SymSet.union st
          in
          sexe new_st body

        | F (_, Thread (exprs)) ->
          List.iter ~f:(sexe st) exprs

        | F (_, FieldAccess (obj, _)) ->
          sexe st obj

        | F (_, ListLiteral exprs) ->
          List.iter ~f:(sexe st) exprs

        | F (_, Match (matchExpr, cases)) ->
          let rec variables_in_pattern p =
            match p with
            | Blank _ -> []
            | F (_, PLiteral _) -> []
            | F (_, PVariable v) -> [v]
            | F (_, PConstructor (_, inner)) ->
              inner
              |> List.map variables_in_pattern
              |> List.concat
          in

          sexe st matchExpr;
          List.iter cases ~f:(fun (p, caseExpr) ->
              let new_st =
                p
                |> variables_in_pattern
                |> SymSet.ofList
                |> SymSet.union st
              in
              sexe new_st caseExpr
            );

        | F (_, ObjectLiteral exprs) ->
          exprs
          |> List.map Tuple.second
          |> List.iter ~f:(sexe st)));
  trace expr st

let variablesIn (ast: expr) : avDict =
  let sym_store = IDTable.make () in
  let trace expr st =
    IDTable.set sym_store (deID (Blank.toID expr)) st
  in
  sym_exec ~trace SymSet.empty ast;
  sym_store
  |> IDTable.toList
  |> StrDict.fromList
  |. StrDict.map SymSet.toList



