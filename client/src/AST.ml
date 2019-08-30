open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

(* ------------------------- *)
(* Generic *)
(* ------------------------- *)
let traverse (fn : expr -> expr) (expr : expr) : expr =
  match expr with
  | Blank _ ->
      expr
  | F (id, nexpr) ->
      F
        ( id
        , match nexpr with
          | Value _ ->
              nexpr
          | Variable _ ->
              nexpr
          | Let (lhs, rhs, body) ->
              Let (lhs, fn rhs, fn body)
          | If (cond, ifbody, elsebody) ->
              If (fn cond, fn ifbody, fn elsebody)
          | FnCall (name, exprs, r) ->
              FnCall (name, List.map ~f:fn exprs, r)
          | Constructor (name, exprs) ->
              Constructor (name, List.map ~f:fn exprs)
          | Lambda (vars, lexpr) ->
              Lambda (vars, fn lexpr)
          | Thread exprs ->
              Thread (List.map ~f:fn exprs)
          | FieldAccess (obj, field) ->
              FieldAccess (fn obj, field)
          | ObjectLiteral pairs ->
              pairs
              |> List.map ~f:(fun (k, v) -> (k, fn v))
              |> fun x -> ObjectLiteral x
          | ListLiteral elems ->
              ListLiteral (List.map ~f:fn elems)
          | FeatureFlag (msg, cond, a, b) ->
              FeatureFlag (msg, fn cond, fn a, fn b)
          | Match (matchExpr, cases) ->
              let traversedCases =
                cases |> List.map ~f:(fun (k, v) -> (k, fn v))
              in
              Match (fn matchExpr, traversedCases)
          | FluidPartial (name, oldExpr) ->
              FluidPartial (name, fn oldExpr)
          | FluidRightPartial (name, oldExpr) ->
              FluidRightPartial (name, fn oldExpr) )


(* -------------------------------- *)
(* PointerData *)
(* -------------------------------- *)

let rec allData (expr : expr) : pointerData list =
  let e2ld e = PExpr e in
  let rl exprs = exprs |> List.map ~f:allData |> List.concat in
  [e2ld expr]
  @
  match expr with
  | Blank _ ->
      []
  | F (_, nexpr) ->
    ( match nexpr with
    | Value _ ->
        []
    | Variable _ ->
        []
    | Let (lhs, rhs, body) ->
        [PVarBind lhs] @ rl [rhs; body]
    | If (cond, ifbody, elsebody) ->
        rl [cond; ifbody; elsebody]
    | FnCall (name, exprs, _) ->
        [PFnCallName name] @ rl exprs
    | Constructor (name, exprs) ->
        PConstructorName name :: rl exprs
    | Lambda (vars, body) ->
        List.map ~f:(fun x -> PVarBind x) vars @ allData body
    | Thread exprs ->
        rl exprs
    | FieldAccess (obj, field) ->
        allData obj @ [PField field]
    | ListLiteral exprs ->
        rl exprs
    | ObjectLiteral pairs ->
        pairs |> List.map ~f:(fun (k, v) -> PKey k :: allData v) |> List.concat
    | FeatureFlag (msg, cond, a, b) ->
        [PFFMsg msg] @ rl [cond; a; b]
    | Match (matchExpr, cases) ->
        let matchData = allData matchExpr in
        let caseData =
          cases
          |> List.map ~f:(fun (p, e) -> Pattern.allData p @ rl [e])
          |> List.concat
        in
        matchData @ caseData
    | FluidPartial (_, oldExpr) ->
        allData oldExpr
    | FluidRightPartial (_, oldExpr) ->
        allData oldExpr )


let find (id : id) (expr : expr) : pointerData option =
  expr
  |> allData
  |> List.filter ~f:(fun d -> id = P.toID d)
  |> assert_ (fun r -> List.length r <= 1)
  (* guard against dups *)
  |> List.head


let findExn (id : id) (expr : expr) : pointerData =
  expr |> find id |> deOption "findExn"


let rec uses (var : varName) (expr : expr) : expr list =
  let is_rebinding newbind =
    match newbind with
    | Blank _ ->
        false
    | F (_, potential) ->
        if potential = var then true else false
  in
  let u = uses var in
  match expr with
  | Blank _ ->
      []
  | F (_, nexpr) ->
    ( match nexpr with
    | Value _ ->
        []
    | Variable potential ->
        if potential = var then [expr] else []
    | Let (lhs, rhs, body) ->
        if is_rebinding lhs then [] else List.concat [u rhs; u body]
    | If (cond, ifbody, elsebody) ->
        List.concat [u cond; u ifbody; u elsebody]
    | FnCall (_, exprs, _) ->
        exprs |> List.map ~f:u |> List.concat
    | Constructor (_, exprs) ->
        exprs |> List.map ~f:u |> List.concat
    | Lambda (vars, lexpr) ->
        if List.any ~f:is_rebinding vars then [] else u lexpr
    | Thread exprs ->
        exprs |> List.map ~f:u |> List.concat
    | FieldAccess (obj, _) ->
        u obj
    | ListLiteral exprs ->
        exprs |> List.map ~f:u |> List.concat
    | ObjectLiteral pairs ->
        pairs |> List.map ~f:Tuple2.second |> List.map ~f:u |> List.concat
    | FeatureFlag (_, cond, a, b) ->
        List.concat [u cond; u a; u b]
    | Match (matchExpr, cases) ->
        let findReplacements (p, e) =
          (* do not replace shadowed variables *)
          let originalNames = Pattern.variableNames p in
          if List.member ~value:var originalNames then [] else u e
        in
        let replacements =
          cases |> List.map ~f:findReplacements |> List.concat
        in
        u matchExpr @ replacements
    | FluidPartial (_, oldExpr) ->
        u oldExpr
    | FluidRightPartial (_, oldExpr) ->
        u oldExpr )


let rec replace_
    (search : pointerData)
    (replacement : pointerData)
    (parent : expr option)
    (expr : expr) : expr =
  let r = replace_ search replacement (Some expr) in
  (* expr is new parent *)
  let sId = P.toID search in
  if B.toID expr = sId
  then
    match replacement with
    | PExpr e ->
        let repl_ =
          match parent with
          (* if pasting it into a thread, make the shape fit *)
          | Some (F (_, Thread (first :: _))) ->
            ( match e with
            | F (id, FnCall (fn, (_ :: rest as args), r_)) ->
                if B.toID first = sId
                then F (id, FnCall (fn, args, r_))
                else F (id, FnCall (fn, rest, r_))
            | _ ->
                e )
          | _ ->
              e
        in
        B.replace sId repl_ expr
    | _ ->
        recoverable ("cannot occur", replacement) expr
  else
    let renameVariable currentName newName target =
      let toPointer name = PExpr (F (gid (), Variable name)) in
      let replaceOccurrence use acc =
        replace_ use (toPointer newName) (Some expr) acc
      in
      uses currentName target
      |> List.map ~f:(fun x -> PExpr x)
      |> List.foldr ~f:replaceOccurrence ~init:target
    in
    let pairCurrentAndNew currentName newName =
      match (currentName, newName) with
      | Some c, Some n ->
          Some (c, n)
      | _ ->
          None
    in
    match (expr, replacement) with
    | F (id, FeatureFlag (msg, cond, a, b)), PFFMsg newMsg ->
        if B.toID msg = sId
        then F (id, FeatureFlag (newMsg, cond, a, b))
        else traverse r expr
    | F (id, Let (lhs, rhs, body)), PVarBind newBinding ->
        if B.toID lhs = sId
        then
          let newBody =
            match
              pairCurrentAndNew (Blank.toMaybe lhs) (Blank.toMaybe newBinding)
            with
            | Some (c, n) ->
                renameVariable c n body
            | _ ->
                body
          in
          F (id, Let (B.replace sId newBinding lhs, rhs, newBody))
        else traverse r expr
    | F (id, Lambda (vars, body)), PVarBind newBinding ->
      ( match List.findIndex ~f:(fun v -> B.toID v = sId) vars with
      | None ->
          traverse r expr
      | Some i ->
          let currentName =
            List.getAt ~index:i vars |> deOption "we somehow lost it?"
          in
          let newVars =
            List.updateAt
              ~index:i
              ~f:(fun old -> B.replace sId newBinding old)
              vars
          in
          let newBody =
            match
              pairCurrentAndNew
                (Blank.toMaybe currentName)
                (Blank.toMaybe newBinding)
            with
            | Some (c, n) ->
                renameVariable c n body
            | _ ->
                body
          in
          F (id, Lambda (newVars, newBody)) )
    | F (id, FieldAccess (obj, field)), PField replacement_ ->
        if B.toID field = sId
        then F (id, FieldAccess (obj, B.replace sId replacement_ field))
        else traverse r expr
    | F (id, Constructor (name, exprs)), PConstructorName replacement_ ->
        if B.toID name = sId
        then F (id, Constructor (B.replace sId replacement_ name, exprs))
        else traverse r expr
    | F (id, ObjectLiteral pairs), PKey replacement_ ->
        pairs
        |> List.map ~f:(fun (k, v) ->
               let newK = if B.toID k = sId then replacement_ else k in
               (newK, r v) )
        |> (fun x -> ObjectLiteral x)
        |> fun e -> F (id, e)
    | F (id, Match (matchExpr, cases)), PPattern newPattern ->
        let newCases =
          cases
          |> List.map ~f:(fun (p, e) ->
                 match Pattern.extractById p (P.toID search) with
                 | Some currentPattern ->
                     let newBody =
                       match (currentPattern, newPattern) with
                       | Blank _, _ | F (_, PLiteral _), _ ->
                           e
                       | F (_, PVariable c), F (_, PVariable n) ->
                           renameVariable c n e
                       | _ ->
                           (*
                   * For now, only allow if all variables in the replaced
                   * pattern are unused in the RHS of the case. Otherwise,
                   * the RHS could end up with unbound variables.
                   *
                   * This check is perfomed by Entry.validate
                   * *)
                           e
                     in
                     (Pattern.replace search replacement p, newBody)
                 | None ->
                     (p, r e) )
        in
        F (id, Match (r matchExpr, newCases))
    | _ ->
        traverse r expr


let replace (search : pointerData) (replacement : pointerData) (expr : expr) :
    expr =
  replace_ search replacement None expr


(* ------------------------- *)
(* Children *)
(* ------------------------- *)
let children (expr : expr) : pointerData list =
  let ces exprs = List.map ~f:(fun e -> PExpr e) exprs in
  match expr with
  | Blank _ ->
      []
  | F (_, nexpr) ->
    ( match nexpr with
    | Value _ ->
        []
    | Variable _ ->
        []
    | If (cond, ifbody, elsebody) ->
        [PExpr cond; PExpr ifbody; PExpr elsebody]
    | FnCall (_, exprs, _) ->
        ces exprs
    | Constructor (name, exprs) ->
        PConstructorName name :: ces exprs
    | Lambda (vars, lexpr) ->
        List.map ~f:(fun vb -> PVarBind vb) vars @ [PExpr lexpr]
    | Thread exprs ->
        ces exprs
    | FieldAccess (obj, field) ->
        [PExpr obj; PField field]
    | Let (lhs, rhs, body) ->
        [PVarBind lhs; PExpr rhs; PExpr body]
    | ObjectLiteral pairs ->
        pairs |> List.map ~f:(fun (k, v) -> [PKey k; PExpr v]) |> List.concat
    | ListLiteral elems ->
        ces elems
    | FeatureFlag (msg, cond, a, b) ->
        [PFFMsg msg; PExpr cond; PExpr a; PExpr b]
    | Match (matchExpr, cases) ->
        (* We list all the descendents of the pattern here. This isn't ideal,
       * but it's challenging with the current setup to do otherwise, because
       * all of these things take exprs *)
        let casePointers =
          cases
          |> List.map ~f:(fun (p, e) ->
                 let ps = Pattern.allData p in
                 ps @ [PExpr e] )
          |> List.concat
        in
        PExpr matchExpr :: casePointers
    | FluidPartial (_, oldExpr) ->
        [PExpr oldExpr]
    | FluidRightPartial (_, oldExpr) ->
        [PExpr oldExpr] )


(* Look through an AST for the expr with the id, then return its children. *)
let rec childrenOf (pid : id) (expr : expr) : pointerData list =
  let co = childrenOf pid in
  if pid = B.toID expr
  then children expr
  else
    match expr with
    | Blank _ ->
        []
    | F (_, nexpr) ->
      ( match nexpr with
      | Value _ ->
          []
      | Variable _ ->
          []
      | Let (_, rhs, body) ->
          co body @ co rhs
      | If (cond, ifbody, elsebody) ->
          co cond @ co ifbody @ co elsebody
      | FnCall (_, exprs, _) ->
          List.map ~f:co exprs |> List.concat
      | Constructor (_, exprs) ->
          List.map ~f:co exprs |> List.concat
      | Lambda (_, lexpr) ->
          co lexpr
      | Thread exprs ->
          List.map ~f:co exprs |> List.concat
      | FieldAccess (obj, _) ->
          co obj
      | ObjectLiteral pairs ->
          pairs |> List.map ~f:Tuple2.second |> List.map ~f:co |> List.concat
      | ListLiteral pairs ->
          pairs |> List.map ~f:co |> List.concat
      | FeatureFlag (_, cond, a, b) ->
          co cond @ co a @ co b
      | Match (matchExpr, cases) ->
          let cCases =
            cases |> List.map ~f:Tuple2.second |> List.map ~f:co |> List.concat
          in
          co matchExpr @ cCases
      | FluidPartial (_, oldExpr) ->
          co oldExpr
      | FluidRightPartial (_, oldExpr) ->
          co oldExpr )


(* ------------------------- *)
(* Parents *)
(* ------------------------- *)
let rec findParentOfWithin_ (eid : id) (haystack : expr) : expr option =
  let fpow = findParentOfWithin_ eid in
  (* the `or` of all items in the list *)
  let fpowList xs =
    xs |> List.map ~f:fpow |> List.filterMap ~f:identity |> List.head
  in
  if List.member ~value:eid (haystack |> children |> List.map ~f:P.toID)
  then Some haystack
  else
    match haystack with
    | Blank _ ->
        None
    | F (_, nexpr) ->
      ( match nexpr with
      | Value _ ->
          None
      | Variable _ ->
          None
      | Let (_, rhs, body) ->
          fpowList [rhs; body]
      | If (cond, ifbody, elsebody) ->
          fpowList [cond; ifbody; elsebody]
      | FnCall (_, exprs, _) ->
          fpowList exprs
      | Constructor (_, exprs) ->
          fpowList exprs
      | Lambda (_, lexpr) ->
          fpow lexpr
      | Thread exprs ->
          fpowList exprs
      | FieldAccess (obj, _) ->
          fpow obj
      | ListLiteral exprs ->
          fpowList exprs
      (* we don't check the children because it's done up top *)
      | ObjectLiteral pairs ->
          pairs |> List.map ~f:Tuple2.second |> fpowList
      | FeatureFlag (_, cond, a, b) ->
          fpowList [cond; a; b]
      | Match (matchExpr, cases) ->
          fpowList (matchExpr :: (cases |> List.map ~f:Tuple2.second))
      | FluidPartial (_, oldExpr) ->
          fpow oldExpr
      | FluidRightPartial (_, oldExpr) ->
          fpow oldExpr )


let findParentOfWithin (id : id) (haystack : expr) : expr =
  deOption "findParentOfWithin" <| findParentOfWithin_ id haystack


(* ------------------------- *)
(* Thread stuff *)
(* ------------------------- *)
let rec listThreadBlanks (expr : expr) : id list =
  let r = listThreadBlanks in
  let rList exprs = exprs |> List.map ~f:listThreadBlanks |> List.concat in
  let rn nexpr =
    match nexpr with
    | Value _ ->
        []
    | Variable _ ->
        []
    | Let (_, rhs, body) ->
        r rhs @ r body
    | FnCall (_, exprs, _) ->
        rList exprs
    | Constructor (_, args) ->
        rList args
    | Lambda (_, body) ->
        r body
    | FieldAccess (obj, _) ->
        r obj
    | If (cond, ifbody, elsebody) ->
        r cond @ r ifbody @ r elsebody
    | Thread exprs ->
        let blanks, filled = List.partition ~f:B.isBlank exprs in
        let blankids = List.map ~f:B.toID blanks in
        let subExprsBlankids = rList filled in
        blankids @ subExprsBlankids
    | ObjectLiteral pairs ->
        pairs |> List.map ~f:Tuple2.second |> rList
    | ListLiteral exprs ->
        rList exprs
    | FeatureFlag (_, cond, a, b) ->
        r cond @ r a @ r b
    | Match (matchExpr, cases) ->
        r matchExpr @ (cases |> List.map ~f:Tuple2.second |> rList)
    | FluidPartial (_, oldExpr) ->
        r oldExpr
    | FluidRightPartial (_, oldExpr) ->
        r oldExpr
  in
  match expr with Blank _ -> [] | F (_, f) -> rn f


let rec closeThreads (expr : expr) : expr =
  match expr with
  | F (id, Thread exprs) ->
      let addBlank =
        match exprs with
        | [] ->
            false
        | [_] ->
            false
        | F (_, FnCall (_, _, _)) :: _ ->
            false
        | _ ->
            true
      in
      let newExprs = List.filter ~f:B.isF exprs |> List.map ~f:closeThreads in
      let adjusted =
        match newExprs with
        (* if an fncall moved into the first slot, we need to add a *)
        (* blank in front. *)
        | F (id_, FnCall (name, args, r)) :: rest ->
            if addBlank
            then [F (id_, FnCall (name, B.new_ () :: args, r))] @ rest
            else [F (id_, FnCall (name, args, r))] @ rest
        | _ ->
            newExprs
      in
      ( match adjusted with
      | [] ->
          Blank id
      | [e] ->
          e
      | _ ->
          F (id, Thread adjusted) )
  | _ ->
      traverse closeThreads expr


let rec closeObjectLiterals (expr : expr) : expr =
  match expr with
  | F (id, ObjectLiteral pairs) ->
      pairs
      |> List.filterMap ~f:(fun (k, v) ->
             if B.isBlank k && B.isBlank v
             then None
             else Some (k, closeObjectLiterals v) )
      |> (fun l -> if l <> [] then l else [(B.new_ (), B.new_ ())])
      |> (fun x -> ObjectLiteral x)
      |> fun x -> F (id, x)
  | _ ->
      traverse closeObjectLiterals expr


let rec closeListLiterals (expr : expr) : expr =
  match expr with
  | F (id, ListLiteral exprs) ->
      let exprs2 = List.map ~f:closeListLiterals exprs in
      let exprs3 = List.filter ~f:B.isF exprs2 in
      F (id, ListLiteral (exprs3 @ [B.new_ ()]))
  | _ ->
      traverse closeObjectLiterals expr


let rec closeMatchPatterns (expr : expr) : expr =
  match expr with
  | F (id, Match (cond, pairs)) ->
      pairs
      |> List.filterMap ~f:(fun (p, e) ->
             if B.isBlank p && B.isBlank e
             then None
             else Some (p, closeMatchPatterns e) )
      |> (fun l -> if l <> [] then l else [(B.new_ (), B.new_ ())])
      |> (fun l -> Match (closeMatchPatterns cond, l))
      |> fun m -> F (id, m)
  | _ ->
      traverse closeMatchPatterns expr


let closeBlanks (expr : expr) : expr =
  expr
  |> closeThreads
  |> closeObjectLiterals
  |> closeListLiterals
  |> closeMatchPatterns


(* Find the child with the id `at` in the thread, and add a blank after it. *)
let extendThreadChild (at : id) (blank : expr) (threadExprs : expr list) :
    expr list =
  List.foldr
    ~f:(fun e list -> if B.toID e = at then e :: blank :: list else e :: list)
    ~init:[]
    threadExprs


let rec maybeExtendThreadAt (id : id) (blank : expr) (expr : expr) : expr =
  match expr with
  | F (tid, Thread exprs) ->
      let newExprs =
        extendThreadChild id blank exprs
        |> List.map ~f:(maybeExtendThreadAt id blank)
      in
      F (tid, Thread newExprs)
  | _ ->
      traverse (maybeExtendThreadAt id blank) expr


(* take an expression, and if *)
(* - it is a thread, add a blank at the end *)
(* - it is part of a thread, insert a blank just after the expr *)
(* - if it is not part of a thread, wrap it in a thread *)
let rec addThreadBlank (id : id) (blank : expr) (expr : expr) : expr =
  let atb = addThreadBlank id blank in
  if id = B.toID expr
  then
    match expr with
    | F (tid, Thread exprs) ->
        F (tid, Thread (exprs @ [blank]))
    | _ ->
        B.newF (Thread [expr; blank])
  else
    match expr with
    | F (tid, Thread exprs) ->
        let replaced = extendThreadChild id blank exprs in
        if replaced = exprs then traverse atb expr else F (tid, Thread replaced)
    | _ ->
        traverse atb expr


let addLambdaBlank (id : id) (expr : expr) : expr =
  match findParentOfWithin_ id expr with
  | Some (F (lid, Lambda (vars, body))) as old ->
      let r = F (lid, Lambda (vars @ [B.new_ ()], body)) in
      replace (old |> deOption "impossible" |> fun x -> PExpr x) (PExpr r) expr
  | _ ->
      expr


let addObjectLiteralBlanks (id : id) (expr : expr) : id * id * expr =
  match findExn id expr with
  | PKey _ ->
    ( match findParentOfWithin id expr with
    | F (olid, ObjectLiteral pairs) as old ->
        let newKey = B.new_ () in
        let newExpr = B.new_ () in
        let newPairs = pairs @ [(newKey, newExpr)] in
        let new_ = F (olid, ObjectLiteral newPairs) in
        let replacement = replace (PExpr old) (PExpr new_) expr in
        (B.toID newKey, B.toID newExpr, replacement)
    | _ ->
        impossible ("key parent must be object", id, expr) )
  | _ ->
      impossible ("must add to key", id, expr)


(* Extend the object literal automatically, only if it's the last key in *)
(* the object. *)
let maybeExtendObjectLiteralAt (pd : pointerData) (ast : expr) : expr =
  let id = P.toID pd in
  match pd with
  | PKey key ->
    ( match findParentOfWithin id ast with
    | F (_, ObjectLiteral pairs) ->
        if pairs |> List.last |> Option.map ~f:Tuple2.first |> ( = ) (Some key)
        then
          let _, _, replacement = addObjectLiteralBlanks id ast in
          replacement
        else ast
    | _ ->
        ast )
  | _ ->
      ast


let addListLiteralBlanks (id : id) (expr : expr) : expr =
  let new1 = B.new_ () in
  let parent = findParentOfWithin id expr in
  match parent with
  | F (lid, ListLiteral exprs) ->
      let newExprs =
        exprs
        |> List.reverse
        |> List.dropWhile ~f:B.isBlank
        |> ( @ ) [new1]
        |> List.reverse
      in
      replace (PExpr parent) (PExpr (F (lid, ListLiteral newExprs))) expr
  | _ ->
      expr


let maybeExtendListLiteralAt (pd : pointerData) (expr : expr) : expr =
  let id = P.toID pd in
  match findParentOfWithin_ id expr with
  | Some (F (_, ListLiteral exprs)) ->
      if exprs |> List.filter ~f:B.isBlank |> List.length |> fun l -> l <= 1
      then
        let replacement = addListLiteralBlanks id expr in
        replacement
      else expr
  | _ ->
      expr


let addPatternBlanks (id : id) (expr : expr) : id * id * expr =
  match findExn id expr with
  | PPattern _ ->
    ( match findParentOfWithin id expr with
    | F (olid, Match (cond, pairs)) as old ->
        let newPat = B.new_ () in
        let newExpr = B.new_ () in
        let pos =
          List.findIndex ~f:(fun (p2, _) -> B.toID p2 = id) pairs
          |> Option.withDefault ~default:0
        in
        let newPairs =
          List.insertAt ~index:(pos + 1) pairs ~value:(newPat, newExpr)
        in
        let new_ = F (olid, Match (cond, newPairs)) in
        let replacement = replace (PExpr old) (PExpr new_) expr in
        (B.toID newPat, B.toID newExpr, replacement)
    | _ ->
        impossible ("pattern parent must be match", id, expr) )
  | _ ->
      impossible ("must add to pattern", id, expr)


let maybeExtendPatternAt (pd : pointerData) (ast : expr) : expr =
  let id = P.toID pd in
  match pd with
  | PPattern pat ->
    ( match findParentOfWithin id ast with
    | F (_, Match (_, pairs)) ->
        if pairs |> List.last |> Option.map ~f:Tuple2.first |> ( = ) (Some pat)
        then
          let _, _, replacement = addPatternBlanks id ast in
          replacement
        else ast
    | _ ->
        ast )
  | _ ->
      ast


(* takes an ID of an expr in the AST to wrap in a thread *)
let rec wrapInThread (id : id) (expr : expr) : expr =
  if B.toID expr = id
  then
    match expr with
    | F (_, Thread _) ->
        expr
    | F (_, _) ->
        B.newF (Thread [expr; B.new_ ()])
    | Blank _ ->
        (* decide based on the displayed value, so flatten *)
        B.newF (Thread [expr])
  else traverse (wrapInThread id) expr


let grandparentIsThread (expr : expr) (parent : expr option) : bool =
  parent
  |> Option.map ~f:(fun p ->
         match findParentOfWithin_ (B.toID p) expr with
         | Some (F (_, Thread ts)) ->
             ts
             |> List.head
             |> Option.map ~f:(( <> ) p)
             |> Option.withDefault ~default:true
         | _ ->
             false )
  |> Option.withDefault ~default:false


let getParamIndex (expr : expr) (id : id) : (string * int) option =
  let parent = findParentOfWithin_ id expr in
  let inThread = grandparentIsThread expr parent in
  match parent with
  | Some (F (_, FnCall (F (_, name), args, _))) ->
      args
      |> List.findIndex ~f:(fun a -> B.toID a = id)
      |> Option.map ~f:(fun i -> if inThread then (name, i + 1) else (name, i))
  | _ ->
      None


let threadPrevious (id : id) (ast : expr) : expr option =
  let parent = findParentOfWithin_ id ast in
  match parent with
  | Some (F (_, Thread exprs)) ->
      exprs
      |> List.filter ~f:(fun e -> B.toID e = id)
      |> List.head
      |> Option.andThen ~f:(fun value -> Util.listPrevious ~value exprs)
  | _ ->
      None


let allCallsToFn (s : string) (e : expr) : expr list =
  e
  |> allData
  |> List.filterMap ~f:(fun pd ->
         match pd with
         | PExpr (F (id, FnCall (F (fnid, name), params, r))) ->
             if name = s
             then Some (F (id, FnCall (F (fnid, name), params, r)))
             else None
         | _ ->
             None )


let usesRail (ast : expr) : bool =
  List.any
    ~f:(fun e ->
      match e with PExpr (F (_, FnCall (_, _, Rail))) -> true | _ -> false )
    (allData ast)


(* ------------------------- *)
(* Ancestors *)
(* ------------------------- *)
let ancestors (id : id) (expr : expr) : expr list =
  let rec rec_ancestors (tofind : id) (walk : expr list) (exp : expr) =
    let rec_ id_ e_ walk_ = rec_ancestors id_ (e_ :: walk_) in
    let reclist id_ e_ walk_ exprs =
      exprs |> List.map ~f:(rec_ id_ e_ walk_) |> List.concat
    in
    if B.toID exp = tofind
    then walk
    else
      match exp with
      | Blank _ ->
          []
      | F (_, nexpr) ->
        ( match nexpr with
        | Value _ ->
            []
        | Variable _ ->
            []
        | Let (_, rhs, body) ->
            reclist id exp walk [rhs; body]
        | If (cond, ifbody, elsebody) ->
            reclist id exp walk [cond; ifbody; elsebody]
        | FnCall (_, exprs, _) ->
            reclist id exp walk exprs
        | Lambda (_, lexpr) ->
            rec_ id exp walk lexpr
        | Thread exprs ->
            reclist id exp walk exprs
        | FieldAccess (obj, _) ->
            rec_ id exp walk obj
        | ListLiteral exprs ->
            reclist id expr walk exprs
        | ObjectLiteral pairs ->
            pairs |> List.map ~f:Tuple2.second |> reclist id expr walk
        | FeatureFlag (_, cond, a, b) ->
            reclist id exp walk [cond; a; b]
        | Match (matchExpr, cases) ->
            reclist id exp walk (matchExpr :: List.map ~f:Tuple2.second cases)
        | Constructor (_, args) ->
            reclist id exp walk args
        | FluidPartial (_, oldExpr) ->
            rec_ id exp walk oldExpr
        | FluidRightPartial (_, oldExpr) ->
            rec_ id exp walk oldExpr )
  in
  rec_ancestors id [] expr


let ancestorsWhere (id : id) (expr : expr) (fn : expr -> bool) : expr list =
  List.filter ~f:fn (ancestors id expr)


let threadAncestors (id : id) (expr : expr) : expr list =
  ancestorsWhere id expr (fun e ->
      match e with F (_, Thread _) -> true | _ -> false )


let getValueParent (p : pointerData) (expr : expr) : pointerData option =
  let parent = findParentOfWithin_ (P.toID p) expr in
  match (P.typeOf p, parent) with
  | Expr, Some (F (_, Thread exprs)) ->
      exprs |> List.map ~f:(fun x -> PExpr x) |> Util.listPrevious ~value:p
  | Field, Some (F (_, FieldAccess (obj, _))) ->
      Some (PExpr obj)
  | Pattern, Some (F (_, Match (cond, _))) ->
      Some (PExpr cond)
  | _ ->
      None


let within (e : nExpr) (id : id) : bool =
  F (ID "invalidIDforASTwithin", e)
  |> allData
  |> List.map ~f:P.toID
  |> List.member ~value:id


let rec clonePattern (pattern : pattern) : pattern =
  let cNPattern npat =
    match npat with
    | PLiteral _ | PVariable _ ->
        npat
    | PConstructor (name, args) ->
        PConstructor (name, List.map ~f:clonePattern args)
  in
  B.clone cNPattern pattern


let rec clone (expr : expr) : expr =
  let c be = clone be in
  let cl bes = List.map ~f:c bes in
  let cString = B.clone identity in
  let cNExpr nexpr =
    match nexpr with
    | Let (lhs, rhs, body) ->
        Let (cString lhs, c rhs, c body)
    | If (cond, ifbody, elsebody) ->
        If (c cond, c ifbody, c elsebody)
    | FnCall (name, exprs, r) ->
        FnCall (name, cl exprs, r)
    | Lambda (vars, body) ->
        Lambda (List.map ~f:cString vars, c body)
    | Thread exprs ->
        Thread (cl exprs)
    | FieldAccess (obj, field) ->
        FieldAccess (c obj, cString field)
    | Value v ->
        Value v
    | Variable name ->
        Variable name
    | ListLiteral exprs ->
        ListLiteral (cl exprs)
    | ObjectLiteral pairs ->
        ObjectLiteral (List.map ~f:(fun (k, v) -> (cString k, c v)) pairs)
    | FeatureFlag (msg, cond, a, b) ->
        FeatureFlag (cString msg, c cond, c a, c b)
    | Match (matchExpr, cases) ->
        Match
          (c matchExpr, List.map ~f:(fun (k, v) -> (clonePattern k, c v)) cases)
    | Constructor (name, args) ->
        Constructor (cString name, cl args)
    | FluidPartial (str, oldExpr) ->
        FluidPartial (str, c oldExpr)
    | FluidRightPartial (str, oldExpr) ->
        FluidRightPartial (str, c oldExpr)
  in
  B.clone cNExpr expr


let isDefinitionOf (var : varName) (exp : expr) : bool =
  match exp with
  | Blank _ ->
      false
  | F (_, e) ->
    ( match e with
    | Let (b, _, _) ->
      (match b with Blank _ -> false | F (_, vb) -> vb = var)
    | Lambda (vars, _) ->
        vars
        |> List.any ~f:(fun v ->
               match v with Blank _ -> false | F (_, vb) -> vb = var )
    | _ ->
        false )


let freeVariables (ast : expr) : (id * varName) list =
  (* Find all variable lookups that lookup a variable that
   * is also _defined_ in this expression. We create a set of
   * these IDs so we can filter them out later. *)
  let definedAndUsed =
    ast
    |> allData
    |> List.filterMap ~f:(fun n ->
           match n with
           | PExpr boe ->
             ( match boe with
             | Blank _ ->
                 None
             | F (_, e) ->
               ( match e with
               (* Grab all uses of the `lhs` of a Let in its body *)
               | Let (F (_, lhs), _, body) ->
                   Some (uses lhs body)
               (* Grab all uses of the `vars` of a Lambda in its body *)
               | Lambda (vars, body) ->
                   vars
                   |> List.filterMap ~f:B.toMaybe
                   |> List.map ~f:(fun v -> uses v body)
                   |> List.concat
                   |> fun x -> Some x
               | Match (_, cases) ->
                   cases
                   (* Grab all uses of the variable bindings in a `pattern`
                    * in the `body` of each match case *)
                   |> List.map ~f:(fun (pattern, body) ->
                          let vars = Pattern.variableNames pattern in
                          List.map ~f:(fun v -> uses v body) vars )
                   |> List.concat
                   |> List.concat
                   |> fun x -> Some x
               | _ ->
                   None ) )
           | _ ->
               None )
    |> List.concat
    |> List.map ~f:(B.toID >> deID)
    |> StrSet.fromList
  in
  ast
  |> allData
  |> List.filterMap ~f:(fun n ->
         match n with
         | PExpr boe ->
           ( match boe with
           | Blank _ ->
               None
           | F (id, e) ->
             ( match e with
             | Variable name ->
                 (* Don't include Variable lookups that we know are looking
                  * up a variable bound in this expression *)
                 if StrSet.member ~value:(deID id) definedAndUsed
                 then None
                 else Some (id, name)
             | _ ->
                 None ) )
         | _ ->
             None )
  |> List.uniqueBy ~f:(fun (_, name) -> name)


module SymSet = StrDict
module IDTable = Belt.MutableMap.String

type sym_set = id SymSet.t

type sym_store = sym_set IDTable.t

let updateDictWithList st l =
  l
  |> List.foldl ~init:st ~f:(fun v d ->
         match v with
         | F (id, varname) ->
             SymSet.update ~key:varname ~f:(fun _v -> Some id) d
         | Blank _ ->
             d )


let rec sym_exec
    ~(trace : expr -> sym_set -> unit) (st : sym_set) (expr : expr) : unit =
  let sexe = sym_exec ~trace in
  ignore
    ( match expr with
    | Blank _ ->
        ()
    | F (_, Value _) ->
        ()
    | F (_, Variable _) ->
        ()
    | F (_, Let (lhs, rhs, body)) ->
        sexe st rhs ;
        let bound =
          match lhs with
          | F (id, name) ->
              SymSet.update ~key:name ~f:(fun _v -> Some id) st
          | Blank _ ->
              st
        in
        sexe bound body
    | F (_, FnCall (_, exprs, _)) ->
        List.iter ~f:(sexe st) exprs
    | F (_, If (cond, ifbody, elsebody))
    | F (_, FeatureFlag (_, cond, elsebody, ifbody)) ->
        sexe st cond ;
        sexe st ifbody ;
        sexe st elsebody
    | F (_, Lambda (vars, body)) ->
        let new_st =
          vars
          |> List.foldl ~init:st ~f:(fun v d ->
                 match v with
                 | F (id, varname) ->
                     SymSet.update ~key:varname ~f:(fun _v -> Some id) d
                 | Blank _ ->
                     d )
        in
        sexe new_st body
    | F (_, Thread exprs) ->
        List.iter ~f:(sexe st) exprs
    | F (_, FieldAccess (obj, _)) ->
        sexe st obj
    | F (_, ListLiteral exprs) ->
        List.iter ~f:(sexe st) exprs
    | F (_, Match (matchExpr, cases)) ->
        let rec variables_in_pattern p =
          match p with
          | Blank _ ->
              []
          | F (_, PLiteral _) ->
              []
          | F (id, PVariable v) ->
              [(id, v)]
          | F (_, PConstructor (_, inner)) ->
              inner |> List.map ~f:variables_in_pattern |> List.concat
        in
        sexe st matchExpr ;
        List.iter cases ~f:(fun (p, caseExpr) ->
            let new_st =
              p
              |> variables_in_pattern
              |> List.foldl ~init:st ~f:(fun v d ->
                     let id, varname = v in
                     SymSet.update ~key:varname ~f:(fun _v -> Some id) d )
            in
            sexe new_st caseExpr )
    | F (_, ObjectLiteral exprs) ->
        exprs |> List.map ~f:Tuple2.second |> List.iter ~f:(sexe st)
    | F (_, Constructor (_, args)) ->
        List.iter ~f:(sexe st) args
    | F (_, FluidPartial (_, oldExpr)) ->
        sexe st oldExpr
    | F (_, FluidRightPartial (_, oldExpr)) ->
        sexe st oldExpr ) ;
  trace expr st


let variablesIn (ast : expr) : avDict =
  let sym_store = IDTable.make () in
  let trace expr st = IDTable.set sym_store (deID (Blank.toID expr)) st in
  sym_exec ~trace SymSet.empty ast ;
  sym_store |> IDTable.toList |> StrDict.fromList
