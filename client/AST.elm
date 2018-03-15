module AST exposing (..)

-- builtin
import List

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P
import Blank


-------------------------
-- Generic
-------------------------
toP : Expr -> Pointer
toP e =
  case e of
    Hole id -> PBlank Expr id
    _ -> PFilled Expr (toID e)

toPD : Expr -> PointerData
toPD e =
  PExpr (toID e) e


toID : Expr -> ID
toID expr =
  case expr of
    Value id _ -> id
    Let id _ _ _ -> id
    If id _ _ _ -> id
    Variable id _ -> id
    FnCall id _ _ -> id
    Lambda id _ _ -> id
    Hole id -> id
    Thread id _ -> id
    FieldAccess id _ _ -> id


isHole : Expr -> Bool
isHole e =
  case e of
    Hole _ -> True
    _ -> False

-------------------------
-- Thread stuff
-------------------------
listThreadHoles : Expr -> List ID
listThreadHoles expr =
  let lthList : List Expr -> List ID
      lthList exprs =
        exprs
        |> List.map listThreadHoles
        |> List.concat
  in
  case expr of
    Value _ v ->
      []

    Let _ lhs rhs expr ->
      lthList [rhs, expr]

    If _ cond ifbody elsebody ->
      lthList [cond, ifbody, elsebody]

    Variable _ name ->
      []

    FnCall _ name exprs ->
      lthList exprs

    Lambda _ vars expr ->
      listThreadHoles expr

    Hole id -> []

    Thread _ exprs ->
      let (holes, notHoles) = List.partition isHole exprs
          holeids = List.map toID holes
          subExprsHoleids = lthList notHoles
      in
          holeids ++ subExprsHoleids

    FieldAccess _ obj _ ->
      listThreadHoles obj


closeThread : Expr -> Expr
closeThread expr =
  -- Close all threads
  let ct = closeThread
      ctList = List.map ct
  in
  case expr of
    Value _ _ -> expr
    Hole _ -> expr
    Variable _ _ -> expr

    Let id lhs rhs expr ->
      Let id lhs (ct rhs) (ct expr)

    If id cond ifbody elsebody ->
      If id (ct cond) (ct ifbody) (ct elsebody)


    FnCall id name exprs ->
      FnCall id name (ctList exprs)

    Lambda id vars expr ->
      Lambda id vars (ct expr)

    FieldAccess id obj name ->
      -- Probably don't want threading in a field access,
      -- but we'll make this work anyway
      FieldAccess id (ct obj) name

    Thread tid exprs ->
      let filtered = List.filter (isHole >> not) exprs
          newExprs = ctList filtered
      in
        case newExprs of
          [] ->
            Hole tid
          [e] ->
            e
          _ ->
            Thread tid newExprs

-- take an expression, and if
-- * it is a thread, add a hole at the end
-- * it is part of a thread, insert a hole just after the expr
-- * if it is not part of a thread, wrap it in a thread
addThreadHole : ID -> Expr -> Expr
addThreadHole id expr =
  let ath child = addThreadHole id child in
  if id == toID expr
  then
    case expr of
      Thread tid exprs ->
        Thread tid (exprs ++ [Hole (gid ())])
      _ ->
        Thread (gid ()) [expr, Hole (gid ())]
  else
    case expr of
      Value _ _ -> expr
      Hole _ -> expr
      Variable _ _ -> expr

      Let id lhs rhs expr ->
        Let id lhs (ath rhs) (ath expr)

      If id cond ifbody elsebody ->
        If id (ath cond) (ath ifbody) (ath elsebody)

      FnCall id name exprs ->
        FnCall id name (List.map ath exprs)

      Lambda id vars expr ->
        Lambda id vars (ath expr)

      FieldAccess id obj name ->
        FieldAccess id (ath obj) name

      Thread tid exprs ->
        let replaced = extendThreadChild id exprs in
        if replaced == exprs
        then Thread tid (List.map ath exprs)
        else Thread tid replaced



-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> Expr -> Expr
wrapInThread id expr =
  let wt e = wrapInThread id e
      wrap e =
        case e of
          Thread _ _ -> e
          _ -> Thread (gid ()) [e, Hole (gid ())]
      nested =
        case expr of
          Value _ _ -> expr
          Hole _ -> expr
          Variable _ _ -> expr

          Let id lhs rhs body ->
            Let id lhs (wt rhs) (wt body)

          If id cond ifbody elsebody ->
            If id (wt cond) (wt ifbody) (wt elsebody)

          FnCall id name exprs ->
            FnCall id name (List.map wt exprs)

          Lambda id vars lexpr ->
            Lambda id vars (wt lexpr)

          Thread id exprs ->
            Thread id (List.map wt exprs)

          FieldAccess id obj field ->
            FieldAccess id (wt obj) field
  in if (toID expr) == id
     then wrap expr
     else nested

-- Find the child with the id `at` in the threadExpr, and add a hole after it.
extendThreadChild : ID -> List Expr -> List Expr
extendThreadChild at threadExprs =
  List.foldr (\e list ->
    if (toID e) == at
    then e :: Hole (gid ()) :: list
    else e :: list)
    [] threadExprs


-- extends thread at pos denoted by ID, if ID is in a thread
maybeExtendThreadAt : ID -> Expr -> Expr
maybeExtendThreadAt id expr =
  let et e = maybeExtendThreadAt id e
  in
    case expr of
      Value _ _ -> expr
      Hole _ -> expr
      Variable _ _ -> expr

      Let id lhs rhs body ->
        Let id lhs (et rhs) (et body)

      If id cond ifbody elsebody ->
        If id (et cond) (et ifbody) (et elsebody)

      FnCall id name exprs ->
        FnCall id name (List.map et exprs)

      Lambda id vars lexpr ->
        Lambda id vars (et lexpr)

      Thread tid exprs ->
        let newExprs = extendThreadChild id exprs
        in Thread tid (List.map et newExprs)

      FieldAccess id obj field ->
        FieldAccess id (et obj) field

isThread : Expr -> Pointer -> Bool
isThread ast p =
  ast |> listThreadHoles |> List.member (P.idOf p)


-------------------------
-- Children
-------------------------
children : Expr -> List Pointer
children e =
  case e of
    Value _ _ -> []
    Hole _ -> []
    Variable _ _ -> []
    If _ cond ifbody elsebody ->
      [toP cond, toP ifbody, toP elsebody]
    FnCall _ name exprs ->
      List.map toP exprs
    Lambda _ vars lexpr ->
      [toP lexpr]
    Thread _ exprs ->
      List.map toP exprs
    FieldAccess _ obj field ->
      [toP obj, Blank.toP Field field]
    Let _ lhs rhs body ->
      [Blank.toP VarBind lhs, toP rhs, toP body]

childrenOf : ID -> Expr -> List Pointer
childrenOf pid expr =
  let co = childrenOf pid
      returnOr fn e =
        if pid == (toID e)
        then children e
        else fn e
  in
  case expr of
    Value _ _ -> []
    Hole _ -> []
    Variable _ _ -> []
    Let id lhs rhs body ->
      returnOr (\_ -> List.concat [co body, co rhs]) expr

    If id cond ifbody elsebody ->
      returnOr (\_ ->
        let c  = co cond
            ib = co ifbody
            eb = co elsebody
        in
            List.concat [c, ib, eb]) expr

    FnCall id name exprs ->
      returnOr (\_ -> exprs |> List.map co |> List.concat) expr

    Lambda id vars lexpr ->
      returnOr (\_ -> co lexpr) expr

    Thread id exprs ->
      returnOr (\_ -> exprs |> List.map co |> List.concat) expr

    FieldAccess id obj field ->
      returnOr (\_ -> co obj) expr

-------------------------
-- Ancestors
-------------------------
ancestors : ID -> Expr -> List Expr
ancestors id expr =
  let rec_ancestors : ID -> List Expr -> Expr -> List Expr
      rec_ancestors tofind walk exp =
        let rec id e walk = rec_ancestors id (e :: walk)
            reclist id e walk exprs =
              exprs |> List.map (rec id e walk) |> List.concat
        in
        if toID exp == tofind
        then walk
        else
          case exp of
            Value _ _ -> []
            Hole _ -> []
            Variable _ _ -> []
            Let i lhs rhs body ->
              reclist id exp walk [rhs, body]
            If i cond ifbody elsebody ->
              reclist id exp walk [cond, ifbody, elsebody]
            FnCall i name exprs ->
              reclist id exp walk exprs
            Lambda i vars lexpr ->
              rec id exp walk lexpr
            Thread i exprs ->
              reclist id exp walk exprs
            FieldAccess i obj field ->
              rec id exp walk obj
  in
      rec_ancestors id [] expr


ancestorsWhere : ID -> Expr -> (Expr -> Bool) -> List Expr
ancestorsWhere id expr fn =
  List.filter fn (ancestors id expr)

threadAncestors : ID -> Expr -> List Expr
threadAncestors id expr =
  ancestorsWhere id expr
    (\e ->
      case e of
        Thread _ _ -> True
        _ -> False)


-------------------------
-- Parents
-------------------------
parentOf : ID -> Expr -> Expr
parentOf id ast =
  deMaybe "parentOf" <| parentOf_ id ast

parentOf_ : ID -> Expr -> Maybe Expr
parentOf_ eid expr =
  let po = parentOf_ eid
      returnOr : (Expr -> Maybe Expr) -> Expr -> Maybe Expr
      returnOr fn e =
        if List.member eid (children e |> List.map P.idOf)
        then Just e
        else fn e
      filterMaybe xs = xs |> List.filterMap identity |> List.head
  in
  case expr of
    Value _ _ -> Nothing
    Hole _ -> Nothing
    Variable _ _ -> Nothing
    Let id lhs rhs body ->
      returnOr (\_ -> filterMaybe [po body, po rhs]) expr

    If id cond ifbody elsebody ->
      returnOr (\_ ->
        let c  = po cond
            ib = po ifbody
            eb = po elsebody
        in
            filterMaybe [c, ib, eb]) expr

    FnCall id name exprs ->
      returnOr (\_ -> exprs |> List.map po |> filterMaybe) expr

    Lambda id vars lexpr ->
      returnOr (\_ -> po lexpr) expr

    Thread id exprs ->
      returnOr (\_ -> exprs |> List.map po |> filterMaybe) expr

    FieldAccess id obj field ->
      if Blank.toID field == eid
      then Just expr
      else returnOr (\_ -> po obj) expr

-- includes self
siblings : Pointer -> Expr -> List Pointer
siblings p ast =
  case parentOf_ (P.idOf p) ast of
    Nothing -> [p]
    Just parent ->
      case parent of
        If _ cond ifbody elsebody ->
          List.map toP [cond, ifbody, elsebody]

        Let _ lhs rhs body ->
          [Blank.toP VarBind lhs, toP rhs, toP body]

        FnCall _ name exprs ->
          List.map toP exprs

        Lambda _ vars lexpr ->
          [toP lexpr]

        Thread _ exprs ->
          List.map toP exprs

        FieldAccess _ obj field ->
          [toP obj, Blank.toP Field field]

        _ -> [p]

getValueParent : Pointer -> Expr -> Maybe Pointer
getValueParent p expr =
  let id = P.idOf p
      parent = parentOf_ id expr
  in
  case P.typeOf p of
    Expr ->
      case parent of
        Just (Thread _ exprs) ->
          exprs
          |> List.map toP
          |> Util.listPrevious p
        _ ->
          Nothing

    Field ->
      case parent of
        Just (FieldAccess id obj _) ->
          Just <| toP obj
        _ ->
          Nothing
    _ ->
      Nothing



--------------------------------
-- Pointers
--------------------------------

allPointers : Expr -> List Pointer
allPointers expr =
  let rl : List Expr -> List Pointer
      rl exprs =
        exprs
        |> List.map allPointers
        |> List.concat
  in
  [toP expr] ++
  case expr of
    Value _ v -> []
    Variable _ name -> []
    Hole id -> []

    Let _ lhs rhs expr ->
      [Blank.toP VarBind lhs] ++ rl [rhs, expr]

    If _ cond ifbody elsebody ->
      rl [cond, ifbody, elsebody]

    FnCall _ name exprs ->
      rl exprs

    Lambda _ vars expr ->
      allPointers expr

    Thread _ exprs ->
      rl exprs

    FieldAccess _ obj field ->
      allPointers obj ++ [Blank.toP Field field]


--------------------------------
-- PointersData
--------------------------------

listData : Expr -> List PointerData
listData expr =
  let e2ld e = PExpr (toID e) e
      rl : List Expr -> List PointerData
      rl exprs =
        exprs
        |> List.map listData
        |> List.concat
  in
  [e2ld expr] ++
  case expr of
    Value _ v -> []
    Variable _ name -> []
    Hole id -> []

    Let _ lhs rhs expr ->
      [PVarBind (Blank.toID lhs) lhs] ++ rl [rhs, expr]

    If _ cond ifbody elsebody ->
      rl [cond, ifbody, elsebody]

    FnCall _ name exprs ->
      rl exprs

    Lambda _ vars expr ->
      listData expr

    Thread _ exprs ->
      rl exprs

    FieldAccess _ obj field ->
      listData obj ++ [PField (Blank.toID field) field]



subtree : ID -> Expr -> PointerData
subtree id ast =
  deMaybe "subtree" (subData id ast)

subData : ID -> Expr -> Maybe PointerData
subData id expr =
  listData expr
  |> List.filter (\d -> id == P.idOfD d)
  |> List.head -- TODO might be multiple

toContent : PointerData -> String
toContent pd =
  case pd of
    PVarBind _ v -> v |> Blank.toMaybe |> Maybe.withDefault ""
    PField _ f -> f |> Blank.toMaybe |> Maybe.withDefault ""
    PExpr _ e ->
      case e of
        Value _ s -> s
        Variable _ v -> v
        _ -> ""
    PEventModifier _ _ -> ""
    PEventName _ _ -> ""
    PEventSpace _ _ -> ""
    PDBColName _ _ -> ""
    PDBColType _ _ -> ""
    PDarkType _ _ -> ""
    PDarkTypeField _ _ -> ""


replace : Pointer -> PointerData -> Expr -> Expr
replace p replacement expr =
  let re = replace p replacement
      rl : List Expr -> List Expr
      rl exprs = List.map re exprs
  in
  if toID expr == P.idOf p
  then
    case replacement of
      PExpr _ e -> e
      _ -> expr
  else
    case expr of
      Let id lhs rhs expr ->
        if Blank.toID lhs == (P.idOf p)
        then
          case replacement of
            PVarBind _ b ->
              Let id b rhs expr
            _ -> expr
        else
          Let id lhs (re rhs) (re expr)

      If id cond ifbody elsebody ->
        If id (re cond) (re ifbody) (re elsebody)

      FnCall id name exprs ->
        FnCall id name (rl exprs)

      Lambda id vars expr ->
        Lambda id vars (re expr)

      Thread id exprs ->
        Thread id (rl exprs)

      FieldAccess id obj field ->
        if Blank.toID field == (P.idOf p)
        then
          case replacement of
            PField _ f ->
              FieldAccess id obj f
            _ -> expr
        else
          FieldAccess id (re obj) field

      Hole id -> expr
      Value id v -> expr
      Variable id name -> expr

deleteExpr : Pointer -> Expr -> ID -> Expr
deleteExpr p ast id =
  let replacement =
        case P.typeOf p of
          VarBind -> PVarBind id (Blank id)
          Expr -> PExpr id (Hole id)
          Field -> PField id (Blank id)
          tipe  -> Debug.crash <| (toString tipe) ++ " is not allowed in an AST"
  in replace p replacement ast

replaceVarBind : Pointer -> VarName -> Expr -> Expr
replaceVarBind p replacement expr =
  let id = gid ()
  in replace p (PVarBind id (Filled id replacement)) expr


replaceField : Pointer -> FieldName -> Expr -> Expr
replaceField p replacement expr =
  let id = gid ()
  in replace p (PField id (Filled id replacement)) expr


clone : Expr -> (ID, Expr)
clone expr =
  let nid = gid ()
      c e =
        let (_, ast) = clone e
        in ast
      cl es =
        List.map c es
      cBlankOr bo =
        let nbid = gid () in
        case bo of
          Blank _ -> Blank nbid
          Filled _ a -> Filled nbid a
  in
    case expr of
      Let _ lhs rhs expr ->
        (nid, Let nid (cBlankOr lhs) (c rhs) (c expr))

      If _ cond ifbody elsebody ->
        (nid, If nid  (c cond) (c ifbody) (c elsebody))

      FnCall _ name exprs ->
        (nid, FnCall nid name (cl exprs))

      Lambda _ vars expr ->
        (nid, Lambda nid vars (c expr))

      Thread _ exprs ->
        (nid, Thread nid (cl exprs))

      FieldAccess _ obj field ->
        (nid, FieldAccess nid (c obj) (cBlankOr field))

      Hole _ ->
        (nid, Hole nid)
      Value _ v ->
        (nid, Value nid v)
      Variable _ name ->
        (nid, Variable nid name)


