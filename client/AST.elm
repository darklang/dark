module AST exposing (..)

-- builtin
import List

-- lib
import List.Extra as LE
-- import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P

isInfix : FnName -> Bool
isInfix name =
  List.member name [ "<", ">", "<=", ">="
                   , "==", "!="
                   , "%", "+", "-", "*"
                   , "^"
                   , "&&", "||"]

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
          init = LE.init newExprs |> Maybe.withDefault []
          last = LE.last newExprs |> deMaybe "last in thread"
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
      [toP obj, P.blankTo Field field]
    Let _ lhs rhs body ->
      [P.blankTo VarBind lhs, toP rhs, toP body]

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

ancestorsWhere : ID -> Expr -> (Expr -> Bool) -> List Expr
ancestorsWhere id expr fn =
  if toID expr == id
  then []
  else
    let this = if fn expr then [expr] else []
        r e = ancestorsWhere id e fn
        rlist es = es |> List.map r |> List.concat
        nested =
          case expr of
            Value _ _ -> []
            Hole _ -> []
            Variable _ _ -> []

            Let id lhs rhs body ->
              rlist [rhs, body]

            If id cond ifbody elsebody ->
              rlist [cond, ifbody, elsebody]

            FnCall id name exprs ->
              rlist exprs

            Lambda id vars lexpr ->
              -- don't recurse into the body of a lambda
              []

            Thread id exprs ->
              rlist exprs

            FieldAccess id obj field ->
              r obj

    in this ++ nested

threadAncestors : ID -> Expr -> List Expr
threadAncestors id expr =
  ancestorsWhere id expr
    (\e ->
      case e of
        Thread _ _ -> True
        _ -> False)


parentOf : ID -> AST -> AST
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
      if blankOrID field == eid
      then Just expr
      else returnOr (\_ -> po obj) expr

-- includes self
siblings : Pointer -> AST -> List Pointer
siblings p ast =
  case parentOf_ (P.idOf p) ast of
    Nothing -> [p]
    Just parent ->
      case parent of
        If _ cond ifbody elsebody ->
          List.map toP [cond, ifbody, elsebody]

        Let _ lhs rhs body ->
          [P.blankTo VarBind lhs, toP rhs, toP body]

        FnCall _ name exprs ->
          List.map toP exprs

        Lambda _ vars lexpr ->
          [toP lexpr]

        Thread _ exprs ->
          List.map toP exprs

        FieldAccess _ obj field ->
          [toP obj, P.blankTo Field field]

        _ -> [p]

--------------------------------
-- PointerList functions
--------------------------------

inAST : ID -> AST -> Bool
inAST id ast =
  ast
  |> listPointers
  |> List.map P.idOf
  |> List.member id

listBlanks : Expr -> List Pointer
listBlanks expr =
  listPointers expr
  |> List.filter P.isBlank

listPointers : Expr -> List Pointer
listPointers expr =
  let rl : List Expr -> List Pointer
      rl exprs =
        exprs
        |> List.map listPointers
        |> List.concat
  in
  [toP expr] ++
  case expr of
    Value _ v -> []
    Variable _ name -> []
    Hole id -> []

    Let _ lhs rhs expr ->
      [P.blankTo VarBind lhs] ++ rl [rhs, expr]

    If _ cond ifbody elsebody ->
      rl [cond, ifbody, elsebody]

    FnCall _ name exprs ->
      rl exprs

    Lambda _ vars expr ->
      listPointers expr

    Thread _ exprs ->
      rl exprs

    FieldAccess _ obj field ->
      listPointers obj ++ [P.blankTo Field field]


--------------------------------
-- PointerData functions
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
      [PVarBind (blankOrID lhs) lhs] ++ rl [rhs, expr]

    If _ cond ifbody elsebody ->
      rl [cond, ifbody, elsebody]

    FnCall _ name exprs ->
      rl exprs

    Lambda _ vars expr ->
      listData expr

    Thread _ exprs ->
      rl exprs

    FieldAccess _ obj field ->
      listData obj ++ [PField (blankOrID field) field]



subtree : ID -> AST -> PointerData
subtree id ast =
  deMaybe "subtree" (subData id ast)

subData : ID -> Expr -> Maybe PointerData
subData id expr =
  listData expr
  |> List.filter (\d -> id == P.idOfD d)
  |> List.head -- TODO might be multiple

isLeaf : ID -> AST -> Bool
isLeaf id ast =
  case subData id ast of
    Nothing -> False
    Just pd ->
      case pd of
        PVarBind _ _ -> True
        PField _ _ -> True
        PExpr _ e ->
          case e of
            Value _ _ -> True
            Hole _ -> True
            Variable _ _ -> True
            FnCall _ _ params -> -- Constant, or piped-in function
              (List.length params) == 0
            _ -> False
        PSpec _ _ -> False
        PDBColName _ _ -> False
        PDBColType _ _ -> False

toContent : PointerData -> String
toContent pd =
  case pd of
    PVarBind _ v -> v |> blankToMaybe |> Maybe.withDefault ""
    PField _ f -> f |> blankToMaybe |> Maybe.withDefault ""
    PExpr _ e ->
      case e of
        Value _ s -> s
        Variable _ v -> v
        _ -> ""
    PSpec _ _ -> ""
    PDBColName _ _ -> ""
    PDBColType _ _ -> ""


replace : Pointer -> PointerData -> AST -> AST
replace p replacement expr =
  let re = replace p replacement
      rl : List Expr -> List Expr
      rl exprs = List.map re exprs
      fail datatype =
        Debug.crash ("cant replace a "
                     ++ datatype
                     ++ " with a "
                     ++ toString replacement)

  in
  if toID expr == P.idOf p
  then
    case replacement of
      PExpr _ e -> e
      _ -> fail "expr"
  else
    case expr of
      Let id lhs rhs expr ->
        if blankOrID lhs == (P.idOf p)
        then
          case replacement of
            PVarBind _ b ->
              Let id b rhs expr
            _ -> fail "varbind"
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
        if blankOrID field == (P.idOf p)
        then
          case replacement of
            PField _ f ->
              FieldAccess id obj f
            _ -> fail "field"
        else
          FieldAccess id (re obj) field

      Hole id -> expr
      Value id v -> expr
      Variable id name -> expr

deleteExpr : Pointer -> AST -> (Pointer, AST)
deleteExpr p ast =
  let id = gid ()
      replacement =
        case P.typeOf p of
          VarBind -> PVarBind id (Blank id)
          Expr -> PExpr id (Hole id)
          Field -> PField id (Blank id)
          tipe  -> Debug.crash <| (toString tipe) ++ " is not allowed in an AST"
  in (PBlank (P.typeOf p) id, replace p replacement ast)

replaceVarBind : Pointer -> VarName -> Expr -> Expr
replaceVarBind p replacement expr =
  let id = gid ()
  in replace p (PVarBind id (Filled id replacement)) expr


replaceField : Pointer -> FieldName -> Expr -> Expr
replaceField p replacement expr =
  let id = gid ()
  in replace p (PField id (Filled id replacement)) expr


