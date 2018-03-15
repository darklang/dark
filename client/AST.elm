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
  PExpr (o2n e)


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
listThreadBlanks : BExpr -> List ID
listThreadBlanks expr =
  let rb = listThreadBlanks
      ltList : List BExpr -> List ID
      ltList exprs =
        exprs
        |> List.map listThreadBlanks
        |> List.concat
      re expr = case expr of
                  NValue v -> []
                  NVariable name -> []

                  NLet lhs rhs expr -> rb rhs ++ rb expr
                  NFnCall name exprs -> ltList exprs
                  NLambda vars expr -> rb expr
                  NFieldAccess obj _ -> rb obj

                  NIf cond ifbody elsebody ->
                    rb cond ++ rb ifbody ++ rb elsebody

                  NThread exprs ->
                    let (blanks, filled) = List.partition Blank.isBlank exprs
                        blankids = List.map Blank.toID blanks
                        subExprsBlankids = ltList filled
                    in
                        blankids ++ subExprsBlankids
  in case expr of
      Blank _ -> [Blank.toID expr]
      F _ f -> re f


closeThread : BExpr -> BExpr
closeThread bexpr =
  -- Close all threads
  let ctb = closeThread
      ctbList = List.map ctb
      cte expr = case expr of
                   NValue _ -> expr
                   NVariable _ -> expr

                   NLet lhs rhs expr ->
                     NLet lhs (ctb rhs) (ctb expr)

                   NIf cond ifbody elsebody ->
                     NIf (ctb cond) (ctb ifbody) (ctb elsebody)

                   NFnCall name exprs ->
                     NFnCall name (ctbList exprs)

                   NLambda vars expr ->
                     NLambda vars (ctb expr)

                   NFieldAccess obj name ->
                     -- Probably don't want threading in a field access,
                     -- but we'll make this work anyway
                     NFieldAccess (ctb obj) name

                   NThread exprs ->
                     NThread (ctbList exprs)
  in case bexpr of
      F id (NThread exprs) ->
        let filtered = List.filter Blank.isF exprs
            newExprs = ctbList filtered
        in
        case newExprs of
          [] -> Blank id
          [e] -> e
          _ -> F id (NThread newExprs)
      F id e -> F id (cte e)
      Blank _ -> bexpr


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
      Thread tid exprs ->
        let replaced = extendThreadChild id exprs in
        if replaced == exprs
        then Thread tid (List.map ath exprs)
        else Thread tid replaced

      _ -> traverse ath expr


traverse : (Expr -> Expr) -> Expr -> Expr
traverse fn expr =
  case expr of
    Value _ _ -> expr
    Hole _ -> expr
    Variable _ _ -> expr

    Let id lhs rhs body ->
      Let id lhs (fn rhs) (fn body)

    If id cond ifbody elsebody ->
      If id (fn cond) (fn ifbody) (fn elsebody)

    FnCall id name exprs ->
      FnCall id name (List.map fn exprs)

    Lambda id vars lexpr ->
      Lambda id vars (fn lexpr)

    Thread id exprs ->
      Thread id (List.map fn exprs)

    FieldAccess id obj field ->
      FieldAccess id (fn obj) field


traverseBExpr : (BExpr -> BExpr) -> BExpr -> BExpr
traverseBExpr fn bexpr =
  case bexpr of
    Blank _ -> bexpr
    F id expr ->
      F id
        (case expr of
          NValue _ -> expr
          NVariable _ -> expr

          NLet lhs rhs body ->
            NLet lhs (fn rhs) (fn body)

          NIf cond ifbody elsebody ->
            NIf (fn cond) (fn ifbody) (fn elsebody)

          NFnCall name exprs ->
            NFnCall name (List.map fn exprs)

          NLambda vars lexpr ->
            NLambda vars (fn lexpr)

          NThread exprs ->
            NThread (List.map fn exprs)

          NFieldAccess obj field ->
            NFieldAccess (fn obj) field)



-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> BExpr -> BExpr
wrapInThread id bexpr =
  if Blank.toID bexpr == id
  then
    case bexpr of
      F _ (NThread _) -> bexpr
      F _ expr -> Blank.newF (NThread [bexpr, Blank.new ()])
      Blank _ -> Blank.newF (NThread [bexpr])
  else
    traverseBExpr (wrapInThread id) bexpr

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
  case expr of
    Thread tid exprs ->
      let newExprs = extendThreadChild id exprs
                     |> List.map (maybeExtendThreadAt id)
      in Thread tid newExprs
    _ -> traverse (maybeExtendThreadAt id) expr

isThread : BExpr -> Pointer -> Bool
isThread expr p =
  expr |> listThreadBlanks |> List.member (P.idOf p)


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

getValueParent : Pointer -> BExpr -> Maybe Pointer
getValueParent p bexpr =
  let id = P.idOf p
      parent = parentOf_ id (n2o bexpr)
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
  let e2ld e = PExpr (o2n e)
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
      [PVarBind lhs] ++ rl [rhs, expr]

    If _ cond ifbody elsebody ->
      rl [cond, ifbody, elsebody]

    FnCall _ name exprs ->
      rl exprs

    Lambda _ vars expr ->
      listData expr

    Thread _ exprs ->
      rl exprs

    FieldAccess _ obj field ->
      listData obj ++ [PField field]



subtree : ID -> BExpr -> PointerData
subtree id ast =
  deMaybe "subtree" (subData id ast)

subData : ID -> BExpr -> Maybe PointerData
subData id bexpr =
  listData (n2o bexpr)
  |> List.filter (\d -> id == P.idOfD d)
  |> List.head -- TODO might be multiple

toContent : PointerData -> String
toContent pd =
  case pd of
    PVarBind v -> v |> Blank.toMaybe |> Maybe.withDefault ""
    PField f -> f |> Blank.toMaybe |> Maybe.withDefault ""
    PExpr e ->
      case e of
        F _ (NValue s) -> s
        F _ (NVariable v) -> v
        _ -> ""
    PEventModifier _ -> ""
    PEventName _ -> ""
    PEventSpace _ -> ""
    PDBColName _ -> ""
    PDBColType _ -> ""
    PDarkType _ -> ""
    PDarkTypeField _ -> ""


replace : Pointer -> PointerData -> BExpr -> BExpr
replace p replacement bexpr =
  let rbe = replace p replacement
      rlb : List BExpr -> List BExpr
      rlb bexprs = List.map rbe bexprs
      re expr = case expr of
                  NLet lhs rhs body ->
                    if Blank.toID lhs == P.idOf p
                    then
                      case replacement of
                        PVarBind b ->
                          NLet b rhs body
                        _ -> expr
                    else
                      NLet lhs (rbe rhs) (rbe body)

                  NIf cond ifbody elsebody ->
                    NIf (rbe cond) (rbe ifbody) (rbe elsebody)

                  NFnCall name exprs ->
                    NFnCall name (rlb exprs)

                  NLambda vars expr ->
                    NLambda vars (rbe expr)

                  NThread exprs ->
                    NThread (rlb exprs)

                  NFieldAccess obj field ->
                    if Blank.toID field == P.idOf p
                    then
                      case replacement of
                        PField f ->
                          NFieldAccess obj f
                        _ -> expr
                    else
                      NFieldAccess (rbe obj) field

                  NValue v -> expr
                  NVariable name -> expr
  in
  if Blank.toID bexpr == P.idOf p
  then
    case replacement of
      PExpr e -> e
      _ -> bexpr
  else
    case bexpr of
      F id  e -> F id (re e)
      Blank _ -> bexpr



deleteExpr : Pointer -> BExpr -> ID -> BExpr
deleteExpr p ast id =
  let replacement = P.emptyD_ id (P.typeOf p)
  in replace p replacement ast

replaceVarBind : Pointer -> VarName -> BExpr -> BExpr
replaceVarBind p replacement expr =
  replace p (PVarBind (Blank.newF replacement)) expr


replaceField : Pointer -> FieldName -> BExpr -> BExpr
replaceField p replacement expr =
  replace p (PField (Blank.newF replacement)) expr


clone : BExpr -> BExpr
clone bexpr =
  let nid = gid ()
      c be = clone be
      cl bes = List.map c bes
      cBlankOr bo =
        let nbid = gid () in
        case bo of
          Blank _ -> Blank nbid
          F _ a -> F nbid a
  in
    case bexpr of
      Blank id -> Blank nid
      F id expr ->
        F nid
          (case expr of
            NLet lhs rhs expr ->
              NLet (cBlankOr lhs) (c rhs) (c expr)

            NIf cond ifbody elsebody ->
              NIf (c cond) (c ifbody) (c elsebody)

            NFnCall name exprs ->
              NFnCall name (cl exprs)

            NLambda vars expr ->
              NLambda vars (c expr)

            NThread exprs ->
              NThread (cl exprs)

            NFieldAccess obj field ->
              NFieldAccess (c obj) (cBlankOr field)
            NValue v ->
              NValue v
            NVariable name ->
              NVariable name)


