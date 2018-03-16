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
toP = Blank.toP Expr

toPD : Expr -> PointerData
toPD e =
  PExpr e

-------------------------
-- Thread stuff
-------------------------
listThreadBlanks : Expr -> List ID
listThreadBlanks expr =
  let rb = listThreadBlanks
      ltList : List Expr -> List ID
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


closeThread : Expr -> Expr
closeThread bexpr =
  -- Close all threads
  case bexpr of
    F id (NThread exprs) ->
      let newExprs = List.filter Blank.isF exprs
                     |> List.map closeThread
      in
      case newExprs of
        [] -> Blank id
        [e] -> e
        _ -> F id (NThread newExprs)
    _ -> traverseExpr closeThread bexpr


-- take an expression, and if
-- * it is a thread, add a hole at the end
-- * it is part of a thread, insert a hole just after the expr
-- * if it is not part of a thread, wrap it in a thread
addThreadBlank : ID -> Expr -> Expr
addThreadBlank id bexpr =
  let atb = addThreadBlank id in
  if id == Blank.toID bexpr
  then
    case bexpr of
      F tid (NThread exprs) ->
        F tid (NThread (exprs ++ [Blank.new ()]))
      _ ->
        Blank.newF (NThread [bexpr, Blank.new ()])
  else
    case bexpr of
      F tid (NThread exprs) ->
        let replaced = extendThreadChild id exprs in
        if replaced == exprs
        then traverseExpr atb bexpr
        else F tid (NThread replaced)

      _ -> traverseExpr atb bexpr


traverseExpr : (Expr -> Expr) -> Expr -> Expr
traverseExpr fn bexpr =
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
wrapInThread : ID -> Expr -> Expr
wrapInThread id bexpr =
  if Blank.toID bexpr == id
  then
    case bexpr of
      F _ (NThread _) -> bexpr
      F _ expr -> Blank.newF (NThread [bexpr, Blank.new ()])
      Blank _ -> Blank.newF (NThread [bexpr])
  else
    traverseExpr (wrapInThread id) bexpr

-- Find the child with the id `at` in the thread, and add a blank after it.
extendThreadChild : ID -> List Expr -> List Expr
extendThreadChild at threadExprs =
  List.foldr (\e list ->
    if (Blank.toID e) == at
    then e :: Blank.new () :: list
    else e :: list)
    [] threadExprs


-- extends thread at pos denoted by ID, if ID is in a thread
maybeExtendThreadAt : ID -> Expr -> Expr
maybeExtendThreadAt id bexpr =
  case bexpr of
    F tid (NThread exprs) ->
      let newExprs = extendThreadChild id exprs
                     |> List.map (maybeExtendThreadAt id)
      in F tid (NThread newExprs)
    _ -> traverseExpr (maybeExtendThreadAt id) bexpr

isThread : Expr -> Pointer -> Bool
isThread expr p =
  expr |> listThreadBlanks |> List.member (P.idOf p)


-------------------------
-- Children
-------------------------
children : Expr -> List Pointer
children e =
  case e of
    Blank _ -> []
    F _ expr ->
      case expr of
        NValue _ -> []
        NVariable _ -> []
        NIf cond ifbody elsebody ->
          [toP cond, toP ifbody, toP elsebody]
        NFnCall name exprs ->
          List.map toP exprs
        NLambda vars lexpr ->
          [toP lexpr]
        NThread exprs ->
          List.map toP exprs
        NFieldAccess obj field ->
          [toP obj, Blank.toP Field field]
        NLet lhs rhs body ->
          [Blank.toP VarBind lhs, toP rhs, toP body]

childrenOf : ID -> Expr -> List Pointer
childrenOf pid bexpr =
  let co = childrenOf pid
      returnOr fn e =
        if pid == Blank.toID e
        then children e
        else fn e
  in
  case bexpr of
    Blank _ -> []
    F _ expr ->
      case expr of
        NValue _ -> []
        NVariable _ -> []
        NLet lhs rhs body ->
          returnOr (\_ -> List.concat [co body, co rhs]) bexpr

        NIf cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = co cond
                ib = co ifbody
                eb = co elsebody
            in List.concat [c, ib, eb]) bexpr

        NFnCall name exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) bexpr

        NLambda vars lexpr ->
          returnOr (\_ -> co lexpr) bexpr

        NThread exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) bexpr

        NFieldAccess obj field ->
          returnOr (\_ -> co obj) bexpr

-------------------------
-- Ancestors
-------------------------
ancestors : ID -> Expr -> List Expr
ancestors id bexpr =
  let rec_ancestors : ID -> List Expr -> Expr -> List Expr
      rec_ancestors tofind walk bexp =
        let rec id be walk = rec_ancestors id (be :: walk)
            reclist id be walk exprs =
              exprs |> List.map (rec id be walk) |> List.concat
        in
        if Blank.toID bexp == tofind
        then walk
        else
          case bexp of
            Blank _ -> []
            F i expr ->
              case expr of
                NValue _ -> []
                NVariable _ -> []
                NLet lhs rhs body ->
                  reclist id bexp walk [rhs, body]
                NIf cond ifbody elsebody ->
                  reclist id bexp walk [cond, ifbody, elsebody]
                NFnCall name exprs ->
                  reclist id bexp walk exprs
                NLambda vars lexpr ->
                  rec id bexp walk lexpr
                NThread exprs ->
                  reclist id bexp walk exprs
                NFieldAccess obj field ->
                  rec id bexp walk obj
  in rec_ancestors id [] bexpr


ancestorsWhere : ID -> Expr -> (Expr -> Bool) -> List Expr
ancestorsWhere id bexpr fn =
  List.filter fn (ancestors id bexpr)

threadAncestors : ID -> Expr -> List Expr
threadAncestors id bexpr =
  ancestorsWhere id bexpr
    (\e ->
      case e of
        F _ (NThread _) -> True
        _ -> False)


-------------------------
-- Parents
-------------------------
parentOf : ID -> Expr -> Expr
parentOf id ast =
  deMaybe "parentOf" <| parentOf_ id ast

parentOf_ : ID -> Expr -> Maybe Expr
parentOf_ eid bexpr =
  let po = parentOf_ eid
      returnOr : (Expr -> Maybe Expr) -> Expr -> Maybe Expr
      returnOr fn e =
        if List.member eid (children e |> List.map P.idOf)
        then Just e
        else fn e
      filterMaybe xs = xs |> List.filterMap identity |> List.head
  in
  case bexpr of
    Blank _ -> Nothing
    F id expr ->
      case expr of
        NValue _ -> Nothing
        NVariable _ -> Nothing
        NLet lhs rhs body ->
          returnOr (\_ -> filterMaybe [po body, po rhs]) bexpr

        NIf cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = po cond
                ib = po ifbody
                eb = po elsebody
            in filterMaybe [c, ib, eb]) bexpr

        NFnCall name exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) bexpr

        NLambda vars lexpr ->
          returnOr (\_ -> po lexpr) bexpr

        NThread exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) bexpr

        NFieldAccess obj field ->
          if Blank.toID field == eid
          then Just bexpr
          else returnOr (\_ -> po obj) bexpr

-- includes self
siblings : Pointer -> Expr -> List Pointer
siblings p expr =
  case parentOf_ (P.idOf p) expr of
    Nothing -> [p]
    Just parent ->
      case parent of
        F _ (NIf cond ifbody elsebody) ->
          List.map toP [cond, ifbody, elsebody]

        F _ (NLet lhs rhs body) ->
          [Blank.toP VarBind lhs, toP rhs, toP body]

        F _ (NFnCall name exprs) ->
          List.map toP exprs

        F _ (NLambda vars lexpr) ->
          [toP lexpr]

        F _ (NThread exprs) ->
          List.map toP exprs

        F _ (NFieldAccess obj field) ->
          [toP obj, Blank.toP Field field]

        _ -> [p]

getValueParent : Pointer -> Expr -> Maybe Pointer
getValueParent p bexpr =
  let id = P.idOf p
      parent = parentOf_ id bexpr
  in
  case P.typeOf p of
    Expr ->
      case parent of
        Just (F _ (NThread exprs)) ->
          exprs
          |> List.map toP
          |> Util.listPrevious p
        _ -> Nothing

    Field ->
      case parent of
        Just (F _ (NFieldAccess obj _)) ->
          Just <| toP obj
        _ -> Nothing

    _ -> Nothing



--------------------------------
-- Pointers
--------------------------------

allPointers : Expr -> List Pointer
allPointers bexpr =
  let rl : List Expr -> List Pointer
      rl bexprs =
        bexprs
        |> List.map allPointers
        |> List.concat
  in
  [toP bexpr] ++
  case bexpr of
    Blank _ -> []
    F _ expr ->
      case expr of
        NValue v -> []
        NVariable name -> []

        NLet lhs rhs expr ->
          [Blank.toP VarBind lhs] ++ rl [rhs, expr]

        NIf cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        NFnCall name exprs ->
          rl exprs

        NLambda vars expr ->
          allPointers expr

        NThread exprs ->
          rl exprs

        NFieldAccess obj field ->
          allPointers obj ++ [Blank.toP Field field]


--------------------------------
-- PointersData
--------------------------------


listData : Expr -> List PointerData
listData bexpr =
  let e2ld e = PExpr e
      rl : List Expr -> List PointerData
      rl exprs =
        exprs
        |> List.map listData
        |> List.concat
  in
  [e2ld bexpr] ++
  case bexpr of
    Blank _ -> []
    F _ expr ->
      case expr of
        NValue v -> []
        NVariable name -> []

        NLet lhs rhs expr ->
          [PVarBind lhs] ++ rl [rhs, expr]

        NIf cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        NFnCall name exprs ->
          rl exprs

        NLambda vars expr ->
          listData expr

        NThread exprs ->
          rl exprs

        NFieldAccess obj field ->
          listData obj ++ [PField field]



subtree : ID -> Expr -> PointerData
subtree id ast =
  deMaybe "subtree" (subData id ast)

subData : ID -> Expr -> Maybe PointerData
subData id bexpr =
  listData bexpr
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


replace : Pointer -> PointerData -> Expr -> Expr
replace p replacement bexpr =
  let r = replace p replacement in
  if Blank.toID bexpr == P.idOf p
  then
    case replacement of
      PExpr e -> e
      _ -> bexpr
  else
    case (bexpr, replacement) of
      (F id (NLet lhs rhs body), PVarBind b) ->
        if Blank.toID lhs == P.idOf p
        then F id (NLet b rhs body)
        else traverseExpr r bexpr

      (F id (NFieldAccess obj field), PField f) ->
        if Blank.toID field == P.idOf p
        then F id (NFieldAccess obj f)
        else traverseExpr r bexpr

      _ -> traverseExpr r bexpr


deleteExpr : Pointer -> Expr -> ID -> Expr
deleteExpr p ast id =
  let replacement = P.emptyD_ id (P.typeOf p)
  in replace p replacement ast

replaceVarBind : Pointer -> VarName -> Expr -> Expr
replaceVarBind p replacement expr =
  replace p (PVarBind (Blank.newF replacement)) expr


replaceField : Pointer -> FieldName -> Expr -> Expr
replaceField p replacement expr =
  replace p (PField (Blank.newF replacement)) expr


clone : Expr -> Expr
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


