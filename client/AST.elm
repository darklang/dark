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
                  Value v -> []
                  Variable name -> []

                  Let lhs rhs expr -> rb rhs ++ rb expr
                  FnCall name exprs -> ltList exprs
                  Lambda vars expr -> rb expr
                  FieldAccess obj _ -> rb obj

                  If cond ifbody elsebody ->
                    rb cond ++ rb ifbody ++ rb elsebody

                  Thread exprs ->
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
    F id (Thread exprs) ->
      let newExprs = List.filter Blank.isF exprs
                     |> List.map closeThread
      in
      case newExprs of
        [] -> Blank id
        [e] -> e
        _ -> F id (Thread newExprs)
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
      F tid (Thread exprs) ->
        F tid (Thread (exprs ++ [Blank.new ()]))
      _ ->
        Blank.newF (Thread [bexpr, Blank.new ()])
  else
    case bexpr of
      F tid (Thread exprs) ->
        let replaced = extendThreadChild id exprs in
        if replaced == exprs
        then traverseExpr atb bexpr
        else F tid (Thread replaced)

      _ -> traverseExpr atb bexpr


traverseExpr : (Expr -> Expr) -> Expr -> Expr
traverseExpr fn bexpr =
  case bexpr of
    Blank _ -> bexpr
    F id expr ->
      F id
        (case expr of
          Value _ -> expr
          Variable _ -> expr

          Let lhs rhs body ->
            Let lhs (fn rhs) (fn body)

          If cond ifbody elsebody ->
            If (fn cond) (fn ifbody) (fn elsebody)

          FnCall name exprs ->
            FnCall name (List.map fn exprs)

          Lambda vars lexpr ->
            Lambda vars (fn lexpr)

          Thread exprs ->
            Thread (List.map fn exprs)

          FieldAccess obj field ->
            FieldAccess (fn obj) field)



-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> Expr -> Expr
wrapInThread id bexpr =
  if Blank.toID bexpr == id
  then
    case bexpr of
      F _ (Thread _) -> bexpr
      F _ expr -> Blank.newF (Thread [bexpr, Blank.new ()])
      Blank _ -> Blank.newF (Thread [bexpr])
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
    F tid (Thread exprs) ->
      let newExprs = extendThreadChild id exprs
                     |> List.map (maybeExtendThreadAt id)
      in F tid (Thread newExprs)
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
        Value _ -> []
        Variable _ -> []
        If cond ifbody elsebody ->
          [toP cond, toP ifbody, toP elsebody]
        FnCall name exprs ->
          List.map toP exprs
        Lambda vars lexpr ->
          [toP lexpr]
        Thread exprs ->
          List.map toP exprs
        FieldAccess obj field ->
          [toP obj, Blank.toP Field field]
        Let lhs rhs body ->
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
        Value _ -> []
        Variable _ -> []
        Let lhs rhs body ->
          returnOr (\_ -> List.concat [co body, co rhs]) bexpr

        If cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = co cond
                ib = co ifbody
                eb = co elsebody
            in List.concat [c, ib, eb]) bexpr

        FnCall name exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) bexpr

        Lambda vars lexpr ->
          returnOr (\_ -> co lexpr) bexpr

        Thread exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) bexpr

        FieldAccess obj field ->
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
                Value _ -> []
                Variable _ -> []
                Let lhs rhs body ->
                  reclist id bexp walk [rhs, body]
                If cond ifbody elsebody ->
                  reclist id bexp walk [cond, ifbody, elsebody]
                FnCall name exprs ->
                  reclist id bexp walk exprs
                Lambda vars lexpr ->
                  rec id bexp walk lexpr
                Thread exprs ->
                  reclist id bexp walk exprs
                FieldAccess obj field ->
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
        F _ (Thread _) -> True
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
        Value _ -> Nothing
        Variable _ -> Nothing
        Let lhs rhs body ->
          returnOr (\_ -> filterMaybe [po body, po rhs]) bexpr

        If cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = po cond
                ib = po ifbody
                eb = po elsebody
            in filterMaybe [c, ib, eb]) bexpr

        FnCall name exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) bexpr

        Lambda vars lexpr ->
          returnOr (\_ -> po lexpr) bexpr

        Thread exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) bexpr

        FieldAccess obj field ->
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
        F _ (If cond ifbody elsebody) ->
          List.map toP [cond, ifbody, elsebody]

        F _ (Let lhs rhs body) ->
          [Blank.toP VarBind lhs, toP rhs, toP body]

        F _ (FnCall name exprs) ->
          List.map toP exprs

        F _ (Lambda vars lexpr) ->
          [toP lexpr]

        F _ (Thread exprs) ->
          List.map toP exprs

        F _ (FieldAccess obj field) ->
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
        Just (F _ (Thread exprs)) ->
          exprs
          |> List.map toP
          |> Util.listPrevious p
        _ -> Nothing

    Field ->
      case parent of
        Just (F _ (FieldAccess obj _)) ->
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
        Value v -> []
        Variable name -> []

        Let lhs rhs expr ->
          [Blank.toP VarBind lhs] ++ rl [rhs, expr]

        If cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        FnCall name exprs ->
          rl exprs

        Lambda vars expr ->
          allPointers expr

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
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
        Value v -> []
        Variable name -> []

        Let lhs rhs expr ->
          [PVarBind lhs] ++ rl [rhs, expr]

        If cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        FnCall name exprs ->
          rl exprs

        Lambda vars expr ->
          listData expr

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
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
        F _ (Value s) -> s
        F _ (Variable v) -> v
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
      (F id (Let lhs rhs body), PVarBind b) ->
        if Blank.toID lhs == P.idOf p
        then F id (Let b rhs body)
        else traverseExpr r bexpr

      (F id (FieldAccess obj field), PField f) ->
        if Blank.toID field == P.idOf p
        then F id (FieldAccess obj f)
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
            Let lhs rhs expr ->
              Let (cBlankOr lhs) (c rhs) (c expr)

            If cond ifbody elsebody ->
              If (c cond) (c ifbody) (c elsebody)

            FnCall name exprs ->
              FnCall name (cl exprs)

            Lambda vars expr ->
              Lambda vars (c expr)

            Thread exprs ->
              Thread (cl exprs)

            FieldAccess obj field ->
              FieldAccess (c obj) (cBlankOr field)
            Value v ->
              Value v
            Variable name ->
              Variable name)


