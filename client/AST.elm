module AST exposing (..)

-- builtin
import List

-- lib
-- import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P
import Blank as B


-------------------------
-- Generic
-------------------------
toP : Expr -> Pointer
toP = B.toP Expr

toPD : Expr -> PointerData
toPD e =
  PExpr e

traverse : (Expr -> Expr) -> Expr -> Expr
traverse fn expr =
  case expr of
    Blank _ -> expr
    F id nexpr ->
      F id
        (case nexpr of
          Value _ -> nexpr
          Variable _ -> nexpr

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


-------------------------
-- Thread stuff
-------------------------
listThreadBlanks : Expr -> List ID
listThreadBlanks expr =
  let r = listThreadBlanks
      rList : List Expr -> List ID
      rList exprs =
        exprs
        |> List.map listThreadBlanks
        |> List.concat
      rn nexpr =
        case nexpr of
          Value v -> []
          Variable name -> []

          Let lhs rhs body -> r rhs ++ r body
          FnCall name exprs -> rList exprs
          Lambda vars body -> r body
          FieldAccess obj _ -> r obj

          If cond ifbody elsebody ->
            r cond ++ r ifbody ++ r elsebody

          Thread exprs ->
            let (blanks, filled) = List.partition B.isBlank exprs
                blankids = List.map B.toID blanks
                subExprsBlankids = rList filled
            in blankids ++ subExprsBlankids
  in case expr of
      Blank _ -> []
      F _ f -> rn f

closeThread : Expr -> Expr
closeThread expr =
  -- Close all threads
  case expr of
    F id (Thread exprs) ->
      let newExprs = List.filter B.isF exprs
                     |> List.map closeThread
      in
      case newExprs of
        [] -> Blank id
        [e] -> e
        _ -> F id (Thread newExprs)
    _ -> traverse closeThread expr


-- take an expression, and if
-- * it is a thread, add a blank at the end
-- * it is part of a thread, insert a blank just after the expr
-- * if it is not part of a thread, wrap it in a thread
addThreadBlank : ID -> Expr -> Expr
addThreadBlank id expr =
  let atb = addThreadBlank id in
  if id == B.toID expr
  then
    case expr of
      F tid (Thread exprs) ->
        F tid (Thread (exprs ++ [B.new ()]))
      _ ->
        B.newF (Thread [expr, B.new ()])
  else
    case expr of
      F tid (Thread exprs) ->
        let replaced = extendThreadChild id exprs in
        if replaced == exprs
        then traverse atb expr
        else F tid (Thread replaced)

      _ -> traverse atb expr


-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> Expr -> Expr
wrapInThread id expr =
  if B.toID expr == id
  then
    case expr of
      F _ (Thread _) -> expr
      F _ _ -> B.newF (Thread [expr, B.new ()])
      Blank _ -> B.newF (Thread [expr])
  else
    traverse (wrapInThread id) expr

-- Find the child with the id `at` in the thread, and add a blank after it.
extendThreadChild : ID -> List Expr -> List Expr
extendThreadChild at threadExprs =
  List.foldr (\e list ->
                if (B.toID e) == at
                then e :: B.new () :: list
                else e :: list)
             []
             threadExprs

-- extends thread at pos denoted by ID, if ID is in a thread
maybeExtendThreadAt : ID -> Expr -> Expr
maybeExtendThreadAt id expr =
  case expr of
    F tid (Thread exprs) ->
      let newExprs = extendThreadChild id exprs
                     |> List.map (maybeExtendThreadAt id)
      in F tid (Thread newExprs)
    _ -> traverse (maybeExtendThreadAt id) expr

-- Is Pointer a blank inside a thread
isThreadBlank : Expr -> Pointer -> Bool
isThreadBlank expr p =
  expr |> listThreadBlanks |> List.member (P.idOf p)

grandparentIsThread : Expr -> Maybe Expr -> Bool
grandparentIsThread expr parent =
  parent
  |> Maybe.map
       (\p ->
         case parentOf_ (B.toID p) expr of
           Just (F _ (Thread ts)) ->
             ts
             |> List.head
             |> Maybe.map ((/=) p)
             |> Maybe.withDefault True
           _ -> False)
  |> Maybe.withDefault False


-------------------------
-- Children
-------------------------
children : Expr -> List Pointer
children expr =
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
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
          [toP obj, B.toP Field field]
        Let lhs rhs body ->
          [B.toP VarBind lhs, toP rhs, toP body]

childrenOf : ID -> Expr -> List Pointer
childrenOf pid expr =
  let co = childrenOf pid
      returnOr fn e =
        if pid == B.toID e
        then children e
        else fn e
  in
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value _ -> []
        Variable _ -> []
        Let lhs rhs body ->
          returnOr (\_ -> List.concat [co body, co rhs]) expr

        If cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = co cond
                ib = co ifbody
                eb = co elsebody
            in List.concat [c, ib, eb]) expr

        FnCall name exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) expr

        Lambda vars lexpr ->
          returnOr (\_ -> co lexpr) expr

        Thread exprs ->
          returnOr (\_ -> exprs |> List.map co |> List.concat) expr

        FieldAccess obj field ->
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
        if B.toID exp == tofind
        then walk
        else
          case exp of
            Blank _ -> []
            F i nexpr ->
              case nexpr of
                Value _ -> []
                Variable _ -> []
                Let lhs rhs body ->
                  reclist id exp walk [rhs, body]
                If cond ifbody elsebody ->
                  reclist id exp walk [cond, ifbody, elsebody]
                FnCall name exprs ->
                  reclist id exp walk exprs
                Lambda vars lexpr ->
                  rec id exp walk lexpr
                Thread exprs ->
                  reclist id exp walk exprs
                FieldAccess obj field ->
                  rec id exp walk obj
  in rec_ancestors id [] expr


ancestorsWhere : ID -> Expr -> (Expr -> Bool) -> List Expr
ancestorsWhere id expr fn =
  List.filter fn (ancestors id expr)

threadAncestors : ID -> Expr -> List Expr
threadAncestors id expr =
  ancestorsWhere id expr
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
    Blank _ -> Nothing
    F id nexpr ->
      case nexpr of
        Value _ -> Nothing
        Variable _ -> Nothing
        Let lhs rhs body ->
          returnOr (\_ -> filterMaybe [po body, po rhs]) expr

        If cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = po cond
                ib = po ifbody
                eb = po elsebody
            in filterMaybe [c, ib, eb]) expr

        FnCall name exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) expr

        Lambda vars lexpr ->
          returnOr (\_ -> po lexpr) expr

        Thread exprs ->
          returnOr (\_ -> exprs |> List.map po |> filterMaybe) expr

        FieldAccess obj field ->
          if B.toID field == eid
          then Just expr
          else returnOr (\_ -> po obj) expr

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
          [B.toP VarBind lhs, toP rhs, toP body]

        F _ (FnCall name exprs) ->
          List.map toP exprs

        F _ (Lambda vars lexpr) ->
          [toP lexpr]

        F _ (Thread exprs) ->
          List.map toP exprs

        F _ (FieldAccess obj field) ->
          [toP obj, B.toP Field field]

        _ -> [p]

getValueParent : Pointer -> Expr -> Maybe Pointer
getValueParent p expr =
  let parent = parentOf_ (P.idOf p) expr in
  case (P.typeOf p, parent) of
    (Expr, Just (F _ (Thread exprs))) ->
      exprs
      |> List.map toP
      |> Util.listPrevious p

    (Field, Just (F _ (FieldAccess obj _))) ->
      Just <| toP obj

    _ -> Nothing



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
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value v -> []
        Variable name -> []

        Let lhs rhs body ->
          [B.toP VarBind lhs] ++ rl [rhs, body]

        If cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        FnCall name exprs ->
          rl exprs

        Lambda vars body ->
          allPointers body

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
          allPointers obj ++ [B.toP Field field]


--------------------------------
-- PointersData
--------------------------------


listData : Expr -> List PointerData
listData expr =
  let e2ld e = PExpr e
      rl : List Expr -> List PointerData
      rl exprs =
        exprs
        |> List.map listData
        |> List.concat
  in
  [e2ld expr] ++
  case expr of
    Blank _ -> []
    F _ nexpr ->
      case nexpr of
        Value v -> []
        Variable name -> []

        Let lhs rhs body ->
          [PVarBind lhs] ++ rl [rhs, body]

        If cond ifbody elsebody ->
          rl [cond, ifbody, elsebody]

        FnCall name exprs ->
          rl exprs

        Lambda vars body ->
          listData body

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
          listData obj ++ [PField field]



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
    PVarBind v -> v |> B.toMaybe |> Maybe.withDefault ""
    PField f -> f |> B.toMaybe |> Maybe.withDefault ""
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
replace p replacement expr =
  let r = replace p replacement in
  if B.toID expr == P.idOf p
  then
    case replacement of
      PExpr e -> e
      _ -> expr
  else
    case (expr, replacement) of
      (F id (Let lhs rhs body), PVarBind b) ->
        if B.toID lhs == P.idOf p
        then F id (Let b rhs body)
        else traverse r expr

      (F id (FieldAccess obj field), PField f) ->
        if B.toID field == P.idOf p
        then F id (FieldAccess obj f)
        else traverse r expr

      _ -> traverse r expr


deleteExpr : Pointer -> Expr -> ID -> Expr
deleteExpr p expr id =
  let replacement = P.emptyD_ id (P.typeOf p)
  in replace p replacement expr

replaceVarBind : Pointer -> VarName -> Expr -> Expr
replaceVarBind p replacement expr =
  replace p (PVarBind (B.newF replacement)) expr


replaceField : Pointer -> FieldName -> Expr -> Expr
replaceField p replacement expr =
  replace p (PField (B.newF replacement)) expr


clone : Expr -> Expr
clone expr =
  let nid = gid ()
      c be = clone be
      cl bes = List.map c bes
      cBlankOr bo =
        let nbid = gid () in
        case bo of
          Blank _ -> Blank nbid
          F _ a -> F nbid a
  in
    case expr of
      Blank id -> Blank nid
      F id nexpr ->
        F nid
          (case nexpr of
            Let lhs rhs body ->
              Let (cBlankOr lhs) (c rhs) (c expr)

            If cond ifbody elsebody ->
              If (c cond) (c ifbody) (c elsebody)

            FnCall name exprs ->
              FnCall name (cl exprs)

            Lambda vars body ->
              Lambda vars (c body)

            Thread exprs ->
              Thread (cl exprs)

            FieldAccess obj field ->
              FieldAccess (c obj) (cBlankOr field)
            Value v ->
              Value v
            Variable name ->
              Variable name)


