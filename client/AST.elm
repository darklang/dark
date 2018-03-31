module AST exposing (..)

-- builtin
import List

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)
import Pointer as P
import Blank as B


-------------------------
-- Generic
-------------------------

traverse : (Expr -> Expr) -> Expr -> Expr
traverse fn expr =
  case expr of
    Blank _ -> expr
    Flagged id msg setting l r ->
      Flagged id msg setting (traverse fn l) (traverse fn r)
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
      Flagged _ _ _ l r -> rList [l, r]
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
    case B.flattenFF expr of
      F _ (Thread _) -> expr
      F _ _ -> B.newF (Thread [expr, B.new ()])
      Blank _ -> B.newF (Thread [expr])
      -- decide based on the displayed value, so flatten
      Flagged _ _ _ _ _ -> Debug.crash "wit"
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

-- Is PointerData a blank inside a thread
isThreadBlank : Expr -> ID -> Bool
isThreadBlank expr p =
  expr |> listThreadBlanks |> List.member p

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

getParamIndex : Expr -> ID -> Maybe (String, Int)
getParamIndex expr id =
  let parent = parentOf_ id expr
      inThread = grandparentIsThread expr parent
  in
  case parent of
    Just (F _ (FnCall name args)) ->
      args
      |> LE.findIndex (\a -> B.toID a == id)
      |> Maybe.map
           (\i -> if inThread
                  then (name, i + 1)
                  else (name, i))
    _ -> Nothing


-------------------------
-- Children
-------------------------
children : Expr -> List PointerData
children expr =
  case expr of
    Blank _ -> []
    Flagged _ _ _ _ _ ->
      -- only return the children of the shown expression
      expr |> B.flattenFF |> children
    F _ nexpr ->
      case nexpr of
        Value _ -> []
        Variable _ -> []
        If cond ifbody elsebody ->
          [PExpr cond, PExpr ifbody, PExpr elsebody]
        FnCall name exprs ->
          List.map PExpr exprs
        Lambda vars lexpr ->
          [PExpr lexpr]
        Thread exprs ->
          List.map PExpr exprs
        FieldAccess obj field ->
          [PExpr obj, PField field]
        Let lhs rhs body ->
          [PVarBind lhs, PExpr rhs, PExpr body]

childrenOf : ID -> Expr -> List PointerData
childrenOf pid expr =
  let co = childrenOf pid
      returnOr fn e =
        if pid == B.toID e
        then children e
        else fn e
  in
  case expr of
    Blank _ -> []
    Flagged _ _ _ _ _ ->
      -- only return the children of the shown expression
      expr |> B.flattenFF |> childrenOf pid
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
            -- no idea what to do here
            Flagged _ _ _ _ _ -> expr |> B.flattenFF |> ancestors id
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
        if List.member eid (children e |> List.map P.toID)
        then Just e
        else fn e
      filterMaybe xs = xs |> List.filterMap identity |> List.head
  in
  case expr of
    Blank _ -> Nothing
    -- not really sure what to do here
    Flagged _ _ _ _ _ -> expr |> B.flattenFF |> parentOf_ eid
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
siblings : PointerData -> Expr -> List PointerData
siblings p expr =
  case parentOf_ (P.toID p) expr of
    Nothing -> [p]
    Just parent ->
      case parent of
        F _ (If cond ifbody elsebody) ->
          List.map PExpr [cond, ifbody, elsebody]

        F _ (Let lhs rhs body) ->
          [ PVarBind lhs, PExpr rhs, PExpr body]

        F _ (FnCall name exprs) ->
          List.map PExpr exprs

        F _ (Lambda vars lexpr) ->
          [PExpr lexpr]

        F _ (Thread exprs) ->
          List.map PExpr exprs

        F _ (FieldAccess obj field) ->
          [PExpr obj, PField field]

        _ -> [p]

getValueParent : PointerData -> Expr -> Maybe PointerData
getValueParent p expr =
  let parent = parentOf_ (P.toID p) expr in
  case (P.typeOf p, parent) of
    (Expr, Just (F _ (Thread exprs))) ->
      exprs
      |> List.map PExpr
      |> Util.listPrevious p

    (Field, Just (F _ (FieldAccess obj _))) ->
      Just <| PExpr obj

    _ -> Nothing



--------------------------------
-- PointerData
--------------------------------

allData : Expr -> List PointerData
allData expr =
  let e2ld e = PExpr e
      rl : List Expr -> List PointerData
      rl exprs =
        exprs
        |> List.map allData
        |> List.concat
  in
  [e2ld expr] ++
  case expr of
    Blank _ -> []
    Flagged _ msg  _ l r -> PFFMsg msg :: rl [l, r]
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
          allData body

        Thread exprs ->
          rl exprs

        FieldAccess obj field ->
          allData obj ++ [PField field]




replace : PointerData -> PointerData -> Expr -> Expr
replace search replacement expr =
  let r = replace search replacement
      sId = P.toID search
  in
  if B.within expr sId
  then
    case replacement of
      PExpr e -> B.replace sId e expr
      _ -> expr
  else
    case (expr, replacement) of
      (F id (Let lhs rhs body), PVarBind replacement) ->
        if B.within lhs sId
        then F id (Let (B.replace sId replacement lhs) rhs body)
        else traverse r expr

      (F id (FieldAccess obj field), PField replacement) ->
        if B.within field sId
        then F id (FieldAccess obj (B.replace sId replacement field))
        else traverse r expr

      _ -> traverse r expr


deleteExpr : PointerData -> Expr -> ID -> Expr
deleteExpr p expr id =
  let replacement = P.emptyD_ id (P.typeOf p)
  in replace p replacement expr

replaceVarBind : PointerData -> VarName -> Expr -> Expr
replaceVarBind p replacement expr =
  replace p (PVarBind (B.newF replacement)) expr


replaceField : PointerData -> FieldName -> Expr -> Expr
replaceField p replacement expr =
  replace p (PField (B.newF replacement)) expr


clone : Expr -> Expr
clone expr =
  let nid = gid ()
      c be = clone be
      cl bes = List.map c bes
      cString : BlankOr String -> BlankOr String
      cString = B.clone identity
      cNExpr nexpr =
        case nexpr of
          Let lhs rhs body -> Let (cString lhs) (c rhs) (c expr)
          If cond ifbody elsebody -> If (c cond) (c ifbody) (c elsebody)
          FnCall name exprs -> FnCall name (cl exprs)
          Lambda vars body -> Lambda vars (c body)
          Thread exprs -> Thread (cl exprs)
          FieldAccess obj field -> FieldAccess (c obj) (cString field)
          Value v -> Value v
          Variable name -> Variable name
  in B.clone cNExpr expr

