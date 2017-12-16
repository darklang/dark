module AST exposing (..)

-- builtin
import List

-- lib
import List.Extra as LE

-- dark
import Types exposing (..)
import Util exposing (deMaybe)

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%", "+", "-", "^", "!="]

isLeaf : ID -> AST -> Bool
isLeaf id ast =
  case subExpr id ast of
    Nothing -> False
    Just e ->
      case e of
        Value _ _ -> True
        Hole _ -> True
        Variable _ _ -> True
        FnCall _ _ params -> -- Constant, or piped-in function
          (List.length params) == 0
        _ -> False


deleteExpr : ID -> AST -> (ID, AST)
deleteExpr id ast =
  let replacement = Hole (ID <| Util.random ())
  in (toID replacement, replaceExpr id replacement ast)

replaceExpr : ID -> Expr -> Expr -> Expr
replaceExpr id replacement expr =
  let re = replaceExpr id replacement
      reList : List Expr -> List Expr
      reList exprs = List.map re exprs
  in
  if toID expr == id
  then replacement
  else
    case expr of

      Let id lhs rhs expr ->
        Let id lhs (re rhs) (re expr)

      If id cond ifbody elsebody ->
        If id (re cond) (re ifbody) (re elsebody)

      FnCall id name exprs ->
        FnCall id name (reList exprs)

      Lambda id vars expr ->
        Lambda id vars (re expr)

      Thread id exprs ->
        Thread id (reList exprs)

      FieldAccess id obj field ->
        FieldAccess id (re obj) field

      Hole id -> expr
      Value id v -> expr
      Variable id name -> expr

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
      let newExprs = ctList exprs
          init = LE.init newExprs |> Maybe.withDefault []
          last = LE.last newExprs |> deMaybe
      in
        case (init, last) of
          ([], _) ->
            last
          ([e], Hole _) ->
            e
          (init, Hole _) ->
            -- recurse to clear multiple threads
            closeThread (Thread tid init)
          (init, _) ->
            Thread tid newExprs



replaceBindHole : ID -> VarName -> AST -> AST
replaceBindHole hid replacement ast =
  replaceBindHole_ hid replacement ast

replaceBindHole_ : ID -> VarName -> Expr -> Expr
replaceBindHole_ hid replacement expr =
  let rbh = replaceBindHole_ hid replacement
      rbhList : List Expr -> List Expr
      rbhList exprs = List.map rbh exprs
  in
  case expr of
    Value id v ->
      Value id v

    Let id lhs rhs expr ->
      let newLhs =
            case lhs of
              Full id s ->
                if id == hid
                then Full id replacement
                else lhs
              Empty id ->
                if id == hid
                then Full id replacement
                else lhs

      in Let id newLhs (rbh rhs) (rbh expr)

    If id cond ifbody elsebody ->
      If id (rbh cond) (rbh ifbody) (rbh elsebody)

    Variable id name ->
      Variable id name

    FnCall id name exprs ->
      FnCall id name (rbhList exprs)

    Lambda id vars expr ->
      Lambda id vars (rbh expr)

    Hole id ->
      Hole id

    Thread id exprs ->
      Thread id (rbhList exprs)

    FieldAccess id obj name ->
      FieldAccess id (rbh obj) name

replaceFieldHole : ID -> VarName -> Expr -> Expr
replaceFieldHole hid replacement expr =
  let rfh = replaceFieldHole hid replacement
      rfhList : List Expr -> List Expr
      rfhList exprs = List.map rfh exprs
  in
  case expr of
    Value id v ->
      Value id v

    Let id lhs rhs expr ->
      Let id lhs (rfh rhs) (rfh expr)

    If id cond ifbody elsebody ->
      If id (rfh cond) (rfh ifbody) (rfh elsebody)

    Variable id name ->
      Variable id name

    FnCall id name exprs ->
      FnCall id name (rfhList exprs)

    Lambda id vars expr ->
      Lambda id vars (rfh expr)

    Hole id ->
      Hole id

    Thread id exprs ->
      Thread id (rfhList exprs)

    FieldAccess id obj name ->
      let newName =
            case name of
              Full id s ->
                if id == hid
                then Full id replacement
                else name
              Empty id ->
                if id == hid
                then Full id replacement
                else name
      in FieldAccess id (rfh obj) newName

isHole : Expr -> Bool
isHole e =
  case e of
    Hole _ -> True
    _ -> False

listBindHoles : Expr -> List ID
listBindHoles expr =
  let lbhList : List Expr -> List ID
      lbhList exprs =
        exprs
        |> List.map listBindHoles
        |> List.concat
      bhList : List VarBind -> List ID
      bhList = List.map holeOrID
  in
  case expr of
    Value _ v ->
      []

    Let _ lhs rhs expr ->
      let exprBindHoles = lbhList [rhs, expr]
          bindHoles = [holeOrID lhs]
      in
          bindHoles ++ exprBindHoles

    If _ cond ifbody elsebody ->
      lbhList [cond, ifbody, elsebody]

    Variable _ name ->
      []

    FnCall _ name exprs ->
      lbhList exprs

    Lambda _ vars expr ->
      listBindHoles expr

    -- note this is empty, which is the difference between this and listHoles
    Hole id ->
      []

    Thread _ exprs ->
      lbhList exprs

    FieldAccess _ obj ident ->
      listBindHoles obj

listFieldHoles : Expr -> List ID
listFieldHoles expr =
  let lfhList : List Expr -> List ID
      lfhList exprs =
        exprs
        |> List.map listFieldHoles
        |> List.concat
  in
  case expr of
    Value _ v ->
      []

    Let _ lhs rhs expr ->
      lfhList [rhs, expr]

    If _ cond ifbody elsebody ->
      lfhList [cond, ifbody, elsebody]

    Variable _ name ->
      []

    FnCall _ name exprs ->
      lfhList exprs

    Lambda _ vars expr ->
      listFieldHoles expr

    -- note this is empty, which is the difference between this and listHoles
    Hole id ->
      []

    Thread _ exprs ->
      lfhList exprs

    FieldAccess _ obj ident ->
      listFieldHoles obj ++ [holeOrID ident]

-- TODO: figure out how we can define this
-- this in terms of listBindHoles and a listExprHoles
--
-- a naive concatenation would be a) another dupe'd walk
-- plus b) lead to even more ordering issues
--
-- time for a proper visitor abstraction?
listHoles : Expr -> List ID
listHoles expr =
  let lhList : List Expr -> List ID
      lhList exprs =
        exprs
        |> List.map listHoles
        |> List.concat
  in
  case expr of
    Value _ v ->
      []

    Let _ lhs rhs expr ->
      let exprHoles = lhList [rhs, expr]
          bindHoles = [holeOrID lhs]
      in
          bindHoles ++ exprHoles

    If _ cond ifbody elsebody ->
      lhList [cond, ifbody, elsebody]

    Variable _ name ->
      []

    FnCall _ name exprs ->
      lhList exprs

    Lambda _ vars expr ->
      listHoles expr

    Hole id -> [id]

    Thread _ exprs ->
      lhList exprs

    FieldAccess _ obj ident ->
      listHoles obj ++ [holeOrID ident]

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
        let newExprs =
              List.foldr (\e list ->
                if (toID e) == id
                then e :: Hole (gid ()) :: list
                else e :: list)
                [] exprs
        in Thread tid (List.map et newExprs)

      FieldAccess id obj field ->
        FieldAccess id (et obj) field


children : Expr -> List ID
children e =
  case e of
    Value _ _ -> []
    Hole _ -> []
    Variable _ _ -> []
    If _ cond ifbody elsebody ->
      [toID cond, toID ifbody, toID elsebody]
    FnCall _ name exprs ->
      List.map toID exprs
    Lambda _ vars lexpr ->
      [toID lexpr]
    Thread _ exprs ->
      List.map toID exprs
    FieldAccess _ obj field ->
      [toID obj, holeOrID field]
    Let _ lhs rhs body ->
      [holeOrID lhs, toID rhs, toID body]

childrenOf : ID -> AST -> List ID
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
              r lexpr

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
  deMaybe <| parentOf_ id ast

parentOf_ : ID -> Expr -> Maybe Expr
parentOf_ eid expr =
  let po = parentOf_ eid
      returnOr fn e =
        if List.member eid (children e)
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
      if holeOrID field == eid
      then Just expr
      else returnOr (\_ -> po obj) expr

-- includes self
siblings : ID -> AST -> List ID
siblings id ast =
  let parent = parentOf_ id ast
  in
      case parent of
        Nothing -> [id]
        Just p ->
          case p of
            If _ cond ifbody elsebody ->
              List.map toID [cond, ifbody, elsebody]

            Let _ lhs rhs body ->
              [holeOrID lhs, toID rhs, toID body]

            FnCall _ name exprs ->
              List.map toID exprs

            Lambda _ vars lexpr ->
              [toID lexpr]

            Thread _ exprs ->
              List.map toID exprs

            FieldAccess _ obj field ->
              [toID obj, holeOrID field]

            _ -> [id]

toContent : AST -> String
toContent a =
  case a of
    Value _ s -> s
    Variable _ v -> v
    _ -> ""

subtree : ID -> AST -> AST
subtree id ast =
  deMaybe (subExpr id ast)

subExpr : ID -> Expr -> Maybe Expr
subExpr id expr =
  let se = subExpr id
      returnOr fn e =
        if (toID e) == id
        then Just e
        else fn e
      nothing = (\_ -> Nothing)
      returnOrNothing = returnOr (\_ -> Nothing)
      filterMaybe xs = xs |> List.filterMap identity |> List.head
  in
  case expr of
        Value _ _ -> returnOrNothing expr
        Hole _ -> returnOrNothing expr
        Variable _ _ -> returnOrNothing expr
        Let id lhs rhs body ->
          returnOr (\_ -> filterMaybe [se body, se rhs]) expr

        If id cond ifbody elsebody ->
          returnOr (\_ ->
            let c  = se cond
                ib = se ifbody
                eb = se elsebody
            in
                filterMaybe [c, ib, eb]) expr

        FnCall id name exprs ->
          returnOr (\_ -> exprs |> List.map se |> filterMaybe) expr

        Lambda id vars lexpr ->
          returnOr (\_ -> se lexpr) expr

        Thread id exprs ->
          returnOr (\_ -> exprs |> List.map se |> filterMaybe) expr

        FieldAccess id obj field ->
          returnOr (\_ -> se obj) expr

