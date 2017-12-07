module AST exposing (..)

-- builtin
import List
-- import Tuple

-- lib
import String.Extra as SE
import Maybe.Extra as ME

-- dark
import Types exposing (..)
import Runtime as RT

import Util exposing (deMaybe)

depthString : Int -> String
depthString n = "precedence-" ++ (toString n)

-- v is short for Visit

vFn : FnName -> Element
vFn name =
  case String.split "::" name of
    [mod, n] ->
      Nested (Nothing, "namegroup atom")
      [ Leaf (Nothing, "module", mod)
      , Leaf (Nothing, "moduleseparator", "::")
      , Leaf (Nothing, "fnname", n)
      ]
    _ -> Leaf (Nothing, "fnname atom", name)

vPrefix : ID -> FnName -> List Expr -> Int -> Element
vPrefix id name exprs nest =
  Nested (Just id, "fncall prefix " ++ (depthString nest))
    ((Nested (Nothing, "op " ++ name) [vFn name])
    :: (List.map (vExpr (nest + 1)) exprs))


vInfix : ID -> FnName -> List Expr -> Int -> Element
vInfix id name exprs nesting =
  case exprs of
    [first, second] ->
      Nested (Just id, "fncall infix " ++ (depthString nesting))
        [ Nested (Nothing, "lhs") [vExpr (nesting + 1) first]
        , Nested (Nothing, "op " ++ name) [vFn name]
        , Nested (Nothing, "rhs") [vExpr nesting second]
        ]
    _ -> vPrefix id ("(" ++ name ++ ")") exprs nesting

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%", "+", "-", "^"]

vVarname : Maybe ID -> VarName -> Element
vVarname mId v = Leaf (mId, "varname atom", v)

vVarBind : VarBind -> Element
vVarBind v =
  case v of
    Full s -> Leaf (Nothing, "varname atom", s)
    Empty id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

vExpr : Int -> Expr -> Element
vExpr nest expr =
  case expr of
    Value id v ->
     let cssClass = v |> RT.tipeOf |> toString |> String.toLower
         valu =
           -- TODO: remove
           if RT.isString v
           then "“" ++ (SE.unquote v) ++ "”"
           else v
     in Leaf (Just id, "atom value " ++ cssClass, valu)

    Let id lhs rhs expr ->
      Nested (Just id, "letexpr")
        [ Leaf (Nothing, "let keyword atom", "let")
        , Nested (Nothing, "letbinding")
              [ vVarBind lhs
              , Leaf (Nothing, "letbind atom", "=")
              , vExpr nest rhs ]
        , Leaf (Nothing, "in keyword atom" , "in")
        , Nested (Nothing, "letbody") [vExpr nest expr]
        ]


    If id cond ifbody elsebody ->
      Nested (Just id, "ifexpr")
        [ Leaf (Nothing, "if keyword atom", "if")
        , Nested (Nothing, "cond") [vExpr (nest + 1) cond]
        , Nested (Nothing, "ifbody") [(vExpr 0 ifbody)]
        , Leaf (Nothing, "else keyword atom", "else")
        , Nested (Nothing, "elsebody") [(vExpr 0 elsebody)]
        ]

    Variable id name ->
      vVarname (Just id) name

    FnCall id name exprs ->
      if isInfix name
      then vInfix id name exprs nest
      else vPrefix id name exprs nest

    Lambda id  vars expr ->
      Nested (Just id, "lambdaexpr")
        [ Nested (Nothing, "lambdabinding") (List.map (vVarname Nothing) vars)
        , Leaf (Nothing, "arrow atom" , "->")
        , Nested (Nothing, "lambdabody") [vExpr 0 expr]
        ]

    Hole id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

    Thread id exprs ->
      Nested (Just id, "threadexpr")
      (exprs
       |> List.map (\e -> Nested (Nothing, "threadmember") [vExpr 0 e])
       |> List.intersperse (Leaf (Nothing, "thread atom", "|>")))

    FieldAccess id obj field ->
      Nested (Just id, "fieldaccessexpr")
      [ Nested (Nothing, "fieldobject") [(vExpr 0 obj)]
      , Leaf (Nothing, "fieldaccessop operator atom", ".")
      , vVarBind field
      ]

replaceExpr : ID -> Expr -> AST -> AST
replaceExpr id replacement ast =
  replaceExpr_ id replacement ast

replaceExpr_ : ID -> Expr -> Expr -> Expr
replaceExpr_ id replacement expr =
  let re = replaceExpr_ id replacement
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
        let countHoles =
              List.foldr (\c acc ->
                case c of
                  Hole _ -> acc + 1
                  _      -> acc) 0
            preCount = countHoles exprs
            reppedExprs = reList exprs
            postCount = countHoles reppedExprs
            nexprs =
              -- if the hole filled was in the current thread, then add a hole
              if preCount /= postCount
              then reppedExprs ++ [Hole (ID (Util.random ()))]
              else reppedExprs
        in
        Thread id nexprs

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

closeThread : ID -> AST -> AST
closeThread threadid ast =
  closeThread_ threadid ast

closeThread_ : ID -> Expr -> Expr
closeThread_ threadid expr =
  let ct = closeThread_ threadid
      ctList = List.map ct
  in
  case expr of
    Value id v ->
      Value id v

    Let id lhs rhs expr ->
      Let id lhs (ct rhs) (ct expr)

    If id cond ifbody elsebody ->
      If id (ct cond) (ct ifbody) (ct elsebody)

    Variable id name ->
      Variable id name

    FnCall id name exprs ->
      FnCall id name (ctList exprs)

    Lambda id vars expr ->
      Lambda id vars (ct expr)

    Hole id ->
      Hole id

    FieldAccess id obj name ->
      -- Probably don't want threading in a field access,
      -- but we'll make this work anyway
      FieldAccess id (ct obj) name

    Thread id exprs ->
      if id == threadid
      then
        let rexprs = List.reverse exprs
            nexprs =
              case rexprs of
                last :: rest ->
                  case last of
                    Hole _ -> rest
                    _ -> last :: rest
                _ -> rexprs
            rnexprs = List.reverse nexprs
        in
            case rnexprs of
              [] -> Hole id
              [x] -> x
              xs -> Thread id xs
      else
        Thread id (ctList exprs)

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
           Full s -> Full s
           Empty id ->
             if id == hid
             then Full replacement
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

replaceFieldHole : ID -> VarName -> AST -> AST
replaceFieldHole hid replacement ast =
  replaceFieldHole_ hid replacement ast

replaceFieldHole_ : ID -> VarName -> Expr -> Expr
replaceFieldHole_ hid replacement expr =
  let rfh = replaceFieldHole_ hid replacement
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
           Full s -> Full s
           Empty id ->
             if id == hid
             then Full replacement
             else name
      in FieldAccess id (rfh obj) newName

isHole : Expr -> Bool
isHole e =
  case e of
    Hole _ -> True
    _ -> False

emptyHoleID : VarBind -> Maybe ID
emptyHoleID vb =
  case vb of
    Empty hid -> Just hid
    Full _ -> Nothing

listBindHoles : Expr -> List ID
listBindHoles expr =
  let lbhList : List Expr -> List ID
      lbhList exprs =
        exprs
        |> List.map listBindHoles
        |> List.concat
      bhList : List VarBind -> List ID
      bhList = List.filterMap emptyHoleID
  in
  case expr of
    Value _ v ->
      []

    Let _ lhs rhs expr ->
      let exprBindHoles = lbhList [rhs, expr]
          bindHoles = ME.toList <| emptyHoleID lhs
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
      listFieldHoles obj ++ (ME.toList (emptyHoleID ident))

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
          bindHoles = ME.toList (emptyHoleID lhs)
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
      listHoles obj ++ (ME.toList <| emptyHoleID ident)

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

walk : AST -> Element
walk = vExpr 0

-- takes an ID of an expr in the AST to wrap in a thread
wrapInThread : ID -> AST -> (AST, ID)
wrapInThread id ast =
  let tw = wrapInThread_ id ast
  in (tw.expr, deMaybe tw.threadID)

type alias ThreadWrap = { expr: Expr, threadID: Maybe ID }
wrapInThread_ : ID -> Expr -> ThreadWrap
wrapInThread_ hid expr =
  let wt e = wrapInThread_ hid e
      wrap e =
        case e of
          Thread id _ -> { expr = e, threadID = Just id }
          _ ->
            let tid = ID (Util.random())
            in { expr = Thread tid [e], threadID = Just tid }
      wrapOr e fn =
        if (toID e) == hid
        then wrap e
        else fn e
      noWrap e = { expr = e, threadID = Nothing }
      filterMaybe xs = xs |> List.filterMap identity |> List.head
  in
      case expr of
        Value _ _ -> wrapOr expr noWrap
        Hole _ -> wrapOr expr noWrap
        Variable _ _ -> wrapOr expr noWrap

        Let id lhs rhs body ->
          wrapOr expr (\_ ->
            let newRhs = wt rhs
                newBody = wt body
                tid = filterMaybe [newRhs.threadID, newBody.threadID]
            in
                { expr = Let id lhs newRhs.expr newBody.expr
                , threadID = tid})

        If id cond ifbody elsebody ->
          wrapOr expr (\_ ->
            let newCond     = wt cond
                newIfbody   = wt ifbody
                newElsebody = wt elsebody
                tid = filterMaybe [newCond.threadID, newIfbody.threadID, newElsebody.threadID]
            in
            { expr = If id newCond.expr newIfbody.expr newElsebody.expr
            , threadID = tid })

        FnCall id name exprs ->
          wrapOr expr (\_ ->
            let nexprs = List.map wt exprs
                newExprs = List.map .expr nexprs
                tid = filterMaybe (List.map .threadID nexprs)
            in
                { expr = FnCall id name newExprs, threadID = tid })

        Lambda id vars lexpr ->
          wrapOr expr (\_ ->
            let newBody = wt lexpr in
            { expr = Lambda id vars newBody.expr, threadID = newBody.threadID })

        Thread id exprs ->
          wrapOr expr (\_ ->
            let nexprs = List.map wt exprs
                newExprs = List.map .expr nexprs
                tid = filterMaybe (List.map .threadID nexprs)
            in
                { expr = Thread id newExprs, threadID = tid })

        FieldAccess id obj field ->
          wrapOr expr (\_ ->
            let nObj = wt obj in
            { expr = FieldAccess id nObj.expr field, threadID = nObj.threadID })

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

