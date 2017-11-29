module AST exposing (..)

-- builtin
import List
import Tuple

-- lib
import String.Extra as SE

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
         valu  =
           -- TODO: remove
           if RT.isString v
           then "“" ++ (SE.unquote v) ++ "”"
           else v
     in Leaf (Just id, "atom value " ++ cssClass, valu)

    Let id vars expr ->
      Nested (Just id, "letexpr")
        [ Leaf (Nothing, "let keyword atom", "let")
        , Nested (Nothing, "letbindings")
            (List.map
              (\(l, r) ->
                Nested (Nothing, "letbinding")
                  [ vVarBind l
                  , Leaf (Nothing, "letbind atom", "=")
                  , vExpr nest r
                  ]
              )
              vars
             )
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

replaceHole : ID -> Expr -> AST -> AST
replaceHole id replacement ast =
  replaceHole_ id replacement ast

replaceHole_ : ID -> Expr -> Expr -> Expr
replaceHole_ hid replacement expr =
  let rh = replaceHole_ hid replacement
      rhList : List Expr -> List Expr
      rhList exprs = List.map rh exprs
  in
  case expr of
    Value id v ->
      Value id v

    Let id vars expr ->
      let vs = List.map (\(vb, e) ->
         case vb of
           -- TODO: replace this with a different replaceBindHole
           -- that gets called from the submit of a special entry box
           Full s -> (Full s, rh e)
           Empty id ->
             if id == hid
             then
               case replacement of
                 Value _ s -> (Full (SE.unquote s), e)
                 _         -> (Empty id, e)
             else (Empty id, rh e)
             ) vars
      in Let id vs (rh expr)

    If id cond ifbody elsebody ->
      If id (rh cond) (rh ifbody) (rh elsebody)

    Variable id name ->
      Variable id name

    FnCall id name exprs ->
      FnCall id name (rhList exprs)

    Lambda id vars expr ->
      Lambda id vars (rh expr)

    Hole id ->
      if id == hid
      then (replaceID id replacement)
      else expr

    Thread id exprs ->
      let countHoles =
            List.foldr (\c acc ->
              case c of
                Hole _ -> acc + 1
                _      -> acc) 0
          preCount = countHoles exprs
          reppedExprs = rhList exprs
          postCount = countHoles reppedExprs
          nexprs =
            -- if the hole filled was in the current thread, then add a hole
            if preCount /= postCount
            then reppedExprs ++ [Hole (ID (Util.random ()))]
            else reppedExprs
      in
      Thread id nexprs

replaceID : ID -> Expr -> Expr
replaceID newID expr =
  case expr of
    Value id s -> Value newID s
    Let id bs e -> Let newID bs e
    If id c t f -> If newID c t f
    Variable id s -> Variable newID s
    FnCall id n b -> FnCall newID n b
    Lambda id vs b -> Lambda newID vs b
    Hole id -> Hole newID
    Thread id es -> Thread newID es


toID : Expr -> ID
toID expr =
  case expr of
    Value id _ -> id
    Let id _ _ -> id
    If id _ _ _ -> id
    Variable id _ -> id
    FnCall id _ _ -> id
    Lambda id _ _ -> id
    Hole id -> id
    Thread id _ -> id

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

    Let id vars expr ->
      let vs = List.map (\(vb, e) -> (vb, ct e)) vars
      in Let id vs (ct expr)

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

    Let id vars expr ->
      let vs = List.map (\(vb, e) ->
         case vb of
           Full s -> (Full s, rbh e)
           Empty id ->
             if id == hid
             then (Full replacement, e)
             else (Empty id, rbh e)
             ) vars
      in Let id vs (rbh expr)

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

bindHoleID : VarBind -> Maybe ID
bindHoleID vb =
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
      bhList = List.filterMap bindHoleID
  in
  case expr of
    Value _ v ->
      []

    Let _ vars expr ->
      let exprBindHoles = vars
                        |> List.map Tuple.second
                        |> (++) [expr]
                        |> lbhList
          bindHoles = vars
                    |> List.map Tuple.first
                    |> bhList
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
      bhList : List VarBind -> List ID
      bhList = List.filterMap bindHoleID
  in
  case expr of
    Value _ v ->
      []

    Let _ vars expr ->
      let exprHoles = vars
                      |> List.map Tuple.second
                      |> (++) [expr]
                      |> lhList
          bindHoles = vars
                    |> List.map Tuple.first
                    |> bhList
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
      pluckId xs =
        xs
        |> List.filterMap .threadID
        |> List.head
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

        Let id vars bexpr ->
          wrapOr expr (\_ ->
            let vs = List.map (\(vb, e) -> (vb, wt e)) vars
                newVars = List.map (\(vb, tw) -> (vb, tw.expr)) vs
                vId = vs |> List.map Tuple.second |> pluckId
                bw = wt bexpr
                tid = filterMaybe [vId, bw.threadID]
            in
                { expr = Let id newVars bw.expr, threadID = tid })

        If id cond ifbody elsebody ->
          wrapOr expr (\_ ->
            let newCond     = wt cond
                newIfbody   = wt ifbody
                newElsebody = wt elsebody
                tid = filterMaybe [newCond.threadID, newIfbody.threadID, newElsebody.threadID]
            in
            { expr = If id newCond.expr newIfbody.expr newElsebody.expr, threadID = tid })

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
        Let id vars bexpr ->
          returnOr (\_ ->
            let vs = vars |> List.map Tuple.second |> List.map se
                be = se bexpr
            in
                filterMaybe (be :: vs)) expr

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

