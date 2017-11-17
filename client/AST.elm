module AST exposing (..)

-- builtin
import List

-- lib
import String.Extra as SE
import List.Extra as LE

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
      Nested "namegroup atom"
      [ Leaf (Nothing, "module", mod)
      , Leaf (Nothing, "moduleseparator", "::")
      , Leaf (Nothing, "fnname", n)
      ]
    _ -> Leaf (Nothing, "fnname atom", name)

vPrefix : FnName -> List Expr -> Int -> Element
vPrefix name exprs nest =
  Nested ("fncall prefix " ++ (depthString nest))
    ((Nested ("op " ++ name) [vFn name])
    :: (List.map (vExpr (nest + 1)) exprs))


vInfix : FnName -> List Expr -> Int -> Element
vInfix name exprs nesting =
  case exprs of
    [first, second] ->
      Nested ("fncall infix " ++ (depthString nesting))
        [ Nested "lhs" [vExpr (nesting + 1) first]
        , Nested ("op " ++ name) [vFn name]
        , Nested "rhs" [vExpr nesting second]
        ]
    _ -> vPrefix ("(" ++ name ++ ")") exprs nesting

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "==", "%", "+"]

vVarname : VarName -> Element
vVarname v = Leaf (Nothing, "varname atom", v)

vVarBind : VarBind -> Element
vVarBind v =
  case v of
    Named s -> Leaf (Nothing, "varname atom", s)
    BindHole id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

vExpr : Int -> Expr -> Element
vExpr nest expr =
  case expr of
    Value _ v ->
     let cssClass = v |> RT.tipeOf |> toString |> String.toLower
         valu  =
           -- TODO: remove
           if RT.isString v
           then "“" ++ (SE.unquote v) ++ "”"
           else v
     in  Leaf (Nothing, "atom value " ++ cssClass, valu)

    Let _ vars expr ->
      Nested "letexpr"
        [ Leaf (Nothing, "let keyword atom", "let")
        , Nested "letbindings"
            (List.map
              (\(l, r) ->
                Nested "letbinding"
                  [ vVarBind l
                  , Leaf (Nothing, "letbind atom", "=")
                  , vExpr nest r
                  ]
              )
              vars
             )
        , Leaf (Nothing, "in keyword atom" , "in")
        , Nested "letbody" [vExpr nest expr]
        ]


    If _ cond ifbody elsebody ->
      Nested "ifexpr"
        [ Leaf (Nothing, "if keyword atom", "if")
        , Nested "cond" [vExpr (nest + 1) cond]
        , Nested "ifbody" [(vExpr 0 ifbody)]
        , Leaf (Nothing, "else keyword atom", "else")
        , Nested "elsebody" [(vExpr 0 elsebody)]
        ]

    Variable _ name ->
      vVarname name

    FnCall _ name exprs ->
      if isInfix name
      then vInfix name exprs nest
      else vPrefix name exprs nest

    Lambda _ vars expr ->
      Nested "lambdaexpr"
        [ Nested "lambdabinding" (List.map vVarname vars)
        , Leaf (Nothing, "arrow atom" , "->")
        , Nested "lambdabody" [vExpr 0 expr]
        ]

    Hole id -> Leaf (Just id, "hole atom", "＿＿＿＿＿＿")

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
           Named s -> (Named s, rh e)
           BindHole id ->
             if id == hid
             then
               case replacement of
                 Value _ s -> (Named (SE.unquote s), e)
                 _         -> (BindHole id, e)
             else (BindHole id, rh e)
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
      then replacement
      else expr

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
           Named s -> (Named s, rbh e)
           BindHole id ->
             if id == hid
             then (Named replacement, e)
             else (BindHole id, rbh e)
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

bindHoleID : VarBind -> Maybe ID
bindHoleID vb =
  case vb of
    BindHole hid -> Just hid
    Named _ -> Nothing

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

findNextHole : ID -> AST -> ID
findNextHole cur ast =
  let holes = listHoles ast
  in case (LE.dropWhile (\x -> x /= cur) holes) of
     cur :: next :: _ -> next
     [cur] -> holes |> List.head |> deMaybe
     [] -> ID 237

walk : AST -> Element
walk = vExpr 0
