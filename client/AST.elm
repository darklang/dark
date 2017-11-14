module AST exposing (..)

-- builtin
import List

-- lib
import String.Extra as SE
import Maybe.Extra as ME
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
                  [ vVarname l
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

findFirstHole_ : Expr -> Maybe HID
findFirstHole_ expr =
  let ffList : List Expr -> Maybe HID
      ffList exprs =
        List.filterMap findFirstHole_ exprs
        |> List.head
  in
  case expr of
    Value _ v ->
      Nothing

    Let _ vars expr ->
      vars
      |> List.map Tuple.second
      |> ffList
      |> ME.or (findFirstHole_ expr)

    If _ cond ifbody elsebody ->
      findFirstHole_ elsebody
      |> ME.or (findFirstHole_ ifbody)
      |> ME.or (findFirstHole_ cond)

    Variable _ name ->
      Nothing

    FnCall _ name exprs ->
      ffList exprs

    Lambda _ vars expr ->
      findFirstHole_ expr

    Hole id -> Just id

findFirstHole : AST -> HID
findFirstHole ast = findFirstHole_ ast |> Maybe.withDefault (HID 3)

replaceHole : HID -> Expr -> AST -> AST
replaceHole hid replacement ast =
  replaceHole_ hid replacement ast

replaceHole_ : HID -> Expr -> Expr -> Expr
replaceHole_ hid replacement expr =
  let rh = replaceHole_ hid replacement
      rhList : List Expr -> List Expr
      rhList exprs = List.map rh exprs
  in
  case expr of
    Value eid v ->
      Value eid v

    Let eid vars expr ->
      let vs = List.map (\(vn, e) -> (vn, rh e)) vars
      in Let eid vs (rh expr)

    If eid cond ifbody elsebody ->
      If eid (rh cond) (rh ifbody) (rh elsebody)

    Variable eid name ->
      Variable eid name

    FnCall eid name exprs ->
      FnCall eid name (rhList exprs)

    Lambda eid vars expr ->
      Lambda eid vars (rh expr)

    Hole id ->
      if id == hid
      then replacement
      else expr

listHoles : Expr -> List HID
listHoles expr =
  let lhList : List Expr -> List HID
      lhList exprs =
        exprs
        |> List.map listHoles
        |> List.concat
  in
  case expr of
    Value _ v ->
      []

    Let _ vars expr ->
      vars
      |> List.map Tuple.second
      |> (++) [expr]
      |> lhList

    If _ cond ifbody elsebody ->
      lhList [cond, ifbody, elsebody]

    Variable _ name ->
      []

    FnCall _ name exprs ->
      lhList exprs

    Lambda _ vars expr ->
      listHoles expr

    Hole id -> [id]

findNextHole : HID -> AST -> HID
findNextHole cur ast =
  let holes = listHoles ast
  in case (LE.dropWhile (\x -> x /= cur) holes) of
     cur :: next :: _ -> next
     [cur] -> holes |> List.head |> deMaybe
     [] -> HID 237

walk : AST -> Element
walk = vExpr 0
