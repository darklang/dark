module AST exposing (..)

-- builtin
import List
import String

-- lib
import String.Extra as SE

-- dark
import Types exposing (..)

indent : Int -> String -> String
indent depth s =
  let spaces = String.repeat depth " " in
  spaces ++ SE.replace "\n" ("\n" ++ spaces) s

mapExceptFirst : (a -> a) -> List a -> List a
mapExceptFirst fn l =
  case l of
    [] -> []
    [val] -> [val]
    (first :: rest) -> first :: (List.map fn rest)

withParens : String -> String
withParens s =
  "(" ++ s ++ ")"

nestedToStringRep : Expr -> String
nestedToStringRep expr =
  case expr of
    Let _ _ -> expr |> pp |> withParens
    If _ _ _ -> expr |> pp |> withParens
    FnCall _ _ -> expr |> pp |> withParens
    Lambda _ _ -> pp expr
    Variable _ -> pp expr
    Value _ -> pp expr

prefixStringRep : FnName -> List Expr -> String
prefixStringRep name exprs =
  name
  ++ " "
  ++ (exprs
      |> List.map nestedToStringRep
      |> String.join " ")

infixStringRep : FnName -> List Expr -> String
infixStringRep name exprs =
  case exprs of
    [first, second] ->
      (nestedToStringRep first)
      ++ " " ++ name
      ++ " " ++ (nestedToStringRep second)
    _ ->
      prefixStringRep (withParens name) []

isInfix : FnName -> Bool
isInfix name =
  List.member name ["<", "=="]


pp : Expr -> String
pp = toStringRep



toStringRep : Expr -> String
toStringRep expr =
  case expr of
    Value v -> v

    Let vars expr ->
      let varStr =
        vars
        |> List.map (\(var, expr) -> var ++ " = " ++ pp expr)
        |> mapExceptFirst (indent 2)
        |> String.join "\n" in
      "let "
      ++ varStr
      ++ " in \n"
      ++ pp expr

    If cond ifblock elseblock ->
      "if " ++ nestedToStringRep cond ++ " {\n"
      ++ indent 2 (pp ifblock)
      ++ "\n} else {\n"
      ++ indent 2 (pp elseblock)
      ++ "\n}"

    FnCall name exprs ->
      if isInfix name
      then infixStringRep name exprs
      else prefixStringRep name exprs

    Lambda vars expr ->
      "\n  (\\"
      ++ (String.join " " vars)
      ++ " -> \n"
      ++ (pp expr |> indent 4)
      ++ ")"

    Variable name ->
      name







