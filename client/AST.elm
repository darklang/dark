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




toStringRep : Expr -> String
toStringRep expr =
  let pp = toStringRep in
  case expr of
    Value v -> v
    If cond ifblock elseblock ->
      "if (" ++ pp cond ++ ") {\n"
      ++ indent 2 (pp ifblock)
      ++ "\n} else {\n"
      ++ indent 2 (pp elseblock)
      ++ "\n}"
    _ -> "TODO"


