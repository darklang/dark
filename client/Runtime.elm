module Runtime exposing (..)

-- stldib

-- libs

-- dark
import Types exposing (..)
import Util exposing (..)


isInt : String -> Bool
isInt s = String.toInt s |> Util.resultIsOk

isFloat : String -> Bool
isFloat s = String.toFloat s |> Util.resultIsOk

isString : String -> Bool
isString s = String.startsWith "\"" s && String.endsWith "\"" s

isBool : String -> Bool
isBool s = String.toLower s == "true" || String.toLower s == "false"

isChar : String -> Bool
isChar s = String.length s == 3 && String.startsWith s "\'" && String.endsWith s "\'"

isCompatible : Tipe -> Tipe -> Bool
isCompatible t1 t2 =
  t1 == TAny || t2 == TAny || t1 == t2


tipe2str : Tipe -> String
tipe2str t =
  case t of
    TInt -> "Int"
    TStr -> "Str"
    TChar -> "Char"
    TBool -> "Bool"
    TFloat -> "Float"
    TObj -> "Obj"
    TList -> "List"
    TBlock -> "Block"
    TOpaque -> "Opaque"
    TNull -> "Nothing"
    TAny -> "Any"
    TIncomplete -> "<incomplete>"
    TResp -> "Response"

str2tipe : String -> Tipe
str2tipe t =
  case t of
  "Int" -> TInt
  "Str" -> TStr
  "Char" -> TChar
  "Bool" -> TBool
  "Float" -> TFloat
  "Obj" -> TObj
  "List" -> TList
  "Block" -> TBlock
  "Opaque" -> TOpaque
  "Nothing" -> TNull
  "Any" -> TAny
  "Response" -> TResp
  "<incomplete>" -> TIncomplete
  "Error" -> TIncomplete -- temporary, maybe
  _ -> Debug.crash <| "invalid typename: " ++ t



tipeOf : String -> Tipe
tipeOf s =
  if isInt s then TInt
  else if isFloat s then TFloat
  else if isString s then TStr
  else if isChar s then TChar
  else if isBool s then TBool
  else
    TIncomplete

