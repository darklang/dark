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
    TAny -> "Any"
    TInt -> "Int"
    TFloat -> "Float"
    TBool -> "Bool"
    TNull -> "Nothing"
    TChar -> "Char"
    TStr -> "Str"
    TList -> "List"
    TObj -> "Obj"
    TBlock -> "Block"
    TIncomplete -> "Incomplete"
    TResp -> "Response"
    TDB -> "Datastore"

str2tipe : String -> Tipe
str2tipe t =
  case t of
  "Any" -> TAny
  "Int" -> TInt
  "Float" -> TFloat
  "Bool" -> TBool
  "Nothing" -> TNull
  "Char" -> TChar
  "Str" -> TStr
  "List" -> TList
  "Obj" -> TObj
  "Block" -> TBlock
  "Incomplete" -> TIncomplete
  "Response" -> TResp
  "Datastore" -> TDB
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

