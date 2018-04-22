module Runtime exposing (..)

-- stldib
import Json.Decode as JSD

-- libs

-- dark
import Types exposing (..)
import Util
import JSON



isInt : String -> Bool
isInt s = (String.toInt s |> Util.resultIsOk)
          -- https://github.com/elm-lang/core/issues/919
          && s /= "+"
          && s /= "-"
          && s /= "0x"

isFloat : String -> Bool
isFloat s = String.toFloat s |> Util.resultIsOk

isString : String -> Bool
isString s = String.startsWith "\"" s && String.endsWith "\"" s

isBool : String -> Bool
isBool s = String.toLower s == "true" || String.toLower s == "false"

isChar : String -> Bool
isChar s = String.length s == 3 && String.startsWith s "\'" && String.endsWith s "\'"

isNull : String -> Bool
isNull s = String.toLower s == "null"

isIncomplete : LiveValue -> Bool
isIncomplete s =
  s.tipe == TIncomplete

isError : LiveValue -> Bool
isError s =
  s.tipe == TError



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
    TError -> "Error"
    TResp -> "Response"
    TDB -> "Datastore"
    TID -> "ID"
    TDate -> "Date"
    TTitle -> "Title"
    TUrl -> "Url"
    TBelongsTo s -> s
    THasMany s -> "[" ++ s ++ "]"

str2tipe : String -> Tipe
str2tipe t =
  case String.toLower t of
  "any" -> TAny
  "int" -> TInt
  "float" -> TFloat
  "bool" -> TBool
  "boolean" -> TBool
  "nothing" -> TNull
  "char" -> TChar
  "str" -> TStr
  "string" -> TStr
  "list" -> TList
  "obj" -> TObj
  "block" -> TBlock
  "incomplete" -> TIncomplete
  "response" -> TResp
  "datastore" -> TDB
  "date" -> TDate
  "error" -> TError
  other ->
    if String.startsWith "[" other && String.endsWith "]" other
    then
      other
      |> String.dropLeft 1
      |> String.dropRight 1
      |> THasMany
    else
      TBelongsTo other

tipeOf : String -> Tipe
tipeOf s =
  if isInt s then TInt
  else if isFloat s then TFloat
  else if isString s then TStr
  else if isChar s then TChar
  else if isBool s then TBool
  else
    TIncomplete

isLiteral : String -> Bool
isLiteral s =
     isInt s
  || isFloat s
  || isString s
  || isChar s
  || isBool s
  || isNull s


extractErrorMessage : LiveValue -> String
extractErrorMessage lv =
  if isError lv
  then
    lv
    |> .json
    |> JSD.decodeString JSON.decodeException
    |> Result.toMaybe
    |> Maybe.map .short
    |> Maybe.withDefault lv.value
  else lv.value


