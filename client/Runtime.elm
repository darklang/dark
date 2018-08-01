module Runtime exposing (..)

-- stldib
import Json.Decode as JSD

-- libs

-- dark
import Types exposing (..)
import Prelude exposing (..)
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
isString s =
  String.startsWith "\"" s
  && String.endsWith "\"" s

isTrue : String -> Bool
isTrue s = String.toLower s == "true"

isFalse : String -> Bool
isFalse s = String.toLower s == "false"

isBool : String -> Bool
isBool s = isTrue s || isFalse s

isChar : String -> Bool
isChar s =
  String.length s == 3
  && String.startsWith s "\'"
  && String.endsWith s "\'"

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
    TNull -> "Null"
    TChar -> "Char"
    TStr -> "String"
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
    TPassword -> "Password"
    TBelongsTo s -> s
    THasMany s -> "[" ++ s ++ "]"
    TDbList a -> "[" ++ (tipe2str a) ++ "]"

str2tipe : String -> Tipe
str2tipe t =
  let parseListTipe lt =
        case lt of
          "str" -> TStr
          "string" -> TStr
          "int" -> TInt
          "integer" -> TInt
          "float" -> TFloat
          "bool" -> TBool
          "boolean" -> TBool
          "password" -> TPassword
          "id" -> TID
          "null" -> todo "not implemented yet"
          "any" -> todo "not implemented yet"
          "list" -> todo "not implemented yet"
          "obj" -> todo "not implemented yet"
          "block" -> todo "not implemented yet"
          "incomplete" -> todo "not implemented yet"
          "response" -> todo "not implemented yet"
          "datastore" -> todo "not implemented yet"
          "date" -> todo "not implemented yet"
          "error" -> todo "not implemented yet"
          table -> THasMany table
  in
  case String.toLower t of
  "any" -> TAny
  "int" -> TInt
  "float" -> TFloat
  "bool" -> TBool
  "boolean" -> TBool
  "null" -> TNull
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
  "password" -> TPassword
  "id" -> TID
  other ->
    if String.startsWith "[" other && String.endsWith "]" other
    then
      other
      |> String.dropLeft 1
      |> String.dropRight 1
      |> parseListTipe
    else
      TBelongsTo other

tipeOf : String -> Tipe
tipeOf s =
  if isInt s then TInt
  else if isFloat s then TFloat
  else if isString s then TStr
  else if isChar s then TChar
  else if isBool s then TBool
  else if isNull s then TNull
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


