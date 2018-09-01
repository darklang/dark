module Runtime exposing (..)

-- stldib
import Json.Decode as JSD

-- libs
import List.Extra as LE

-- dark
import Types exposing (..)
-- import Prelude exposing (..)
-- import Util
-- import JSON

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
    TOption -> "Option"
    TPassword -> "Password"
    TUuid -> "UUID"
    TErrorRail -> "ErrorRail"
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
          "uuid" -> TUuid
          "option" -> TOption
          "null" -> TNull
          "any" -> TAny
          "list" -> TList
          "obj" -> TObj
          "block" -> TBlock
          "incomplete" -> TIncomplete
          "response" -> TResp
          "datastore" -> TDB
          "date" -> TDate
          "error" -> TError
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
    "option" -> TOption
    "password" -> TPassword
    "id" -> TID
    "uuid" -> TUuid
    other ->
      if String.startsWith "[" other && String.endsWith "]" other
      then
        other
        |> String.dropLeft 1
        |> String.dropRight 1
        |> parseListTipe
      else
        TBelongsTo other

typeOf : Dval -> Tipe
typeOf dv =
  case dv of
    DInt _ -> TInt
    DFloat _ -> TFloat
    DBool _ -> TBool
    DNull -> TNull
    DChar _ -> TChar
    DStr _ -> TStr
    DList _ -> TList
    DObj _ -> TObj
    DBlock -> TBlock
    DIncomplete -> TIncomplete
    DError _ -> TError
    DResp _ -> TResp
    DDB _ -> TDB
    DID _ -> TID
    DDate _ -> TDate
    DTitle _ -> TTitle
    DUrl _ -> TUrl
    DOption _ -> TOption
    DErrorRail _ -> TErrorRail
    DPassword _ -> TPassword
    DUuid _ -> TUuid

isLiteral : Dval -> Bool
isLiteral dv =
  case dv of
    DInt _ -> True
    DFloat _ -> True
    DBool _ -> True
    DNull -> True
    DChar _ -> True
    DStr _ -> True
    _ -> False

isComplete : Dval -> Bool
isComplete dv =
  case dv of
    DError _ -> False
    DIncomplete -> False
    _ -> True

isTrue : Dval -> Bool
isTrue dv =
  dv == DBool True

toString : Dval -> String
toString dv =
  "TODO"

extractErrorMessage : Dval -> String
extractErrorMessage lv =
  "TODO"
  -- if isError lv
  -- then
  --   lv
  --   |> .json
  --   |> JSD.decodeString JSON.decodeException
  --   |> Result.toMaybe
  --   |> Maybe.map .short
  --   |> Maybe.withDefault lv.value
  -- else lv.value


