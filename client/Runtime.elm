module Runtime exposing (..)

-- stldib
-- import Json.Decode as JSD

-- libs
import List.Extra as LE

-- dark
import Types exposing (..)
-- import Prelude exposing (..)
-- import Util
import StrDict

isCompatible : Tipe -> Tipe -> Bool
isCompatible t1 t2 =
  (t1 == TAny) || (t2 == TAny) || (t1 == t2)

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
    DResp _ _ -> TResp
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
    DOption OptNothing -> True
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

toRepr_ : Int -> Dval -> String
toRepr_ oldIndent dv =
  let wrap value =
        "<" ++ (dv |> typeOf |> tipe2str) ++ ": " ++ value ++ ">"
      asType =
        "<" ++ (dv |> typeOf |> tipe2str) ++ ">"
      nl = "\n" ++ (String.repeat oldIndent " ")
      inl = "\n" ++ (String.repeat (oldIndent + 2) " ")
      indent = oldIndent + 2
      objToString l =
        l
        |> List.map (\(k,v) -> k ++ ": " ++ toRepr_ indent v)
        |> String.join ("," ++ inl)
        |> \s -> "{" ++ inl ++ s ++ nl ++ "}"
  in
  case dv of
    DInt i -> toString i
    DFloat f -> toString f
    DStr s -> "\"" ++ s ++ "\""
    DBool True -> "true"
    DBool False -> "false"
    DChar c -> "'" ++ String.fromList [c] ++ "'"
    DNull -> "null"
    DID s -> wrap s
    DDate s -> wrap s
    DTitle s -> wrap s
    DUrl s -> wrap s
    DDB s -> wrap s
    DUuid s -> wrap s
    DError s -> wrap s
    DPassword s -> wrap s
    DBlock -> asType
    DIncomplete -> asType
    DResp (Redirect url) dv_ -> "302 " ++ url ++ nl ++ toRepr_ indent dv_
    DResp (Response code hs) dv_ ->
      let headers = objToString (List.map (Tuple.mapSecond (\s -> DStr s)) hs) in
      toString code ++ " " ++ headers ++ nl ++ toRepr dv_
    DOption OptNothing -> "Nothing"
    DOption (OptJust dv_) -> "Some " ++ (toRepr dv_)
    DErrorRail dv_ -> wrap (toRepr dv_)
    -- TODO: newlines and indentation
    DList l ->
      case l of
        [] -> "[]"
        DObj _ :: rest ->
          "["
          ++ inl
          ++ String.join (inl ++ ", ") (List.map (toRepr_ indent) l)
          ++ nl
          ++ "]"
        l ->
          "[ " ++ String.join ", " (List.map (toRepr_ indent) l) ++ "]"
    DObj o ->
      objToString (StrDict.toList o)

-- Copied from Dval.to_repr in backend code
toRepr : Dval -> String
toRepr dv =
  toRepr_ 0 dv

inputValueAsString : InputValueDict -> String
inputValueAsString iv =
  iv
  |> \i -> DObj i
  |> toRepr
  |> String.split "\n"
  |> List.drop 1
  |> LE.init
  |> Maybe.withDefault []
  |> List.map (String.dropLeft 2)
  |> String.join "\n"


