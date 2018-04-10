module Functions exposing (..)

-- lib
import List.Extra as LE
import Navigation

-- dark
import Types exposing (..)
import Toplevel as TL
import Util exposing (deMaybe)

ufpToP : UserFunctionParameter -> Maybe Parameter
ufpToP ufp =
    case (ufp.name, ufp.tipe) of
      (F _ name, F _ tipe) ->
        { name = name
        , tipe = tipe
        , block_args = ufp.block_args
        , optional = ufp.optional
        , description = ufp.description
        } |> Just
      _ -> Nothing

ufmToF : UserFunctionMetadata -> Maybe Function
ufmToF ufm =
  let ps = List.filterMap ufpToP ufm.parameters
      sameLength = (List.length ps) == (List.length ufm.parameters)
  in
      case (ufm.name, ufm.returnTipe, sameLength) of
        (F _ name, F _ tipe, True) ->
          { name = name
          , parameters = ps
          , description = ufm.description
          , returnTipe = tipe
          , infix = ufm.infix
          } |> Just
        _ -> Nothing

find : Model -> TLID -> Maybe UserFunction
find m id =
  LE.find (\f -> id == f.tlid) m.userFunctions

findExn : Model -> TLID -> UserFunction
findExn m id =
  find m id |> deMaybe "Functions.findExn"


sameName : String -> UserFunction -> Bool
sameName name uf =
  case uf.metadata.name of
    F _ n -> n == name
    _ -> False


findByName : Model -> String -> Maybe UserFunction
findByName m s =
  LE.find (sameName s) m.userFunctions

findByNameExn : Model -> String -> UserFunction
findByNameExn m s =
  findByName m s |> deMaybe "Functions.findByNameExn"

startEditing : Model -> Modification
startEditing m =
  case unwrapCursorState m.cursorState of
    Selecting tlid (Just id) ->
      let tl = TL.getTL m tlid
          pd = TL.findExn tl id
      in
          case pd of
            PExpr (F _ (FnCall name _)) ->
              edit (findByNameExn m name)
            _ ->
              Debug.crash "should only be called on an FnCall for a UserFunction"
    _ -> NoChange


edit : UserFunction -> Modification
edit uf =
  MakeCmd (Navigation.modifyUrl (urlForFn uf))

urlForFn : UserFunction -> String
urlForFn uf =
  "/admin/ui#" ++ ("fn=" ++ (toString (deTLID uf.tlid)))

