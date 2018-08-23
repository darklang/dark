module Url exposing (..)

-- builtin
import Dict

-- lib
import Html
import Html.Attributes as Attrs
import Navigation
-- import Maybe.Extra as ME


-- dark
import Types exposing (..)
import Prelude exposing (..)
import Functions
import Defaults
import Viewport


urlFor : Page -> String
urlFor page =
  let posStr pos =
    "x=" ++ toString pos.x ++ "&y=" ++ toString pos.y
  in
  case page of
    Toplevels pos ->
      "#" ++  posStr pos
    Fn tlid pos ->
      "#fn=" ++ toString (deTLID tlid) ++ "&" ++ posStr pos

linkFor : Page -> String -> List (Html.Html Msg) -> Html.Html Msg
linkFor page class content =
  Html.a
    [ Attrs.href (urlFor page)
    , Attrs.class class]
    content

-- When scrolling, there are way too many events to process them through
-- the History/location handlers. So instead we process them directly,
-- and update the browser url periodically.
maybeUpdateScrollUrl : Model -> Modification
maybeUpdateScrollUrl m =
  let pos = Viewport.pagePos m.currentPage
      state = m.urlState
  in
  if pos /= state.lastPos
  then Many [ TweakModel (\m -> { m | urlState = { state | lastPos = pos } })
            , MakeCmd (Navigation.modifyUrl (urlFor m.currentPage))
            ]
  else NoChange


parseLocation : Model -> Navigation.Location -> Maybe Page
parseLocation m loc =
  let unstructured = loc.hash
                   |> String.dropLeft 1 -- remove "#"
                   |> String.split "&"
                   |> List.map (String.split "=")
                   |> List.filterMap
                      (\arr ->
                        case arr of
                          a :: b :: [] -> Just (String.toLower a, b)
                          _ -> Nothing)
                   |> Dict.fromList
      center =
        case (Dict.get "x" unstructured, Dict.get "y" unstructured) of
          (Just x, Just y) ->
            case (String.toInt x, String.toInt y) of
              (Ok x, Ok y) -> Just { x = x, y = y }
              _  -> Nothing
          _ -> Nothing
      editedFn =
        case (Dict.get "fn" unstructured) of
          Just sid ->
            case String.toInt sid of
              Ok id ->
                Just <|
                  Fn (TLID id)
                     (Maybe.withDefault Defaults.fnPos center)
              _ -> Nothing
          _ -> Nothing
  in
  case (center, editedFn) of
    (_, Just fn) -> editedFn
    (Just pos, _) -> Just (Toplevels pos)
    _ -> Nothing


changeLocation : Model -> Navigation.Location -> Modification
changeLocation m loc =
  let mPage = parseLocation m loc in
  case mPage of
    Just (Fn id pos) ->
      case Functions.find m id of
        Nothing -> Error "No function"
        _ -> SetPage (Fn id pos)
    Just page -> SetPage page
    _ -> NoChange

