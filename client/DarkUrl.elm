module DarkUrl exposing (..)

-- builtin
import Dict
import List
import String

-- lib
import Html
import Html.Attributes as Attrs
import Url

-- dark
import Types exposing (..)
import Prelude exposing (..)
import Functions
import Defaults


hashUrlParams : List (String, String) -> String
hashUrlParams params =
  let merged = List.map (\(k, v) -> k ++ "=" ++ v) params
  in "#" ++ (String.join "&" merged)

urlOf : Page -> Pos -> String
urlOf page pos =
  let head =
        case page of
          Toplevels _ -> []
          Fn tlid _ -> [("fn", String.fromInt (deTLID tlid))]
      tail =
        [ ("x", String.fromInt pos.x)
        , ("y", String.fromInt pos.y) ]
  in hashUrlParams (head ++ tail)

urlFor : Page -> String
urlFor page =
  urlOf page
    (case page of
      Toplevels pos -> pos
      Fn _ pos -> pos
    )

navigateTo : Page -> Cmd Msg
navigateTo page =
  Navigation.newUrl (urlFor page)

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
  let pos =
        case m.currentPage of
          Toplevels _ -> m.canvas.offset
          Fn tlid _ -> m.canvas.fnOffset
      state = m.urlState
  in
  if pos /= state.lastPos
  then
    Many
      [ TweakModel (\m_ -> { m_ | urlState = { state | lastPos = pos } })
      , MakeCmd (Navigation.modifyUrl (urlOf m.currentPage pos))
      ]
  else NoChange


parseLocation : Model -> Url.Url -> Maybe Page
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
              (Ok x_, Ok y_) -> Just { x = x_, y = y_ }
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


changeLocation : Model -> Url.Url -> Modification
changeLocation m loc =
  let mPage = parseLocation m loc in
  case mPage of
    Just (Fn id pos) ->
      case Functions.find m id of
        Nothing -> DisplayError "No function"
        _ -> SetPage (Fn id pos)
    Just page -> SetPage page
    _ -> NoChange
