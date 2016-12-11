port module Main exposing (..)

import Html exposing (div, h1)
import Html.Attributes as Attrs
import Html.Events as Events
import Keyboard
import Mouse
import Json.Decode
import Dom

import Native.Timestamp
import Task


timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a



main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions}

-- MODEL
type alias Model = { nodes : List DataStore, state : String, errors : List String }
type alias DataStore = { name : String, fields : List (String, String) }

emptyDS : DataStore
emptyDS = { name = "", fields = [] }

-- states
state_NOTHING = "NOTHING"
state_INITIAL = state_NOTHING
state_ADDING_DS_NAME = "ADDING_DS_NAME"
state_ADDING_DS_FIELD = "ADDING_DS_FIELD_NAME"
state_ADDING_DS_TYPE = "ADDING_DS_FIELD_TYPE"


init : ( Model, Cmd Msg )
init = ( { nodes = []
         , state = state_INITIAL
         , errors = ["No errors"]
         }, Cmd.none )

-- UPDATE
type Msg
    = MouseMsg Mouse.Position
    | InputKeyMsg Keyboard.KeyCode String
    | KeyMsg Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())

focusInput = Dom.focus inputID |> Task.attempt FocusResult
addError error model =
    let time = timestamp ()
               in
    List.take 4 ((error ++ "-" ++ toString time) :: model.errors)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMsg _ ->
            ({ model | state = state_ADDING_DS_NAME
                     , nodes = emptyDS :: model.nodes }
             , focusInput)
        InputKeyMsg 13 target ->
            ({ model | errors = addError "TODO" model}, Cmd.none)
        InputKeyMsg key _ ->
            ({ model | errors = addError "Ignoring input" model}, Cmd.none)
        KeyMsg key ->
            ({ model | errors = addError "Not supported yet" model}, Cmd.none)
        FocusResult result ->
            ( model, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
--        , Keyboard.downs KeyMsg
        ]

-- VIEW
view : Model -> Html.Html Msg
view model =
    Html.div [] [ viewInput
                , viewErrors model.errors
                , viewState model.state
                , viewAllNodes model.nodes
                ]

inputID = "darkInput"

onKeyDown : (Int -> String -> msg) -> Html.Attribute msg
onKeyDown msg =
  Events.on "keydown" (Json.Decode.map2 msg Events.keyCode Events.targetValue)

viewInput = div [] [ Html.input [ Attrs.id inputID
                                , onKeyDown InputKeyMsg
                                ] [] ]

viewState state = div [] [ Html.text ("state: " ++ state) ]
viewErrors errors = div [] ((Html.text "errors: ") :: (List.map (\x -> (Html.div [] [Html.text x])) errors))

displayNode node = Html.text node.name

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map displayNode nodes
        nHeading = h1 [] [Html.text "All nodes"]
    in
        div [] (nHeading :: allNodes)

-- Util
