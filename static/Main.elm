import Html exposing (div, h1)
import Html.Attributes as Attrs
import Html.Events as Events
import Keyboard
import Mouse
import Json.Decode

main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions}

-- MODEL
type alias Model = { nodes : List DataStore, current : Maybe DataStore }
type alias DataStore = { name : String, fields : List (String, String) }

emptyDS : DataStore
emptyDS = { name = "", fields = [] }

init : ( Model, Cmd Msg )
init =
    ( { nodes = [], current = Maybe.Nothing }, Cmd.none )

-- UPDATE

type CurrentMsg
    = SetName String

type Msg
    = MouseMsg Mouse.Position
    | CurrentMsg CurrentMsg
    | KeyMsg Keyboard.KeyCode String

updateCurrent : CurrentMsg -> DataStore -> DataStore
updateCurrent msg current =
    case msg of
        SetName name ->
            { current | name = name }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let new_model =
    case msg of
        MouseMsg _ ->
            { model | current = Maybe.Just emptyDS }
        KeyMsg 13 target ->
            { model | current = Maybe.map (updateCurrent (SetName target)) model.current }
        CurrentMsg cmsg ->
            { model | current = Maybe.map (updateCurrent cmsg) model.current }
        _ -> model
      cmd = Cmd.none

  in (new_model, cmd )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        -- , Keyboard.downs KeyMsg
        ]

-- VIEW


onKeyDown : (Int -> String -> msg) -> Html.Attribute msg
onKeyDown msg =
  Events.on "keydown" (Json.Decode.map2 msg Events.keyCode Events.targetValue)

view : Model -> Html.Html Msg
view model =
    Html.div [] [viewAllNodes model.nodes, viewCurrentNode model.current]


displayNode node = Html.text node.name

nodeForm {name} = if name == ""
                  then Html.input [Attrs.autofocus True, onKeyDown KeyMsg ] []
                  else Html.text name

viewCurrentNode : Maybe DataStore -> Html.Html Msg
viewCurrentNode current =
    let nodeHtml = case current of
                       Maybe.Nothing -> Html.text "None"
                       Maybe.Just val -> nodeForm val
        node = div [] [nodeHtml]
        heading = h1 [] [Html.text "Current node"]
    in div [] [heading, node]

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map displayNode nodes
        nHeading = h1 [] [Html.text "All nodes"]
    in
        div [] (nHeading :: allNodes)
