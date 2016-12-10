import Html exposing (div, h1)
import Keyboard
import Mouse

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
emptyDS = { name = "new", fields = [] }

init : ( Model, Cmd Msg )
init =
    ( { nodes = [], current = Maybe.Nothing }, Cmd.none )

-- UPDATE
type Msg
    = MouseMsg Mouse.Position
    -- | KeyMsg Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let new =
    case msg of
        MouseMsg _ ->
            { model | current = Maybe.Just emptyDS }
  in (new, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        -- , Keyboard.downs KeyMsg
        ]

-- VIEW
view : Model -> Html.Html Msg
view model =
    Html.div [] [viewAllNodes model.nodes, viewCurrentNode model.current]


displayNode node = Html.text node.name

viewCurrentNode : Maybe DataStore -> Html.Html Msg
viewCurrentNode current =
    let nodeHtml = case current of
                       Maybe.Nothing -> Html.text "None"
                       Maybe.Just val -> displayNode val
        node = div [] [nodeHtml]
        heading = h1 [] [Html.text "Current node"]

    in div [] [heading, node]

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map displayNode nodes
        nHeading = h1 [] [Html.text "All nodes"]
    in
        div [] (nHeading :: allNodes)
