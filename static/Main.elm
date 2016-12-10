import Html
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
    let
        nodes = Html.div [] (nHeading :: allNodes)
        current = Html.div [] [cHeading, cNode]
        cHeading = Html.h1 [] [Html.text "Current node"]
        cNode = case model.current of
                    Maybe.Just val -> Html.div [] [displayNode val]
                    _ -> Html.div [] [Html.text "None"]
        allNodes = List.map displayNode model.nodes
        nHeading = Html.h1 [] [Html.text "All nodes"]
        displayNode node = Html.text node.name
    in
        Html.div [] [nodes, current]
