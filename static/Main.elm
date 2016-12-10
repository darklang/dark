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
type alias Model = { nodes : List DataStore }
type alias DataStore = { name : String, fields : List (String, String) }

emptyDS = { name = "new", fields = [] }

init : ( Model, Cmd Msg )
init =
    ( { nodes = [] }, Cmd.none )

-- UPDATE
type Msg
    = MouseMsg Mouse.Position
    -- | KeyMsg Keyboard.KeyCode

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let new =
    case msg of
        MouseMsg _ ->
            { model | nodes = emptyDS :: model.nodes }
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
    let displayNode node = [ Html.text node.name ]
    in Html.div [] (List.concat ( List.map displayNode model.nodes ))
