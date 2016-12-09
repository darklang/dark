import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  Html.beginnerProgram
    { model = model
    , view = view
    , update = update
    }


-- MODEL
type alias Model =
  { nodes : List DataStore }

type alias Function =
    { name : String }

type alias DataStore =
    { name : String,
      fields : List (String, String) }

model : Model
model =
  Model []



-- UPDATE
type Msg
    = Create String


update : Msg -> Model -> Model
update msg model =
  case msg of
    Create name ->
      { model | nodes = (DataStore name []) :: model.nodes }


-- VIEW
view : Model -> Html Msg
view model =
    let
        displayNode node = [ text node.name ]
    in
        div [] (List.concat ( List.map displayNode model.nodes ))
