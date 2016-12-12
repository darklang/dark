port module Main exposing (..)

-- builtins
import Html exposing (div, h1, text)
import Html.Attributes as Attrs
import Html.Events as Events
import Result
import Char

-- lib
import Keyboard
import Mouse
import Dom
import Task
import Http
import Http
import Json.Encode as JSE
import Json.Decode as JSD
import Json.Decode.Pipeline as JSDP
-- mine
import Native.Timestamp




main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions}

-- MODEL
type alias Model = { graph : Graph
                   , inputValue : String
                   , state : State
                   , errors : List String }
type alias DataStore = { name : String, fields : List (String, String) }

type alias Graph = { nodes : List DataStore }

emptyDS : DataStore
emptyDS = { name = "", fields = [] }

-- states
type State
    = NOTHING
    | ADDING_DS_NAME
    | ADDING_DS_FIELD_NAME
    | ADDING_DS_FIELD_TYPE

init : ( Model, Cmd Msg )
init = ( { graph = {nodes = []}
         , inputValue = ""
         , state = NOTHING
         , errors = ["No errors"]
         }, Cmd.none )

-- UPDATE

type NodeMsg
    = SetName String
    | SetFieldName String
    | SetFieldType String

type Msg
    = MouseMsg Mouse.Position
    | InputKeyMsg Keyboard.KeyCode String
    | KeyMsg Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | AddDataStore (Result Http.Error Graph)


focusInput = Dom.focus inputID |> Task.attempt FocusResult
addError error model =
    let time = timestamp ()
               in
    List.take 4 ((error ++ "-" ++ toString time) :: model.errors)


replaceFirst : List a -> (a -> a) -> List a
replaceFirst ls fn = let head = Maybe.map fn (List.head ls)
                         rest = case (List.tail ls) of
                                    Nothing -> []
                                    Just tail -> tail
                     in case head of
                            Nothing -> rest
                            Just n -> n :: rest

replaceLast : List a -> (a -> a) -> List a
replaceLast ls fn = List.reverse (replaceFirst (List.reverse ls) fn)


updateNode : NodeMsg -> Graph -> Graph
updateNode msg graph =
    let newNodes =
            replaceFirst graph.nodes (\n ->
                                          case msg of
                                              SetName name -> { n | name = name }
                                              SetFieldName name -> { n | fields = n.fields ++ [(name, "")] }
                                              SetFieldType type_ -> { n | fields = replaceLast n.fields (\(a,b) -> (a, type_)) })
    in { graph | nodes = newNodes }


decodeNode : JSD.Decoder DataStore
decodeNode =
  JSDP.decode DataStore
      |> JSDP.required "name" JSD.string
      |> JSDP.required "fields" (JSD.list (JSD.keyValuePairs JSD.string))

decodeGraph : JSD.Decoder Graph
decodeGraph =
    JSDP.decode Graph
        -- |> JSDP.required "edges" (JSD.list JSD.string)
        |> JSDP.required "nodes" (JSD.list decodeNode)


addServerDS : String -> Cmd Msg
addServerDS name =
    let payload = JSE.object [("name", JSE.string name)]
        json = Http.jsonBody payload
        request = Http.post "/admin/api/add_datastore" json decodeGraph
    in Http.send AddDataStore request


update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case (m.state, msg) of
        (NOTHING, MouseMsg _) ->
            ({ m | state = ADDING_DS_NAME
             }, focusInput)
        (ADDING_DS_NAME, InputKeyMsg 13 target) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
                 -- , nodes = updateNode (SetName m.inputValue) m.nodes
             }, Cmd.batch [focusInput, addServerDS m.inputValue])
        (ADDING_DS_FIELD_NAME, InputKeyMsg 13 target) ->
            if target == ""
            then ({ m | state = NOTHING
                      , inputValue = ""
                  }, Cmd.none)
            else ({ m | state = ADDING_DS_FIELD_TYPE
                      , inputValue = ""
                      , graph = updateNode (SetFieldName target) m.graph
                  }, focusInput)
        (ADDING_DS_FIELD_TYPE, InputKeyMsg 13 target) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
                 , graph = updateNode (SetFieldType target) m.graph }
                 , focusInput)
        (_, FocusResult (Ok ())) ->
            ( m, Cmd.none )
        (_, InputKeyMsg key _) ->
            -- TODO: this breaks backspace and stuff
            ({ m | inputValue = m.inputValue ++ (key |> Char.fromCode |> String.fromChar)}, Cmd.none)
        t ->
            ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, Cmd.none )

        -- KeyMsg key ->
        --     ({ model | errors = addError "Not supported yet" model}, Cmd.none)

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
    Html.div [] [ viewInput model.inputValue
                , viewErrors model.errors
                , viewState model.state
                , viewAllNodes model.graph.nodes
                ]

inputID = "darkInput"

onKeyDown : (Int -> String -> msg) -> Html.Attribute msg
onKeyDown msg =
  Events.on "keydown" (JSD.map2 msg Events.keyCode Events.targetValue)

viewInput value = div [] [ Html.input [ Attrs.id inputID
                                      , Attrs.value value
                                      , onKeyDown InputKeyMsg
                                      ] [] ]

str2line str = div [] [text str]
viewState state = div [] [ text ("state: " ++ toString state) ]
viewErrors errors = div [] ((text "errors: ") :: (List.map str2line errors))

displayNode node = div [] (str2line ("'" ++ node.name ++ "'") :: (List.map (toString >> str2line) node.fields))

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map displayNode nodes
        nHeading = h1 [] [text "All nodes"]
    in
        div [] (nHeading :: allNodes)



-- Util
timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a
