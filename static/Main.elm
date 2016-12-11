port module Main exposing (..)

-- builtins
import Html exposing (div, h1)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode
import Result
import Char

-- lib
import Keyboard
import Mouse
import Dom
import Task

-- mine
import Native.Timestamp




main : Program Never Model Msg
main = Html.program
       { init = init
       , view = view
       , update = update
       , subscriptions = subscriptions}

-- MODEL
type alias Model = { nodes : List DataStore
                   , inputValue : String
                   , state : State
                   , errors : List String }
type alias DataStore = { name : String, fields : List (String, String) }

emptyDS : DataStore
emptyDS = { name = "", fields = [] }

-- states
type State
    = NOTHING
    | ADDING_DS_NAME
    | ADDING_DS_FIELD_NAME
    | ADDING_DS_FIELD_TYPE

init : ( Model, Cmd Msg )
init = ( { nodes = [emptyDS]
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


updateNode : NodeMsg -> List DataStore -> List DataStore
updateNode msg nodes =
    replaceFirst nodes
        (\n ->
             case msg of
                 SetName name -> { n | name = name }
                 SetFieldName name -> { n | fields = n.fields ++ [(name, "")] }
                 SetFieldType type_ -> { n | fields = replaceLast n.fields (\(a,b) -> (a, type_)) }
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case (m.state, msg) of
        (NOTHING, MouseMsg _) ->
            ({ m | state = ADDING_DS_NAME
                 , nodes = emptyDS :: m.nodes }
             , focusInput)
        (ADDING_DS_NAME, InputKeyMsg 13 target) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
                 , nodes = updateNode (SetName m.inputValue) m.nodes
             }, focusInput)
        (ADDING_DS_FIELD_NAME, InputKeyMsg 13 target) ->
            if target == ""
            then ({ m | state = NOTHING
                      , inputValue = ""
                  }, Cmd.none)
            else ({ m | state = ADDING_DS_FIELD_TYPE
                      , inputValue = ""
                      , nodes = updateNode (SetFieldName target) m.nodes
                  }, focusInput)
        (ADDING_DS_FIELD_TYPE, InputKeyMsg 13 target) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
                 , nodes = updateNode (SetFieldType target) m.nodes }
                 , focusInput)
        (_, FocusResult (Ok ())) ->
            ( m, Cmd.none )
        (_, InputKeyMsg key _) ->
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
                , viewAllNodes model.nodes
                ]

inputID = "darkInput"

onKeyDown : (Int -> String -> msg) -> Html.Attribute msg
onKeyDown msg =
  Events.on "keydown" (Json.Decode.map2 msg Events.keyCode Events.targetValue)

viewInput value = div [] [ Html.input [ Attrs.id inputID
                                      , Attrs.value value
                                      , onKeyDown InputKeyMsg
                                      ] [] ]

str2line str = div [] [Html.text str]
viewState state = div [] [ Html.text ("state: " ++ toString state) ]
viewErrors errors = div [] ((Html.text "errors: ") :: (List.map str2line errors))

displayNode node = div [] (str2line ("'" ++ node.name ++ "'") :: (List.map (toString >> str2line) node.fields))

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map displayNode nodes
        nHeading = h1 [] [Html.text "All nodes"]
    in
        div [] (nHeading :: allNodes)



-- Util
timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a
