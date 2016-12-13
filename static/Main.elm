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


-- TOP-LEVEL
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
                   , tempFieldName : String
                   , errors : List String
                   }

type alias DataStore = { name : String
                       , fields : List (String, String)
                       }

type alias Graph = { nodes : List DataStore
                   , cursor : String
                   }

init : ( Model, Cmd Msg )
init = ( { graph = {nodes = [], cursor = ""}
         , state = NOTHING
         , errors = ["No errors"]
         , inputValue = ""
         , tempFieldName = ""
         }, Cmd.none )



-- RPC
type RPC
    = AddDatastore String
    | AddDatastoreField String String

rpc : Model -> RPC -> Cmd Msg
rpc model call =
    let payload = encodeRPC model call
        json = Http.jsonBody payload
        request = Http.post "/admin/api/rpc" json decodeGraph
    in Http.send RPCCallBack request

encodeRPC : Model -> RPC -> JSE.Value
encodeRPC m call =
    let (cmd, args) =
            case call of
                AddDatastore name -> ("add_datastore"
                                     , JSE.object [("name", JSE.string name)])
                AddDatastoreField name type_ -> ("add_datastore_field",
                                                 JSE.object [ ("name", JSE.string name)
                                                            , ("type", JSE.string type_)])
    in JSE.object [ ("command", JSE.string cmd)
                  , ("args", args)
                  , ("cursor", JSE.string m.graph.cursor)]


decodeNode : JSD.Decoder DataStore
decodeNode =
  JSDP.decode DataStore
      |> JSDP.required "name" JSD.string
      |> JSDP.required "fields" (JSD.keyValuePairs JSD.string)

decodeGraph : JSD.Decoder Graph
decodeGraph =
    JSDP.decode Graph
        -- |> JSDP.required "edges" (JSD.list JSD.string)
        |> JSDP.required "nodes" (JSD.list decodeNode)
        |> JSDP.required "cursor" JSD.string



-- UPDATE
type Msg
    = MouseMsg Mouse.Position
    | InputMsg String
    | SubmitMsg
    | KeyMsg Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (Result Http.Error Graph)

type State
    = NOTHING
    | ADDING_DS_NAME
    | ADDING_DS_FIELD_NAME
    | ADDING_DS_FIELD_TYPE

update : Msg -> Model -> (Model, Cmd Msg)
update msg m =
    case (m.state, msg) of
        (NOTHING, MouseMsg _) ->
            ({ m | state = ADDING_DS_NAME
             }, focusInput)
        (ADDING_DS_NAME, SubmitMsg) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
             }, Cmd.batch [focusInput, rpc m <| AddDatastore m.inputValue])
        (ADDING_DS_FIELD_NAME, SubmitMsg) ->
            if m.inputValue == ""
            then -- the DS has all its fields
                ({ m | state = NOTHING
                     , inputValue = ""
                 }, Cmd.none)
            else  -- save the field name, we'll submit it later the type
                ({ m | state = ADDING_DS_FIELD_TYPE
                     , inputValue = ""
                     , tempFieldName = m.inputValue
                 }, focusInput)
        (ADDING_DS_FIELD_TYPE, SubmitMsg) ->
            ({ m | state = ADDING_DS_FIELD_NAME
                 , inputValue = ""
             }, Cmd.batch [focusInput, rpc m <| AddDatastoreField m.tempFieldName m.inputValue])

        (_, RPCCallBack (Ok graph)) ->
            ({ m | graph = graph
             }, Cmd.none)
        (_, RPCCallBack (Err error)) ->
            ({ m | errors = addError ("Bad RPC call: " ++ toString(error)) m
             }, Cmd.none)

        (_, FocusResult (Ok ())) ->
            -- Yay, you focused a field! Ignore.
            ( m, Cmd.none )
        (_, InputMsg target) ->
            -- Syncs the form with the model. The actual submit is in SubmitMsg
            ({ m | inputValue = target
             }, Cmd.none)
        t -> -- All other cases
            ({ m | errors = addError ("Nothing for " ++ (toString t)) m }, Cmd.none )

        -- KeyMsg key -> -- Keyboard input
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
    div [] [ viewInput model.inputValue
           , viewState model.state
           , viewErrors model.errors
           , viewAllNodes model.graph.nodes
           ]

viewInput value = div [] [
               Html.form [
                    Events.onSubmit (SubmitMsg)
                   ] [
                    Html.input [ Attrs.id inputID
                               , Events.onInput InputMsg
                               , Attrs.value value
                               ] []
                   ]
              ]

viewState state = div [] [ text ("state: " ++ toString state) ]
viewErrors errors = div [] ((text "errors: ") :: (List.map str2div errors))
viewNode node = div [] (str2div ("'" ++ node.name ++ "'") :: (List.map (toString >> str2div) node.fields))

viewAllNodes : List DataStore -> Html.Html Msg
viewAllNodes nodes =
    let allNodes = List.map viewNode nodes
        nHeading = h1 [] [text "All nodes"]
    in
        div [] (nHeading :: allNodes)




-- UTIL
timestamp : () -> Int
timestamp a = Native.Timestamp.timestamp a

inputID = "darkInput"
focusInput = Dom.focus inputID |> Task.attempt FocusResult

addError error model =
    let time = timestamp ()
               in
    List.take 4 ((error ++ "-" ++ toString time) :: model.errors)

str2div str = div [] [text str]
