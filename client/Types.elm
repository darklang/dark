module Types exposing (..)

import Dict exposing (Dict)
import Http
import Dom
import Keyboard
import Mouse



type alias Name = String
type alias FieldName = Name
type alias ParamName = Name
type alias TypeName = Name

type ID = ID Int
type alias Cursor = Maybe ID

type alias Pos = Mouse.Position
type alias MouseEvent = {pos: Mouse.Position, button: Int}
type alias Offset = {x: Int, y: Int, offsetCheck: Int}
type alias CanvasPos = {x: Int, y: Int, canvasPosCheck : Int}

type Type = Function
          | Datastore
          | Value
          | Page

type alias NodeDict = Dict Int Node
type alias Node = { name : Name
                  , id : ID
                  , pos : Pos
                  , tipe : Type
                  -- for DSes
                  , fields : List (FieldName, TypeName)
                  -- for functions
                  , parameters : List ParamName
                  }

type alias Edge = { source : ID
                  , target : ID
                  , targetParam : ParamName
                  }
type alias LiveValue = Maybe (String, String)

type alias LeftButton = Bool
type Msg
    = ClearCursor Mouse.Position
    | NodeClick Node
    | RecordClick Mouse.Position
    | DragNodeStart Node MouseEvent
    | DragNodeMove ID Offset Mouse.Position
    | DragNodeEnd ID Mouse.Position
    | DragSlotStart Node ParamName MouseEvent
    | DragSlotMove Mouse.Position
    | DragSlotEnd Node
    | DragSlotStop Mouse.Position
    | InputMsg String
    | SubmitMsg
    | KeyPress Keyboard.KeyCode
    | CheckEscape Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (Result Http.Error (NodeDict, List Edge, Cursor, LiveValue))
    | NoMsg -- use this for init

type RPC
    = LoadInitialGraph
    | AddDatastore Name Pos
    | AddDatastoreField ID FieldName TypeName
    | AddFunctionCall Name Pos
    | AddValue String Pos
    | UpdateNodePosition ID -- no pos cause it's in the node
    | AddEdge ID (ID, ParamName)
    | DeleteNode ID
    | ClearEdges ID
    | RemoveLastField ID

type State
    = ADD_FUNCTION
    | ADD_DS
    | ADD_DS_FIELD_NAME
    | ADD_DS_FIELD_TYPE
    | ADD_VALUE


type alias Model = { nodes : NodeDict
                   , edges : List Edge
                   , cursor : Cursor
                   , live : LiveValue
                   , inputValue : String
                   , focused : Bool
                   , state : State
                   , tempFieldName : FieldName
                   , errors : List String
                   , lastPos : Pos
                   , drag : Drag
                   , lastMsg : Msg
                   }
type Drag = NoDrag
          | DragNode ID Offset -- offset between the click and the node pos
          | DragSlot ID ParamName Mouse.Position -- starting point of edge
