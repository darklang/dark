module Types exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Dom
import Keyboard
import Mouse

-- libs
import Keyboard.Event


type alias Name = String
type alias FieldName = Name
type alias ParamName = Name
type alias TypeName = Name
type alias LiveValue = (String, String)

type ID = ID Int
deID : ID -> Int
deID (ID x) = x


type alias Pos = Mouse.Position
type alias MouseEvent = {pos: Mouse.Position, button: Int}
type alias Offset = {x: Int, y: Int, offsetCheck: Int}
type alias CanvasPos = {x: Int, y: Int, canvasPosCheck : Int}
type alias LeftButton = Bool


type NodeType = FunctionCall
              | FunctionDef
              | Datastore
              | Value
              | Page

type alias NodeDict = Dict Int Node
type alias Node = { name : Name
                  , id : ID
                  , pos : Pos
                  , tipe : NodeType
                  , live : LiveValue
                  -- for DSes
                  , fields : List (FieldName, TypeName)
                  -- for functions
                  , parameters : List ParamName
                  , constants : Dict String String
                  }

type alias Edge = { source : ID
                  , target : ID
                  , param : ParamName
                  }

type Hole = ResultHole Node
          | ParamHole Node String Int

type EntryCursor = Creating Pos
                 | Filling Node Hole Pos

type State = Selecting ID
           | Entering EntryCursor
           | Picking
           | Dragging Drag
           | Deselected


type Msg
    = ClearCursor Mouse.Position
    | NodeClick Node
    | RecordClick Mouse.Position
    -- we have the actual node when this is created, but by the time we use the
    -- others the node will be changed
    | DragNodeStart Node MouseEvent
    | DragNodeMove ID Offset Mouse.Position
    | DragNodeEnd ID Mouse.Position
    | DragSlotStart Node ParamName MouseEvent
    | DragSlotMove Mouse.Position
    | DragSlotEnd Node
    | DragSlotStop Mouse.Position
    | EntryInputMsg String
    | EntryKeyPress Keyboard.Event.KeyboardEvent
    | GlobalKeyPress Keyboard.Event.KeyboardEvent
    | EntrySubmitMsg
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (List RPC) (Result Http.Error (NodeDict, List Edge, Maybe ID))
    | Initialization

type RPC
    = LoadInitialGraph
    | AddDatastore Name Pos
    | AddDatastoreField ID FieldName TypeName
    | AddFunctionCall Name Pos (List ImplicitEdge)
    | AddConstant Name ID ParamName
    | AddAnon Pos
    | AddValue String Pos (List ImplicitEdge)
    | UpdateNodePosition ID Pos
    | AddEdge ID (ID, ParamName)
    | DeleteNode ID
    | ClearEdges ID
    | RemoveLastField ID

type alias Autocomplete = { defaults : List String
                          , current : List String
                          , index : Int
                          , value : String
                          }

type alias Model = { nodes : NodeDict
                   , edges : List Edge
                   , error : (String, Int)
                   , lastMsg : Msg
                   -- these values are serialized via Editor
                   , tempFieldName : FieldName
                   , state : State
                   , complete : Autocomplete
                   }

type AutocompleteMod = SetEntry String
                     | Reset
                     | SelectDown
                     | SelectUp

type Modification = Error String
                  | Select ID
                  | Enter EntryCursor
                  | Pick
                  | Deselect
                  | RPC RPC
                  | ModelMod (Model -> Model)
                  | Drag Drag
                  | NoChange
                  | AutocompleteMod AutocompleteMod
                  | Many (List Modification)


type alias Flags = { state: Maybe Editor
                   , complete: List String}

-- Values that we serialize
type alias Editor = {}

type ImplicitEdge = ReceivingEdge ID -- source (target is decided by the receiver after it's created)
                  | ParamEdge ID ParamName -- target id and target param, the source is implicit
                  | Constant String ParamName -- target id and target param, the target is implicit, no source

type Drag = DragNode ID Offset -- offset between the click and the node pos
          | DragSlot Node ParamName Mouse.Position -- starting point of edge
