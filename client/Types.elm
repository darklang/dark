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
                  -- for DSes
                  , fields : List (FieldName, TypeName)
                  -- for functions
                  , parameters : List ParamName
                  }

type alias Edge = { source : ID
                  , target : ID
                  , param : ParamName
                  }
-- There can be:
-- + entry but no cursor (click somewhere that isn't a node)
-- + cursor and entry (filling a hole)
-- + neither cursor nor entry (after pressing escape)
-- + cursor but no entry (when dragging)
type Cursor = Deselected
            | Creating Pos
            | Dragging Node
            | Filling Node Pos -- todo, include hole here?

-- Does the new Node fill a hole?
type Hole = ResultHole Node
          | ParamHole Node String Int

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
    | ReplInputMsg String
    | ReplSubmitMsg
    | EntryInputMsg String
    | EntrySubmitMsg
    | KeyPress Keyboard.KeyCode
    | CheckEscape Keyboard.KeyCode
    | FocusResult (Result Dom.Error ())
    | RPCCallBack (List RPC) (Result Http.Error (NodeDict, List Edge, Maybe ID))

type RPC
    = LoadInitialGraph
    | AddDatastore Name Pos
    | AddDatastoreField ID FieldName TypeName
    | AddFunctionCall Name Pos (List ImplicitEdge)
    | AddAnon Pos
    | AddValue String Pos (List ImplicitEdge)
    | UpdateNodePosition ID Pos
    | AddEdge ID (ID, ParamName)
    | DeleteNode ID
    | ClearEdges ID
    | RemoveLastField ID

type alias Model = { nodes : NodeDict
                   , edges : List Edge
                   , errors : List String
                   , dragPos : Pos
                   , drag : Drag
                   -- these values are serialized via Editor
                   , tempFieldName : FieldName
                   , cursor : Cursor
                   , replValue : String
                   , entryValue : String
                   }

-- Values that we serialize
type alias Editor = { cursor : (Maybe Int, Maybe Pos)
                    , entryValue : String
                    , replValue : String
                    , tempFieldName : FieldName
                    }

type ImplicitEdge = ReceivingEdge ID -- source (target is decided by the receiver after it's created)
                  | ParamEdge ID ParamName -- target id and target param, the source is implicit

type Drag = NoDrag
          | DragNode ID Offset -- offset between the click and the node pos
          | DragSlot ID ParamName Mouse.Position -- starting point of edge
