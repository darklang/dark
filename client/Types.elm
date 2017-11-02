module Types exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Dom
import Navigation
import Mouse

-- libs
import Keyboard.Event exposing (KeyboardEvent)

type alias Name = String
type alias FieldName = Name
type alias ParamName = Name
type alias VariableName = Name
type alias Cursor = Int

type alias Exception =
  { short : String
  , long : String
  , tipe : String
  , actual : String
  , actualType : String
  , result : String
  , resultType : String
  , expected : String
  , info : Dict String String
  , workarounds : List String }

type alias LiveValue = { value : String
                       , tipe : Tipe
                       , json : String
                       , exc : Maybe Exception}

type ID = ID Int
deID : ID -> Int
deID (ID x) = x

type Tipe = TInt
          | TStr
          | TChar
          | TBool
          | TFloat
          | TObj
          | TList
          | TAny
          | TBlock
          | TOpaque
          | TNull
          | TIncomplete

-- There are two coordinate systems. Pos is an absolute position in the
-- canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur
-- within the viewport and we map Absolute positions back to the
-- viewport to display in the browser.
type alias Pos = {x: Int, y: Int }
type alias VPos = {vx: Int, vy: Int }

-- MPos is a Node's position. Only roots have a stored position
-- server-side, but we need to position the other nodes
type DepPos = DPos Pos | DVPos VPos
type MPos = Root Pos
          | Free (Maybe VPos) -- relative to the viewport
          | Dependent (Maybe DepPos)
          | NoPos (Maybe Pos)

type alias MouseEvent = {pos: VPos, button: Int}
type alias IsLeftButton = Bool

type NodeType = FunctionCall
              | Datastore
              | Value
              | Page
              | Arg

type alias NodeDict = Dict Int Node
type alias NodeList = List Node
type alias Node = { name : Name
                  , id : ID
                  , pos : MPos
                  , tipe : NodeType
                  , liveValue : LiveValue
                  , face: String
                  , arguments : List (Parameter, Argument)
                  -- for blocks
                  , isBlockParent : Bool
                  , deleteWith : List ID
                  , cursor: Cursor
                  }

type alias Variable = (VariableName, Node)

type EdgeType = BlockEdge | FnEdge
type Argument = Const String
              | Edge ID EdgeType
              | NoArg

type Hole = ResultHole Node
          | ParamHole Node Parameter Int

type EntryCursor = Creating Pos
                 | Filling Node Hole

type alias IsReentering = Bool
type State = Selecting ID
           | Entering IsReentering EntryCursor
           | Dragging ID VPos HasMoved State
           | Deselected

type Msg
    = GlobalClick MouseEvent
    | NodeClickDown Node MouseEvent
    -- we have the actual node when NodeClickUp is created, but by the time we
    -- use it the proper node will be changed
    | NodeClickUp ID MouseEvent
    | DragNodeMove ID Mouse.Position
    | EntryInputMsg String
    | EntrySubmitMsg
    | GlobalKeyPress KeyboardEvent
    | FocusEntry (Result Dom.Error ())
    | FocusAutocompleteItem (Result Dom.Error ())
    | RPCCallBack Focus (List RPC) (Result Http.Error NodeDict)
    | PhantomCallBack EntryCursor (List RPC) (Result Http.Error NodeDict)
    | SaveTestCallBack (Result Http.Error String)
    | LocationChange Navigation.Location
    | AddRandom
    | ClearGraph
    | SaveTestButton
    | Initialization

type Focus = FocusNothing -- deselect
           | FocusExact ID
           | FocusNext ID
           | FocusSame -- unchanged

type RPC
    = NoOp
    | AddDatastore ID Name MPos
    | AddDatastoreField ID FieldName Tipe
    | AddFunctionCall ID Name MPos
    | AddBlock ID MPos (List ID) (List String)
    | AddValue ID String MPos
    | SetConstant Name (ID, ParamName)
    | SetEdge ID (ID, ParamName)
    | DeleteNode ID
    | UpdateNodeCursor ID Cursor
    | UpdateNodePosition ID MPos
    | DeleteAll
    | Savepoint
    | Undo
    | Redo

type alias Autocomplete = { functions : List Function
                          , completions : List AutocompleteItem
                          , index : Int
                          , value : String
                          , open : Bool
                          , liveValue : Maybe LiveValue
                          , tipe : Maybe Tipe
                          , nodes : Maybe NodeList
                          }
type AutocompleteItem = ACFunction Function
                      | ACField FieldName
                      | ACVariable Variable

type VariantTest = StubVariant

type alias Model = { nodes : NodeDict
                   , phantoms : NodeDict
                   , center : Pos
                   , error : Maybe String
                   , lastMsg : Msg
                   , lastMod : Modification
                   , tests   : List VariantTest
                   -- these values are serialized via Editor
                   , tempFieldName : FieldName
                   , state : State
                   , complete : Autocomplete
                   }

type AutocompleteMod = ACQuery String
                     | ACOpen Bool
                     | ACReset
                     | ACClear
                     | ACComplete String
                     | ACSelectDown
                     | ACSelectUp
                     | ACFilterByLiveValue LiveValue
                     | ACFilterByParamType Tipe NodeList

type alias HasMoved = Bool
type Modification = Error String
                  | ClearError
                  | Select ID
                  | Deselect
                  | Enter IsReentering EntryCursor -- reenter?
                  | RPC (List RPC, Focus)
                  | ModelMod (Model -> Model)
                  | NoChange
                  | MakeCmd (Cmd Msg)
                  | AutocompleteMod AutocompleteMod
                  | Phantom
                  | Many (List Modification)
                  | ChangeCursor Int
                  | Drag ID VPos HasMoved State
                  | SetState State

-- name, type optional
type alias Parameter = { name: Name
                       , tipe: Tipe
                       , block_args: List String
                       , optional: Bool
                       , description: String
                       }

type alias Function = { name: Name
                      , parameters: List Parameter
                      , description: String
                      , returnTipe: Tipe
                      }

type alias FlagParameter = { name: Name
                           , tipe: String
                           , block_args: List String
                           , optional: Bool
                           , description: String
                           }

type alias FlagFunction = { name: Name
                          , parameters: List FlagParameter
                          , description: String
                          , return_type: String
                          }



type alias Flags =
  {
    state: Maybe Editor
  , complete: List FlagFunction
  }

-- Values that we serialize
type alias Editor = {}

