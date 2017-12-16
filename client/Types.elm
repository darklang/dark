module Types exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Dom
import Navigation
import Mouse

-- libs
import Keyboard.Event exposing (KeyboardEvent)

-- Dark
import Util

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

type Tipe = TInt
          | TStr
          | TChar
          | TBool
          | TFloat
          | TObj
          | TList
          | TAny
          | TBlock
          | TNull
          | TIncomplete
          | TResp
          | TDB

-- There are two coordinate systems. Pos is an absolute position in the
-- canvas. Nodes and Edges have Pos'. VPos is the viewport: clicks occur
-- within the viewport and we map Absolute positions back to the
-- viewport to display in the browser.
type alias Pos = {x: Int, y: Int }
type alias VPos = {vx: Int, vy: Int }

type alias MouseEvent = {pos: VPos, button: Int}
type alias IsLeftButton = Bool

type alias LiveValue = { value : String
                       , tipe : Tipe
                       , json : String
                       , exc : Maybe Exception}
type alias Special = Int
type TLID = TLID Int
type ID = ID Int

deID : ID -> Int
deID (ID i) = i

gid : () -> ID -- Generate ID
gid unit = ID (Util.random unit)


type EntryCursor = Creating Pos
                 | Filling TLID ID

type alias HasMoved = Bool
type alias ThreadID = ID
type State = Selecting TLID (Maybe ID)
           | Entering EntryCursor
           | Dragging TLID VPos HasMoved State
           | Deselected

unwrapState : State -> State
unwrapState s =
  case s of
    Dragging _ _ _ unwrap -> unwrap
    _ -> s

type alias RPCResult = (List Toplevel, List TLAResult)
type Msg
    = GlobalClick MouseEvent
    | ToplevelClickDown Toplevel MouseEvent
    -- we have the actual node when NodeClickUp is created, but by the time we
    -- use it the proper node will be changed
    | ToplevelClickUp TLID MouseEvent
    | DragToplevel TLID Mouse.Position
    | EntryInputMsg String
    | EntrySubmitMsg
    | GlobalKeyPress KeyboardEvent
    | FocusEntry (Result Dom.Error ())
    | FocusAutocompleteItem (Result Dom.Error ())
    | RPCCallBack Focus (List RPC) (Result Http.Error RPCResult)
    | SaveTestCallBack (Result Http.Error String)
    | LocationChange Navigation.Location
    | AddRandom
    | ClearGraph
    | SaveTestButton
    | Initialization

type alias Predecessor = Maybe ID
type Focus = FocusNothing -- deselect
           | Refocus TLID
           | FocusExact TLID ID
           | FocusNext TLID Predecessor
           | FocusSame -- unchanged

type RPC
    = NoOp
    | SetHandler TLID Pos Handler
    | CreateDB TLID Pos DBName
    | AddDBCol TLID ID ID
    | SetDBColName TLID ID DBColName
    | SetDBColType TLID ID DBColType
    | DeleteTL TLID
    | MoveTL TLID Pos
    | DeleteAll
    | Savepoint
    | Undo
    | Redo

type alias Autocomplete = { functions : List Function
                          , varnames : List VarName
                          , completions : List AutocompleteItem
                          , index : Int
                          , value : String
                          , open : Bool
                          , showFunctions : Bool
                          , liveValue : Maybe LiveValue
                          , tipe : Maybe Tipe
                          }

type AutocompleteItem = ACFunction Function
                      | ACField String
                      | ACVariable VarName

type VariantTest = StubVariant



type alias Class = String
type Element = Leaf (Maybe ID, Class, String)
             | Nested (Maybe ID, Class) (List Element)

type alias VarName = String
type alias FnName = String
type alias FieldName = String

type alias VarBind = HoleOr VarName
type alias Field = HoleOr FieldName

type Expr = If ID Expr Expr Expr
          | FnCall ID FnName (List Expr)
          | Variable ID VarName
          | Let ID VarBind Expr Expr
          | Lambda ID (List VarName) Expr
          | Value ID String
          | Hole ID
          | Thread ID (List Expr)
          | FieldAccess ID Expr Field

type alias AST = Expr

type HoleType = BindHole Handler
              | SpecHole Handler
              | ExprHole Handler
              | FieldHole Handler
              | DBColNameHole DB
              | DBColTypeHole DB
              | NotAHole

type HoleOr a = Empty ID
              | Full ID a

holeOrID : HoleOr a -> ID
holeOrID a =
  case a of
    Empty id -> id
    Full id _ -> id

type alias HandlerSpec = { module_ : HoleOr String
                         , name : HoleOr String
                         , modifier : HoleOr String
                         }

type alias Handler = { ast : AST
                     , spec : HandlerSpec }

type alias DBName = String
type alias DBColName = String
type alias DBColType = String
type alias DB = { name : DBName
                , cols : List (HoleOr DBColName, HoleOr DBColType)}

type TLData = TLHandler Handler
            | TLDB DB

type alias Toplevel = { id : TLID
                      , pos : Pos
                      , data : TLData
                      }


type alias LVDict = Dict Int LiveValue
type alias AVDict = Dict Int (List VarName)
type alias TLAResult = { id: TLID
                       , astValue: LiveValue
                       , liveValues : LVDict
                       , availableVarnames : AVDict
                       }

type alias Model = { center : Pos
                   , error : Maybe String
                   , lastMsg : Msg
                   , lastMod : Modification
                   , tests : List VariantTest
                   , complete : Autocomplete
                   , state : State
                   , toplevels : List Toplevel
                   , analysis : List TLAResult
                   }

type AutocompleteMod = ACSetQuery String
                     | ACAppendQuery String
                     | ACOpen Bool
                     | ACReset
                     | ACClear
                     | ACSelectDown
                     | ACSelectUp
                     | ACFilterByLiveValue (Maybe LiveValue)
                     | ACSetAvailableVarnames (List VarName)
                     | ACShowFunctions Bool
                     -- | ACFilterByParamType Tipe NodeList

type Modification = Error String
                  | ClearError
                  | Select TLID (Maybe ID)
                  | Deselect
                  | SetToplevels (List Toplevel) (List TLAResult)
                  | Enter EntryCursor
                  | RPC (List RPC, Focus)
                  | SetCenter Pos
                  | NoChange
                  | MakeCmd (Cmd Msg)
                  | AutocompleteMod AutocompleteMod
                  | Many (List Modification)
                  | Drag TLID VPos HasMoved State
                  | SetState State

-- name, type optional
type alias Parameter = { name: String
                       , tipe: Tipe
                       , block_args: List String
                       , optional: Bool
                       , description: String
                       }

type alias Function = { name: String
                      , parameters: List Parameter
                      , description: String
                      , returnTipe: Tipe
                      }

type alias FlagParameter = { name: String
                           , tipe: String
                           , block_args: List String
                           , optional: Bool
                           , description: String
                           }

type alias FlagFunction = { name: String
                          , parameters: List FlagParameter
                          , description: String
                          , return_type: String
                          }



type alias Flags =
  { editorState: Maybe Editor
  , complete: List FlagFunction
  }

-- Values that we serialize
type alias Editor = { }

