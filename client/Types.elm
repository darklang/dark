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


-----------------------------
-- State
-----------------------------
type EntryCursor = Creating Pos
                 | Filling TLID Pointer

type alias HasMoved = Bool
type State = Selecting TLID (Maybe Pointer)
           | Entering EntryCursor
           | Dragging TLID VPos HasMoved State
           | Deselected

unwrapState : State -> State
unwrapState s =
  case s of
    Dragging _ _ _ unwrap -> unwrap
    _ -> s

-----------------------------
-- Msg
-- main Elm Architecture bus type
-----------------------------
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
    | RPCCallBack Focus Modification (List RPC) (Result Http.Error RPCResult)
    | SaveTestCallBack (Result Http.Error String)
    | LocationChange Navigation.Location
    | AddRandom
    | FinishIntegrationTest
    | ClearGraph
    | SaveTestButton
    | Initialization

type alias Predecessor = Maybe Pointer
type alias Successor = Maybe Pointer
type Focus = FocusNothing -- deselect
           | Refocus TLID
           | FocusExact TLID Pointer
           | FocusNext TLID Predecessor
           | FocusSame -- unchanged

-----------------------------
-- RPCs
-----------------------------
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

-----------------------------
-- Autocompletes
-----------------------------
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



-----------------------------
-- AB tests
-----------------------------
type VariantTest = StubVariant



-----------------------------
-- AST
-----------------------------

type alias Class = String
type Element = Leaf (Maybe ID, Class, String)
             | Nested (Maybe ID, Class) (List Element)

type alias VarName = String
type alias FnName = String
type alias FieldName = String

type alias VarBind = BlankOr VarName
type alias Field = BlankOr FieldName

-- TODO(ian): VarBinds should potentially be an
-- Expr as they're IDs that exist _inside_ the AST
-- but can't be touched/talked to because functions dealing
-- with the AST -- work on the ID<->Expr mapping
-- like `subExpr : ID -> Expr -> Expr`
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

-----------------------------
-- High-level ID wrappers
-- so we're not using IDs in important APIs
-----------------------------
type PointerType = VarBind
                 | Spec
                 | Expr
                 | Field
                 | DBColName
                 | DBColType

type Pointer = PBlank PointerType ID
             | PFilled PointerType ID

type BlankOr a = Blank ID
               | Filled ID a

blankOrID : BlankOr a -> ID
blankOrID a =
  case a of
    Blank id -> id
    Filled id _ -> id

-----------------------------
-- Top-levels
-----------------------------
type alias HandlerSpec = { module_ : BlankOr String
                         , name : BlankOr String
                         , modifier : BlankOr String
                         }

type alias Handler = { ast : AST
                     , spec : HandlerSpec }

type alias DBName = String
type alias DBColName = String
type alias DBColType = String
type alias DB = { name : DBName
                , cols : List (BlankOr DBColName, BlankOr DBColType)}

type TLData = TLHandler Handler
            | TLDB DB

type alias Toplevel = { id : TLID
                      , pos : Pos
                      , data : TLData
                      }


-----------------------------
-- Analysis
-----------------------------
type alias LVDict = Dict Int LiveValue
type alias AVDict = Dict Int (List VarName)
type alias TLAResult = { id: TLID
                       , astValue: LiveValue
                       , liveValues : LVDict
                       , availableVarnames : AVDict
                       }

-----------------------------
-- Model
-----------------------------
type alias Model = { center : Pos
                   , error : Maybe String
                   , lastMsg : Msg
                   , lastMod : Modification
                   , tests : List VariantTest
                   , complete : Autocomplete
                   , state : State
                   , toplevels : List Toplevel
                   , analysis : List TLAResult
                   , integrationTestState : IntegrationTestState
                   }

-- Values that we serialize
type alias Editor = { }

-----------------------------
-- Testing
-----------------------------

-- avoid ever-expanding aliases
type alias TestResult = Result String ()
type IntegrationTestState = IntegrationTestExpectation (Model -> TestResult)
                          | IntegrationTestFinished TestResult
                          | NoIntegrationTest


-----------------------------
-- Modifications
-----------------------------
type Modification = Error String
                  | ClearError
                  | Select TLID (Maybe Pointer)
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
                  | TriggerIntegrationTest String
                  | EndIntegrationTest
                  | SetState State

-----------------------------
-- Flags / function types
-----------------------------

type alias Flags =
  { editorState: Maybe Editor
  , complete: List FlagFunction
  }


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

