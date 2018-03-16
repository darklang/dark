module Types exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Dom
import Navigation
import Mouse
import Time exposing (Time)
import PageVisibility

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
          | TNull
          | TBlock
          | TIncomplete
          | TError
          | TResp
          | TDB
          | TID
          | TDate
          | TTitle
          | TUrl
          | TBelongsTo String
          | THasMany String

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

deTLID : TLID -> Int
deTLID (TLID i) = i

gid : () -> ID -- Generate ID
gid unit = ID (Util.random unit)

gtlid : () -> TLID -- Generate ID
gtlid unit = TLID (Util.random unit)

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

tlidOf : State -> Maybe TLID
tlidOf s =
  case unwrapState s of
    Selecting tlid _ ->
      Just tlid
    Entering cursor ->
      case cursor of
        Creating _ ->
          Nothing
        Filling tlid _ ->
          Just tlid
    Deselected -> Nothing
    _ -> Debug.crash "can't have dragging"



-----------------------------
-- Msg
-- main Elm Architecture bus type
-----------------------------
type Action = RefreshAnalyses

type alias GlobalVariable = String
type alias RPCResult = (List Toplevel, List TLAResult, List GlobalVariable)
type alias GetAnalysisRPCResult = (List TLAResult, List GlobalVariable)
type Msg
    = GlobalClick MouseEvent
    | NothingClick MouseEvent
    | ToplevelClickDown Toplevel MouseEvent
    -- we have the actual node when ToplevelClickUp is created,
    -- but by the time we use it the proper node will be changed
    | ToplevelClickUp TLID (Maybe Pointer) MouseEvent
    | DragToplevel TLID Mouse.Position
    | MouseEnter Pointer MouseEvent
    | MouseLeave Pointer MouseEvent
    | EntryInputMsg String
    | EntrySubmitMsg
    | GlobalKeyPress KeyboardEvent
    | AutocompleteClick String
    | FocusEntry (Result Dom.Error ())
    | FocusAutocompleteItem (Result Dom.Error ())
    | RPCCallback Focus Modification (List RPC) (Result Http.Error RPCResult)
    | SaveTestRPCCallback (Result Http.Error String)
    | GetAnalysisRPCCallback (Result Http.Error GetAnalysisRPCResult)
    | LocationChange Navigation.Location
    | AddRandom
    | FinishIntegrationTest
    | ClearGraph
    | SaveTestButton
    | ToggleSync
    | Initialization
    | NavigateTo String
    | WindowResize Int Int
    | ClockTick Action Time
    | PageVisibilityChange PageVisibility.Visibility
    | PageFocusChange PageVisibility.Visibility

type alias Predecessor = Maybe Pointer
type alias Successor = Maybe Pointer
type Focus = FocusNothing -- deselect
           | Refocus TLID
           | FocusExact TLID Pointer
           | FocusNext TLID Predecessor
           | FocusFirstAST TLID
           | FocusSame -- unchanged
           | FocusNoChange -- unchanged

-----------------------------
-- RPCs
-----------------------------
type RPC
    = SetHandler TLID Pos Handler
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
                          , completions : List (List AutocompleteItem)
                          , allCompletions : List AutocompleteItem
                          , index : Int
                          , value : String
                          , target : Maybe (TLID, Pointer)
                          , tipe : Maybe Tipe
                          }

type AutocompleteItem = ACFunction Function
                      | ACField String
                      | ACVariable VarName
                      | ACExtra String

type AutocompleteMod = ACSetQuery String
                     | ACAppendQuery String
                     | ACReset
                     | ACSelectDown
                     | ACSelectUp
                     | ACSetTarget (Maybe (TLID, Pointer))
                     | ACRegenerate
                     -- | ACFilterByParamType Tipe NodeList



-----------------------------
-- AB tests
-----------------------------
type VariantTest = StubVariant

-----------------------------
-- View
-----------------------------
type DivSelected = DivSelected | DivUnselected
type MouseOverDiv = MouseOverDiv | MouseNotOverDiv
type alias Class = String
type alias Clickable = Maybe (TLID, Pointer)


-----------------------------
-- AST
-----------------------------

type alias VarName = String
type alias FnName = String
type alias FieldName = String

type alias VarBind = BlankOr VarName
type alias Field = BlankOr FieldName

type alias Expr = BlankOr NExpr
type NExpr = NIf Expr Expr Expr
           | NFnCall FnName (List Expr)
           | NVariable VarName
           | NLet VarBind Expr Expr
           | NLambda (List VarName) Expr
           | NValue String
           | NThread (List Expr)
           | NFieldAccess Expr Field

-----------------------------
-- High-level ID wrappers
-- so we're not using IDs in important APIs
-----------------------------
type PointerType = VarBind
                 | EventName
                 | EventSpace
                 | EventModifier
                 | Expr
                 | Field
                 | DBColName
                 | DBColType
                 | DarkType
                 | DarkTypeField

type Pointer = PBlank PointerType ID
             | PFilled PointerType ID

type PointerData = PVarBind VarBind
                 | PEventName (BlankOr String)
                 | PEventModifier (BlankOr String)
                 | PEventSpace (BlankOr String)
                 | PExpr Expr
                 | PField Field
                 | PDBColName (BlankOr String)
                 | PDBColType (BlankOr String)
                 | PDarkType (BlankOr DarkType)
                 | PDarkTypeField (BlankOr String)

type BlankOr a = Blank ID
               | F ID a

type PointerOwner = POSpecHeader
                  | POAst
                  | PODb
                  | POSpecType

-----------------------------
-- Top-levels
-----------------------------
type DarkType = DTEmpty -- empty body
              | DTAny
              | DTString
              | DTInt
              | DTObj (List (BlankOr String, BlankOr DarkType))

type alias SpecTypes = { input : BlankOr DarkType
                       , output : BlankOr DarkType
                       }

type alias HandlerSpec = { module_ : BlankOr String
                         , name : BlankOr String
                         , modifier : BlankOr String
                         , types : SpecTypes
                         }

type alias Handler = { ast : Expr
                     , spec : HandlerSpec
                     }

type alias DBName = String
type alias DBColName = String
type alias DBColType = String
type alias DB = { name : DBName
                , cols : List (BlankOr DBColName, BlankOr DBColType)}

type TLData = TLHandler Handler
            | TLDB DB

type alias Toplevel = { id : TLID
                      , cursor: Int
                      , pos : Pos
                      , data : TLData
                      }


-----------------------------
-- Analysis
-----------------------------
type alias LVDict = Dict Int LiveValue
type alias AVDict = Dict Int (List VarName)
type alias AResult = { astValue: LiveValue
                     , liveValues : LVDict
                     , availableVarnames : AVDict
                     }
type alias TLAResult = { id: TLID
                       , results: List AResult
                       }

-----------------------------
-- Model
-----------------------------

type alias Clipboard = Maybe PointerData
type alias Model = { center : Pos
                   , error : Maybe String
                   , lastMsg : Msg
                   , lastMod : Modification
                   , tests : List VariantTest
                   , complete : Autocomplete
                   , state : State
                   , hovering : List Pointer
                   , toplevels : List Toplevel
                   , analysis : List TLAResult
                   , globals : List GlobalVariable
                   , integrationTestState : IntegrationTestState
                   , visibility : PageVisibility.Visibility
                   , clipboard : Clipboard
                   , syncEnabled : Bool
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
                  | SetHover Pointer
                  | ClearHover Pointer
                  | Deselect
                  | SetToplevels (List Toplevel) (List TLAResult) (List GlobalVariable)
                  | Enter EntryCursor
                  | RPC (List RPC, Focus)
                  | GetAnalysisRPC
                  | SetCenter Pos
                  | NoChange
                  | MakeCmd (Cmd Msg)
                  | AutocompleteMod AutocompleteMod
                  | Many (List Modification)
                  | Drag TLID VPos HasMoved State
                  | TriggerIntegrationTest String
                  | EndIntegrationTest
                  | SetState State
                  | CopyToClipboard Clipboard
                  | SetCursor TLID Int
                  -- designed for one-off small changes
                  | TweakModel (Model -> Model)

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
                      , infix: Bool
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
                          , infix: Bool
                          }

