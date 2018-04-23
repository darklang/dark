module Types exposing (..)

-- builtin
import Dict exposing (Dict)
import Http
import Dom
import Navigation
import Mouse
import Time exposing (Time)
import PageVisibility
import Json.Decode as JSD

-- libs
import Keyboard.Event exposing (KeyboardEvent)

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

-----------------------------
-- CursorState
-----------------------------
type alias DarkKeyboardEvent =
    { standard : KeyboardEvent
    , selectionStart : Maybe Int
    , selectionEnd : Maybe Int
    }
type EntryCursor = Creating Pos
                 | Filling TLID ID

type alias HasMoved = Bool
type CursorState = Selecting TLID (Maybe ID)
                 | Entering EntryCursor
                 | Dragging TLID VPos HasMoved CursorState
                 | Deselected


-----------------------------
-- Msg
-- main Elm Architecture bus type
-----------------------------
type TimerAction = RefreshAnalyses
                 | CheckUrlHashPosition

type alias GlobalVariable = String
type alias RPCResult = ( List Toplevel
                       , List TLAResult
                       , List GlobalVariable
                       , List UserFunction
                       , List FourOhFour
                       , List TLID)
type alias GetAnalysisRPCResult = ( List TLAResult
                                  , List GlobalVariable
                                  , List FourOhFour
                                  , List TLID)
type Msg
    = GlobalClick MouseEvent
    | NothingClick MouseEvent
    | ToplevelMouseDown TLID MouseEvent
    -- we have the actual node when ToplevelMouseUp is created,
    -- but by the time we use it the proper node will be changed
    | ToplevelMouseUp TLID MouseEvent
    | ToplevelClick TLID MouseEvent
    | DragToplevel TLID Mouse.Position
    | EntryInputMsg String
    | EntrySubmitMsg
    | GlobalKeyPress DarkKeyboardEvent
    | SliderChange ID
    | SliderMoving ID String
    | AutocompleteClick String
    | FocusEntry (Result Dom.Error ())
    | FocusAutocompleteItem (Result Dom.Error ())
    | RPCCallback Focus Modification RPCParams (Result Http.Error RPCResult)
    | SaveTestRPCCallback (Result Http.Error String)
    | GetAnalysisRPCCallback (Result Http.Error GetAnalysisRPCResult)
    | LocationChange Navigation.Location
    | AddRandom
    | FinishIntegrationTest
    | SaveTestButton
    | ToggleSync
    | ExecuteFunctionButton TLID ID
    | Initialization
    | NavigateTo String
    | CreateHandlerFrom404 FourOhFour
    | WindowResize Int Int
    | TimerFire TimerAction Time
    | JSError String
    | PageVisibilityChange PageVisibility.Visibility
    | PageFocusChange PageVisibility.Visibility
    | StartFeatureFlag
    | EndFeatureFlag ID
    | EditFunction
    | ReturnToMainCanvas
    | BlankOrClick TLID ID MouseEvent
    | BlankOrDoubleClick TLID ID MouseEvent
    | BlankOrMouseEnter TLID ID MouseEvent
    | BlankOrMouseLeave TLID ID MouseEvent
    | MouseWheel (List Int)
    | DataClick TLID Int MouseEvent
    | DataMouseEnter TLID Int MouseEvent
    | DataMouseLeave TLID Int MouseEvent

type alias Predecessor = Maybe PointerData
type alias Successor = Maybe PointerData
type Focus = FocusNothing -- deselect
           | FocusExact TLID ID
           | FocusNext TLID (Maybe ID)
           | FocusPageAndCursor CurrentPage CursorState
           | FocusSame -- unchanged
           | FocusNoChange -- unchanged

-----------------------------
-- RPCs
-----------------------------
type Op
    = SetHandler TLID Pos Handler
    | CreateDB TLID Pos DBName
    | AddDBCol TLID ID ID
    | SetDBColName TLID ID DBColName
    | SetDBColType TLID ID DBColType
    | DeleteTL TLID
    | MoveTL TLID Pos
    | Savepoint (List TLID)
    | UndoTL TLID
    | RedoTL TLID
    | SetFunction UserFunction
    | ChangeDBColName TLID ID DBColName
    | ChangeDBColType TLID ID DBColType

type alias RPCParams = { ops : List Op
                       , executableFns : List (TLID, ID, Int)
                       }


-----------------------------
-- Autocompletes
-----------------------------
type alias Autocomplete = { functions : List Function
                          , completions : List (List AutocompleteItem)
                          , allCompletions : List AutocompleteItem
                          , index : Int
                          , value : String
                          , target : Maybe (TLID, PointerData)
                          , tipe : Maybe Tipe
                          }

type alias Literal = Int
type OmniAction = NewDB DBName
type AutocompleteItem = ACFunction Function
                      | ACField String
                      | ACVariable VarName
                      | ACExtra String
                      | ACLiteral Literal
                      | ACOmniAction OmniAction

type alias Target = (TLID, PointerData)
type AutocompleteMod = ACSetQuery String
                     | ACAppendQuery String
                     | ACReset
                     | ACSelectDown
                     | ACSelectUp
                     | ACSetTarget (Maybe Target)
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


-----------------------------
-- AST
-----------------------------

type alias VarName = String
type alias FnName = String
type alias FieldName = String

type alias VarBind = BlankOr VarName
type alias Field = BlankOr FieldName

type alias Expr = BlankOr NExpr
type NExpr = If Expr Expr Expr
           | FnCall FnName (List Expr)
           | Variable VarName
           | Let VarBind Expr Expr
           | Lambda (List VarName) Expr
           | Value String
           | Thread (List Expr)
           | FieldAccess Expr Field

-----------------------------
-- Pointers
-----------------------------

type PointerData = PVarBind VarBind
                 | PEventName (BlankOr String)
                 | PEventModifier (BlankOr String)
                 | PEventSpace (BlankOr String)
                 | PExpr Expr
                 | PField Field
                 | PDBColName (BlankOr String)
                 | PDBColType (BlankOr String)
                 | PDarkType DarkType
                 | PDarkTypeField (BlankOr String)
                 | PFFMsg (BlankOr String)
                 | PFnName (BlankOr String)
                 | PParamName (BlankOr String)
                 | PParamTipe (BlankOr Tipe)


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
                 | FFMsg
                 | FnName
                 | ParamName
                 | ParamTipe

type BlankOr a = Blank ID
               | F ID a
               | Flagged
                   ID
                   (BlankOr String)
                   Int
                   (BlankOr a)
                   (BlankOr a)

type PointerOwner = POSpecHeader
                  | POAst
                  | PODb
                  | POSpecType

-----------------------------
-- Top-levels
-----------------------------
type alias DarkType = BlankOr NDarkType
type NDarkType = DTEmpty -- empty body
               | DTAny
               | DTString
               | DTInt
               | DTObj (List (BlankOr String, DarkType))

type alias SpecTypes = { input : DarkType
                       , output : DarkType
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
            | TLFunc UserFunction

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
type alias InputDict = Dict VarName LiveValue
type alias AResult = { astValue: LiveValue
                     , liveValues : LVDict
                     , availableVarnames : AVDict
                     , inputValues : InputDict
                     }
type alias TLAResult = { id: TLID
                       , results: List AResult
                       }

type alias FourOhFour = (String, String, String, List JSD.Value)


type alias Name = String
type CurrentPage = Toplevels
                 | Fn TLID

-----------------------------
-- Model
-----------------------------

type alias Clipboard = Maybe PointerData

type alias SyncState = { enabled : Bool
                       , inFlight : Bool
                       , ticks : Int
                       }

type alias UrlState = { lastPos : Pos
                      }

type alias Model = { center : Pos
                   , error : Maybe String
                   , lastMsg : Msg
                   , lastMod : Modification
                   , tests : List VariantTest
                   , complete : Autocomplete
                   , userFunctions : List UserFunction
                   , builtInFunctions : List Function
                   , cursorState : CursorState
                   , currentPage : CurrentPage
                   , hovering : List ID
                   , toplevels : List Toplevel
                   , analysis : List TLAResult
                   , globals : List GlobalVariable
                   , f404s : List FourOhFour
                   , unlockedDBs : List TLID
                   , integrationTestState : IntegrationTestState
                   , visibility : PageVisibility.Visibility
                   , clipboard : Clipboard
                   , syncState : SyncState
                   , urlState : UrlState
                   }

-- Values that we serialize
type alias SerializableEditor = { clipboard : Maybe PointerData
                                , syncEnabled : Bool
                                , cursorState : CursorState
                                }

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
                  | Select TLID (Maybe ID)
                  | SetHover ID
                  | ClearHover ID
                  | Deselect
                  | SetToplevels
                      (List Toplevel)
                      (List TLAResult)
                      (List GlobalVariable)
                      (List UserFunction)
                      (List FourOhFour)
                      (List TLID)
                      Bool
                  | Enter EntryCursor
                  | RPCFull (RPCParams, Focus)
                  | RPC (List Op, Focus) -- shortcut for RPCFull
                  | GetAnalysisRPC
                  | SetCenter Pos
                  | NoChange
                  | MakeCmd (Cmd Msg)
                  | AutocompleteMod AutocompleteMod
                  | Many (List Modification)
                  | Drag TLID VPos HasMoved CursorState
                  | TriggerIntegrationTest String
                  | EndIntegrationTest
                  | SetCursorState CursorState
                  | SetCurrentPage CurrentPage
                  | CopyToClipboard Clipboard
                  | SetCursor TLID Int
                  -- designed for one-off small changes
                  | TweakModel (Model -> Model)

-----------------------------
-- Flags / function types
-----------------------------

type alias Flags =
  { editorState: Maybe String
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
                      , previewExecutionSafe: Bool
                      , infix: Bool
                      }

type alias UserFunctionParameter = { name: BlankOr String
                                   , tipe: BlankOr Tipe
                                   , block_args: List String
                                   , optional: Bool
                                   , description: String
                                   }

type alias UserFunctionMetadata = { name: BlankOr String
                                  , parameters: List UserFunctionParameter
                                  , description: String
                                  , returnTipe: BlankOr Tipe
                                  , infix: Bool
                                  }

type alias UserFunction = { tlid: TLID
                          , metadata: UserFunctionMetadata
                          , ast: Expr
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
                          , preview_execution_safe: Bool
                          , infix: Bool
                          }

-----------------------------
-- URL Fragments
-----------------------------

type alias UrlFragmentData = { center: Maybe Pos
                             , editedFn : Maybe TLID
                             }
