type tipe = Types.tipe =
  | TInt
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
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TBelongsTo of string
  | THasMany of string
  | TDbList of tipe
  [@@bs.deriving accessors]

type id = Types.id = ID of int [@@bs.deriving accessors]
type tlid = Types.tlid = TLID of int [@@bs.deriving accessors]

type pos = Types.pos = { x: int; y: int } [@@bs.deriving accessors]
type vPos = Types.vPos = { vx: int; vy: int } [@@bs.deriving accessors]

type entryCursor = Types.entryCursor =
    Creating of pos
  | Filling of tlid * id
[@@bs.deriving accessors]

type hasMoved = bool

type cursorState = Types.cursorState =
  | Selecting of tlid * id option
  | Entering of entryCursor
  | Dragging of tlid * vPos * hasMoved * cursorState
  | SelectingCommand of tlid * id
  | Deselected
  [@@bs.deriving accessors]


