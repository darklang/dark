open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, show, yojson, sexp, bin_io]
type id = int [@@deriving eq, show, yojson, sexp, bin_io]

type 'a or_blank = Blank of id
                 | Filled of id * 'a
                 | Flagged of string * int * ('a or_blank) * ('a or_blank)
                 [@@deriving eq, show, yojson, sexp, bin_io]

type tipe_ =
  | TAny (* extra type meaning anything *)
  | TInt
  | TFloat
  | TBool
  | TNull
  | TChar
  | TStr
  | TList
  | TObj
  | TIncomplete
  | TError
  | TBlock
  | TResp
  | TDB
  | TID
  | TDate
  | TTitle
  | TUrl
  (* Storage related hackery *)
  | TBelongsTo of string
  | THasMany of string
  [@@deriving eq, show, yojson]


module DbT = struct
  type col = string or_blank * tipe_ or_blank
            [@@deriving eq, show, yojson]
  type db = { tlid: tlid
            ; display_name: string
            ; actual_name: string
            ; cols: col list
            } [@@deriving eq, show, yojson]
end


module SpecTypes = struct
  type n_dark_type = Empty
                   | Any
                   | String
                   | Int
                   | Obj of (string or_blank * dark_type ) list
                   [@@deriving eq, show, yojson, sexp, bin_io]
  and dark_type = n_dark_type or_blank
                [@@deriving eq, show, yojson, sexp, bin_io]
end

module RuntimeT = struct
  type fnname = string [@@deriving eq, yojson, show, sexp, bin_io]
  type fieldname = string [@@deriving eq, yojson, show, sexp, bin_io]
  type varname = string [@@deriving eq, yojson, show, sexp, bin_io]

  type varbinding = varname or_blank
  [@@deriving eq, yojson, show, sexp, bin_io]

  type field = fieldname or_blank
  [@@deriving eq, yojson, show, sexp, bin_io]

  type nexpr = If of expr * expr * expr
             | Thread of expr list
             | FnCall of fnname * expr list
             | Variable of varname
             | Let of varbinding * expr * expr
             | Lambda of varname list * expr
             | Value of string
             | FieldAccess of expr * field
  [@@deriving eq, yojson, show, sexp, bin_io]
and expr = nexpr or_blank [@@deriving eq, yojson, show, sexp, bin_io]
  (* ------------------------ *)
  (* Dvals*)
  (* ------------------------ *)
  type dhttp = Redirect of string
             | Response of int * (string * string) list [@@deriving show, eq, yojson]

  module DvalMap = String.Map
  type dval_map = dval DvalMap.t [@opaque]
  and dval =
    (* basic types  *)
    | DInt of int
    | DFloat of float
    | DBool of bool
    | DNull (* TODO: make null more like option *)
    | DChar of char
    | DStr of string
    (* compound types *)
    | DList of dval list
    | DObj of dval_map
    (* special types *)
    | DIncomplete
    | DError of string
    | DBlock of (dval list -> dval)
    (* user types: awaiting a better type system *)
    | DResp of (dhttp * dval)
    | DDB of DbT.db
    | DID of Uuid.t
    | DDate of Time.t
    | DTitle of string
    | DUrl of string
    [@@deriving show]

  type tipe = tipe_ [@@deriving eq, show, yojson]

  module EnvMap = Int.Map
  type env_map = (dval_map list) EnvMap.t [@opaque]

  (* this is _why_ we're executing the AST, to allow us to not
   * emit certain side-effects (eg. DB writes) when showing previews *)
  type context = Preview
               | Real [@@deriving eq, show, yojson]

  exception TypeError of dval list

  type param = { name: string
               ; tipe: tipe
               ; block_args : string list
               ; optional : bool
               ; description : string
               } [@@deriving eq, show, yojson]

  type funcimpl = InProcess of (dval list -> dval)
                | API of (dval_map -> dval)
                | UserCreated of expr

  type fn_metadata = { prefix_names : string list
                     ; infix_names : string list
                     ; parameters : param list
                     ; return_type : tipe
                     ; description : string
                     ; previewExecutionSafe : bool
                     } [@@deriving eq, show, yojson]

  (* TODO: merge fn and user_fn *)
  type fn = { prefix_names : string list
            ; infix_names : string list
            ; parameters : param list
            ; return_type : tipe
            ; description : string
            ; preview : (dval list -> int -> dval list) option
            ; func : funcimpl
            ; previewExecutionSafe : bool
            }

  type user_fn = { metadata : fn_metadata
                 ; ast:  expr
                 } [@@deriving eq, show, yojson]

  let user_fn_to_fn uf =
    { prefix_names = uf.metadata.prefix_names
    ; infix_names = uf.metadata.infix_names
    ; parameters = uf.metadata.parameters
    ; return_type = uf.metadata.return_type
    ; description = uf.metadata.description
    ; previewExecutionSafe = uf.metadata.previewExecutionSafe
    ; preview = None
    ; func = UserCreated uf.ast
    }
end


