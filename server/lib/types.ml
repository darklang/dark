open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, show, yojson, sexp, bin_io]

type host = string [@@deriving eq, show, yojson, sexp, bin_io]

(* READ FOR WEIRD SHIT, including the definitions of id and or_blank *)
include Types_bin_io_hack

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
  | TDbList of tipe_
  [@@deriving eq, show, yojson, sexp, bin_io]


module DbT = struct
  type col = string or_blank * tipe_ or_blank
            [@@deriving eq, show, yojson, sexp]
  type db = { tlid: tlid
            ; host: string
            ; display_name: string
            ; actual_name: string
            ; cols: col list
            } [@@deriving eq, show, yojson, sexp]
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
             | Response of int * (string * string) list
             [@@deriving show, eq, yojson, sexp]

  type feature_flag = Analysis
                    | FromUser of string
                    [@@deriving yojson]


  module DvalMap = String.Map
  (* TODO: make these printable *)
  type dval_map = dval DvalMap.t [@opaque]
  and uuid = Uuid.Stable.V1.t [@opaque]
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
    | DID of uuid
    | DDate of Time.t
    | DTitle of string
    | DUrl of string
    [@@deriving show, sexp]

  type tipe = tipe_ [@@deriving eq, show, yojson, sexp, bin_io]

  module EnvMap = Int.Map
  type env_map = (dval_map list) EnvMap.t [@opaque]

  module Symtable = DvalMap
  type symtable = dval_map

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
               } [@@deriving eq, show, yojson, sexp, bin_io]

  type ufn_param = { name: string or_blank
                   ; tipe: tipe or_blank
                   ; block_args : string list
                   ; optional : bool
                   ; description : string
                   } [@@deriving eq, show, yojson, sexp, bin_io]

  let ufn_param_to_param p : param option =
    match (p.name, p.tipe) with
    | (Filled (_, n), Filled (_, t)) ->
       { name = n
       ; tipe = t
       ; block_args = p.block_args
       ; optional = p.optional
       ; description = p.description
       } |> Some
    | _ -> None

  type ufn_metadata = { name : string or_blank
                      ; parameters : ufn_param list
                      ; return_type : tipe or_blank
                      ; description : string
                      ; infix : bool
                      } [@@deriving eq, show, yojson, sexp, bin_io]

  type user_fn = { tlid: tlid
                 ; metadata : ufn_metadata
                 ; ast:  expr
                 } [@@deriving eq, show, yojson, sexp, bin_io]

  type exec_state = { ff: feature_flag
                    ; tlid: tlid
                    ; host: string
                    ; user_fns: user_fn list
                    ; exe_fn_ids: id list
                    ; env: symtable
                    ; dbs: DbT.db list
                    ; id: int
                    }

  type funcimpl = InProcess of (exec_state * dval list -> dval)
                | API of (dval_map -> dval)
                | UserCreated of expr

  (* TODO: merge fn and user_fn *)
  type fn = { prefix_names : string list
            ; infix_names : string list
            ; parameters : param list
            ; return_type : tipe
            ; description : string
            ; preview : (dval list -> int -> dval list) option
            ; func : funcimpl
            ; preview_execution_safe : bool
            }

  let user_fn_to_fn uf : fn option =
    let name =
      match uf.metadata.name with
      | Filled (_, n) -> Some n
      | _ -> None
    in
    let rt =
      match uf.metadata.return_type with
      | Filled (_, t) -> Some t
      | _ -> None
    in
    let params =
      List.filter_map ~f:ufn_param_to_param uf.metadata.parameters
    in
    let params_all_filled =
      (List.length params) = (List.length uf.metadata.parameters)
    in
    match (name, rt, params_all_filled) with
    | (Some n, Some t, true) ->
      { prefix_names = [n]
      ; infix_names = []
      ; parameters = params
      ; return_type = t
      ; description = uf.metadata.description
      ; preview_execution_safe = false
      ; preview = None
      ; func = UserCreated uf.ast
      } |> Some
    | _ -> None
end


