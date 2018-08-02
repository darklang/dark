open Core_kernel

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type pos = { x:int; y:int }[@@deriving eq, compare, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, compare, show, yojson, sexp, bin_io]

type host = string [@@deriving eq, compare, show, yojson, sexp, bin_io]

type id = int [@@deriving eq, compare, show, yojson, sexp, bin_io]
type id_list = id list [@@deriving eq, compare, show, yojson, sexp, bin_io]
type 'a or_blank = Blank of id
                 | Filled of id * 'a
                 [@@deriving eq, compare, show, yojson, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
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
  | TPassword
  [@@deriving eq, compare, show, yojson, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

module RuntimeT = struct
(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type fnname = string [@@deriving eq, compare, yojson, show, sexp, bin_io]
  type fieldname = string [@@deriving eq, compare, yojson, show, sexp, bin_io]
  type varname = string [@@deriving eq, compare, yojson, show, sexp, bin_io]
  type keyname = string [@@deriving eq, compare, yojson, show, sexp, bin_io]

  type varbinding = varname or_blank
  [@@deriving eq, compare, yojson, show, sexp, bin_io]

  type field = fieldname or_blank
  [@@deriving eq, compare, yojson, show, sexp, bin_io]

  type key = keyname or_blank
  [@@deriving eq, compare, yojson, show, sexp, bin_io]

  type nexpr = If of expr * expr * expr
             | Thread of expr list
             | FnCall of fnname * expr list
             | Variable of varname
             | Let of varbinding * expr * expr
             | Lambda of varbinding list * expr
             | Value of string
             | FieldAccess of expr * field
             | ObjectLiteral of (key * expr) list
             | ListLiteral of expr list
             | FeatureFlag of (string or_blank) * expr * expr * expr
  [@@deriving eq, compare, yojson, show, sexp, bin_io]
and expr = nexpr or_blank [@@deriving eq, compare, yojson, show, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module DbT = struct
    type col = string or_blank * tipe_ or_blank
              [@@deriving eq, compare, show, yojson, sexp]
(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type migration_kind = ChangeColType
                          [@@deriving eq, compare, show, yojson, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
    type db_migration = { starting_version : int
                        ; kind : migration_kind
                        ; rollforward : expr
                        ; rollback : expr
                        ; target : id
                        }
                        [@@deriving eq, compare, show, yojson, sexp]
    type db = { tlid: tlid
              ; host: string
              ; name: string
              ; cols: col list
              ; version: int
              ; old_migrations : db_migration list
              ; active_migration : db_migration option
              } [@@deriving eq, compare, show, yojson, sexp]
  end

  module HandlerT = struct

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type n_dark_type = Empty
                     | Any
                     | String
                     | Int
                     | Obj of (string or_blank * dark_type ) list
                     [@@deriving eq, compare, show, yojson, sexp, bin_io]
    and dark_type = n_dark_type or_blank
                    [@@deriving eq, compare, show, yojson, sexp, bin_io]

    type spec_types = { input : dark_type
                      ; output : dark_type
                      } [@@deriving eq, show, yojson, sexp, bin_io]


    type spec = { module_ : string or_blank [@key "module"]
                ; name : string or_blank
                ; modifier : string or_blank
                ; types : spec_types
                } [@@deriving eq, show, yojson, sexp, bin_io]

    type handler = { tlid: tlid
                   ; ast: expr
                   ; spec : spec
                   } [@@deriving eq, show, yojson, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
  end


  (* ------------------------ *)
  (* Dvals*)
  (* ------------------------ *)
  type dhttp = Redirect of string
             | Response of int * (string * string) list
             [@@deriving show, eq, yojson, sexp, eq, compare]

  type feature_flag = Analysis
                    | RealKey of string
                    [@@deriving yojson]


  type 'a block = 'a list -> 'a [@opaque][@@deriving show, sexp]
  let equal_block _ _ _ = false
  let compare_block _ _ _ = -1

  (* TODO SPLIT: what is the format vs Core's Uuid.Stable.V1 *)
  type uuid = Uuidm.t [@opaque][@@deriving show, eq, compare]
  let uuid_of_sexp s = failwith "TODO"
  let sexp_of_uuid s = failwith "TODO"
  type time = Time.Stable.With_utc_sexp.V2.t
              [@opaque]
              [@@deriving compare, sexp, show]
  let equal_time t1 t2 = t1 = t2


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
    | DBlock of dval block
    (* user types: awaiting a better type system *)
    | DResp of (dhttp * dval)
    | DDB of DbT.db
    | DID of uuid
    | DDate of time
    | DTitle of string
    | DUrl of string
    | DPassword of Bytes.t
    [@@deriving show, sexp, eq, compare]
  type dval_list = dval list [@@deriving show]

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type tipe = tipe_ [@@deriving eq, show, yojson, sexp, bin_io]
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module TipeMap = String.Map
  type tipe_map = tipe String.Map.t

  module EnvMap = Int.Map
  type env_map = (dval_map list) EnvMap.t [@opaque]

  module Symtable = DvalMap
  type symtable = dval_map

  (* this is _why_ we're executing the AST, to allow us to not
   * emit certain side-effects (eg. DB writes) when showing previews *)
  type context = Preview
               | Real [@@deriving eq, show, yojson]

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
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
(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)


  type function_desc = Uuidm.t * tlid * string * id

  type exec_state = { tlid: tlid
                    ; canvas_id : Uuidm.t
                    ; account_id : Uuidm.t
                    ; host: string
                    ; user_fns: user_fn list
                    ; exe_fn_ids: id list
                    ; input_cursor : int
                    ; env: symtable
                    ; dbs: DbT.db list
                    ; execution_id: int
                    ; load_fn_result :
                        function_desc -> dval list -> (dval * Time.t) option
                    ; store_fn_result :
                        function_desc ->
                        dval list ->
                        dval ->
                        unit
                    ; fail_fn : (?msg : string -> unit -> dval) option
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

  let ufn_param_to_param (p : ufn_param) : param option =
    match (p.name, p.tipe) with
    | (Filled (_, n), Filled (_, t)) ->
       { name = n
       ; tipe = t
       ; block_args = p.block_args
       ; optional = p.optional
       ; description = p.description
       } |> Some
    | _ -> None

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


