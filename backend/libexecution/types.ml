open Core_kernel
module Int63 = Prelude.Int63

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type pos =
  { x : int
  ; y : int }
[@@deriving eq, compare, show, yojson, bin_io]

(* We choose int63 so that we get the same type in jsoo, instead of 31 bit. Our
 * client generated ids which are uint32, so we need to go bigger. *)
type id = Int63.t [@@deriving eq, compare, show, bin_io, yojson]

let id_of_int = Int63.of_int

let id_of_string = Int63.of_string

let string_of_id = Int63.to_string

module IDTable = Int63.Table

module IDMap = struct
  module T = struct
    include Map.Make (Int63)
  end

  include T

  let to_yojson fn map : Yojson.Safe.t =
    map
    |> T.to_alist
    |> List.map ~f:(fun (k, v) -> (string_of_id k, fn v))
    |> fun l -> `Assoc l


  let pp
      (valueFormatter : Format.formatter -> 'value -> unit)
      (fmt : Format.formatter)
      (map : 'value t) =
    Format.pp_print_string fmt "{ " ;
    Map.iteri map (fun ~key ~data ->
        Format.pp_print_string fmt (string_of_id key) ;
        Format.pp_print_string fmt ": " ;
        valueFormatter fmt data ;
        Format.pp_print_string fmt ",  ") ;
    Format.pp_print_string fmt "}" ;
    ()


  let of_yojson fn json =
    match json with
    | `Assoc l ->
        l
        |> List.map ~f:(fun (k, v) ->
               Result.map (fn v) ~f:(fun v -> (id_of_string k, v)))
        |> Result.combine_errors
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Result.bind ~f:(fun l ->
               match T.of_alist l with
               | `Duplicate_key k ->
                   Error ("duplicate key: " ^ string_of_id k)
               | `Ok m ->
                   Ok m)
    | _ ->
        Error "Expected an object"
end

type host = string [@@deriving eq, compare, show, bin_io]

type tlid = id [@@deriving eq, compare, show, yojson, bin_io]

module TLIDTable = IDTable

type 'a or_blank =
  | Blank of id
  | Filled of id * 'a
  | Partial of id * string
[@@deriving eq, compare, show {with_path = false}, yojson, bin_io]

(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type tipe_ =
  | TAny
  (* extra type meaning anything *)
  | TInt
  | TFloat
  | TBool
  | TNull
  | TDeprecated1
  | TStr
  | TList
  | TObj
  | TIncomplete
  | TError
  | TBlock
  | TResp
  | TDB
  | TDeprecated6
  | TDate
  | TDeprecated2
  | TDeprecated3
  (* Storage related hackery *)
  | TDeprecated4 of string
  | TDeprecated5 of string
  | TDbList of tipe_
  | TPassword
  | TUuid
  | TOption
  | TErrorRail
  | TCharacter
  | TResult
  (* name * version *)
  | TUserType of string * int
  | TBytes
[@@deriving eq, compare, show, yojson, bin_io]

(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

module RuntimeT = struct
  (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type fnname = string [@@deriving eq, compare, yojson, show, bin_io]

  type fieldname = string [@@deriving eq, compare, yojson, show, bin_io]

  type varname = string [@@deriving eq, compare, yojson, show, bin_io]

  type keyname = string [@@deriving eq, compare, yojson, show, bin_io]

  type varbinding = varname or_blank
  [@@deriving eq, compare, yojson, show, bin_io]

  type field = fieldname or_blank [@@deriving eq, compare, yojson, show, bin_io]

  type key = keyname or_blank [@@deriving eq, compare, yojson, show, bin_io]

  type npattern =
    | PVariable of varname
    | PLiteral of string
    | PConstructor of string * pattern list

  and pattern = npattern or_blank [@@deriving eq, compare, yojson, show, bin_io]

  type nexpr =
    | If of expr * expr * expr
    | Thread of expr list
    | FnCall of fnname * expr list
    | Variable of varname
    | Let of varbinding * expr * expr
    | Lambda of varbinding list * expr
    | Value of string
    | FieldAccess of expr * field
    | ObjectLiteral of (key * expr) list
    | ListLiteral of expr list
    | FeatureFlag of string or_blank * expr * expr * expr
    (* it's like this, instead of a bool on fncall, to avoid a
     * migration because we don't know how this is going to work
     * in the end. *)
    | FnCallSendToRail of fnname * expr list
    | Match of expr * (pattern * expr) list
    | Constructor of string or_blank * expr list
    (* For editing an expression, the string holds the autocomplete query,
     * and the expression holds the old value, which remains valid until the
     * new expression is complete. *)
    | FluidPartial of string * expr
    (* For changing an expression to a binop, we need to hold the expression
     * even though it isn't a valid binop yet. The string is  the soon-to-be
     * binop, and expr is the soon-to-be lhs or the binop. *)
    | FluidRightPartial of string * expr

  and expr = nexpr or_blank
  [@@deriving eq, compare, yojson, show {with_path = false}, bin_io]

  (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module DbT = struct
    (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type col = string or_blank * tipe_ or_blank
    [@@deriving eq, compare, show, yojson, bin_io]

    type migration_kind = DeprecatedMigrationKind
    [@@deriving eq, compare, show, yojson, bin_io]

    type db_migration_state =
      | DBMigrationAbandoned
      | DBMigrationInitialized
    [@@deriving eq, compare, show, yojson, bin_io]

    type db_migration =
      { starting_version : int
      ; version : int
      ; state : db_migration_state
      ; rollforward : expr
      ; rollback : expr
      ; cols : col list }
    [@@deriving eq, compare, show, yojson, bin_io]

    type db =
      { tlid : tlid
      ; name : string or_blank
      ; cols : col list
      ; version : int
      ; old_migrations : db_migration list
      ; active_migration : db_migration option }
    [@@deriving eq, compare, show, yojson, bin_io]

    (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
  end

  module HandlerT = struct
    (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type dtdeprecated = int or_blank
    [@@deriving eq, compare, show, yojson, bin_io]

    type spec_types =
      { input : dtdeprecated
      ; output : dtdeprecated }
    [@@deriving eq, show, yojson, bin_io]

    type spec =
      { module_ : string or_blank [@key "module"]
      ; name : string or_blank
      ; modifier : string or_blank
      ; types : spec_types }
    [@@deriving eq, show, yojson, bin_io]

    type handler =
      { tlid : tlid
      ; ast : expr
      ; spec : spec }
    [@@deriving eq, show, yojson, bin_io]

    (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
  end

  module HandlerF = struct
    type spec =
      { module_ : string or_blank [@key "module"]
      ; name : string or_blank
      ; modifier : string or_blank }
    [@@deriving eq, show, yojson]

    type handler =
      { tlid : tlid
      ; ast : Libshared.FluidExpression.t
      ; spec : spec }
    [@@deriving eq, show]
  end

  (* ------------------------ *)
  (* Dvals*)
  (* ------------------------ *)
  type dhttp =
    | Redirect of string
    | Response of int * (string * string) list
  [@@deriving show, eq, yojson, compare]

  (* block *)
  type 'a block = 'a list -> 'a [@@deriving show]

  let equal_block _ _ _ = false

  let compare_block _ _ _ = -1

  (* These aren't used at the moment, I hope, so just pick anything that
   * typechecks *)
  let block_to_yojson f b = `Null

  let block_of_yojson f _ = Ok (fun l -> List.hd_exn l)

  (* uuid *)
  type uuid = Uuidm.t [@@deriving show, eq, compare]

  let uuid_to_yojson uuid = `String (Uuidm.to_string uuid)

  let uuid_of_yojson json =
    match json with
    | `String s ->
        Uuidm.of_string s
        |> Core_kernel.Result.of_option ~error:"can't be parsed"
    | _ ->
        Error "not a string"


  let uuid_of_sexp st =
    match st with
    | Sexp.Atom s ->
        Option.value_exn ~message:"failure uuid_of_sexp" (Uuidm.of_string s)
    | _ ->
        failwith "failure uuid_of_sexp"


  let sexp_of_uuid u = Sexp.Atom (Uuidm.to_string u)

  (* time *)
  type time = Time.Stable.With_utc_sexp.V2.t [@@deriving compare]

  let pp_time f (t : time) = Format.fprintf f "%s" (Util.isostring_of_date t)

  let equal_time t1 t2 = t1 = t2

  let time_to_yojson time = `String (Util.isostring_of_date time)

  let time_of_yojson json =
    match json with
    | `String s ->
        Ok (Util.date_of_isostring s)
    | _ ->
        Error "Invalid time"


  (*
   * Raw Bytes, with (to|of)_yojson.
   * Extends native Bytes with yojson un/marshaling functions
   * *)
  module RawBytes = struct
    include Bytes

    let to_yojson bytes = `String (bytes |> Libtarget.base64url_bytes)

    let of_yojson json =
      match json with
      | `String s ->
          Ok (s |> Libtarget.bytes_from_base64url)
      | _ ->
          Error "Expected a string"
  end

  (* Password Bytes, with (to|of)_yojson, but redacted *)
  module PasswordBytes = struct
    include Bytes

    let to_yojson bytes = `String "Redacted"

    let of_yojson json =
      match json with
      | `String s ->
          Ok (s |> Bytes.of_string)
      | _ ->
          Error "Expected a string"
  end

  (* Special types:
     DIncomplete:

       A DIncomplete represents incomplete computation, whose source is
       always a Blank. When the code runs into a blank, it must return
       incomplete because the code is not finished. An incomplete value
       results in a 500 because it is a developer error.

       Propagating DIncompletes is straightforward: any computation
       relying on an incomplete must itself be incomplete.

       Some examples:
       - calling a function with an incomplete as a parameter is an
         incomplete function call.
       - an if statement with an incomplete in the cond must be incomplete.

       But computation that doesn't rely on the incomplete value can
       ignore it:

       - an if statement which with a blank in the ifbody and a
         complete expression in the elsebody will execute just fine if
         cond is false. It has not hit any part of the program that is
         being worked on.

       - a list with blanks in it can just ignore the blanks.
       - an incomplete in a list should be filtered out, because the
         program has not been completed, and so that list entry just
         doesn't "exist" yet.
       - incompletes in keys or values of objects cause the entire row
         to be ignored.

    DErrorRail:
      A DErrorRail represents a value which has been sent over to the
      errorrail. Because the computation is happening on the errorrail,
      no other computation occurs.

      In all cases, we can consider it equivalent to goto
      end_of_function.

      - an if with an derrorrail in an subexpression is a derrorrail
      - a list containing a derrorrail is a derrorail

  *)
  module DvalMap = struct
    module T = Prelude.StrDict
    include T

    let to_yojson fn map = map |> T.map ~f:fn |> T.to_list |> fun l -> `Assoc l

    let of_yojson fn json =
      match json with
      | `Assoc l ->
          l
          |> List.map ~f:(fun (k, v) ->
                 Result.map (fn v) ~f:(fun dv -> (k, dv)))
          |> Result.combine_errors
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Result.bind ~f:T.from_list_unique
      | _ ->
          Error "Expected an object"
  end

  type dval_map = dval DvalMap.t

  and optionT =
    | OptJust of dval
    | OptNothing

  and resultT =
    | ResOk of dval
    | ResError of dval

  and dval_source =
    | SourceNone
    | SourceId of id

  and dblock_args =
    { symtable : dval_map
    ; params : (id * string) list
    ; body : expr }

  and dval =
    (* basic types  *)
    | DInt of Dint.t
    | DFloat of float
    | DBool of bool
    | DNull
    | DStr of Unicode_string.t
    (* compound types *)
    | DList of dval list
    | DObj of dval_map
    (* special types - see notes above *)
    | DIncomplete of dval_source
    | DError of (dval_source * string)
    | DBlock of dblock_args
    | DErrorRail of dval
    (* user types: awaiting a better type system *)
    | DResp of (dhttp * dval)
    | DDB of string
    | DDate of time
    | DPassword of PasswordBytes.t
    | DUuid of uuid
    | DOption of optionT
    | DCharacter of Unicode_string.Character.t
    | DResult of resultT
    | DBytes of RawBytes.t
  [@@deriving show {with_path = false}, eq, yojson, compare]

  type dval_list = dval list

  (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type tipe = tipe_ [@@deriving eq, show, yojson, bin_io]

  (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module TipeMap = String.Map

  type tipe_map = tipe String.Map.t

  type string_dval_pair = string * dval [@@deriving show, eq]

  type input_vars = string_dval_pair list

  (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type param =
    { name : string
    ; tipe : tipe
    ; block_args : string list
    ; optional : bool
    ; description : string }
  [@@deriving eq, show, yojson, bin_io]

  type ufn_param =
    { name : string or_blank
    ; tipe : tipe or_blank
    ; block_args : string list
    ; optional : bool
    ; description : string }
  [@@deriving eq, show, yojson, bin_io]

  type ufn_metadata =
    { name : string or_blank
    ; parameters : ufn_param list
    ; return_type : tipe or_blank
    ; description : string
    ; infix : bool }
  [@@deriving eq, show, yojson, bin_io]

  type user_fn =
    { tlid : tlid
    ; metadata : ufn_metadata
    ; ast : expr }
  [@@deriving eq, show, yojson, bin_io]

  type user_record_field =
    { name : string or_blank
    ; tipe : tipe_ or_blank }
  [@@deriving eq, show, yojson, bin_io]

  type user_tipe_definition = UTRecord of user_record_field list
  [@@deriving eq, show, yojson, bin_io]

  type user_tipe =
    { tlid : tlid
    ; name : string or_blank
    ; version : int
    ; definition : user_tipe_definition }
  [@@deriving eq, show, yojson, bin_io]

  (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  type function_desc = tlid * string * id [@@deriving yojson]

  type load_fn_result_type =
    function_desc -> dval list -> (dval * Time.t) option

  type store_fn_result_type = function_desc -> dval list -> dval -> unit

  type load_fn_arguments_type = tlid -> (dval_map * Time.t) list

  type store_fn_arguments_type = tlid -> dval_map -> unit

  type fail_fn_type = (?msg:string -> unit -> dval) option

  (* this is _why_ we're executing the AST, to allow us to not
   * emit certain side-effects (eg. DB writes) when showing previews *)
  type context =
    | Preview
    | Real
  [@@deriving eq, show, yojson]

  type execution_result =
    | ExecutedResult of dval
    | NonExecutedResult of dval
  [@@deriving yojson]

  type exec_state =
    { tlid : tlid
    ; canvas_id : Uuidm.t
    ; account_id : Uuidm.t
    ; user_fns : user_fn list
    ; user_tipes : user_tipe list
    ; dbs : DbT.db list
    ; trace : id -> execution_result -> unit
    ; trace_tlid : tlid -> unit
    ; context : context
    ; execution_id : id
    ; exec : state:exec_state -> dval_map -> expr -> dval
          (* Some parts of the execution need to call AST.exec, but cannot call
           * AST.exec without a cyclic dependency. This function enables that, and it
           * is safe to do so because all of the state is in the exec_state
           * structure. *)
    ; load_fn_result : load_fn_result_type
    ; store_fn_result : store_fn_result_type
    ; load_fn_arguments : load_fn_arguments_type
    ; store_fn_arguments : store_fn_arguments_type
    ; fail_fn : fail_fn_type }

  type funcimpl =
    | InProcess of (exec_state * dval list -> dval)
    | API of (dval_map -> dval)
    | UserCreated of (tlid * expr)

  (* TODO: merge fn and user_fn *)
  type fn =
    { prefix_names : string list
    ; infix_names : string list
    ; parameters : param list
    ; return_type : tipe
    ; description : string
    ; func : funcimpl
    ; preview_execution_safe : bool
    ; deprecated : bool }

  let ufn_param_to_param (p : ufn_param) : param option =
    match (p.name, p.tipe) with
    | Filled (_, n), Filled (_, t) ->
        { name = n
        ; tipe = t
        ; block_args = p.block_args
        ; optional = p.optional
        ; description = p.description }
        |> Some
    | _ ->
        None


  let user_fn_to_fn uf : fn option =
    let name =
      match uf.metadata.name with Filled (_, n) -> Some n | _ -> None
    in
    let rt =
      match uf.metadata.return_type with Filled (_, t) -> Some t | _ -> None
    in
    let params = List.filter_map ~f:ufn_param_to_param uf.metadata.parameters in
    let params_all_filled =
      List.length params = List.length uf.metadata.parameters
    in
    match (name, rt, params_all_filled) with
    | Some n, Some t, true ->
        { prefix_names = [n]
        ; infix_names = []
        ; parameters = params
        ; return_type = t
        ; description = uf.metadata.description
        ; preview_execution_safe = false
        ; func = UserCreated (uf.tlid, uf.ast)
        ; deprecated = false }
        |> Some
    | _ ->
        None
end
