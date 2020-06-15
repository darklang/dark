open Core_kernel
module Int63 = Prelude.Int63

(* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
type pos =
  { x : int
  ; y : int }
[@@deriving eq, ord, show, yojson, bin_io]

type fluid_expr = Libshared.FluidExpression.t [@@deriving eq, ord, show, yojson]

(* We choose int63 so that we get the same type in jsoo, instead of 31 bit. Our
 * client generated ids which are uint32, so we need to go bigger. *)
type id = Int63.t [@@deriving eq, ord, show, bin_io, yojson]

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

type host = string [@@deriving eq, ord, show, bin_io]

type tlid = id [@@deriving eq, ord, show, yojson, bin_io]

module TLIDTable = IDTable

type 'a or_blank =
  | Blank of id
  | Filled of id * 'a
  | Partial of id * string
[@@deriving eq, ord, show {with_path = false}, yojson, bin_io]

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
[@@deriving eq, ord, show, yojson, bin_io]

(* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

module RuntimeT = struct
  (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type fnname = string [@@deriving eq, ord, yojson, show, bin_io]

  type fieldname = string [@@deriving eq, ord, yojson, show, bin_io]

  type varname = string [@@deriving eq, ord, yojson, show, bin_io]

  type keyname = string [@@deriving eq, ord, yojson, show, bin_io]

  type varbinding = varname or_blank [@@deriving eq, ord, yojson, show, bin_io]

  type field = fieldname or_blank [@@deriving eq, ord, yojson, show, bin_io]

  type key = keyname or_blank [@@deriving eq, ord, yojson, show, bin_io]

  type npattern =
    | PVariable of varname
    | PLiteral of string
    | PConstructor of string * pattern list

  and pattern = npattern or_blank [@@deriving eq, ord, yojson, show, bin_io]

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
    | FluidLeftPartial of string * expr

  and expr = nexpr or_blank
  [@@deriving eq, ord, yojson, show {with_path = false}, bin_io]

  (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module DbT = struct
    (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type col = string or_blank * tipe_ or_blank
    [@@deriving eq, ord, show, yojson, bin_io]

    type migration_kind = DeprecatedMigrationKind
    [@@deriving eq, ord, show, yojson, bin_io]

    type db_migration_state =
      | DBMigrationAbandoned
      | DBMigrationInitialized
    [@@deriving eq, ord, show, yojson, bin_io]

    type 'expr_type db_migration =
      { starting_version : int
      ; version : int
      ; state : db_migration_state
      ; rollforward : 'expr_type
      ; rollback : 'expr_type
      ; cols : col list }
    [@@deriving eq, ord, show, yojson, bin_io]

    type 'expr_type db =
      { tlid : tlid
      ; name : string or_blank
      ; cols : col list
      ; version : int
      ; old_migrations : 'expr_type db_migration list
      ; active_migration : 'expr_type db_migration option }
    [@@deriving eq, ord, show, yojson, bin_io]

    (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
  end

  module HandlerT = struct
    (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
    type dtdeprecated = int or_blank [@@deriving eq, ord, show, yojson, bin_io]

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

    type 'expr_type handler =
      { tlid : tlid
      ; ast : 'expr_type
      ; spec : spec }
    [@@deriving eq, show, yojson, bin_io]

    (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)
  end

  (* ------------------------ *)
  (* Dvals *)
  (* ------------------------ *)
  type dhttp =
    | Redirect of string
    | Response of int * (string * string) list
  [@@deriving show, eq, yojson, ord]

  (* uuid *)
  type uuid = Uuidm.t [@@deriving show, eq, ord]

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
  type time = Time.Stable.With_utc_sexp.V2.t [@@deriving ord]

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

  (* To support migrating to fluid, these take a type parameter, which is
   * concretely defined to use `expr` at the bottom. *)
  type 'expr_type dval_map = 'expr_type dval DvalMap.t

  and 'expr_type optionT =
    | OptJust of 'expr_type dval
    | OptNothing

  and 'expr_type resultT =
    | ResOk of 'expr_type dval
    | ResError of 'expr_type dval

  and dval_source =
    | SourceNone
    | SourceId of tlid * id

  and 'expr_type dblock_args =
    { symtable : 'expr_type dval_map
    ; params : (id * string) list
    ; body : 'expr_type }

  and 'expr_type dval =
    (* basic types  *)
    | DInt of Dint.t
    | DFloat of float
    | DBool of bool
    | DNull
    | DStr of Unicode_string.t
    (* compound types *)
    | DList of 'expr_type dval list
    | DObj of 'expr_type dval_map
    (* special types - see notes above *)
    | DIncomplete of dval_source
    | DError of (dval_source * string)
    | DBlock of 'expr_type dblock_args
    | DErrorRail of 'expr_type dval
    (* user types: awaiting a better type system *)
    | DResp of (dhttp * 'expr_type dval)
    | DDB of string
    | DDate of time
    | DPassword of PasswordBytes.t
    | DUuid of uuid
    | DOption of 'expr_type optionT
    | DCharacter of Unicode_string.Character.t
    | DResult of 'expr_type resultT
    | DBytes of RawBytes.t

  and 'expr_type dval_list = 'expr_type dval list

  (* Concrete definitions for expr *)
  and expr_dval = expr dval

  and expr_dblock_args = expr dblock_args

  and expr_dval_map = expr dval_map

  and expr_optionT = expr optionT

  and expr_resultT = expr resultT

  and expr_dval_list = expr dval_list

  (* Concrete definitions for fluid *)
  and fluid_dval = fluid_expr dval

  and fluid_dblock_args = fluid_expr dblock_args

  and fluid_dval_map = fluid_expr dval_map

  and fluid_optionT = fluid_expr optionT

  and fluid_resultT = fluid_expr resultT

  and fluid_dval_list = fluid_expr dval_list
  [@@deriving show {with_path = false}, eq, ord, yojson]

  (* DO NOT CHANGE BELOW WITHOUT READING docs/oplist-serialization.md *)
  type tipe = tipe_ [@@deriving eq, show, yojson, bin_io]

  (* DO NOT CHANGE ABOVE WITHOUT READING docs/oplist-serialization.md *)

  module TipeMap = String.Map

  type tipe_map = tipe String.Map.t

  type 'expr_type string_dval_pair = string * 'expr_type dval
  [@@deriving show, eq]

  type 'expr_type input_vars = 'expr_type string_dval_pair list

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

  type 'expr_type user_fn =
    { tlid : tlid
    ; metadata : ufn_metadata
    ; ast : 'expr_type }
  [@@deriving eq, show, yojson, bin_io]

  type 'expr_type package_fn =
    { metadata : ufn_metadata
    ; ast : 'expr_type }
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

  type secret =
    { secret_name : string
    ; secret_value : string }
  [@@deriving eq, show, yojson]

  type function_desc = tlid * string * id [@@deriving yojson]

  type 'expr_type load_fn_result_type =
    function_desc -> 'expr_type dval list -> ('expr_type dval * Time.t) option

  type 'expr_type store_fn_result_type =
    function_desc -> 'expr_type dval list -> 'expr_type dval -> unit

  type 'expr_type load_fn_arguments_type =
    tlid -> ('expr_type dval_map * Time.t) list

  type 'expr_type store_fn_arguments_type = tlid -> 'expr_type dval_map -> unit

  type 'expr_type fail_fn_type = (?msg:string -> unit -> 'expr_type dval) option

  (* this is _why_ we're executing the AST, to allow us to not
   * emit certain side-effects (eg. DB writes) when showing previews *)
  type context =
    | Preview
    | Real
  [@@deriving eq, show, yojson]

  type 'expr_type execution_result =
    | ExecutedResult of 'expr_type dval
    | NonExecutedResult of 'expr_type dval
  [@@deriving eq, show, yojson]

  type fn_preview_safety =
    | Safe
    | Unsafe
  [@@deriving eq, show, yojson]

  type 'expr_type exec_state =
    { tlid : tlid
    ; canvas_id : Uuidm.t
    ; account_id : Uuidm.t
    ; user_fns : 'expr_type user_fn list
    ; user_tipes : user_tipe list
    ; package_fns : 'expr_type fn list
    ; dbs : fluid_expr DbT.db list
    ; secrets : secret list
    ; trace : on_execution_path:bool -> id -> 'expr_type dval -> unit
    ; trace_tlid : tlid -> unit
    ; callstack :
        (* Used for recursion detection in the editor. In the editor, we call all
         * paths to show live values, but with recursion that causes infinite
         * recursion. *)
        Tc.StrSet.t
    ; context : context
    ; execution_id : id
    ; on_execution_path :
        (* Whether the currently executing code is really being executed (as
         * opposed to being executed for traces) *)
        bool
    ; exec :
           state:'expr_type exec_state
        -> 'expr_type dval_map
        -> 'expr_type
        -> 'expr_type dval
          (* Some parts of the execution need to call AST.exec, but cannot call
           * AST.exec without a cyclic dependency. This function enables that, and it
           * is safe to do so because all of the state is in the exec_state
           * structure. *)
    ; load_fn_result : 'expr_type load_fn_result_type
    ; store_fn_result : 'expr_type store_fn_result_type
    ; load_fn_arguments : 'expr_type load_fn_arguments_type
    ; store_fn_arguments : 'expr_type store_fn_arguments_type
    ; executing_fnname : string
    ; fail_fn : 'expr_type fail_fn_type }

  and 'expr_type funcimpl =
    | InProcess of
        ('expr_type exec_state * 'expr_type dval list -> 'expr_type dval)
    | API of ('expr_type dval_map -> 'expr_type dval)
    | UserCreated of (tlid * 'expr_type)
    | PackageFunction of 'expr_type

  (* TODO: merge fn and user_fn *)
  and 'expr_type fn =
    { prefix_names : string list
    ; infix_names : string list
    ; parameters : param list
    ; return_type : tipe
    ; description : string
    ; func : 'expr_type funcimpl
    ; preview_safety : fn_preview_safety
    ; deprecated : bool }

  (* We need equal_ and show_ for Canvas.canvas to derive *)
  let equal_fn _ (a : 'expr_type fn) (b : 'expr_type fn) =
    a.prefix_names = b.prefix_names


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


  let user_fn_to_fn (uf : 'expr_type user_fn) : 'expr_type fn option =
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
        ; preview_safety = Unsafe
        ; func = UserCreated (uf.tlid, uf.ast)
        ; deprecated = false }
        |> Some
    | _ ->
        None
end
