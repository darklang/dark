open Core_kernel
module Int63 = Prelude.Int63

(* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)

type pos =
  { x : int
  ; y : int }
[@@deriving eq, ord, show, yojson, bin_io]

(* We choose int63 so that we get the same type in jsoo, instead of 31 bit. Our
 * client generated ids which are uint32, so we need to go bigger. *)
type id = Int63.t [@@deriving eq, ord, show, bin_io, yojson]

type tlid = id [@@deriving eq, ord, show, yojson, bin_io]

(* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)
type 'a or_blank =
  | Blank of id
  | Filled of id * 'a
  | Partial of id * string
[@@deriving eq, ord, show {with_path = false}, yojson, bin_io]

type tipe =
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
  | TDbList of tipe
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

module RuntimeT = struct
  (* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)
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

  module DbT = struct
    (* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)
    type col = string or_blank * tipe or_blank
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
  end

  module HandlerT = struct
    (* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)
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
  end

  (* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)

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
    ; tipe : tipe or_blank }
  [@@deriving eq, show, yojson, bin_io]

  type user_tipe_definition = UTRecord of user_record_field list
  [@@deriving eq, show, yojson, bin_io]

  type user_tipe =
    { tlid : tlid
    ; name : string or_blank
    ; version : int
    ; definition : user_tipe_definition }
  [@@deriving eq, show, yojson, bin_io]
end

(* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)

type 'expr_type op =
  | SetHandler of tlid * pos * 'expr_type RuntimeT.HandlerT.handler
  | CreateDB of tlid * pos * string
  | AddDBCol of tlid * id * id
  | SetDBColName of tlid * id * string
  | SetDBColType of tlid * id * string
  | DeleteTL of tlid
  | MoveTL of tlid * pos
  | SetFunction of 'expr_type RuntimeT.user_fn
  | ChangeDBColName of tlid * id * string
  | ChangeDBColType of tlid * id * string
  | UndoTL of tlid
  | RedoTL of tlid
  | DeprecatedInitDbm of tlid * id * id * id * RuntimeT.DbT.migration_kind
  | SetExpr of tlid * id * 'expr_type
  | TLSavepoint of tlid
  | DeleteFunction of tlid
  | CreateDBMigration of
      tlid * id * id * (string or_blank * string or_blank) list
  | AddDBColToDBMigration of tlid * id * id
  | SetDBColNameInDBMigration of tlid * id * string
  | SetDBColTypeInDBMigration of tlid * id * string
  | AbandonDBMigration of tlid
  | DeleteColInDBMigration of tlid * id
  | DeleteDBCol of tlid * id
  | RenameDBname of tlid * string
  | CreateDBWithBlankOr of tlid * pos * id * string
  | DeleteTLForever of tlid
  | DeleteFunctionForever of tlid
  | SetType of RuntimeT.user_tipe
  | DeleteType of tlid
  | DeleteTypeForever of tlid
[@@deriving eq, yojson, show, bin_io]

type 'expr_type oplist = 'expr_type op list
[@@deriving eq, yojson, show, bin_io]

type 'expr_type tlid_oplists = (tlid * 'expr_type oplist) list
[@@deriving eq, yojson, show, bin_io]

(* DO NOT CHANGE ANYTHING IN THIS FILE WITHOUT READING docs/oplist-serialization.md *)
