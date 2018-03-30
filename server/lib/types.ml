open Core

type pos = { x:int; y:int }[@@deriving eq, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, show, yojson, sexp, bin_io]
type id = int [@@deriving eq, show, yojson, sexp, bin_io]

type 'a or_blank = Blank of id
                 | Filled of id * 'a
                 | Flagged of id * (string or_blank) * int * ('a or_blank) * ('a or_blank)
                 [@@deriving eq, show, yojson, sexp]


let bin_shape_or_blank =
  let _group =
    Bin_prot.Shape.group
      (Bin_prot.Shape.Location.of_string "lib/types.ml:8:0")
      [((Bin_prot.Shape.Tid.of_string "or_blank"),
         [Bin_prot.Shape.Vid.of_string "a"],
         (Bin_prot.Shape.variant
            [("Blank", [bin_shape_id]);
            ("Filled",
              [bin_shape_id;
              Bin_prot.Shape.var
                (Bin_prot.Shape.Location.of_string "lib/types.ml:9:34")
                (Bin_prot.Shape.Vid.of_string "a")]);
            ("Flagged",
              [bin_shape_id;
              (Bin_prot.Shape.rec_app
                 (Bin_prot.Shape.Tid.of_string "or_blank"))
                [bin_shape_string];
              bin_shape_int;
              (Bin_prot.Shape.rec_app
                 (Bin_prot.Shape.Tid.of_string "or_blank"))
                [Bin_prot.Shape.var
                   (Bin_prot.Shape.Location.of_string "lib/types.ml:10:62")
                   (Bin_prot.Shape.Vid.of_string "a")];
              (Bin_prot.Shape.rec_app
                 (Bin_prot.Shape.Tid.of_string "or_blank"))
                [Bin_prot.Shape.var
                   (Bin_prot.Shape.Location.of_string "lib/types.ml:10:78")
                   (Bin_prot.Shape.Vid.of_string "a")]])]))] in
  fun a ->
    (Bin_prot.Shape.top_app _group (Bin_prot.Shape.Tid.of_string "or_blank"))
      [a]
let _ = bin_shape_or_blank
let rec bin_size_or_blank : 'a. 'a Bin_prot.Size.sizer -> 'a or_blank -> int =
  fun _size_of_a ->
  function
  | Blank v1 -> let size = 1 in Pervasives.(+) size (bin_size_id v1)
  | Filled (v1, v2) ->
      let size = 1 in
      let size = Pervasives.(+) size (bin_size_id v1) in
      Pervasives.(+) size (_size_of_a v2)
  | Flagged (v1, v2, v3, v4, v5) ->
      let size = 1 in
      let size = Pervasives.(+) size (bin_size_id v1) in
      let size = Pervasives.(+) size (bin_size_or_blank bin_size_string v2) in
      let size = Pervasives.(+) size (bin_size_int v3) in
      let size = Pervasives.(+) size (bin_size_or_blank _size_of_a v4) in
      Pervasives.(+) size (bin_size_or_blank _size_of_a v5)
let _ = bin_size_or_blank
let rec bin_write_or_blank : 'a. 'a Bin_prot.Write.writer -> _ ->
  pos: _-> 'a or_blank -> _ =
  fun _write_a buf ~pos ->
  function
  | Blank v1 ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 0 in
      bin_write_id buf ~pos v1
  | Filled (v1, v2) ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 1 in
      let pos = bin_write_id buf ~pos v1 in _write_a buf ~pos v2
  | Flagged (v1, v2, v3, v4, v5) ->
      let pos = Bin_prot.Write.bin_write_int_8bit buf ~pos 2 in
      let pos = bin_write_id buf ~pos v1 in
      let pos = (bin_write_or_blank bin_write_string) buf ~pos v2 in
      let pos = bin_write_int buf ~pos v3 in
      let pos = (bin_write_or_blank _write_a) buf ~pos v4 in
      (bin_write_or_blank _write_a) buf ~pos v5
let _ = bin_write_or_blank
let bin_writer_or_blank bin_writer_a =
  {
    Bin_prot.Type_class.size =
      (fun v -> bin_size_or_blank bin_writer_a.Bin_prot.Type_class.size v);
    write =
      (fun v -> bin_write_or_blank bin_writer_a.Bin_prot.Type_class.write v)
  }
let _ = bin_writer_or_blank
let rec __bin_read_or_blank__ _of__a _buf ~pos_ref  _vint =
  Bin_prot.Common.raise_variant_wrong_type "lib/types.ml.or_blank" (!pos_ref)
and bin_read_or_blank : 'a. 'a Bin_prot.Read.reader -> _ -> pos_ref: _
  -> 'a or_blank =
  fun _of__a buf ~pos_ref ->
  match Bin_prot.Read.bin_read_int_8bit buf ~pos_ref with
  | 0 -> let arg_1 = bin_read_id buf ~pos_ref in Blank arg_1
  | 1 ->
      let arg_1 = bin_read_id buf ~pos_ref in
      let arg_2 = _of__a buf ~pos_ref in Filled (arg_1, arg_2)
  | 2 ->
      let arg_1 = bin_read_id buf ~pos_ref in
      let arg_2 = (bin_read_or_blank bin_read_string) buf ~pos_ref in
      let arg_3 = bin_read_int buf ~pos_ref in
      let arg_4 = (bin_read_or_blank _of__a) buf ~pos_ref in
      let arg_5 = (bin_read_or_blank _of__a) buf ~pos_ref in
      Flagged (arg_1, arg_2, arg_3, arg_4, arg_5)
  | _ ->
      Bin_prot.Common.raise_read_error
        (Bin_prot.Common.ReadError.Sum_tag "lib/types.ml.or_blank")
        (!pos_ref)
let _ = __bin_read_or_blank__
and _ = bin_read_or_blank
let bin_reader_or_blank bin_reader_a =
  {
    Bin_prot.Type_class.read =
      (fun buf ->
         fun ~pos_ref ->
           (bin_read_or_blank bin_reader_a.Bin_prot.Type_class.read) buf
             ~pos_ref);
    vtag_read =
      (fun buf ->
         fun ~pos_ref ->
           fun vtag ->
             (__bin_read_or_blank__ bin_reader_a.Bin_prot.Type_class.read)
               buf ~pos_ref vtag)
  }
let _ = bin_reader_or_blank
let bin_or_blank bin_a =
  {
    Bin_prot.Type_class.writer =
      (bin_writer_or_blank bin_a.Bin_prot.Type_class.writer);
    reader = (bin_reader_or_blank bin_a.Bin_prot.Type_class.reader);
    shape = (bin_shape_or_blank bin_a.Bin_prot.Type_class.shape)
  }
let _ = bin_or_blank


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
  [@@deriving eq, show, yojson, sexp, bin_io]


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

  type tipe = tipe_ [@@deriving eq, show, yojson, sexp, bin_io]

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
               } [@@deriving eq, show, yojson, sexp, bin_io]

  type funcimpl = InProcess of (dval list -> dval)
                | API of (dval_map -> dval)
                | UserCreated of expr

  type fn_metadata = { name : string
                     ; parameters : param list
                     ; return_type : tipe
                     ; description : string
                     ; infix : bool
                     } [@@deriving eq, show, yojson, sexp, bin_io]

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

  type user_fn = { tlid: tlid
                 ; metadata : fn_metadata
                 ; ast:  expr
                 } [@@deriving eq, show, yojson, sexp, bin_io]

  let user_fn_to_fn uf =
    { prefix_names = [uf.metadata.name]
    ; infix_names = []
    ; parameters = uf.metadata.parameters
    ; return_type = uf.metadata.return_type
    ; description = uf.metadata.description
    ; previewExecutionSafe = false
    ; preview = None
    ; func = UserCreated uf.ast
    }
end


