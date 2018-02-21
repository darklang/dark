open Core

(* This only exists to deserialize old binaries that were saved by
 * Bin_prot. In future, an easier way to do this would be to use
 * darkfile_to_json from the checkout at which the file was written,
 * then tweak it manually or with jq or sed, and then convert it with
 * the current version of json_to_darkfile. *)

type pos = { x:int; y:int }[@@deriving eq, show, yojson, sexp, bin_io]

type tlid = int [@@deriving eq, show, yojson, sexp, bin_io]
type id = int [@@deriving eq, show, yojson, sexp, bin_io]

type 'a or_blank = Blank of id
                 | Filled of id * 'a
                 [@@deriving eq, show, yojson, sexp, bin_io]

type spec = { module_ : string Types.or_blank [@key "module"]
            ; name : string Types.or_blank
            ; modifier : string Types.or_blank
            } [@@deriving eq, show, yojson, sexp, bin_io]

type handler = { tlid: Types.tlid
               ; ast: Ast.ast
               ; spec : spec
               } [@@deriving eq, show, yojson, sexp, bin_io]



type op = NoOp
        | SetHandler of tlid * pos * handler
        | CreateDB of tlid * pos * string
        | AddDBCol of tlid * id * id
        | SetDBColName of tlid * id * string
        | SetDBColType of tlid * id * string
        | DeleteTL of tlid
        | MoveTL of tlid * pos
        | Savepoint
        | DeleteAll
        | Undo
        | Redo
[@@deriving eq, yojson, show, sexp, bin_io]

type oplist = op list [@@deriving eq, yojson, show, sexp, bin_io]

let convert (ops : oplist) : Op.oplist =
  List.map ops
    ~f:(fun (op: op) : Op.op ->
        match op with
        | SetHandler (_, {x;y}, {tlid; ast; spec}) ->
          let {name;modifier;module_} = spec in
          let newspec : Handler.spec =
            { name = name
            ; modifier = modifier
            ; module_ = module_
            ; types = { input = Blank (Util.create_id ())
                      ; output = Blank (Util.create_id ())}} in

          Op.SetHandler (tlid, {x;y}, {tlid; ast; spec = newspec})
        | CreateDB (tlid, {x;y}, string) ->
          Op.CreateDB (tlid, {x;y}, string)
        | AddDBCol (tlid, id1, id2) ->
          Op.AddDBCol (tlid, id1, id2)
        | SetDBColName (tlid, id, string) ->
          Op.SetDBColName (tlid, id, string)
        | SetDBColType (tlid, id, string) ->
          Op.SetDBColType (tlid, id, string)
        | DeleteTL (tlid) ->
          Op.DeleteTL (tlid)
        | MoveTL (tlid, {x;y}) ->
          Op.MoveTL (tlid, {x;y})
        | Savepoint -> Op.Savepoint
        | DeleteAll -> Op.DeleteAll
        | Undo -> Op.Undo
        | Redo -> Op.Redo
        | NoOp -> Op.NoOp)

let load_binary filename : Op.oplist =
  bin_read_oplist
  |> Core_extended.Bin_io_utils.load filename
  |> convert

