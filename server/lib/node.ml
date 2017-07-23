open Core

type loc = { x: int; y: int} [@@deriving eq]
type id = int [@@deriving eq]
type param = string [@@deriving eq]
type dval = Runtime.dval
type param_map = Runtime.param_map

class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : param_map -> dval
    method id = id
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method update_loc _loc =
      loc <- _loc
    method to_frontend : Yojson.Basic.json =
      `Assoc (List.append
                [ ("name", `String self#name)
                ; ("id", `Int id)
                ; ("type", `String self#tipe)
                ; ("x", `Int loc.x)
                ; ("y", `Int loc.y)
                ]
                self#extra_fields)
    method extra_fields = []
    method parameters : string list = []
  end

class value strrep id loc =
  object
    inherit node id loc
    val expr : dval = Runtime.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute (_: param_map) : dval = expr
    method! extra_fields = [("value", `String strrep)]
  end

class func name id loc (strict:bool) =
  object (self)
    inherit node id loc
    (* Throw an exception if it doesn't exist *)
    val name : string = if strict then ignore @@ Lib.get_fn_exn name; name
    method name = name
    method execute (args : param_map) : dval =
      match Lib.get_fn name with
      | Some fn -> Runtime.exe fn args
      | None -> if strict then ignore @@ Lib.get_fn_exn name; DStr ""
    method! is_page = name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" name
      then name
      else "function"
    method! parameters = match Lib.get_fn name with
        | Some fn -> fn.parameters
        | None -> []
    method! extra_fields =
      [("parameters",
        `List (List.map
                 ~f:(fun s -> `String s)
                 self#parameters))]
  end

class anon id loc =
  object
    inherit node id loc
    method name = "<anon>"
    method execute (_: param_map) : dval =
      DStr "todo: execute anon fn"
    method tipe = "definition"
    method! parameters = ["todo"]
  end

class datastore table id loc =
  object
    inherit node id loc
    val table : string = table
    method execute (_ : param_map) : dval = DStr "todo datastore execute"
    method name = "DS-" ^ table
    method tipe = "datastore"
  end

let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (_:node) (_:node) = "<node todo>"
