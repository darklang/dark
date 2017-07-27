open Core
open Types

type dval = Runtime.dval
type param_map = Runtime.param_map

(* For serializing to json only *)
type valuejson = { value: string
                 ; tipe: string [@key "type"]
                 } [@@deriving yojson]
type nodejson = { name: string
                 ; id: id
                 ; tipe: string [@key "type"]
                 ; x: int
                 ; y: int
                 ; live: valuejson
                 ; parameters: string list
                 } [@@deriving yojson]


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
    method to_frontend ((value, tipe) : string * string) : Yojson.Safe.json =
      nodejson_to_yojson { name = self#name
                         ; id = id
                         ; tipe = self#tipe
                         ; x = loc.y
                         ; y = loc.y
                         ; live = { value = value ; tipe = tipe }
                         ; parameters = self#parameters
                         }
    method parameters : string list = []
  end

class value strrep id loc =
  object
    inherit node id loc
    val expr : dval = Runtime.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute (_: param_map) : dval = expr
  end

class func n id loc =
  object (self)
    inherit node id loc
    (* Throw an exception if it doesn't exist *)
    method private fn = (Lib.get_fn_exn n)
    method name = self#fn.name
    method execute (args : param_map) : dval =
      Runtime.exe self#fn args
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      else "function"
    method parameters : string list = self#fn.parameters
  end

class datastore table id loc =
  object
    inherit node id loc
    val table : string = table
    method execute (_ : param_map) : dval = DStr "todo datastore execute"
    method name = "DS-" ^ table
    method tipe = "datastore"
  end

(* ----------------------- *)
(* Anonymous functions *)
(* ----------------------- *)

(* the value of the anon *)
class anon id (executor: dval -> dval) loc =
  object
    inherit node id loc
    method name = "<anon>"
    method execute (_: param_map) : dval =
      DAnon (id, executor)
    method tipe = "definition"
    method! parameters = ["todo"]
  end

(* the function definition of the anon *)
class anon_inner id loc =
  object
    inherit node id loc
    method name = "<anoninner>"
    method execute (args: param_map) : dval =
      String.Map.find_exn args "return"
    method tipe = "definition"
    method! parameters = ["return"]
  end


let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (_:node) (_:node) = "<node todo>"
