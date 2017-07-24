open Core
open Types

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
    method! parameters = self#fn.parameters
    method! extra_fields =
      [("parameters",
        `List (List.map
                 ~f:(fun s -> `String s)
                 self#parameters))]
  end

(* the value of the anon *)
class anon id (executor: dval -> dval) loc =
  object
    inherit node id loc
    method name = "<anon>"
    method execute (_: param_map) : dval =
      print_endline "anon func executing";
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
      print_endline "inner func executing";
      String.Map.find_exn args "return"
    method tipe = "definition"
    method! parameters = ["return"]
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
