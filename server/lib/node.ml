open Core
open Types

type dval = Runtime.dval
type param_map = Runtime.param_map
module ParamMap = Runtime.ParamMap

(* For serializing to json only *)
type valuejson = { value: string
                 ; tipe: string [@key "type"]
                 } [@@deriving yojson, show]
type nodejson = { name: string
                ; id: id
                ; tipe: string [@key "type"]
                ; x: int
                ; y: int
                ; live: valuejson
                ; parameters: string list
                ; constants: (string option) list
                } [@@deriving yojson, show]
type nodejsonlist = nodejson list [@@deriving yojson, show]


class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : param_map -> dval
    method add_constant (name: string) (value: string) : unit =
      Exception.raise "Cannot add a constant to a generic node"
    method id = id
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method parameters : string list = []
    method has_parameter (param : string) : bool =
      List.mem ~equal:String.equal self#parameters param
    method constants : param_map = ParamMap.empty
    method update_loc _loc : unit =
      loc <- _loc
    method to_frontend ((value, tipe) : string * string) : nodejson =
      { name = self#name
      ; id = id
      ; tipe = self#tipe
      ; x = loc.x
      ; y = loc.y
      ; live = { value = value ; tipe = tipe }
      ; parameters = self#parameters
      ; constants = List.map ~f:(fun p -> p
                                          |> ParamMap.find self#constants
                                          |> Option.map ~f:Runtime.to_repr)
            self#parameters
      }
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
    val mutable constants : param_map = ParamMap.empty
    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn n)
    method name = self#fn.name
    method execute (args : param_map) : dval =
      Runtime.exe self#fn args
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      else "function"
    method parameters : string list = self#fn.parameters
    method constants = constants
    method add_constant (name: string) (value: string) =
      constants <- ParamMap.add constants ~key:name ~data:(Runtime.parse value)

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

let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test"))
