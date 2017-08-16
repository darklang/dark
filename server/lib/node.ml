open Core
open Types

type dval = Runtime.dval
type arg_map = Runtime.arg_map
type param = Runtime.param
module ArgMap = Runtime.ArgMap

(* For serializing to json only *)
type valuejson = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 } [@@deriving yojson, show]
type nodejson = { name: string
                ; id: id
                ; tipe: string [@key "type"]
                ; x: int
                ; y: int
                ; live: valuejson
                ; parameters: Runtime.param list
                ; constants: (string option) list
                } [@@deriving yojson, show]
type nodejsonlist = nodejson list [@@deriving yojson, show]


class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : arg_map -> dval
    method add_constant (name: string) (value: string) : unit =
      Exception.raise "Cannot add a constant to a generic node"
    method id = id
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method parameters : param list = []
    method has_parameter (paramname : string) : bool =
      List.exists ~f:(fun p -> p.name = paramname) self#parameters
    method constants : arg_map = ArgMap.empty
    method update_loc _loc : unit =
      loc <- _loc
    method to_frontend (value, tipe, json) : nodejson =
      { name = self#name
      ; id = id
      ; tipe = self#tipe
      ; x = loc.x
      ; y = loc.y
      ; live = { value = value ; tipe = tipe; json = json }
      ; parameters = self#parameters
      ; constants = List.map ~f:(fun p -> p.name
                                          |> ArgMap.find self#constants
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
    method execute (_: arg_map) : dval = expr
  end

class func n id loc =
  object (self)
    inherit node id loc
    val mutable constants : arg_map = ArgMap.empty
    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn n)
    method name = self#fn.name
    method execute (args : arg_map) : dval =
      Runtime.exe self#fn args
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      else "function"
    method parameters : param list = self#fn.parameters
    method constants = constants
    method add_constant (name: string) (value: string) =
      constants <- ArgMap.add constants ~key:name ~data:(Runtime.parse value)

  end

class datastore table id loc =
  object
    inherit node id loc
    val table : string = table
    method execute (_ : arg_map) : dval = DStr "todo datastore execute"
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
    method execute (_: arg_map) : dval =
      DAnon (id, executor)
    method tipe = "definition"
    method! parameters = [] (* todo *)
  end

(* the function definition of the anon *)
class anon_inner id loc =
  object
    inherit node id loc
    method name = "<anoninner>"
    method execute (args: arg_map) : dval =
      String.Map.find_exn args "return"
    method tipe = "definition"
    method! parameters = [Lib.req "return" Runtime.tAny]
  end


let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test", "test"))
