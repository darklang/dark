open Core
open Types

module RT = Runtime

type dval = Runtime.dval [@@deriving show, yojson]
type param = Runtime.param [@@deriving show, yojson]
type argument = Runtime.argument [@@deriving show, yojson]

module ArgMap = Runtime.ArgMap
type arg_map = Runtime.arg_map

module DvalMap = Runtime.DvalMap
type dval_map = Runtime.dval_map

module IdMap = String.Map
type id_map = id IdMap.t

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
                ; parameters: param list
                ; arguments: argument list
                } [@@deriving yojson, show]
type nodejsonlist = nodejson list [@@deriving yojson, show]


class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : dval_map -> dval
    method id = id
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method parameters : param list = []
    method has_parameter (paramname : string) : bool =
      List.exists ~f:(fun p -> p.name = paramname) self#parameters
    method arguments : arg_map = ArgMap.empty
    method set_arg (name: string) (value: argument) : unit =
      Exception.raise "This node doesn't support this operation"
    method clear_args : unit =
      Exception.raise "This node doesn't support this operation"
    method delete_arg (name: string) : unit =
      Exception.raise "This node doesn't support this operation"
    method edges : id_map = IdMap.empty
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
      ; arguments = List.map
            ~f:(fun p -> RT.ArgMap.find_exn self#arguments p.name)
            self#parameters
      }
  end

class value strrep id loc =
  object
    inherit node id loc
    val expr : dval = Runtime.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute (_: dval_map) : dval = expr
  end

class func n id loc =
  object (self)
    inherit node id loc
    val mutable args : arg_map =
      (Libs.get_fn_exn n).parameters
      |> List.map ~f:(fun (p: param) -> (p.name, Runtime.AConst DIncomplete))
      |> Runtime.ArgMap.of_alist_exn

    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn n)
    method name = self#fn.name
    method execute (args : dval_map) : dval =
      Runtime.exe self#fn args
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      else "function"
    method parameters : param list = self#fn.parameters
    method arguments = args
    method set_arg (name: string) (value: argument) : unit =
      args <- ArgMap.change args name (fun _ -> Some value)
    method clear_args : unit =
      args <- ArgMap.map args (fun _ -> Runtime.blank_arg)
    method delete_arg (name: string) : unit =
      self#set_arg name Runtime.blank_arg

  end

class datastore table id loc =
  object
    inherit node id loc
    val table : string = table
    method execute (_ : dval_map) : dval = DStr "todo datastore execute"
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
    method execute (_: dval_map) : dval =
      DAnon (id, executor)
    method tipe = "definition"
    method! parameters = [] (* todo *)
  end

(* the function definition of the anon *)
class anon_inner id loc =
  object
    inherit node id loc
    method name = "<anoninner>"
    method execute (args: dval_map) : dval =
      DvalMap.find_exn args "return"
    method tipe = "definition"
    method! parameters = [Lib.req "return" Runtime.tAny]
  end


let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test", "test"))
