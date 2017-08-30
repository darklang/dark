open Core
open Types

module RT = Runtime

type dval = RT.dval [@@deriving show, yojson]
type param = RT.param [@@deriving show, yojson]
type argument = RT.argument [@@deriving show, yojson]

module ArgMap = RT.ArgMap
type arg_map = RT.arg_map

module DvalMap = RT.DvalMap
type dval_map = RT.dval_map

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


(* ------------------------ *)
(* graph defintion *)
(* ------------------------ *)

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
    method arguments : arg_map = RT.ArgMap.empty
    method set_arg (name: string) (value: argument) : unit =
      Exception.raise "This node doesn't support set_arg"
    method clear_args : unit =
      Exception.raise "This node doesn't support clear_args"
    method delete_arg (name: string) : unit =
      Exception.raise "This node doesn't support delete_arg"
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

let equal_node (a:node) (b:node) =
  a#id = b#id

module NodeMap = Int.Map
type nodemap = node NodeMap.t [@@deriving eq]


let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test", "test"))

let pp_nodemap nm =
  let to_s ~key ~data = (show_id key) ^ ": " ^ (show_node data) in
  let objs = NodeMap.mapi ~f:to_s nm in
  "{"
  ^ (String.concat ~sep:", " (NodeMap.data objs))
  ^ "}"

type fndef =
  { nodes : nodemap [@printer fun fmt nm -> fprintf fmt "%s" (pp_nodemap nm)]
  } [@@deriving eq, show]


let edit_fn (id: id) (def: fndef) (f: (node option -> node option)) : fndef =
  { nodes = NodeMap.change def.nodes id ~f }

(* ------------------ *)
(* Nodes that appear in the graph *)
(* ------------------ *)
class value strrep id loc =
  object
    inherit node id loc
    val expr : dval = RT.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute (_: dval_map) : dval = expr
  end

class virtual has_arguments id loc =
  object (self)
    inherit node id loc
    (* Invariant: args should always be the same size as the parameter
       list *)
    val mutable args : arg_map = RT.ArgMap.empty
    method arguments = args
    method set_arg (name: string) (value: argument) : unit =
      args <- ArgMap.change args name (fun _ -> Some value)
    method clear_args : unit =
      args <- ArgMap.map args (fun _ -> RT.blank_arg)
    method delete_arg (name: string) : unit =
      self#set_arg name RT.blank_arg
  end

class func n id loc =
  object (self)
    inherit has_arguments id loc
    initializer
      args <-
        (Libs.get_fn_exn n).parameters
        |> List.map ~f:(fun (p: param) -> (p.name, RT.AConst DIncomplete))
        |> RT.ArgMap.of_alist_exn

    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn n)
    method parameters : param list = self#fn.parameters
    method name = self#fn.name
    method execute (args : dval_map) : dval =
      RT.exe self#fn args
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      (* TODO: rename to "call" *)
      else "function"
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

(* Anonymous functions are graphs with built-in parameters and a return
   value. They have their own nodes in a separate scope from the
   parents. They are used for higher-order functions.

   As we build these functions up, we start with a known number of
   inputs - initially 1 - and a single output.

   An anonymous function returns the anonymous function when executed.
   It can also be executed via the code passed in the DAnon -- which
   gets run by the calling function -- which runs the actual
   computation.

   This node exists both in the inner graph and the outer graph (might
   need different IDs). In the inner graph, executing the node gets the
   return value *)

let anonexecutor (context: fndef) (id: id) : (dval list -> dval) =
  (fun args ->
     (* get return node *)
     (* execute return node *)
     (* presumably there's a edge to the args *)
     DNull
  )


class anonfn id loc =
  object
    inherit node id loc
    val graph : fndef = { nodes = NodeMap.empty }
    method name = "<anonfn>"
    method execute (_) : dval =
      DAnon (id, anonexecutor graph id)
    method tipe = "definition"
    method! parameters = []
  end
