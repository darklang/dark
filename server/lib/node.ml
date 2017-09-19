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
                ; anon_id: id option
                ; return_id: id option
                ; arg_ids : id list
                } [@@deriving yojson, show]
type nodejsonlist = nodejson list [@@deriving yojson, show]

(* ------------------------ *)
(* graph defintion *)
(* ------------------------ *)

type 'a gfns_ = {
  getf : (id -> 'a) ;
  get_children : (id -> 'a list)
}

class virtual node id loc =
  object (self)
    val id : id = id
    val mutable loc : loc = loc
    method virtual name : string
    method virtual tipe : string
    method virtual execute : node gfns_ -> RT.execute_t
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
    method dependent_nodes : id list = []
    method anon_id = None
    method return_id = None
    method arg_ids = []
    method update_loc _loc : unit =
      loc <- _loc
    method preview (gfns: node gfns_) (args: dval_map) : dval list =
      self#preview gfns args
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
      ; anon_id = self#anon_id
      ; return_id = self#return_id
      ; arg_ids = self#arg_ids
      }
  end

type gfns = node gfns_

let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test", "test"))


(* ------------------------- *)
(* Graph traversal and execution *)
(* ------------------------- *)
module ValCache = Int.Map
type valcache = dval ValCache.t
let rec execute (id: id)
  ?(eager: valcache=ValCache.empty) (g: gfns) : dval =
  match ValCache.find eager id with
  | Some v -> v
  | None ->
    let n = g.getf id in
    n#arguments
    |> RT.ArgMap.mapi ~f:(fun ~key:(param:string) ~data:(arg:RT.argument) ->
        match arg with
        | RT.AConst dv -> dv
        | RT.AEdge id -> execute id ~eager g)
    |> n#execute g

let rec preview (id: id) (g: gfns) : dval list =
  let n = g.getf id in
  n#arguments
  |> RT.ArgMap.mapi ~f:(fun ~key:(param:string) ~data:(arg:RT.argument) ->
      match arg with
      | RT.AConst dv -> dv
      | RT.AEdge id -> execute id g)
  |> n#preview g



(* ------------------ *)
(* Nodes that appear in the graph *)
(* ------------------ *)
class value id loc strrep =
  object
    inherit node id loc
    val expr : dval = RT.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute _ _ = expr
  end

class virtual has_arguments id loc = (*  *)
  object (self)
    inherit node id loc
    val mutable args : arg_map = RT.ArgMap.empty
    (* Invariant: args should always be the same size as the parameter
       list *)
    initializer
      args <-
        self#parameters
        |> List.map ~f:(fun (p: param) -> (p.name, RT.AConst DIncomplete))
        |> RT.ArgMap.of_alist_exn
    method arguments = args
    method set_arg (name: string) (value: argument) : unit =
      args <- ArgMap.change args name (fun _ -> Some value)
    method clear_args : unit =
      args <- ArgMap.map args (fun _ -> RT.blank_arg)
    method delete_arg (name: string) : unit =
      self#set_arg name RT.blank_arg
  end

module MemoCache = String.Map
type memo_cache = dval MemoCache.t

class func id loc n =
  object (self)
    inherit has_arguments id loc

    val mutable memo : memo_cache = MemoCache.empty
    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn n)
    method parameters : param list = self#fn.parameters
    method name = self#fn.name
    method execute (g: gfns) (args: dval_map) : dval =
      if not self#fn.pure
      then RT.exe self#fn args
      else
        if DvalMap.exists args ~f:(fun x -> x = DIncomplete)
        then RT.exe self#fn args
        else
          let com = RT.to_comparable_repr args in
          match MemoCache.find memo com with
            | None -> let x = RT.exe self#fn args in
                      memo <- MemoCache.add memo com x;
                      x
            | Some v -> v

     (* Get a value to use as the preview for anonfns used by this node *)
    method preview (g: gfns) (args: dval_map) : dval list =
      match self#fn.preview with
      | None -> List.init (List.length self#fn.parameters) (fun _ -> RT.DIncomplete)
      | Some f -> self#fn.parameters
                     |> List.map ~f:(fun (p: param) -> p.name)
                     |> List.map ~f:(DvalMap.find_exn args)
                     |> f
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      (* TODO: rename to "call" *)
      else "function"
  end

class datastore id loc table =
  object
    inherit node id loc
    val table : string = table
    method execute _ (_ : dval_map) : dval = DStr "todo datastore execute"
    method name = "DS-" ^ table
    method tipe = "datastore"
  end

(* ----------------------- *)
(* Anonymous functions *)
(* ----------------------- *)

(* Anonymous functions are graphs with built-in parameters and a return
   value. They have their own nodes in a separate scope from the parents (TODO:
   they actually don't, they should in theory, but it was easier to implement
   one big node collection). They are used for higher-order functions.

   As we build these functions up, we start with a known number of
   inputs - initially 1 - and a single output.

   TODO: `preview` is sorta confused. I'm unclear what it does.
   *)

let anonexecutor (rid: id) (argids: id list) (g: gfns) : (dval list -> dval) =
  (fun (args : dval list) ->
     let eager = List.zip_exn argids args |> ValCache.of_alist_exn in
     execute rid ~eager g
  )

class returnnode id loc nid argids =
  object (self)
    inherit has_arguments id loc
    method dependent_nodes = nid :: argids
    method name = "<return>"
    method! parameters = [{ name = "return"
                          ; tipe = RT.tAny
                          ; arity = 0
                          ; optional = false
                          ; description = "" }]
    method tipe = "return"
    method execute (g: gfns) (args) : dval =
      DvalMap.find_exn args "return"
    method! anon_id = Some nid
    method! return_id = Some id
    method! arg_ids = argids
  end

class argnode id loc index nid rid argids =
  object
    inherit node id loc
    method dependent_nodes = nid :: rid :: argids
    method name = "<arg>"
    method tipe = "arg"
    method execute (g: gfns) _ : dval =
      (* This arg gets its preview value from the preview of the node being
       * passed the anon *)
      match g.get_children nid with
      | [] -> DIncomplete
      | [caller] -> List.nth_exn (preview caller#id g) index
      | _ -> failwith "more than 1"
    method! anon_id = Some nid
    method! return_id = Some rid
    method! arg_ids = argids
  end

class anonfn id loc rid argids =
  object
    inherit node id loc
    method dependent_nodes = rid :: argids
    method name = "<anonfn>"
    method execute (g: gfns) (_) : dval =
      DAnon (id, anonexecutor rid argids g)
    method tipe = "definition"
    method! parameters = []
    method! return_id = Some rid
    method! arg_ids = argids
  end
