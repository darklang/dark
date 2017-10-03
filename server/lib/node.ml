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

module Scope = RT.Scope
type scope = RT.scope

module IdMap = String.Map
type id_map = id IdMap.t

let log = Log.pp ~name:"execution"
let loG = Log.pP ~name:"execution"

(* For serializing to json only *)
type valuejson = { value: string
                 ; tipe: string [@key "type"]
                 ; json: string
                 ; exc: Exception.exception_data option
                 } [@@deriving to_yojson, show]
type nodejson = { name: string
                ; id: id
                ; tipe: string [@key "type"]
                ; pos : pos
                ; live: valuejson
                ; cursor: int
                ; parameters: param list
                ; arguments: argument list
                ; anon_id: id option
                ; arg_ids : id list
                } [@@deriving to_yojson, show]
type nodejsonlist = nodejson list [@@deriving to_yojson, show]

(* ------------------------ *)
(* graph defintion *)
(* ------------------------ *)


type 'a gfns_ = { getf : (id -> 'a)
                ; get_children : (id -> 'a list)
                ; get_deepest : (id -> (int * 'a) list)
                }

class virtual node id pos =
  object (self)
    val id : id = id
    val mutable pos : pos = pos
    val mutable cursor : int = 0
    method virtual name : string
    method virtual tipe : string
    method virtual execute : ?ind:int -> ?cursor:int -> scope:scope -> node gfns_ -> RT.execute_t
    method id = id
    method debug_name : string = "(" ^ string_of_int id ^ ") " ^ self#name
    method pos = pos
    method is_page = false
    method is_datasink = false
    method is_datasource = false
    method parameters : param list = []
    method has_parameter (paramname : string) : bool =
      List.exists ~f:(fun p -> p.name = paramname) self#parameters
    method arguments : arg_map = RT.ArgMap.empty
    method set_arg (name: string) (value: argument) : unit =
      Exception.internal "This node doesn't support set_arg"
    method clear_args : unit =
      Exception.internal "This node doesn't support clear_args"
    method delete_arg (name: string) : unit =
      Exception.internal "This node doesn't support delete_arg"
    method edges : id_map = IdMap.empty
    method dependent_nodes (_:node gfns_) : id list = []
    method anon_id = None
    method arg_ids = []
    method update_pos _pos : unit =
      pos <- _pos
    method cursor = cursor
    method update_cursor (_cursor:int) : unit =
      Exception.internal "This node doesn't support cursor"
    method preview (cursor: int) (gfns: node gfns_) (args: dval_map) : dval list =
      Exception.internal "This node doesn't support preview"
    method to_frontend (value, tipe, json, exc : string * string * string * Exception.exception_data option) : nodejson =
      { name = self#name
      ; id = id
      ; tipe = self#tipe
      ; pos = pos
      ; live = { value = value ; tipe = tipe; json = json; exc=exc }
      ; parameters = self#parameters
      ; arguments = List.map
            ~f:(fun p -> RT.ArgMap.find_exn self#arguments p.name)
            self#parameters
      ; anon_id = self#anon_id
      ; arg_ids = self#arg_ids
      ; cursor = cursor
      }
  end

type gfns = node gfns_

let equal_node (a:node) (b:node) =
  a#id = b#id

let show_node (n:node) =
  show_nodejson (n#to_frontend ("test", "test", "test", None))

let debug_id (g:gfns) (id:id) =
  (g.getf id)#debug_name


(* ------------------------- *)
(* Graph traversal and execution *)
(* ------------------------- *)

let rec execute ?(ind:int=0) ?(cursor=0) (id: id) ~scope:scope (g: gfns) : dval =
  match Scope.find scope id with
  | Some v -> v
  | None ->
    let n = g.getf id in
    loG ~ind "Execute" n#debug_name;
    n#arguments
    |> RT.ArgMap.mapi ~f:(fun ~key:(param:string) ~data:(arg:RT.argument) ->
        match arg with
        | RT.AConst dv -> dv
        | RT.AEdge id -> execute ~ind:(ind+1) ~cursor id ~scope g)
    |> n#execute ~ind:(ind+1) ~cursor ~scope:scope g

let rec preview (id: id) (g: gfns) (cursor: int) : dval list =
  let n = g.getf id in
  loG "previewing" n#debug_name;
  n#arguments
  |> RT.ArgMap.mapi ~f:(fun ~key:(param:string) ~data:(arg:RT.argument) ->
      match arg with
      | RT.AConst dv -> dv
      | RT.AEdge id -> execute ~cursor ~scope:Scope.empty id g)
  |> n#preview cursor g



(* ------------------ *)
(* Nodes that appear in the graph *)
(* ------------------ *)
class value id pos strrep =
  object(self)
    inherit node id pos
    val expr : dval = RT.parse strrep
    method name : string = strrep
    method tipe = "value"
    method execute ?(ind=0) ?(cursor=0) ~scope:scope _ _ =
      loG "execute expr" self#debug_name;
      expr
  end

class virtual has_arguments id pos =
  object (self)
    inherit node id pos
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


class func (id : id) pos (name : string) =
  object (self)
    inherit has_arguments id pos

    val mutable memo : memo_cache = MemoCache.empty
    (* Throw an exception if it doesn't exist *)
    method private fn = (Libs.get_fn_exn name)
    method private argpairs : (RT.param * argument) list =
        List.map ~f:(fun p -> (p, DvalMap.find_exn args p.name)) self#fn.parameters
    method! dependent_nodes (g: gfns) =
      (* If the node has an anonymous function argument, delete it if this node is *)
      (* deleted *)
      self#argpairs
        |> List.filter_map ~f:(fun (p, arg : param * argument) : id option ->
                                 match (p.tipe, arg) with
                                 | (RT.TFun, RT.AEdge id) -> Some id
                                 | _ -> None)
    method! parameters : param list = self#fn.parameters
    method name = self#fn.name
    method execute ?(ind=0) ?(cursor=0) ~scope:scope (g: gfns) (args: dval_map) : dval =
      loG ~ind "exe function" self#debug_name;
      loG ~ind "w/ args" ~f:RT.dvalmap_to_string args;
      if not self#fn.pure
      then
        let result = RT.exe ~ind self#fn args in
        RT.pp ~ind ~name:"execution" ("r: " ^ self#debug_name) result
      else
        if DvalMap.exists args ~f:(fun x -> x = DIncomplete)
        then
          let result = RT.exe ~ind self#fn args in
          RT.pp ~ind ~name:"execution" ("r: " ^ self#debug_name) result
        else
          let com = RT.to_comparable_repr args in
          match MemoCache.find memo com with
            | None -> let x = RT.exe ~ind self#fn args in
                      memo <- MemoCache.add memo com x;
                      RT.pP ~name:"execution" ~ind ("r: " ^ self#debug_name) x;
                      x
            | Some v ->
                RT.pP ~name:"execution" ~ind ("r (cached):" ^ self#debug_name) v;
                v

     (* Get a value to use as the preview for anonfns used by this node *)
    method! preview (cursor: int) (g: gfns) (args: dval_map) : dval list =
      loG "previewing function" name;
      match self#fn.preview with
      (* if there's no value, no point previewing 5 *)
      | None -> Util.list_repeat (List.length self#fn.parameters) RT.DIncomplete
      | Some f -> self#fn.parameters
                     |> List.map ~f:(fun (p: param) -> p.name)
                     |> List.map ~f:(DvalMap.find_exn args)
                     |> fun dvs -> f dvs cursor
    method! is_page = self#name = "Page_page"
    method tipe = if String.is_substring ~substring:"page" self#name
      then self#name
      (* TODO: rename to "call" *)
      else "function"
  end

class datastore id pos table =
  object
    inherit node id pos
    val table : string = table
    method execute ?(ind=0) ?(cursor=0) ~scope:scope _ (_ : dval_map) : dval = DStr "todo datastore execute"
    method name = "DS-" ^ table
    method tipe = "datastore"
  end

(* ----------------------- *)
(* Anonymous functions *)
(* ----------------------- *)

(* Anonymous functions are graphs with built-in arguments. There is no return
 * value, we simply return the last operation (which is a bit haphazardly
 * defined right now)
 *
   They have their own nodes in a separate graph from the parents (TODO:
   they actually don't, they should in theory, but it was easier to implement
   one big node collection). They are used for higher-order functions.

   As we build these functions up, we start with a known number of
   inputs - initially 1 - and a single output.

   TODO: `preview` is sorta confused. I'm unclear what it does.
   *)

let anonexecutor ?(ind=0) ~scope:scope (debugname:string) (rid: id) (argids: id list) (g: gfns) : (dval list -> dval) =
   loG ~ind ("get anonexecutor " ^ debugname ^ " w/ return ") (debug_id g rid);
   loG ~ind "with params: " (List.map ~f:(debug_id g) argids);
  (fun (args : dval list) ->
     loG ~ind ("exe anonexecutor " ^ debugname ^ " w/ return ") (debug_id g rid);
     loG ~ind "with params: " (List.map ~f:(debug_id g) argids);
     let newscope = List.zip_exn argids args |> Scope.of_alist_exn in
     let scope = Scope.merge newscope scope ~f:(fun ~key v ->
       match v with
       | `Left v -> Some v
       | `Right v -> Some v
       | `Both (v1, v2) -> Some v1) in
     loG ~ind "with scope: " scope;
     let result = execute ~ind rid ~scope g in
     RT.pp ~name:"execution" ~ind ("r anonexecutor " ^ debugname ^ " w/ return " ^ (debug_id g rid)) result
  )

class argnode id pos name index nid argids =
  (* argnodes shouldn't actually be called directly since their value is never *)
  (* used. (In computation, their value is fetched from the scope, not from the *)
  (* node). As a result, their value is only used to preview, so they get the value *)
  (* from their parent's value. *)
  object(self)
    inherit node id pos
    method! dependent_nodes _ = [nid]
    method name = name
    method tipe = "arg"
    method execute ?(ind=0) ?(cursor=0) ~scope:scope (g: gfns) args : dval =
      loG ~ind ("argnode" ^ self#debug_name ^ " called with ") ~f:RT.dvalmap_to_string args;
      match g.get_children nid with
      | [] -> DIncomplete
      | [caller] ->
        let anon_node = g.getf nid in
        let preview_result = preview caller#id g anon_node#cursor in
        (match List.nth preview_result index with
        | Some element -> element
        | None -> DIncomplete)
      | _ -> failwith "more than 1"
    method! anon_id = Some nid
    method! arg_ids = argids
    method update_cursor (_cursor:int) : unit =
      cursor <- _cursor
  end

class anonfn id pos argids =
  object
    inherit node id pos
    method dependent_nodes (g: gfns) =
      List.append argids (g.get_children id |> List.map ~f:(fun n -> n#id))
    method name = "<anonfn>"
    method execute ?(ind=0) ?(cursor=0) ~scope:scope (g: gfns) (_) : dval =
      let debugname = g.get_children id |> List.hd_exn |> (fun n -> n#debug_name) in
      let return = argids
      |> List.map ~f:(g.get_deepest)
      |> List.concat
      (* The ordering is intensional here. Sort first on largest y, then smallest x *)
      |> List.sort ~cmp:(fun ((d1, n1):int*node) ((d2, n2):int*node) -> compare d1 d2)
      |> List.hd_exn
      |> Tuple.T2.get2 in
      DAnon (id, anonexecutor ~ind ~scope debugname return#id argids g)
    method update_cursor (_cursor:int) : unit =
      cursor <- _cursor
    method tipe = "definition"
    method! parameters = []
    method! arg_ids = argids
  end


