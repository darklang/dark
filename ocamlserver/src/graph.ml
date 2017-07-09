type id = int
type loc = { x : int; y : int }
type param = string

type op = Add_fn of string * id * loc
        | Add_datastore of id * loc
        | Add_value of string * id * loc
        | Add_datastore_field of id * loc
        | Update_node_position of id * loc
        | Delete_node of id
        | Add_edge of id * id * param
        | Delete_edge of id * id * param
        | Clear_edges of id

(* graph ops *)
type grops = {
  name : string;
  ops : op list;
} ;

type graph = {
  nodes : (int, int) Hashtbl.t;
  edges : (int, (int * string) list) Hashtbl.t;
}

let create name : grops =
  { name = name
  ; ops = []
  }

let create_graph : graph =
  { nodes = Hashtbl.create 0
  ; edges = Hashtbl.create 0
}

let load name = create name

let to_frontend graph = "";;
