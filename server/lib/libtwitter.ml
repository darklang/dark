open Core
open Runtime

open Lib


let schema = Swagger.parse "lib/twitter_api.json"

let call_twitter name (args: dval list) : dval =
  Twitter.get (name ^ ".json") args

let sw_type2dark tipe =
  match tipe with
  | "string" -> tStr
  | "int" -> tInt
  | _ -> failwith ("todo: type: " ^ tipe)

let twurl2name (name: string) : string =
  (* /1.1/{NAME}.json *)
  try
    String.slice name 5 (-5)
  with
  | e -> let _ = print_endline name in
    raise e



let param2param (sw: Swagger.parameter) : param =
  { name = sw.name
  ; optional = not sw.required
  ; tipe = sw.dataType |> sw_type2dark
  ; description = sw.description
  }

let fns =
  schema.apis
  |> List.map
    ~f:(fun (api: Swagger.api) ->
        api.operations
        |> List.filter ~f:(fun (op: Swagger.operation) ->
            op.httpMethod = "GET")
        |> List.hd
        |> Option.map ~f:(fun (get:Swagger.operation) ->
            { n = "Twitter::" ^ (twurl2name api.path)
            ; o = []
            ; r = tAny
            ; f = call_twitter (twurl2name api.path)
            ; p = List.map ~f:param2param get.parameters
            ; d = Option.value ~default:"" get.summary
            }))
  |> List.filter_map ~f:ident
