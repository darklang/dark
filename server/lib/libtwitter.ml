open Core

open Lib

open Types.RuntimeT
open Runtime

let schema = Swagger.parse "lib/twitter_api.json"

let call_twitter path (args: dval_map) : dval =
  Twitter.get path args

let sw_type2dark tipe =
  match tipe with
  | "string" -> TStr
  | "int" -> TInt
  | _ -> failwith ("todo: type: " ^ tipe)

let twurl2name (url: string) : string =
  (* /1.1/{NAME}.json *)
  url
  |> String.substr_replace_first ~pattern:"/" ~with_:""
  |> String.substr_replace_first ~pattern:"1.1/" ~with_:""
  |> String.substr_replace_first ~pattern:".json" ~with_:""



let param2param (sw: Swagger.parameter) : param =
  { name = sw.name
  ; optional = not sw.required
  ; block_args = []
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
            ; r = TAny
            ; f = Runtime.API (call_twitter api.path)
            ; p = List.map ~f:param2param get.parameters
            ; d = Option.value ~default:"" get.summary
            ; pr = None
            ; pu = true
            }))
  |> List.filter_map ~f:ident
