open Lwt

module Clu = Cohttp_lwt_unix
module C = Cohttp
module S = Clu.Server
module Request = Clu.Request
module Header = C.Header
module J = Yojson.Basic.Util
module G = Graph

let p s = Printf.printf s;; flush stdout;;

let server =
  let callback _ req req_body =
    let uri = req |> Request.uri in
    (* let meth = req |> Request.meth |> Code.string_of_method in *)
    (* let headers = req |> Request.headers |> Header.to_string in *)
    let auth = req |> Request.headers |> Header.get_authorization in

    let admin_rpc_handler body : string =
      let () = "payload: " ^ body |> print_endline in
      let payload = body |> Yojson.Basic.from_string in

      let g = G.load "blog" in
      let command = J.member "command" payload |> J.to_string in
      let args = J.member "args" payload in

      let str field = J.member field args |> J.to_string in
      let int field = J.member field args |> J.to_int in
      let loc : (unit -> Node.loc) =
        (fun _ : Node.loc -> { x = int "x"; y = int "y" }) in
      let id = Util.create_id in

      let (op : Graph.op option), (cursor : int option) = match command with
        | "load_initial_graph" ->
          None,
          None

        | "add_datastore" ->
          Some (G.Add_datastore (str "name", id, loc ())),
          Some id

        | "add_function_call" ->
          Some (G.Add_fn (str "name", id, loc ())),
          Some id

        | "add_datastore_field" ->
          let (list, tipe) =
            match Core.String.split_on_chars
                    (str "tipe") ~on:['['; ']'] with
            | ["["; s; "]"] -> (true, s)
            | [s] -> (false, s)
            | _ -> failwith "other pattern"
          in
          Some
            (G.Add_datastore_field (int "id", str "name", tipe, list)),
          Some (int "id")

        | "add_value" ->
          Some (G.Add_value (str "value", id, loc ())),
          Some id

        | "update_node_position" ->
          Some (G.Update_node_position (int "id", loc ())),
          None

        | "add_edge" ->
          Some (G.Add_edge (int "src", int "target", str "param")),
          None

        | "delete_node" ->
          Some (G.Delete_node (int "id")),
          None

        | "clear_edges" ->
          Some (G.Clear_edges (int "id")),
          None

        | _ ->
          let _ = failwith "Command not allowed: " ^ command in
          None, None

      in
      let g = match op with
        | Some op -> G.add_op g op
        | None -> g in
      Graph.to_frontend g cursor |> Yojson.Basic.to_string
    in

    let admin_ui_handler () =
      Util.slurp "templates/ui.html"
    in

    let static_handler f : string =
      let l = String.length f in
      let f = String.sub f 1 (l-1) in
      match f with
      | "static/base.css" -> Util.slurp f
      | "static/reset-normalize.css" -> Util.slurp f
      | "static/elm.js" -> Util.slurp f
      | _ -> failwith "File not found"
    in

    let auth_handler handler
      = match auth with
      | (Some `Basic ("dark", "2DqMHguUfsAGCPerWgyHRxPi"))
        -> handler
      | _
        -> Cohttp_lwt_unix.Server.respond_need_auth (`Basic "dark") ()
    in

    let route_handler handler =
      req_body |> Cohttp_lwt_body.to_string >|=
      (fun req_body ->
         match (Uri.path uri) with
         | "/admin/api/rpc" -> admin_rpc_handler req_body
         | "/sitemap.xml" -> ""
         | "/favicon.ico" -> ""
         | "/admin/ui" -> admin_ui_handler ()
         | p when (String.length p) < 8 -> "app routing"
         | p when (String.equal (String.sub p 0 8) "/static/")
           -> static_handler p
         | _ -> "app routing")
      >>= (fun body -> S.respond_string ~status:`OK ~body ())
    in
    ()
    |> route_handler
    |> auth_handler
  in
  S.create ~mode:(`TCP (`Port 8000)) (S.make ~callback ())

let run () = ignore (Lwt_main.run server)
