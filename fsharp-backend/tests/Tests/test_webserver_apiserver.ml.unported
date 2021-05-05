let t_authenticate_then_handle_code_and_cookie () =
  (* basic auth headers *)
  let form_encoded l = Uri.encoded_of_query l in
  let form_encoding =
    Header.init_with "Content-type" "application/x-www-form-urlencoded "
  in
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  (* uri doesn't matter very much since this should be uri-agnostic *)
  (* takes a req, returns the status code and the  parameters for Set-cookie: __session=whatever; [...] *)
  let ath_cookie ((req, body) : Req.t * string) :
      int * (string option * string option) =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt resp, _ =
         Telemetry.with_root "test" (fun span ->
             Webserver.authenticate_then_handle
               ~execution_id:test_id
               span
               (fun ~session ~csrf_token span req ->
                 Webserver.respond ~execution_id:test_id span `OK "test handler")
               req
               body)
       in
       let code = resp |> Resp.status |> Code.code_of_status in
       let redirect =
         resp |> Resp.headers |> fun x -> Header.get x "Location"
       in
       resp
       |> Resp.headers
       |> (fun x -> Header.get x "set-cookie")
       |> (fun x ->
            Option.bind x ~f:(fun sc ->
                let first, params = String.lsplit2_exn ~on:';' sc in
                let name, value = String.lsplit2_exn ~on:'=' first in
                (* make sure some other cookie isn't getting set *)
                if name = "__session" then Some (String.lstrip params) else None))
       |> fun x -> return (code, (x, redirect)))
  in
  AT.check
    (AT.list
       (AT.pair AT.int (AT.pair (AT.option AT.string) (AT.option AT.string))))
    "authenticate_then_handle sets cookies correctly"
    (List.map
       ~f:ath_cookie
       (* valid basic auth login on darklang.com *)
       [ ( Req.make
             ~meth:`POST
             ~headers:form_encoding
             (Uri.of_string "http://darklang.com/login")
         , form_encoded
             [("username", ["test"]); ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"])]
         )
         (* valid basic auth login on localhost *)
       ; ( Req.make
             ~meth:`POST
             ~headers:form_encoding
             (Uri.of_string "http://darklang.localhost/login")
         , form_encoded
             [("username", ["test"]); ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"])]
         )
         (* invalid basic auth logins *)
       ; ( Req.make
             ~headers:form_encoding
             ~meth:`POST
             (Uri.of_string "http://darklang.com/login")
         , form_encoded [("username", ["test"]); ("password", [""])] )
       ; ( Req.make
             ~meth:`POST
             ~headers:form_encoding
             (Uri.of_string "http://darklang.com/login")
         , form_encoded
             [("username", [""]); ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"])] )
         (* plain request, no auth *)
       ; (Req.make (Uri.of_string "http://darklang.localhost/a/test"), "")
         (* login form loads *)
       ; (Req.make (Uri.of_string "http://darklang.localhost/login"), "") ])
    [ ( 302
      , ( Some "Max-Age=604800; domain=darklang.com; path=/; secure; httponly"
        , Some "/a/test" ) )
    ; ( 302
      , ( Some "Max-Age=604800; domain=darklang.localhost; path=/; httponly"
        , Some "/a/test" ) )
    ; (302, (None, Some "/login?error=Invalid%2520username%2520or%2520password"))
    ; (302, (None, Some "/login?error=Invalid%2520username%2520or%2520password"))
    ; ( 302
      , ( None
        , Some "/login?redirect=%252F%252Fdarklang.localhost%252Fa%252Ftest" )
      )
    ; (200, (None, None)) ]


let t_check_csrf_then_handle () =
  (* csrf header *)
  let csrf token = Header.of_list [("X-CSRF-Token", token)] in
  let test_session = Lwt_main.run (Auth.SessionLwt.new_for_username "test") in
  let correct_token = Auth.SessionLwt.csrf_token_for test_session in
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  (* Fake URL; this should be url-agnostic *)
  let url = Uri.of_string "http://darklang.com/a/test" in
  let ccth ((username, req) : string * Req.t) : int =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt resp, _ =
         Telemetry.with_root "test" (fun span ->
             Webserver.check_csrf_then_handle
               ~execution_id:test_id
               ~session:test_session
               span
               (fun span req ->
                 Webserver.respond ~execution_id:test_id span `OK "test handler")
               req)
       in
       resp |> Resp.status |> Code.code_of_status |> return)
  in
  AT.check
    (AT.list AT.int)
    "authenticate_then_handle sets status codes and cookies correctly"
    (List.map
       ~f:ccth
       (* GET works, with no token *)
       [ ("test", Req.make ~meth:`GET url) (* POST works with the right token *)
       ; ("test", Req.make ~headers:(csrf correct_token) ~meth:`POST url)
         (* But not with no token *)
       ; ("test", Req.make ~meth:`POST url) (* And not with the wrong token. *)
       ; ("test", Req.make ~headers:(csrf "x") ~meth:`POST url) ])
    [200; 200; 401; 401]


let admin_handler_code
    ?(meth = `GET) ?(body = "") ?(csrf = true) (username, endpoint) =
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  let session = Lwt_main.run (Auth.SessionLwt.new_for_username username) in
  Lwt_main.run
    (let uri =
       Uri.of_string ("http://builtwithdark.localhost:8000" ^ endpoint)
     in
     let headers =
       Header.of_list
         ( if csrf
         then [("X-CSRF-Token", Auth.SessionLwt.csrf_token_for session)]
         else [] )
     in
     let%lwt () = Nocrypto_entropy_lwt.initialize () in
     let%lwt resp, _ =
       Telemetry.with_root "test" (fun span ->
           Webserver.admin_handler
             ~execution_id:test_id
             ~uri
             ~body
             ~session
             ~csrf_token:(Auth.SessionLwt.csrf_token_for session)
             span
             (Req.make ~meth ~headers uri))
     in
     resp |> Resp.status |> Code.code_of_status |> return)


let t_admin_handler_ui () =
  let ah_ui_response (username, canvas) =
    admin_handler_code (username, "/a/" ^ canvas ^ "/")
  in
  AT.check
    (AT.list AT.int)
    "UI routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_ui_response
       [ ("test", "test") (* everyone can edit sample *)
       ; ("test", "sample") (* a la dabblefox *)
       ; ("test", "test-something")
         (* arbitrary canvas belonging to another user *)
       ; ("test", "test_admin") (* admin can look at test *)
       ; ("test_admin", "test") ])
    [200; 200; 200; 401; 200]


let t_admin_handler_api () =
  let ah_api_response (username, endpoint, body) =
    admin_handler_code ~meth:`POST ~body (username, endpoint)
  in
  AT.check
    (AT.list AT.int)
    "/api/ routes in admin_handler check authorization correctly."
    (List.map
       ~f:ah_api_response
       [ ("test", "/api/test/initial_load", "")
       ; ("test", "/api/test_admin/initial_load", "") ])
    [200; 401]
