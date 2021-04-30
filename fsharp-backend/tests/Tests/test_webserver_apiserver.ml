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
