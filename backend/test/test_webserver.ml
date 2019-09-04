open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Lwt
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module AT = Alcotest

let t_should_use_https () =
  AT.check
    (AT.list AT.bool)
    "should_use_https works"
    (List.map
       ~f:(fun x -> Webserver.should_use_https (Uri.of_string x))
       [ "http://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "http://localhost"
       ; "http://test.localhost" ])
    [true; true; false; false]


let t_redirect_to () =
  AT.check
    (AT.list (AT.option AT.string))
    "redirect_to works"
    (List.map
       ~f:(fun x ->
         x
         |> Uri.of_string
         |> Webserver.redirect_to
         |> Option.map ~f:Uri.to_string )
       [ "http://example.com"
       ; "http://builtwithdark.com"
       ; "https://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "https://test.builtwithdark.com"
       ; "http://test.builtwithdark.com/x/y?z=a" ])
    [ None
    ; Some "https://builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com/x/y?z=a" ]


let t_canonicalize_maintains_schemes () =
  (* We don't test the https variants as the request we can't make requests
   * that have them, and they never occur in either dev or prod. *)
  let tests =
    [ ( "//example.com"
      , ("http://example.com/", "http://example.com/", "https://example.com/")
      )
    ; ( "//builtwithdark.com"
      , ( "http://builtwithdark.com/"
        , "http://builtwithdark.com/"
        , "https://builtwithdark.com/" ) )
    ; ( "http://builtwithdark.com"
      , ( "http://builtwithdark.com/"
        , "http://builtwithdark.com/"
        , "https://builtwithdark.com/" ) )
    ; ( "http://test.builtwithdark.com"
      , ( "http://test.builtwithdark.com/"
        , "http://test.builtwithdark.com/"
        , "https://test.builtwithdark.com/" ) )
    ; ( "http://test.builtwithdark.com/x/y?z=a"
      , ( "http://test.builtwithdark.com/x/y?z=a"
        , "http://test.builtwithdark.com/x/y?z=a"
        , "https://test.builtwithdark.com/x/y?z=a" ) ) ]
  in
  tests
  |> List.map ~f:(fun (url, (e1, e2, e3)) ->
         [ (url, Header.init (), e1)
         ; (url, Header.init_with "x-forwarded-proto" "http", e2)
         ; (url, Header.init_with "x-forwarded-proto" "https", e3) ] )
  |> List.concat
  |> List.iter ~f:(fun (url, headers, expected) ->
         AT.check
           AT.string
           (url ^ " " ^ Header.to_string headers)
           expected
           ( Req.make ~meth:`GET ~headers (Uri.of_string url)
           |> Webserver.canonicalize_request
           |> Req.uri
           |> Uri.to_string ) )


let t_bad_ssl_cert _ =
  check_error_contains
    "should get bad_ssl"
    (exec_ast "(HttpClient::get 'https://self-signed.badssl.com' {} {} {})")
    "Bad HTTP request: Peer certificate cannot be authenticated with given CA certificates"


let t_sanitize_uri_path_with_repeated_slashes () =
  AT.check
    AT.string
    "/foo//bar->/foo/bar"
    (Webserver.sanitize_uri_path "/foo//bar")
    "/foo/bar"


let t_head_and_get_requests_are_coalesced () =
  let test_name = "head-and-get-requests-are-coalsced" in
  let setup_canvas () =
    let n1 = hop (http_handler (ast_for "'test_body'")) in
    let canvas = ops2c_exn ("test-" ^ test_name) [n1] in
    Log.infO "canvas account" ~params:[("_", !canvas |> C.show_canvas)] ;
    C.save_all !canvas ;
    canvas
  in
  let respond_to_head_from_get (req : Req.t) : int * (int * string) =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let test_id = Types.id_of_int 1234 in
       let canvas = setup_canvas () in
       let%lwt resp, body =
         Webserver.canvas_handler
           ~execution_id:test_id
           ~canvas:!canvas.host
           ~ip:""
           ~uri:(req |> Req.uri)
           ~body:""
           req
       in
       let code = resp |> Resp.status |> Code.code_of_status in
       let body_string = Cohttp_lwt__.Body.to_string body |> Lwt_main.run in
       resp
       |> Resp.headers
       |> (fun headers ->
            match Header.get headers "Content-Length" with
            | None ->
                0
            | Some h ->
                int_of_string h )
       |> fun content_length -> return (code, (content_length, body_string)))
  in
  let expected_body = "\"test_body\"" in
  let expected_content_length = String.length expected_body in
  AT.check
    (AT.list (AT.pair AT.int (AT.pair AT.int AT.string)))
    "canvas_handler returns same content-length for HEAD and GET requests"
    (List.map
       ~f:respond_to_head_from_get
       (* valid basic auth login on darklang.com *)
       [ Req.make
           ?meth:(Some `GET)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
         (* valid basic auth login on localhost *)
       ; Req.make
           ?meth:(Some `HEAD)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
       ])
    [ (200, (expected_content_length, expected_body))
    ; (200, (expected_content_length, "")) ]


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
         Webserver.authenticate_then_handle
           ~execution_id:test_id
           (fun ~session ~csrf_token req ->
             Webserver.respond ~execution_id:test_id `OK "test handler" )
           req
           body
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
                if name = "__session"
                then Some (String.lstrip params)
                else None ) )
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
             [ ("username", ["test"])
             ; ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"]) ] )
         (* valid basic auth login on localhost *)
       ; ( Req.make
             ~meth:`POST
             ~headers:form_encoding
             (Uri.of_string "http://darklang.localhost/login")
         , form_encoded
             [ ("username", ["test"])
             ; ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"]) ] )
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
             [("username", [""]); ("password", ["fVm2CUePzGKCwoEQQdNJktUQ"])]
         )
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
    ; ( 302
      , (None, Some "/login?error=Invalid%2520username%2520or%2520password") )
    ; ( 302
      , (None, Some "/login?error=Invalid%2520username%2520or%2520password") )
    ; ( 302
      , ( None
        , Some "/login?redirect=%252F%252Fdarklang.localhost%252Fa%252Ftest" )
      )
    ; (200, (None, None)) ]


let t_check_csrf_then_handle () =
  (* csrf header *)
  let csrf token = Header.of_list [("X-CSRF-Token", token)] in
  let test_session = Lwt_main.run (Auth.Session.new_for_username "test") in
  let correct_token = Auth.Session.csrf_token_for test_session in
  (* sample execution id, makes grepping test logs easier *)
  let test_id = Types.id_of_int 1234 in
  (* Fake URL; this should be url-agnostic *)
  let url = Uri.of_string "http://darklang.com/a/test" in
  let ccth ((username, req) : string * Req.t) : int =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let%lwt resp, _ =
         Webserver.check_csrf_then_handle
           ~execution_id:test_id
           ~session:test_session
           (fun req ->
             Webserver.respond ~execution_id:test_id `OK "test handler" )
           req
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
  let session = Lwt_main.run (Auth.Session.new_for_username username) in
  Lwt_main.run
    (let uri =
       Uri.of_string ("http://builtwithdark.localhost:8000" ^ endpoint)
     in
     let headers =
       Header.of_list
         ( if csrf
         then [("X-CSRF-Token", Auth.Session.csrf_token_for session)]
         else [] )
     in
     let%lwt () = Nocrypto_entropy_lwt.initialize () in
     let%lwt resp, _ =
       Webserver.admin_handler
         ~execution_id:test_id
         ~uri
         ~body
         ~session
         ~csrf_token:(Auth.Session.csrf_token_for session)
         (Req.make ~meth ~headers uri)
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


let t_head_and_get_requests_are_coalesced () =
  let test_name = "head-and-get-requests-are-coalsced" in
  let setup_canvas () =
    let n1 = hop (http_handler (ast_for "'test_body'")) in
    let canvas = ops2c_exn ("test-" ^ test_name) [n1] in
    Log.infO "canvas account" ~params:[("_", !canvas |> C.show_canvas)] ;
    C.save_all !canvas ;
    canvas
  in
  let respond_to_head_from_get (req : Req.t) : int * (int * string) =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let test_id = Types.id_of_int 1234 in
       let canvas = setup_canvas () in
       let%lwt resp, body =
         Webserver.canvas_handler
           ~execution_id:test_id
           ~canvas:!canvas.host
           ~ip:""
           ~uri:(req |> Req.uri)
           ~body:""
           req
       in
       let code = resp |> Resp.status |> Code.code_of_status in
       let body_string = Cohttp_lwt__.Body.to_string body |> Lwt_main.run in
       resp
       |> Resp.headers
       |> (fun headers ->
            match Header.get headers "Content-Length" with
            | None ->
                0
            | Some h ->
                int_of_string h )
       |> fun content_length -> return (code, (content_length, body_string)))
  in
  let expected_body = "\"test_body\"" in
  let expected_content_length = String.length expected_body in
  AT.check
    (AT.list (AT.pair AT.int (AT.pair AT.int AT.string)))
    "canvas_handler returns same content-length for HEAD and GET requests"
    (List.map
       ~f:respond_to_head_from_get
       (* valid basic auth login on darklang.com *)
       [ Req.make
           ?meth:(Some `GET)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
         (* valid basic auth login on localhost *)
       ; Req.make
           ?meth:(Some `HEAD)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
       ])
    [ (200, (expected_content_length, expected_body))
    ; (200, (expected_content_length, "")) ]


let t_http_request_redirects () =
  let setup_canvas () =
    let n1 = hop (http_handler (ast_for "'test_body'")) in
    let canvas = ops2c_exn "test" [n1] in
    Log.infO "canvas account" ~params:[("_", !canvas |> C.show_canvas)] ;
    C.save_all !canvas ;
    canvas
  in
  let respond (req : Req.t) : int =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let test_id = Types.id_of_int 1234 in
       ignore (setup_canvas ()) ;
       let%lwt resp, body =
         Webserver.callback
           ~k8s_callback:(fun _ ~execution_id ->
             Cohttp_lwt_unix.Server.respond_string
               ~status:(Cohttp.Code.status_of_code 911)
               ~body:""
               () )
           ""
           req
           ""
           test_id
       in
       resp |> Resp.status |> Code.code_of_status |> return)
  in
  AT.check
    AT.int
    "http requests redirect"
    302
    (respond
       (Req.make
          ?meth:(Some `GET)
          (Uri.of_string "http://test.builtwithdark.com/test")))


let suite =
  [ ("Webserver.should_use_https works", `Quick, t_should_use_https)
  ; ("Webserver.redirect_to works", `Quick, t_redirect_to) (* errorrail *)
  ; ("bad ssl cert", `Slow, t_bad_ssl_cert)
  ; ( "authenticate_then_handle sets status codes and cookies correctly "
    , `Quick
    , t_authenticate_then_handle_code_and_cookie )
  ; ( "check_csrf_then_handle checks CSRF authentication correctly  "
    , `Quick
    , t_check_csrf_then_handle )
  ; ("UI routes in admin_handler work ", `Quick, t_admin_handler_ui)
  ; ("/api/ routes in admin_handler work ", `Quick, t_admin_handler_api)
  ; ( "head and get requests are coalsced"
    , `Quick
    , t_head_and_get_requests_are_coalesced )
  ; ("canonicalizing requests works", `Quick, t_canonicalize_maintains_schemes)
  ; ("http requests redirect", `Quick, t_http_request_redirects) ]
