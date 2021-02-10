
check_dval
  "String::trimStart works (Both Unicode)"
  (* Leading em-space, inner thin space, trailing space *)
  (exec_ast
     (fn
        "String::trimStart"
        [str " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 "]))
  (dstr "foo\xe2\x80\x83bar\xe2\x80\x83 ") ;
check_dval
  "String::trimEnd works (Both Unicode)"
  (* Leading em-space, inner thin space, trailing space *)
  (exec_ast
     (fn "String::trimEnd" [str " \xe2\x80\x83foo\xe2\x80\x83bar\xe2\x80\x83 "]))
  (dstr " \xe2\x80\x83foo\xe2\x80\x83bar") ;
check_dval
  "String::trimStart works (PreservesEmoji)"
  (exec_ast
     (fn "String::trimStart" [str " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 "]))
  (dstr "\xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 ") ;
check_dval
  "String::trimEnd works (PreservesEmoji)"
  (exec_ast
     (fn "String::trimEnd" [str " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84 "]))
  (dstr " \xf0\x9f\x98\x84foobar\xf0\x9f\x98\x84") ;


let t_jwt_functions_work () =
  let privatekey =
    "-----BEGIN RSA PRIVATE KEY-----
MIIEpQIBAAKCAQEAvxW2wuTTK2d0ob5mu/ASJ9vYDc/SXy06QAIepF9x9eoVZZVZ
d8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqLn9/0Ag9ua4ml/ft7COprfEYA7klN
c+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGWCV7+3DF2RvDV2okk3x1ZKyBy2Rw2
uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTwMlHbmVv9QMY5UetA9o05uPaAXH4B
CCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p4Ur22mtma+6ree45gsdnzlj1OASW
DQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ44wIDAQABAoIBAQC+0olj0a3MT5Fa
oNDpZ9JJubLmAB8e6wSbvUIqdiJRKUXa3y2sgNtVjLTzieKfNXhCaHIxUTdH5DWq
p0G7yo+qxbRghlaHz7tTitsQSUGzphjx3YQaewIujQ6EJXbDZZZBsNLqYHfQgbW+
1eV/qGvzyckLzd1G9OUrSv/mS+GrPQ00kpIJIX+EInFOPQ04DheppGNdlxoAUwQQ
XUUhE1LifY4DyyK71mNlUoYyCs+0ozLzbxQwr9n8PKnLKdukL6X0g3tlKEbqQWPv
vz2J8QZeSyhnZM9AjtYdVqTO6qs4l9dyWjdpDRIV9WylasOsIbb8XP8bv2NpH2Ua
6a54L/RJAoGBAPVWwU1jU6e86WrnocJf3miydkhF5VV1tporiuAi391N84zCG509
rWZWa0xsD2tq2+yNDry1qdqMGmvBXKoTJAx3cjpvK/uK7Tkd+tnislDLw8Wq/fCz
NBdSidGIuASXdh4Bo9OK8iYMBgfpUGXRKAs4rO45mwrS/+b0YYZSiX/1AoGBAMdj
amEa5SzXw7tSqtp4Vr4pp4H52YULKI84UKvEDQOROfazQrZMHxbtaSMXG69x7SBr
r48MuRYWd8KZ3iUkYjQLhr4n4zw5DS4AVJqgrLootVWHgt6Ey29Xa1g+B4pZOre5
PJcrxNsG0OjIAEUsTb+yeURSphVjYe+xlXlYD0Z3AoGACdxExKF7WUCEeSF6JN/J
hpe1nU4B259xiVy6piuAp9pcMYoTpgw2jehnQ5kMPZr739QDhZ4fh4MeBLquyL8g
McgTNToGoIOC6UrFLECqPgkSgz1OG4B4VX+hvmQqUTTtMGOMfBIXjWPqUiMUciMn
4tuSR7jU/GhilJu517Y1hIkCgYEAiZ5ypEdd+s+Jx1dNmbEJngM+HJYIrq1+9ytV
ctjEarvoGACugQiVRMvkj1W5xCSMGJ568+9CKJ6lVmnBTD2KkoWKIOGDE+QE1sVf
n8Jatbq3PitkBpX9nAHok2Vs6u6feoOd8HFDVDGmK6Uvmo7zsuZKkP/CpmyMAla9
5p0DHg0CgYEAg0Wwqo3sDFSyKii25/Sffjr6tf1ab+3gFMpahRslkUvyFE/ZweKb
T/YWcgYPzBA6q8LBfGRdh80kveFKRluUERb0PuK+jiHXz42SJ4zEIaToWeK1TQ6I
FW78LEsgtnna+JpWEr+ugcGN/FH8e9PLJDK7Z/HSLPtV8E6V/ls3VDM=
-----END RSA PRIVATE KEY-----"
  in
  let publickey =
    "-----BEGIN PUBLIC KEY-----
MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvxW2wuTTK2d0ob5mu/AS
J9vYDc/SXy06QAIepF9x9eoVZZVZd8ksxvk3JGp/L0+KHuVyXoZFRzE9rU4skIqL
n9/0Ag9ua4ml/ft7COprfEYA7klNc+xp2lwnGsxL70KHyHvHo5tDK1OWT81ivOGW
CV7+3DF2RvDV2okk3x1ZKyBy2Rw2uUjl0EzWLycYQjhRrby3gjVtUVanUgStsgTw
MlHbmVv9QMY5UetA9o05uPaAXH4BCCw+SqhEEJqES4V+Y6WEfFWZTmvWv0GV+i/p
4Ur22mtma+6ree45gsdnzlj1OASWDQx/7vj7Ickt+eTwrVqyRWb9iNZPXj3ZrkJ4
4wIDAQAB
-----END PUBLIC KEY-----"
  in
  let ast_v0 (privkey : string) (pubkey : string) : Libshared.FluidExpression.t
      =
    let'
      "privatekey"
      (str privkey)
      (let'
         "publickey"
         (str pubkey)
         (let'
            "payload"
            (record [("abc", str "def")])
            (let'
               "extraHeaders"
               (record [("ghi", str "jkl")])
               (fn
                  "JWT::verifyAndExtract"
                  [ var "publickey"
                  ; fn
                      "JWT::signAndEncodeWithHeaders"
                      [var "privatekey"; var "extraHeaders"; var "payload"] ]))))
  in
  let ast_v1 (privkey : string) (pubkey : string) : Libshared.FluidExpression.t
      =
    let'
      "privatekey"
      (str privkey)
      (let'
         "publickey"
         (str pubkey)
         (let'
            "payload"
            (record [("abc", str "def")])
            (let'
               "extraHeaders"
               (record [("ghi", str "jkl")])
               (fn
                  "JWT::verifyAndExtract_v1"
                  ~ster:Rail
                  [ var "publickey"
                  ; fn
                      "JWT::signAndEncodeWithHeaders_v1"
                      ~ster:Rail
                      [var "privatekey"; var "extraHeaders"; var "payload"] ]))))
  in
  check_dval
    "JWT::verifyAndExtract works on output of JWT::signAndEncodeWithheaders"
    ( [ ( "payload"
        , DObj (DvalMap.from_list [("abc", Dval.dstr_of_string_exn "def")]) )
      ; ( "header"
        , DObj
            (DvalMap.from_list
               [ ("type", Dval.dstr_of_string_exn "JWT")
               ; ("alg", Dval.dstr_of_string_exn "RS256")
               ; ("ghi", Dval.dstr_of_string_exn "jkl") ]) ) ]
    |> DvalMap.from_list
    |> fun x -> DOption (OptJust (DObj x)) )
    (exec_ast (ast_v0 privatekey publickey)) ;
  check_dval
    "JWT::verifyAndExtract_v1 works on output of JWT::signAndEncodeWithheaders"
    ( [ ( "payload"
        , DObj (DvalMap.from_list [("abc", Dval.dstr_of_string_exn "def")]) )
      ; ( "header"
        , DObj
            (DvalMap.from_list
               [ ("type", Dval.dstr_of_string_exn "JWT")
               ; ("alg", Dval.dstr_of_string_exn "RS256")
               ; ("ghi", Dval.dstr_of_string_exn "jkl") ]) ) ]
    |> DvalMap.from_list
    |> DObj )
    (exec_ast (ast_v1 privatekey publickey)) ;
  check_dval
    "JWT::signAndEncodeWithheaders_v1 gives error for private key"
    (DErrorRail
       (DResult
          (ResError
             (Dval.dstr_of_string_exn "Invalid private key: not an RSA key"))))
    (exec_ast (ast_v1 "invalid private key" publickey)) ;
  check_dval
    "JWT::verifyAndExtract_v1 gives error for pubkey"
    (DErrorRail
       (DResult (ResError (Dval.dstr_of_string_exn "Invalid public key"))))
    (exec_ast (ast_v1 privatekey "invalid public key"))


let t_internal_functions () =
  Libbackend.Account.set_admin "test" true ;
  check_dval
    "We should get an error on failed validation"
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "Invalid username 'Name with space', must match /^[a-z][a-z0-9_]{2,20}$/")))
    (exec_ast
       (fn
          "DarkInternal::upsertUser_v1"
          [ str "Name with space"
          ; str "valid@email.com"
          ; str "accidentalusername" ])) ;
  ()


let t_old_functions_deprecated () =
  let counts = ref StrDict.empty in
  List.iter (Core.String.Map.to_alist !Libs.static_fns) ~f:(fun (name, fn) ->
      let key = Str.global_replace (Str.regexp "_v[0-9]+") "" name in
      if not fn.deprecated
      then
        counts :=
          StrDict.update !counts ~key ~f:(fun count ->
              count |> Option.withDefault ~default:0 |> ( + ) 1 |> Some) ;
      ()) ;
  StrDict.iter !counts ~f:(fun name count ->
      AT.check AT.int (name ^ " only has one undeprecated fn") 1 count) ;
  ()


let t_math_stdlibs () =
  check_dval
    "Math::pi works"
    (DFloat 3.141592653589793)
    (exec_ast (fn "Math::pi" [])) ;
  check_dval
    "Math::tau works"
    (DFloat 6.283185307179586)
    (exec_ast (fn "Math::tau" [])) ;
  check_dval
    "Math::degrees works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::degrees" [float' 360 0])) ;
  check_dval
    "Math::degrees works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast (fn "Math::degrees" [float' (-180) 0])) ;
  check_dval
    "Math::turns works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::turns" [float' 1 0])) ;
  check_dval
    "Math::turns works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast (fn "Math::turns" [fn "Float::negate" [float' 0 5]])) ;
  check_dval
    "Math::radians works (pos)"
    (DFloat 6.283185307179586 (* tau *))
    (exec_ast (fn "Math::radians" [float' 6 283185307179586])) ;
  check_dval
    "Math::radians works (neg)"
    (DFloat (-3.141592653589793) (* -pi *))
    (exec_ast
       (fn "Math::radians" [fn "Float::negate" [float' 3 141592653589793]])) ;
  check_dval
    "Math::cos works (0)"
    (DFloat 1.0)
    (exec_ast (fn "Math::cos" [float' 0 0])) ;
  check_dval
    "Math::cos works (pi)"
    (DFloat (-1.0))
    (exec_ast (fn "Math::cos" [fn "Math::pi" []])) ;
  check_dval
    "Math::sin works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::sin" [float' 0 0])) ;
  check_dval
    "Math::sin works (pi/2)"
    (DFloat 1.0)
    (exec_ast (fn "Math::sin" [fn "/" [fn "Math::pi" []; float' 2 0]])) ;
  check_dval
    "Math::tan works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::tan" [float' 0 0])) ;
  check_dval
    "Math::tan works (pi/4)"
    (DFloat 0.9999999999999999 (* ~1.0 *))
    (exec_ast (fn "Math::tan" [fn "/" [fn "Math::pi" []; float' 4 0]])) ;
  check_dval
    "Math::acos works in range (1)"
    (DOption (OptJust (DFloat 0.0)))
    (exec_ast (fn "Math::acos" [float' 1 0])) ;
  check_dval
    "Math::acos works in range (-1)"
    (DOption (OptJust (DFloat 3.141592653589793 (* pi *))))
    (exec_ast (fn "Math::acos" [float' (-1) 0])) ;
  check_dval
    "Math::acos works (out of range)"
    (DOption OptNothing)
    (exec_ast (fn "Math::acos" [float' 5 0])) ;
  check_dval
    "Math::asin works (in range - 0)"
    (DOption (OptJust (DFloat 0.0)))
    (exec_ast (fn "Math::asin" [float' 0 0])) ;
  check_dval
    "Math::asin works (in range - 1)"
    (DOption (OptJust (DFloat 1.5707963267948966 (* pi/2 *))))
    (exec_ast (fn "Math::asin" [float' 1 0])) ;
  check_dval
    "Math::asin works (out of range)"
    (DOption OptNothing)
    (exec_ast (fn "Math::asin" [float' 5 0])) ;
  check_dval
    "Math::atan works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::atan" [float' 0 0])) ;
  check_dval
    "Math::atan works (1)"
    (DFloat 0.7853981633974483 (* pi/4 *))
    (exec_ast (fn "Math::atan" [float' 1 0])) ;
  check_dval
    "Math::atan2 works (0/1)"
    (DFloat 0.0)
    (exec_ast (fn "Math::atan2" [float' 0 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (1/1)"
    (DFloat 0.7853981633974483 (* pi/4 *))
    (exec_ast (fn "Math::atan2" [float' 1 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (-1/1)"
    (DFloat (-0.7853981633974483) (* -pi/4 *))
    (exec_ast (fn "Math::atan2" [float' (-1) 0; float' 1 0])) ;
  check_dval
    "Math::atan2 works (1/-1)"
    (DFloat 2.356194490192345 (* 3/4 pi *))
    (exec_ast (fn "Math::atan2" [float' 1 0; float' (-1) 0])) ;
  check_dval
    "Math::atan2 works (-1/-1)"
    (DFloat (-2.356194490192345) (* -3/4 pi *))
    (exec_ast (fn "Math::atan2" [float' (-1) 0; float' (-1) 0])) ;
  check_dval
    "Math::cosh works (0)"
    (DFloat 1.0)
    (exec_ast (fn "Math::cosh" [float' 0 0])) ;
  check_dval
    "Math::sinh works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::sinh" [float' 0 0])) ;
  check_dval
    "Math::tanh works (0)"
    (DFloat 0.0)
    (exec_ast (fn "Math::tanh" [float' 0 0])) ;
  ()


let t_libhttp () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "Http::setCookie_v2 works (no params)"
    (DObj (DvalMap.from_list [("Set-Cookie", dstr "myCookie=myVal")]))
    (exec_ast
       (fn "Http::setCookie_v2" [str "myCookie"; str "myVal"; record []])) ;
  check_dval
    "Http::setCookie_v2 works (all params)"
    (DObj
       (DvalMap.from_list
          [ ( "Set-Cookie"
            , dstr
                "myCookie=myVal; Secure; SameSite=Strict; Path=/my/path; Max-Age=3600; HttpOnly; Expires=Sun, 28 Jul 2019 22:42:00 GMT; Domain=darklang.com"
            ) ]))
    (exec_ast
       (match'
          (fn "Date::parse_v1" [str "2019-07-28T22:42:00Z"])
          [ ( pOk (pVar "date")
            , fn
                "Http::setCookie_v2"
                [ str "myCookie"
                ; str "myVal"
                ; record
                    [ ("Expires", var "date")
                    ; ("Max-Age", int 3600)
                    ; ("Domain", str "darklang.com")
                    ; ("Path", str "/my/path")
                    ; ("Secure", bool true)
                    ; ("HttpOnly", bool true)
                    ; ("SameSite", str "Strict") ] ] ) ])) ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong expires type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Expires", int 5)]]))
    "Expected the Set-Cookie parameter `Expires` passed to `Http::setCookie_v2` to be a `Date`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong max-age type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Max-Age", str "foo")]]))
    "Expected the Set-Cookie parameter `Max-Age` passed to `Http::setCookie_v2` to be an `Int` representing seconds, but it had type `String` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong domain type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Domain", int 5)]]))
    "Expected the Set-Cookie parameter `Domain` passed to `Http::setCookie_v2` to be a `String`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong path type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Path", int 5)]]))
    "Expected the Set-Cookie parameter `Path` passed to `Http::setCookie_v2` to be a `String`, but it had type `Int` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong secure type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("Secure", int 5)]]))
    "Expected the Set-Cookie parameter `Secure` passed to `Http::setCookie_v2` to have the value `true` or `false`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong httponly type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("HttpOnly", int 5)]]))
    "Expected the Set-Cookie parameter `HttpOnly` passed to `Http::setCookie_v2` to have the value `true` or `false`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong samesite type)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [str "myCookie"; str "myVal"; record [("SameSite", int 5)]]))
    "Expected the Set-Cookie parameter `SameSite` passed to `Http::setCookie_v2` to have the value `\"Strict\"`, `\"Lax\"`, or `\"None\"`, but it had the value `5` instead." ;
  check_error_contains
    "Http::setCookie_v2 errors (wrong samesite value)"
    (exec_ast
       (fn
          "Http::setCookie_v2"
          [ str "myCookie"
          ; str "myVal"
          ; record [("SameSite", str "allthesecures")] ]))
    "Expected the Set-Cookie parameter `SameSite` passed to `Http::setCookie_v2` to have the value `\"Strict\"`, `\"Lax\"`, or `\"None\"`, but it had the value `\"allthesecures\"` instead." ;
  ()


let t_libhttpclient () =
  check_error_contains
    "HttpClient::get_v5 illegal urls"
    (exec_ast
       (fn
          "HttpClient::get_v5"
          [str "http://thenonexistingurlforsure.com"; record []; record []]))
    "Couldn't resolve host name" ;
  ()


(* This test doesn't bother to destructure + examine the contents of the dobj;
 * it's just intended to ensure that the thing runs and doesn't DError. Esp
 * since the contents of the DObj depend on the database's stats and those
 * aren't stable in the test env! We could do a spot check on the keys present,
 * but I don't think that's terribly useful right now, type guarantees are
 * enough. *)
let t_darkinternal_table_stats_works () =
  let ast = fn "DarkInternal::getAndLogTableSizes" [] in
  AT.check
    AT.bool
    "DarkInternal::table_stats works (no params)"
    true
    (match exec_ast ast with DObj _ -> true | _ -> false) ;
  ()
