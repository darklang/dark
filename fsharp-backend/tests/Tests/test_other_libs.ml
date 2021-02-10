
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
