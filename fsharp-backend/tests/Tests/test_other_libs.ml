
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



let t_libbytes () =
  check_dval
    "Length is right"
    (Dval.dint 6)
    (exec_ast (fn "Bytes::length" [fn "String::toBytes" [str "abcdef"]])) ;
  ()


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


let t_url_encode () =
  let open Libshared.FluidShortcuts in
  check_dval
    "percent escaping works"
    (Dval.dstr_of_string_exn
       "https%3A%2F%2Fgoogle.com%3Fq%3Dleft%20shark%26l%3Den")
    (exec_ast
       (fn "Twitter::urlencode" [str "https://google.com?q=left shark&l=en"]))


let t_float_stdlibs () =
  let infinity' = binop "/" (float' 1 0) (float' 0 0) in
  let neg_infinity' = binop "/" (float' (-1) 0) (float' 0 0) in
  let nan' = binop "/" (float' 0 0) (float' 0 0) in
  check_dval
    "Float::sum works"
    (DFloat 1.2)
    (exec_ast (fn "Float::sum" [list [float' 1 0; float' 0 2]])) ;
  AT.check
    AT.bool
    "Float::sum fails on list elements that are not floats"
    true
    ( match exec_ast (fn "Float::sum" [list [float' 1 0; int 2]]) with
    | DError _ ->
        true
    | _ ->
        false ) ;
  check_dval
    "Float::ceiling works"
    (Dval.dint 2)
    (exec_ast (fn "Float::ceiling" [float' 1 3])) ;
  check_dval
    "Float::floor works"
    (Dval.dint 1)
    (exec_ast (fn "Float::floor" [float' 1 8])) ;
  check_dval
    "Float::round works"
    (Dval.dint 2)
    (exec_ast (fn "Float::round" [float' 1 5])) ;
  check_dval
    "Float::truncate works"
    (Dval.dint (-2367))
    (exec_ast (fn "Float::truncate" [float' (-2367) 9267])) ;
  check_dval
    "Float::min works (neg)"
    (DFloat (-10.0))
    (exec_ast (fn "Float::min" [float' (-10) 0; float' 1 0])) ;
  check_dval
    "Float::min works (pos)"
    (DFloat 1.0)
    (exec_ast (fn "Float::min" [float' 10 0; float' 1 0])) ;
  check_dval
    "Float::min works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::min" [float' 10 0; nan'])) ;
  check_dval
    "Float::min works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 1.0)
    (exec_ast (fn "Float::min" [infinity'; float' 1 0])) ;
  check_dval
    "Float::max works (neg)"
    (DFloat 1.0)
    (exec_ast (fn "Float::max" [float' (-10) 0; float' 1 0])) ;
  check_dval
    "Float::max works (pos)"
    (DFloat 10.0)
    (exec_ast (fn "Float::max" [float' 10 0; float' 1 0])) ;
  check_dval
    "Float::max works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::max" [float' 10 0; nan'])) ;
  check_dval
    "Float::max works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.infinity)
    (exec_ast (fn "Float::max" [infinity'; float' 1 0])) ;
  check_dval
    "Float::clamp works (in bounds)"
    (DFloat (-2.0))
    (exec_ast (fn "Float::clamp" [float' (-2) 0; float' (-5) 0; float' 5 0])) ;
  check_dval
    "Float::clamp works (below min)"
    (DFloat (-2.0))
    (exec_ast (fn "Float::clamp" [float' (-3) 0; float' (-2) 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (above max)"
    (DFloat 2.0)
    (exec_ast (fn "Float::clamp" [float' 3 0; float' 0 0; float' 2 0])) ;
  check_dval
    "Float::clamp works (limitA = limitB)"
    (DFloat 1.0)
    (exec_ast (fn "Float::clamp" [float' (-5) 0; float' 1 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (limitB > limitA)"
    (DFloat 1.0)
    (exec_ast (fn "Float::clamp" [float' 1 0; float' 2 0; float' 1 0])) ;
  check_dval
    "Float::clamp works (val = infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [infinity'; float' (-1) 0; float' 0 5])) ;
  check_dval
    "Float::clamp works (min = -infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [float' 0 5; neg_infinity'; float' 1 0])) ;
  check_dval
    "Float::clamp works (val = infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat 0.5)
    (exec_ast (fn "Float::clamp" [infinity'; float' (-1) 0; float' 0 5])) ;
  check_dval
    "Float::clamp works (val = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::clamp" [nan'; float' (-1) 0; float' 1 0])) ;
  check_error_contains
    "Float::clamp errors (limitA = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (exec_ast (fn "Float::clamp" [float' 0 5; nan'; float' 1 0]))
    "Internal Float.clamp exception" ;
  check_error_contains
    "Float::clamp errors (limitB = nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (exec_ast (fn "Float::clamp" [float' 0 5; float' 1 0; nan']))
    "Internal Float.clamp exception" ;
  check_dval
    "Float::sqrt works"
    (DFloat 5.0)
    (exec_ast (fn "Float::sqrt" [float' 25 0])) ;
  check_dval
    "Float::power works"
    (DFloat 2.0)
    (exec_ast (fn "Float::power" [float' 4 0; float' 0 5])) ;
  check_dval
    "Float::power works"
    (DFloat 0.5)
    (* Writing -0.5 is currently awkward; this relies on string concatenation: *)
    (exec_ast (fn "Float::power" [float' 4 0; floatStr "-0" "5"])) ;
  check_dval
    "Float::absoluteValue works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::absoluteValue" [nan'])) ;
  check_dval
    "Float::absoluteValue works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.infinity)
    (exec_ast (fn "Float::absoluteValue" [neg_infinity'])) ;
  check_dval
    "Float::absoluteValue works (neg)"
    (DFloat 5.6)
    (exec_ast (fn "Float::absoluteValue" [float' (-5) 6])) ;
  check_dval
    "Float::absoluteValue works (pos)"
    (DFloat 5.6)
    (exec_ast (fn "Float::absoluteValue" [float' (-5) 6])) ;
  check_dval
    "Float::negate works (nan)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.nan)
    (exec_ast (fn "Float::negate" [nan'])) ;
  check_dval
    "Float::negate works (infinity)"
    (* TODO: figure out the nan/infinity situation for floats *)
    (DFloat Float.neg_infinity)
    (exec_ast (fn "Float::negate" [infinity'])) ;
  check_dval
    "Float::negate works (neg)"
    (DFloat 5.6)
    (exec_ast (fn "Float::negate" [float' (-5) 6])) ;
  check_dval
    "Float::negate works (pos)"
    (DFloat (-5.6))
    (exec_ast (fn "Float::negate" [float' 5 6])) ;
  check_dval
    "Float::divide works"
    (DFloat 4.5)
    (exec_ast (fn "Float::divide" [float' 9 0; float' 2 0])) ;
  check_dval
    "Float::add works"
    (DFloat 2.5)
    (exec_ast (fn "Float::add" [float' 1 2; float' 1 3])) ;
  check_dval
    "Float::multiply works"
    (DFloat 13.0)
    (exec_ast (fn "Float::multiply" [float' 26 0; float' 0 5])) ;
  check_dval
    "Float::subtract works"
    (DFloat 0.8)
    (exec_ast (fn "Float::subtract" [float' 1 0; float' 0 2])) ;
  check_dval
    "Float::greaterThan works"
    (DBool true)
    (exec_ast (fn "Float::greaterThan" [float' 0 2; float' 0 1])) ;
  check_dval
    "Float::greaterThanOrEqualTo works"
    (DBool true)
    (exec_ast (fn "Float::greaterThanOrEqualTo" [float' 0 1; float' 0 1])) ;
  check_dval
    "Float::lessThan works"
    (DBool false)
    (exec_ast (fn "Float::lessThan" [float' 0 2; float' 0 1])) ;
  check_dval
    "Float::lessThanOrEqualTo works"
    (DBool true)
    (exec_ast (fn "Float::lessThanOrEqualTo" [float' 0 1; float' 0 1])) ;
  ()


let t_int_stdlibs () =
  let dstr = Dval.dstr_of_string_exn in
  check_dval
    "Int::mod works (sweep, pos)"
    (DList
       [ Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1 ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::mod" [var "v"; int 4]) ])) ;
  check_error_contains
    "Int::mod errors (_, 0)"
    (exec_ast (fn "Int::mod" [int 5; int 0]))
    "Expected the argument `b` argument passed to `Int::mod` to be positive, but it was `0`." ;
  check_error_contains
    "Int::mod errors (_, neg)"
    (exec_ast (fn "Int::mod" [int 5; int (-5)]))
    "Expected the argument `b` argument passed to `Int::mod` to be positive, but it was `-5`." ;
  check_dval
    "% works (sweep, pos)"
    (DList
       [ Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1
       ; Dval.dint 2
       ; Dval.dint 3
       ; Dval.dint 0
       ; Dval.dint 1 ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (binop "%" (var "v") (int 4)) ])) ;
  check_error_contains
    "% errors (_, 0)"
    (exec_ast (binop "%" (int 5) (int 0)))
    "Expected the argument `b` argument passed to `%` to be positive, but it was `0`." ;
  check_error_contains
    "% errors (_, neg)"
    (exec_ast (binop "%" (int 5) (int (-5))))
    "Expected the argument `b` argument passed to `%` to be positive, but it was `-5`." ;
  (*  (* Int::mod_v1 is not yet available; see implementation for why *)
  check_dval
    "Int::mod_v1 works (sweep, pos)"
    (DList
       [ DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::mod_v1" [var "v"; int 4]) ])) ;
  check_dval
    "Int::mod_v1 errors (_, 0)"
    (DResult (ResError (dstr "`modulus` must be positive but was 0")))
    (exec_ast (fn "Int::mod_v1" [int 5; int 0])) ;
  check_dval
    "Int::mod_v1 errors (_, neg)"
    (DResult (ResError (dstr "`modulus` must be positive but was -5")))
    (exec_ast (fn "Int::mod_v1" [int 5; int (-5)])) ; *)
  check_dval
    "Int::remainder works (sweep, pos)"
    (DList
       [ DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint (-3)))
       ; DResult (ResOk (Dval.dint (-2)))
       ; DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::remainder" [var "v"; int 4]) ])) ;
  check_dval
    "Int::remainder works (sweep, neg)"
    (DList
       [ DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint (-3)))
       ; DResult (ResOk (Dval.dint (-2)))
       ; DResult (ResOk (Dval.dint (-1)))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1))
       ; DResult (ResOk (Dval.dint 2))
       ; DResult (ResOk (Dval.dint 3))
       ; DResult (ResOk (Dval.dint 0))
       ; DResult (ResOk (Dval.dint 1)) ])
    (exec_ast
       (fn
          "List::map"
          [ fn "List::range" [int (-5); int 5]
          ; lambda ["v"] (fn "Int::remainder" [var "v"; int (-4)]) ])) ;
  check_dval
    "Int::remainder errors (0)"
    (DResult (ResError (dstr "`divisor` must be non-zero")))
    (exec_ast (fn "Int::remainder" [int 5; int 0])) ;
  check_dval
    "Int::max works"
    (Dval.dint 6)
    (exec_ast (fn "Int::max" [int 5; int 6])) ;
  check_dval
    "Int::min works"
    (exec_ast (fn "Int::min" [int 5; int 6]))
    (Dval.dint 5) ;
  check_dval
    "Int::absoluteValue works (neg)"
    (Dval.dint 5)
    (exec_ast (fn "Int::absoluteValue" [int (-5)])) ;
  check_dval
    "Int::absoluteValue works (pos)"
    (Dval.dint 5)
    (exec_ast (fn "Int::absoluteValue" [int 5])) ;
  check_dval
    "Int::clamp works (in bounds)"
    (Dval.dint (-2))
    (exec_ast (fn "Int::clamp" [int (-2); int (-5); int 5])) ;
  check_dval
    "Int::clamp works (below min)"
    (Dval.dint (-2))
    (exec_ast (fn "Int::clamp" [int (-3); int (-2); int 1])) ;
  check_dval
    "Int::clamp works (above max)"
    (Dval.dint 2)
    (exec_ast (fn "Int::clamp" [int 3; int 0; int 2])) ;
  check_dval
    "Int::clamp works (limitA = limitB)"
    (Dval.dint 1)
    (exec_ast (fn "Int::clamp" [int (-5); int 1; int 1])) ;
  check_dval
    "Int::clamp works (limitB > limitA)"
    (Dval.dint 1)
    (exec_ast (fn "Int::clamp" [int 1; int 2; int 1])) ;
  check_dval
    "Int::negate works (neg)"
    (Dval.dint 5)
    (exec_ast (fn "Int::negate" [int (-5)])) ;
  check_dval
    "Int::negate works (pos)"
    (Dval.dint (-5))
    (exec_ast (fn "Int::negate" [int 5])) ;
  check_dval
    "Int::greaterThan works"
    (DBool true)
    (exec_ast (fn "Int::greaterThan" [int 20; int 1])) ;
  ()


let t_bool_stdlibs () =
  check_dval
    "Bool::and works (true, false)"
    (exec_ast (fn "Bool::and" [bool true; bool false]))
    (DBool false) ;
  check_dval
    "Bool::and works (false, true)"
    (exec_ast (fn "Bool::and" [bool false; bool true]))
    (DBool false) ;
  check_dval
    "Bool::and works (false, false)"
    (exec_ast (fn "Bool::and" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::and works (true, true)"
    (exec_ast (fn "Bool::and" [bool true; bool true]))
    (DBool true) ;
  check_dval
    "Bool::xor works (true, false)"
    (exec_ast (fn "Bool::xor" [bool true; bool false]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, true)"
    (exec_ast (fn "Bool::xor" [bool false; bool true]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, false)"
    (exec_ast (fn "Bool::xor" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::xor works (true, true)"
    (exec_ast (fn "Bool::xor" [bool true; bool true]))
    (DBool false) ;
  check_dval
    "Bool::not works (true)"
    (exec_ast (fn "Bool::not" [bool true]))
    (DBool false) ;
  check_dval
    "Bool::not works (false)"
    (exec_ast (fn "Bool::not" [bool false]))
    (DBool true) ;
  check_dval
    "Bool::or works (true, false)"
    (exec_ast (fn "Bool::or" [bool true; bool false]))
    (DBool true) ;
  check_dval
    "Bool::or works (true, true)"
    (exec_ast (fn "Bool::or" [bool true; bool true]))
    (DBool true) ;
  check_dval
    "Bool::or works (false, false)"
    (exec_ast (fn "Bool::or" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::isNull works (null)"
    (exec_ast (fn "Bool::isNull" [null]))
    (DBool true) ;
  check_dval
    "Bool::isNull works (not null)"
    (exec_ast (fn "Bool::isNull" [bool true]))
    (DBool false) ;
  ()


(* Test does not demonstrate how it'd be used with complex inputs/lambdas (say,
 * comparing two semvers); the goal is simply to demonstrate:
 * - a basic happy-path works
 * - guards for returning non-int or invalid int (not in {-1,0,1}) error *)
let t_liblist_sort_by_comparator_works () =
  let dlist_of_intlist (is : int list) : dval =
    is
    |> List.map ~f:(fun i -> Dint.of_int i |> DInt)
    |> DList
    |> ResOk
    |> DResult
  in
  let listSortByComparator (lambdaBody : Libshared.FluidExpression.t) :
      Libshared.FluidExpression.t =
    fn
      "List::sortByComparator"
      [list [int 3; int 1; int 2]; lambda ["a"; "b"] lambdaBody]
  in
  check_dval
    "List::sortByComparator works, a valid case"
    (exec_ast
       (listSortByComparator
          (if' (fn "Int::lessThan" [var "a"; var "b"]) (int (-1)) (int 1))))
    (dlist_of_intlist [1; 2; 3]) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns non-ints"
    (exec_ast (listSortByComparator (float' 0 1)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned non-int: 0.1"))) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns invalid ints"
    (exec_ast (listSortByComparator (int 3)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned another int: 3"))) ;
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
