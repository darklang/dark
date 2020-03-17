open Core_kernel
open Libexecution
open Types.RuntimeT
open Prelude
open Utils
open Libshared.FluidShortcuts

let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast'
       (fn
          "List::uniqueBy"
          [ list [int 1; int 2; int 3; int 4]
          ; lambda ["x"] (fn "Int::divide" [var "x"; int 2]) ]))
    (DList [Dval.dint 1; Dval.dint 3; Dval.dint 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast'
       (fn
          "List::uniqueBy"
          [list [int 1; int 2; int 3; int 4]; lambda ["x"] (var "x")]))
    (DList [Dval.dint 1; Dval.dint 2; Dval.dint 3; Dval.dint 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast' (fn "String::base64Decode" [str "random string"]))
    "Not a valid base64 string" ;
  check_dval
    "getAt1"
    (exec_ast' (fn "List::getAt" [list [int 1; int 2; int 3; int 4]; int 0]))
    (DOption (OptJust (Dval.dint 1))) ;
  check_dval
    "getAt2"
    (exec_ast' (fn "List::getAt" [list [int 1; int 2; int 3; int 4]; int 4]))
    (DOption OptNothing) ;
  ()


let t_option_stdlibs_work () =
  check_dval
    "map just"
    (exec_ast "(Option::map (Just 4) (\\x -> (Int::divide x 2)))")
    (DOption (OptJust (Dval.dint 2))) ;
  check_dval
    "map nothing"
    (exec_ast "(Option::map (Nothing) (\\x -> (Int::divide x 2)))")
    (DOption OptNothing) ;
  check_dval
    "map  v1 just"
    (exec_ast "(Option::map_v1 (Just 4) (\\x -> (Int::divide x 2)))")
    (DOption (OptJust (Dval.dint 2))) ;
  check_dval
    "map v1 nothing"
    (exec_ast "(Option::map_v1 (Nothing) (\\x -> (Int::divide x 2)))")
    (DOption OptNothing) ;
  check_incomplete
    "map just incomplete"
    (exec_ast "(Option::map_v1 _ (\\x -> (x)))") ;
  check_dval
    "withDefault just"
    (exec_ast "(Option::withDefault (Just 6) 5)")
    (Dval.dint 6) ;
  check_dval
    "withDefault nothing"
    (exec_ast "(Option::withDefault (Nothing) 5)")
    (Dval.dint 5) ;
  check_dval
    "andThen just,nothing"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  check_dval
    "andThen just,just"
    (exec_ast "(Option::andThen (Just 5) (\\x -> (Just (+ 1 x))))")
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "andThen nothing,just"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Just 5)))")
    (DOption OptNothing) ;
  check_dval
    "andThen nothing,nothing"
    (exec_ast "(Option::andThen (Nothing) (\\x -> (Nothing)))")
    (DOption OptNothing) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match exec_ast "(Option::andThen (Just 8) (\\x -> (Int::divide x 2)))" with
    | DError (_, msg) ->
        Prelude.String.contains
          ~substring:"Expected `f` to return an option"
          msg
    | _ ->
        false )
    true ;
  ()


let t_result_stdlibs_work () =
  let test_string = Dval.dstr_of_string_exn "test" in
  check_dval
    "map ok"
    (exec_ast "(Result::map (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 2))) ;
  check_dval
    "map error"
    (exec_ast "(Result::map (Error 'test') (\\x -> (Int::divide x 2)))")
    (DResult (ResError test_string)) ;
  check_dval
    "map v1 ok"
    (exec_ast "(Result::map_v1 (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 2))) ;
  check_dval
    "map v1 error"
    (exec_ast "(Result::map_v1 (Error 'test') (\\x -> (Int::divide x 2)))")
    (DResult (ResError test_string)) ;
  check_dval
    "map v1 incomplete"
    (exec_ast "(Result::map_v1 _ (\\x -> (Int::divide x 2)))")
    (DIncomplete SourceNone) ;
  check_dval
    "maperror ok"
    (exec_ast "(Result::mapError (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "maperror error"
    (exec_ast
       "(Result::mapError (Error 'test') (\\x -> (String::append x '-appended')))")
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "maperror v1 ok"
    (exec_ast "(Result::mapError_v1 (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "maperror v1 error"
    (exec_ast
       "(Result::mapError_v1 (Error 'test') (\\x -> (String::append x '-appended')))")
    (DResult (ResError (Dval.dstr_of_string_exn "test-appended"))) ;
  check_dval
    "withDefault ok"
    (exec_ast "(Result::withDefault (Ok 6) 5)")
    (Dval.dint 6) ;
  check_dval
    "withDefault error"
    (exec_ast "(Result::withDefault (Error 'test') 5)")
    (Dval.dint 5) ;
  check_dval
    "fromOption just"
    (exec_ast "(Result::fromOption (Just 6) 'test')")
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "fromOption nothing"
    (exec_ast "(Result::fromOption (Nothing) 'test')")
    (DResult (ResError test_string)) ;
  check_incomplete
    "fromOption_v1 propogates incomplete"
    (exec_ast "(Result::fromOption_v1 (Just _) 'test')") ;
  check_dval
    "fromOption_v1 propogates error"
    (exec_ast "(Result::fromOption_v1 (Just (Error 'test')) 'test')")
    (DResult (ResOk (DResult (ResError test_string)))) ;
  check_dval
    "toOption ok"
    (exec_ast "(Result::toOption (Ok 6))")
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "toOption error"
    (exec_ast "(Result::toOption (Error 'test'))")
    (DOption OptNothing) ;
  check_incomplete
    "toOption_v1 propogates incomplete"
    (exec_ast "(Result::toOption_v1 (Ok _))") ;
  check_dval
    "toOption_v1 propogates errors"
    (exec_ast "(Result::toOption_v1 (Error 'test'))")
    (DOption OptNothing) ;
  check_dval
    "andThen ok,error"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen ok,ok"
    (exec_ast "(Result::andThen (Ok 5) (\\x -> (Ok (+ 1 x))))")
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "andThen error,ok"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Ok 5)))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen error,error"
    (exec_ast "(Result::andThen (Error 'test') (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen wrong type"
    ( match exec_ast "(Result::andThen (Ok 8) (\\x -> (Int::divide x 2)))" with
    | DError (_, msg) ->
        Prelude.String.contains ~substring:"Expected `f` to return a result" msg
    | _ ->
        false )
    true ;
  check_dval
    "andThen_v1 ok,error"
    (exec_ast "(Result::andThen_v1 (Ok 5) (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen_v1 ok,ok"
    (exec_ast "(Result::andThen_v1 (Ok 5) (\\x -> (Ok (+ 1 x))))")
    (DResult (ResOk (Dval.dint 6))) ;
  check_dval
    "andThen_v1 error,ok"
    (exec_ast "(Result::andThen_v1 (Error 'test') (\\x -> (Ok 5)))")
    (DResult (ResError test_string)) ;
  check_dval
    "andThen_v1 error,error"
    (exec_ast "(Result::andThen_v1 (Error 'test') (\\x -> (Error 'test')))")
    (DResult (ResError test_string)) ;
  AT.check
    AT.bool
    "andThen_v1 wrong type"
    ( match
        exec_ast "(Result::andThen_v1 (Ok 8) (\\x -> (Int::divide x 2)))"
      with
    | DError (_, msg) ->
        Prelude.String.contains ~substring:"Expected `f` to return a result" msg
    | _ ->
        false )
    true ;
  check_condition
    "andThen_v1 propogates errors"
    (exec_ast "(Result::andThen_v1 (Ok 5) (\\x -> (_)))")
    ~f:(fun x -> match x with DError _ -> true | _ -> false) ;
  ()


let t_dict_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
  let dint i = DInt (Dint.of_int i) in
  check_dval
    "dict keys"
    (exec_ast "(Dict::keys (obj (key1 'val1')))")
    (DList [dstr "key1"]) ;
  check_dval
    "dict values"
    (exec_ast "(Dict::values (obj (key1 'val1')))")
    (DList [dstr "val1"]) ;
  check_dval
    "dict get"
    (exec_ast "(Dict::get_v1 (obj (key1 'val1')) 'key1')")
    (DOption (OptJust (dstr "val1"))) ;
  check_dval
    "dict foreach"
    (exec_ast
       "(Dict::foreach (obj (key1 'val1') (key2 'val2')) (\\x -> (++ x '_append')))")
    (DObj
       (DvalMap.from_list
          [("key1", dstr "val1_append"); ("key2", dstr "val2_append")])) ;
  check_dval
    "dict map"
    (exec_ast
       "(Dict::map (obj (key1 'val1') (key2 'val2')) (\\k x -> (++ k x)))")
    (DObj
       (DvalMap.from_list
          [("key1", dstr "key1val1"); ("key2", dstr "key2val2")])) ;
  check_dval "dict empty" (exec_ast "(Dict::empty)") (DObj DvalMap.empty) ;
  check_dval
    "dict merge"
    (exec_ast "(Dict::merge (obj (key1 'val1')) (obj (key2 'val2')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1"); ("key2", dstr "val2")])) ;
  check_dval
    "dict toJSON"
    (exec_ast "(Dict::toJSON (obj (key1 'val1') (key2 'val2')))")
    (dstr "{ \"key1\": \"val1\", \"key2\": \"val2\" }") ;
  check_dval
    "dict filter keeps val"
    (exec_ast
       "(Dict::filter_v1 (obj (key1 'val1') (key2 'val2')) (\\k v -> (== v 'val1')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1")])) ;
  check_dval
    "dict filter keeps key"
    (exec_ast
       "(Dict::filter_v1 (obj (key1 'val1') (key2 'val2')) (\\k v -> (== k 'key1')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1")])) ;
  check_incomplete
    "dict filter propagates incomplete from lambda"
    (exec_ast
       "(Dict::filter_v1 (obj (key1 'val1') (key2 'val2')) (\\k v -> (== k _)))") ;
  check_dval
    "dict filter ignores incomplete from obj"
    (DObj (DvalMap.from_list [("key1", dint 1); ("key3", dint 3)]))
    (exec_ast
       "(Dict::filter_v1 (obj (key1 1) (key2 _) (key3 3)) (\\k v -> (> v 0)))") ;
  ()


let t_password_hashing_and_checking_works () =
  let ast =
    "(let password 'password'
               (Password::check (Password::hash password)
               password))"
  in
  check_dval
    "A `Password::hash'd string `Password::check's against itself."
    (exec_ast ast)
    (DBool true)


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
    |> Core.String.substr_replace_all ~pattern:"\n" ~with_:"\\n"
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
    |> Core.String.substr_replace_all ~pattern:"\n" ~with_:"\\n"
  in
  let ast_v0 (privkey : string) (pubkey : string) : string =
    Printf.sprintf
      "(let privatekey '%s'
                 (let publickey '%s'
                   (let payload (obj (abc 'def'))
                     (let extraHeaders (obj (ghi 'jkl'))
                       (JWT::verifyAndExtract publickey
                         (JWT::signAndEncodeWithHeaders
                           privatekey
                           extraHeaders
                           payload))))))"
      privkey
      pubkey
  in
  let ast_v1 (privkey : string) (pubkey : string) : string =
    Printf.sprintf
      "(let privatekey '%s'
                 (let publickey '%s'
                   (let payload (obj (abc 'def'))
                     (let extraHeaders (obj (ghi 'jkl'))
                       (`JWT::verifyAndExtract_v1 publickey
                         (`JWT::signAndEncodeWithHeaders_v1
                           privatekey
                           extraHeaders
                           payload))))))"
      privkey
      pubkey
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


let t_date_functions_work () =
  check_dval
    "Valid Date::parse_v0 produces a Date"
    (DDate (Util.date_of_isostring "2019-07-28T22:42:00Z"))
    (exec_ast "(Date::parse '2019-07-28T22:42:00Z')") ;
  check_dval
    "Invalid Date::parse_v0 produces an error"
    (DBool true)
    (exec_ast "(Bool::isError (Date::parse 'asd'))") ;
  check_dval
    "Valid Date::parse_v1 produces an Ok Date"
    (DResult (ResOk (DDate (Util.date_of_isostring "2019-07-28T22:42:00Z"))))
    (exec_ast "(Date::parse_v1 '2019-07-28T22:42:00Z')") ;
  check_dval
    "Invalid Date::parse_v1 produces an Error result"
    (DResult (ResError (Dval.dstr_of_string_exn "Invalid date format")))
    (exec_ast "(Date::parse_v1 'asd')") ;
  check_dval
    "Valid Date::parse_v1 roundtrips"
    (DResult (ResOk (Dval.dstr_of_string_exn "2019-07-28T22:42:00Z")))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::toString d)))") ;
  (* Subparts of a date *)
  check_dval
    "Year works"
    (DResult (ResOk (Dval.dint 2019)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::year d)))") ;
  check_dval
    "Month works"
    (DResult (ResOk (Dval.dint 7)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::month d)))") ;
  check_dval
    "Day works"
    (DResult (ResOk (Dval.dint 28)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::day d)))") ;
  check_dval
    "Date::hour works"
    (DResult (ResOk (Dval.dint 22)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::hour d)))") ;
  check_dval
    "Date::minute works"
    (DResult (ResOk (Dval.dint 42)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:00Z') (\\d -> (Date::minute d)))") ;
  check_dval
    "Date::second works"
    (DResult (ResOk (Dval.dint 45)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:45Z') (\\d -> (Date::second d)))") ;
  check_dval
    "Date::toSeconds roundtrips"
    (DResult (ResOk (Dval.dstr_of_string_exn "2019-07-28T22:42:45Z")))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-07-28T22:42:45Z') (\\d -> (toString (Date::fromSeconds (Date::toSeconds d)))))") ;
  check_dval
    "Date::fromSeconds roundtrips"
    (Dval.dint 1095379198)
    (exec_ast "(Date::toSeconds (Date::fromSeconds 1095379198))") ;
  check_dval
    "Date::hour works - leif's test case"
    (DResult (ResOk (Dval.dint 3)))
    (exec_ast
       "(Result::map (Date::parse_v1 '2019-12-27T03:27:36Z') (\\d -> (Date::hour_v1 d)))") ;
  ()


let t_sha256hmac_for_aws () =
  check_dval
    (* These values come from https://docs.aws.amazon.com/general/latest/gr/sigv4-calculate-signature.html*)
    "Crypto::sha256hmac behaves as AWS' example expects (link in test)"
    (Dval.dstr_of_string_exn
       "5d672d79c15b13162d9279b0855cfba6789a8edb4c82c400e06b5924a6f2b5d7")
    (exec_ast
       "(let scope '20150830/us-east-1/iam/aws4_request'
       (let content 'f536975d06c0309214f805bb90ccff089219ecd68b2577efef23edd43b7e1a59'
       (let strs ('AWS4-HMAC-SHA256' '20150830T123600Z' scope content)
       (let strToSign  (String::join strs (String::newline))
       (let secret (String::toBytes 'AWS4wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY')
       (let data (String::toBytes '20150830')
       (let date (Crypto::sha256hmac secret data)
       (let region (Crypto::sha256hmac date (String::toBytes 'us-east-1'))
       (let service (Crypto::sha256hmac region (String::toBytes 'iam'))
       (let signing (Crypto::sha256hmac service (String::toBytes 'aws4_request'))
       (let signed (Crypto::sha256hmac signing (String::toBytes strToSign))
        (String::toLowercase_v1 (Bytes::hexEncode signed))
       ))))))))))) ") ;
  ()


let t_crypto_sha () =
  check_dval
    "Crypto::sha256 produces the correct digest"
    (Dval.dstr_of_string_exn
       "E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855")
    (exec_ast "(Bytes::hexEncode (Crypto::sha256 (String::toBytes '')))") ;
  check_dval
    "Crypto::sha384 produces the correct digest"
    (Dval.dstr_of_string_exn
       "38B060A751AC96384CD9327EB1B1E36A21FDB71114BE07434C0CC7BF63F6E1DA274EDEBFE76F65FBD51AD2F14898B95B")
    (exec_ast "(Bytes::hexEncode (Crypto::sha384 (String::toBytes '')))") ;
  ()


let t_libbytes () =
  check_dval
    "Length is right"
    (Dval.dint 6)
    (exec_ast' (fn "Bytes::length" [fn "String::toBytes" [str "abcdef"]])) ;
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
       "(DarkInternal::upsertUser_v1 'Name with space' 'valid@email.com' 'accidentalusername')") ;
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
    (exec_ast'
       (fn "Twitter::urlencode" [str "https://google.com?q=left shark&l=en"]))


let t_float_stdlibs () =
  check_dval "Float::sum works" (exec_ast "(Float::sum (1.0 0.2))") (DFloat 1.2) ;
  AT.check
    AT.bool
    "Float::sum fails on list elements that are not floats"
    (match exec_ast "(Float::sum (1.0 2))" with DError _ -> true | _ -> false)
    true ;
  check_dval
    "Float::ceiling works"
    (exec_ast "(Float::ceiling 1.3)")
    (Dval.dint 2) ;
  check_dval "Float::floor works" (exec_ast "(Float::floor 1.8)") (Dval.dint 1) ;
  check_dval "Float::round works" (exec_ast "(Float::round 1.5)") (Dval.dint 2) ;
  check_dval
    "Float::truncate works"
    (Dval.dint (-2367))
    (exec_ast' (fn "Float::truncate" [float' (-2367) 9267])) ;
  check_dval "Float::sqrt works" (exec_ast "(Float::sqrt 25.0)") (DFloat 5.0) ;
  check_dval
    "Float::divide works"
    (exec_ast "(Float::divide 9.0 2.0)")
    (DFloat 4.5) ;
  check_dval "Float::add works" (exec_ast "(Float::add 1.2 1.3)") (DFloat 2.5) ;
  check_dval
    "Float::multiply works"
    (exec_ast "(Float::multiply 26.0 0.5)")
    (DFloat 13.0) ;
  check_dval
    "Float::subtract works"
    (exec_ast "(Float::subtract 1.0 0.2)")
    (DFloat 0.8) ;
  check_dval
    "Float::greaterThan works"
    (exec_ast "(Float::greaterThan 0.2 0.1)")
    (DBool true) ;
  check_dval
    "Float::greaterThanOrEqualTo works"
    (exec_ast "(Float::greaterThanOrEqualTo 0.1 0.1)")
    (DBool true) ;
  check_dval
    "Float::lessThan works"
    (exec_ast "(Float::lessThan 0.2 0.1)")
    (DBool false) ;
  check_dval
    "Float::lessThanOrEqualTo works"
    (exec_ast "(Float::lessThanOrEqualTo 0.1 0.1)")
    (DBool true) ;
  ()


let t_int_stdlibs () =
  check_dval
    "Int::max works"
    (exec_ast' (fn "Int::max" [int 5; int 6]))
    (Dval.dint 6) ;
  check_dval
    "Int::min works"
    (exec_ast' (fn "Int::min" [int 5; int 6]))
    (Dval.dint 5) ;
  ()


let t_bool_stdlibs () =
  check_dval
    "Bool::xor works (true, false)"
    (exec_ast' (fn "Bool::xor" [bool true; bool false]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, true)"
    (exec_ast' (fn "Bool::xor" [bool false; bool true]))
    (DBool true) ;
  check_dval
    "Bool::xor works (false, false)"
    (exec_ast' (fn "Bool::xor" [bool false; bool false]))
    (DBool false) ;
  check_dval
    "Bool::xor works (true, true)"
    (exec_ast' (fn "Bool::xor" [bool true; bool true]))
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
    (exec_ast'
       (listSortByComparator
          (if' (fn "Int::lessThan" [var "a"; var "b"]) (int (-1)) (int 1))))
    (dlist_of_intlist [1; 2; 3]) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns non-ints"
    (exec_ast' (listSortByComparator (float' 0 1)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned non-int: 0.1"))) ;
  check_dval
    "List::sortByComparator returns a ResError if lambda returns invalid ints"
    (exec_ast' (listSortByComparator (int 3)))
    (DResult
       (ResError
          (Dval.dstr_of_string_exn
             "`f` must return one of -1, 0, 1, but returned another int: 3"))) ;
  ()


let suite =
  [ ("Stdlib fns work", `Quick, t_stdlib_works)
  ; ("Option stdlibs work", `Quick, t_option_stdlibs_work)
  ; ("Result stdlibs work", `Quick, t_result_stdlibs_work)
  ; ("Dict stdlibs work", `Quick, t_dict_stdlibs_work)
  ; ( "End-user password hashing and checking works"
    , `Quick
    , t_password_hashing_and_checking_works )
  ; ("JWT lib works.", `Quick, t_jwt_functions_work)
  ; ("Date lib works", `Quick, t_date_functions_work)
  ; ("Functions deprecated correctly", `Quick, t_old_functions_deprecated)
  ; ("Internal functions work", `Quick, t_internal_functions)
  ; ("Crypto::sha digest functions work", `Quick, t_crypto_sha)
  ; ("Crypto::sha256hmac works for AWS", `Quick, t_sha256hmac_for_aws)
  ; ("URL percent encoding", `Quick, t_url_encode)
  ; ("Float stdlibs work", `Quick, t_float_stdlibs)
  ; ("Int stdlibs work", `Quick, t_int_stdlibs)
  ; ("Bool stdlibs work", `Quick, t_bool_stdlibs)
  ; ("Bytes stdlibs work", `Quick, t_libbytes)
  ; ("List::sortByComparator works", `Quick, t_liblist_sort_by_comparator_works)
  ]
