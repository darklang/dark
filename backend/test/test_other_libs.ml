open Core_kernel
open Libexecution
open Types.RuntimeT
open Prelude
open Utils

let t_stdlib_works () =
  check_dval
    "uniqueBy1"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> (Int::divide x 2)))")
    (DList [Dval.dint 1; Dval.dint 3; Dval.dint 4]) ;
  check_dval
    "uniqueBy2"
    (exec_ast "(List::uniqueBy (1 2 3 4) (\\x -> x))")
    (DList [Dval.dint 1; Dval.dint 2; Dval.dint 3; Dval.dint 4]) ;
  check_error_contains
    "base64decode"
    (exec_ast "(String::base64Decode 'random string')")
    "Not a valid base64 string" ;
  check_dval
    "getAt1"
    (exec_ast "(List::getAt (1 2 3 4) 0)")
    (DOption (OptJust (Dval.dint 1))) ;
  check_dval
    "getAt2"
    (exec_ast "(List::getAt (1 2 3 4) 4)")
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
    ( match
        exec_ast "(Option::andThen (Just 8) (\\x -> (Int::divide x 2)))"
      with
    | DError msg ->
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
    "maperror ok"
    (exec_ast "(Result::mapError (Ok 4) (\\x -> (Int::divide x 2)))")
    (DResult (ResOk (Dval.dint 4))) ;
  check_dval
    "maperror error"
    (exec_ast
       "(Result::mapError (Error 'test') (\\x -> (String::append x '-appended')))")
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
  check_dval
    "toOption ok"
    (exec_ast "(Result::toOption (Ok 6))")
    (DOption (OptJust (Dval.dint 6))) ;
  check_dval
    "toOption error"
    (exec_ast "(Result::toOption (Error 'test'))")
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
    | DError msg ->
        Prelude.String.contains
          ~substring:"Expected `f` to return a result"
          msg
    | _ ->
        false )
    true ;
  ()


let t_dict_stdlibs_work () =
  let dstr = Dval.dstr_of_string_exn in
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
       (DvalMap.from_list [("key1", dstr "key1val1"); ("key2", dstr "key2val2")])) ;
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
       "(Dict::filter (obj (key1 'val1') (key2 'val2')) (\\k v -> (== v 'val1')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1")])) ;
  check_dval
    "dict filter keeps key"
    (exec_ast
       "(Dict::filter (obj (key1 'val1') (key2 'val2')) (\\k v -> (== k 'key1')))")
    (DObj (DvalMap.from_list [("key1", dstr "val1")])) ;
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
  let ast =
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
      privatekey
      publickey
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
    (exec_ast ast)


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
  ()


let t_old_functions_deprecated () =
  let counts = ref StrDict.empty in
  List.iter (Core.String.Map.to_alist !Libs.static_fns) ~f:(fun (name, fn) ->
      let key = Str.global_replace (Str.regexp "_v[0-9]+") "" name in
      if not fn.deprecated
      then
        counts :=
          StrDict.update !counts ~key ~f:(fun count ->
              count |> Option.withDefault ~default:0 |> ( + ) 1 |> Some ) ;
      () ) ;
  StrDict.iter !counts ~f:(fun name count ->
      AT.check AT.int (name ^ " only has one undeprecated fn") 1 count ) ;
  ()


(* Lib.static_fns *)

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
  ; ("Functions deprecated correctly", `Quick, t_old_functions_deprecated) ]
