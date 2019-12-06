open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Ast
open Lwt
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module AT = Alcotest

let t_authenticate_user () =
  AT.check
    AT.bool
    "Account.authenticate_user works for the test user"
    true
    ( Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
    && (not (Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ"))
    && (not (Account.authenticate "test" "no"))
    && not (Account.authenticate "test_unhashed" "no") )


let t_special_case_accounts_work () =
  AT.check
    AT.bool
    "lee is allowed"
    true
    (Authorization.can_edit_canvas ~canvas:"rootvc" ~username:"lee") ;
  AT.check
    AT.bool
    "donkey isn't allowed"
    false
    (Authorization.can_edit_canvas ~canvas:"rootvc" ~username:"donkey") ;
  AT.check
    AT.bool
    "only goes one way"
    false
    (Authorization.can_edit_canvas ~canvas:"lee" ~username:"rootvc") ;
  ()


let t_set_user_access () =
  AT.check
    (AT.list (AT.pair AT.bool AT.bool))
    "Changes with Authorization.set_user_access affect can_view_canvas and can_edit_canvas."
    [(false, false); (true, false); (true, true); (false, false)]
    (let user_id = Option.value_exn (Account.id_of_username "test") in
     let org_id = Option.value_exn (Account.id_of_username "test_admin") in
     let view_and_edit () =
       ( Authorization.can_view_canvas ~canvas:"test_admin" ~username:"test"
       , Authorization.can_edit_canvas ~canvas:"test_admin" ~username:"test" )
     in
     [ (* make sure that there's initially no read or write access *)
       ( Authorization.set_user_access user_id org_id None ;
         view_and_edit () )
     ; (* set Read, see what happens *)
       ( Authorization.set_user_access user_id org_id (Some Authorization.Read) ;
         view_and_edit () )
     ; (* set ReadWrite, see what happens *)
       ( Authorization.set_user_access
           user_id
           org_id
           (Some Authorization.ReadWrite) ;
         view_and_edit () )
     ; (* Revoke permissions, see what happens. *)
       ( Authorization.set_user_access user_id org_id None ;
         view_and_edit () ) ])


let t_permission_ord_instance () =
  AT.check AT.bool "rw > r" true (Authorization.ReadWrite > Authorization.Read) ;
  AT.check AT.bool "r < rw" true (Authorization.Read < Authorization.ReadWrite) ;
  AT.check AT.bool "some r > none" true (Some Authorization.Read > None) ;
  AT.check AT.bool "none <  r" true (None < Some Authorization.Read) ;
  ()


let t_account_validation_works () =
  let check_result = AT.check (AT.result AT.unit AT.string) in
  check_result
    "validate_email"
    (Error "Invalid email 'novalidemail'")
    (Account.Testing.validate_email "novalidemail") ;
  check_result
    "uppercase start"
    (Error "Invalid username 'Upper', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "Upper") ;
  check_result
    "uppercase in middle"
    (Error "Invalid username 'uPPer', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "uPPer") ;
  check_result
    "username too short"
    (Error "Invalid username 'a', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "a") ;
  check_result
    "non ascii"
    (Error "Invalid username 'aaa❤️', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "aaa❤️") ;
  check_result
    "hyphen"
    (Error "Invalid username 'aaa-aaa', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "aaa-aaa") ;
  check_result
    "spaces"
    (Error "Invalid username 'aaa aaa', must match /^[a-z][a-z0-9_]{2,20}$/")
    (Account.Testing.validate_username "aaa aaa") ;
  check_result
    "underscore"
    (Ok ())
    (Account.Testing.validate_username "aaa_aaa") ;
  check_result
    "normal"
    (Ok ())
    (Account.Testing.validate_username "myusername09") ;
  check_result "paul" (Ok ()) (Account.Testing.validate_username "paul") ;
  check_result
    "email"
    (Ok ())
    (Account.Testing.validate_email "me@example.com") ;
  ()


let suite =
  [ ( "Account.authenticate_user works when it should"
    , `Quick
    , t_authenticate_user )
  ; ("Special case accounts work", `Quick, t_special_case_accounts_work)
  ; ("Authorization.set_user_access works", `Quick, t_set_user_access)
  ; ( "The ord instance on 'permission' works."
    , `Quick
    , t_permission_ord_instance )
  ; ("Account validation works", `Quick, t_account_validation_works) ]
