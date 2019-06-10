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
    (Account.can_edit_canvas ~auth_domain:"rootvc" ~username:"lee") ;
  AT.check
    AT.bool
    "donkey isn't allowed"
    false
    (Account.can_edit_canvas ~auth_domain:"rootvc" ~username:"donkey") ;
  AT.check
    AT.bool
    "only goes one way"
    false
    (Account.can_edit_canvas ~auth_domain:"lee" ~username:"rootvc") ;
  ()


let suite =
  [ ( "Account.authenticate_user works when it should"
    , `Quick
    , t_authenticate_user )
  ; ("Special case accounts work", `Quick, t_special_case_accounts_work) ]
