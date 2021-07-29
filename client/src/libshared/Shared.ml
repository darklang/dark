type id = UnsharedTypes.id
[@@ppx.deriving show {with_path = false}, eq, ord, yojson {optional = true}]

type analysisID = id
[@@ppx.deriving show {with_path = false}, eq, ord, yojson {optional = true}]

let gid = Unshared.gid

module Recover = struct
  (* We never want to crash the app. Instead, send a rollbar notification of the invalid state and try to continue. *)
  let recover
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : string)
      (recoveryVal : 'r) : 'r =
    if sendToRollbar then Unshared.reportError ("Recover: " ^ msg) debug ;
    recoveryVal


  let recoverOpt
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : 'msg)
      ~(default : 'r)
      (x : 'r option) : 'r =
    match x with
    | Some y ->
        y
    | None ->
        recover
          ~sendToRollbar
          ~debug
          ("Got None but expected something: " ^ msg)
          default


  (* Like recoverOpt, but for when you want to return 'r option instead of
   * specifying a default *)
  let recoverOption
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : 'msg)
      (x : 'r option) : 'r option =
    match x with
    | Some y ->
        Some y
    | None ->
        recover
          ~sendToRollbar
          ~debug
          ("Got None but expected something: " ^ msg)
          None


  (* Assert `cond`, returning val either way.  All assertion functions report
   * to rollbar if they fail. *)
  let assert_
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : string)
      (cond : bool)
      (returnVal : 'r) : 'r =
    if cond
    then returnVal
    else recover ("Assertion failure: " ^ msg) ~sendToRollbar ~debug returnVal


  (* Assert `cond` as a statement.  All assertion functions report to rollbar
   * if they fail. *)
  let asserT
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : 'msg)
      (cond : bool) : unit =
    assert_ ~sendToRollbar ~debug msg cond ()


  (* Assert that `f a` returns true, passing the value back as a result. All
   * assertion functions report to rollbar if they fail. *)
  let assertFn
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : string)
      ~(f : 'r -> bool)
      (returnVal : 'r) : 'r =
    assert_ ~sendToRollbar ~debug msg (f returnVal) returnVal


  (* Assert that `f a` returns true, as a statement. All assertion functions
   * report to rollbar if they fail. *)
  let asserTFn
      ?(sendToRollbar : bool = true)
      ?(debug : 'd option)
      (msg : string)
      ~(f : 'a -> bool) : unit =
    assertFn ~f ~sendToRollbar ~debug msg ()


  (* Like recover but with the message TODO *)
  let todo ?(sendToRollbar : bool = true) (msg : string) (recoveryVal : 'b) : 'b
      =
    recover ~sendToRollbar ~debug:recoveryVal ("TODO: " ^ msg) recoveryVal
end
