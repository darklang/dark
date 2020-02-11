type id = UnsharedTypes.id [@@deriving show {with_path = false}, eq]

type analysisID = id [@@deriving show {with_path = false}, eq]

let gid = Unshared.gid

module Recover = struct
  (* We never want to crash the app. Instead, send a rollbar notification of the invalid state and try to continue. *)
  let recover ?(debug : 'd option) (msg : string) (recoveryVal : 'r) : 'r =
    Unshared.reportError ("Recover: " ^ msg) debug ;
    recoveryVal


  let recoverOpt
      ?(debug : 'd option) (msg : 'msg) ~(default : 'r) (x : 'r option) : 'r =
    match x with
    | Some y ->
        y
    | None ->
        recover ~debug ("Got None but expected something: " ^ msg) default


  (* Like recoverOpt, but for when you want to return 'r option instead of
   * specifying a default *)
  let recoverOption ?(debug : 'd option) (msg : 'msg) (x : 'r option) :
      'r option =
    match x with
    | Some y ->
        Some y
    | None ->
        recover ~debug ("Got None but expected something: " ^ msg) None


  (* Assert `cond`, returning val either way.  All assertion functions report
   * to rollbar if they fail. *)
  let assert_ ?(debug : 'd option) (msg : string) (cond : bool) (returnVal : 'r)
      : 'r =
    if cond
    then returnVal
    else recover ("Assertion failure: " ^ msg) ~debug returnVal


  (* Assert `cond` as a statement.  All assertion functions report to rollbar
   * if they fail. *)
  let asserT ?(debug : 'd option) (msg : 'msg) (cond : bool) : unit =
    assert_ ~debug msg cond ()


  (* Assert that `f a` returns true, passing the value back as a result. All
   * assertion functions report to rollbar if they fail. *)
  let assertFn
      ?(debug : 'd option) (msg : string) ~(f : 'r -> bool) (returnVal : 'r) :
      'r =
    assert_ ~debug msg (f returnVal) returnVal


  (* Assert that `f a` returns true, as a statement. All assertion functions
   * report to rollbar if they fail. *)
  let asserTFn ?(debug : 'd option) (msg : string) ~(f : 'a -> bool) : unit =
    assertFn ~f ~debug msg ()


  (* Like recover but with the message TODO *)
  let todo (msg : string) (recoveryVal : 'b) : 'b =
    recover ~debug:recoveryVal ("TODO: " ^ msg) recoveryVal
end
