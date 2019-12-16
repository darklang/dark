(* See docs/unittests.md for documentation on how to use this. *)

open Tc

module Private = struct
  type t =
    { categories : string list
    ; name : string
    ; success : bool
    ; actual : string option
    ; expected : string option }

  let results : t list ref = ref []

  let categories : string list ref = ref []

  let testName : string ref = ref ""

  type 'a expectation = Expect of 'a
end

let pattern : Js.Re.t option ref = ref None

let verbose : bool ref = ref false

let describe (name : string) (testFn : unit -> unit) : unit =
  let open Private in
  if !verbose then Js.log2 "Starting : " name ;
  categories := name :: !categories ;
  testFn () ;
  match !categories with [] -> () | _ :: rest -> categories := rest


let test (name : string) (testFn : unit -> Private.t) : unit =
  let open Private in
  let shouldRun =
    match !pattern with
    | None ->
        true
    | Some pattern ->
        let fullname = String.join ~sep:" " (name :: !categories) in
        Util.Regex.contains ~re:pattern fullname
  in
  if shouldRun
  then (
    if !verbose then Js.log2 "Running: " name ;
    testName := name ;
    let result = testFn () in
    results := result :: !results )
  else ()


let expect (value : 'a) = Private.Expect value

let toEqual (actual : 'a) (Expect expected : 'a Private.expectation) =
  let open Private in
  if actual = expected
  then
    { categories = !categories
    ; name = !testName
    ; success = true
    ; actual = None
    ; expected = None }
  else
    { categories = !categories
    ; name = !testName
    ; success = true
    ; actual = Js.Json.stringifyAny actual
    ; expected = Js.Json.stringifyAny expected }


let toBe = toEqual

let pass () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !testName
  ; success = true
  ; actual = None
  ; expected = None }


let fail () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !testName
  ; success = true
  ; actual = Some "fail was called"
  ; expected = None }


let finish () =
  let open Private in
  Js.log ("Ran " ^ string_of_int (List.length !results) ^ " tests") ;
  ()
