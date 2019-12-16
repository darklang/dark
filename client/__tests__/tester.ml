(* See docs/unittests.md for documentation on how to use this. *)

open Tc

module Private = struct
  type success =
    | Passed
    | Failed
    | Skipped

  type t =
    { categories : string list
    ; name : string
    ; success : success
    ; actual : string option
    ; expected : string option }

  let results : t list ref = ref []

  let categories : string list ref = ref []

  let runningTest : string ref = ref ""

  type 'a expectation = Expect of 'a
end

(* ------------------ *)
(* Command line options *)
(* ------------------ *)

let pattern : Js.Re.t option ref = ref None

let verbose : bool ref = ref false

(* ------------------ *)
(* Output / results *)
(* ------------------ *)

let categoryIndent () =
  String.repeat ~count:(List.length !Private.categories) " "


let reset = {j|\x1b[0m|j}

let grey = {j|\x1b[37m|j}

let cyan = {j|\x1b[36m|j}

let green = {j|\x1b[32m|j}

let yellow = {j|\x1b[33m|j}

let brightWhite = {j|\x1b[1m\x1b[37m|j}

let categoryColor () : string =
  match List.length !Private.categories with
  | 1 ->
      brightWhite
  | 2 ->
      cyan
  | 3 ->
      yellow
  | 4 ->
      green
  | _ ->
      grey


let testIndent () = categoryIndent () ^ "  "

let print_category_start name : unit =
  Js.log (categoryIndent () ^ {j|↣|j} ^ categoryColor () ^ " " ^ name ^ reset) ;
  ()


let print_test_skip name : unit =
  Js.log (testIndent () ^ {j|🙅|j} ^ " " ^ name)


let print_test_end name (success : Private.success) : unit =
  let open Private in
  let shortName = String.slice ~from:0 ~to_:60 name in
  if success = Passed
  then Js.log (testIndent () ^ {j|✅|j} ^ " " ^ shortName)
  else if success = Failed
  then Js.log (testIndent () ^ {j|❌|j} ^ " " ^ name)
  else print_test_skip name


(* ------------------ *)
(* Framework - test creation functions *)
(* ------------------ *)

let describe (name : string) (testFn : unit -> unit) : unit =
  let open Private in
  categories := name :: !categories ;
  if List.length !categories <= 2 || !verbose then print_category_start name ;
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
  if shouldRun then runningTest := name else print_test_skip name ;
  let result =
    if shouldRun
    then testFn ()
    else
      { categories = !categories
      ; name = !runningTest
      ; success = Skipped
      ; actual = None
      ; expected = None }
  in
  if shouldRun && (result.success = Failed || !verbose)
  then print_test_end name result.success ;
  results := result :: !results


(* ------------------ *)
(* Framework - test evaluation functions *)
(* ------------------ *)
let expect (value : 'a) = Private.Expect value

let toEqual (actual : 'a) (Expect expected : 'a Private.expectation) =
  let open Private in
  if actual = expected
  then
    { categories = !categories
    ; name = !runningTest
    ; success = Passed
    ; actual = None
    ; expected = None }
  else
    { categories = !categories
    ; name = !runningTest
    ; success = Failed
    ; actual = Js.Json.stringifyAny actual
    ; expected = Js.Json.stringifyAny expected }


let toBe = toEqual

let pass () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !runningTest
  ; success = Passed
  ; actual = None
  ; expected = None }


let fail () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !runningTest
  ; success = Failed
  ; actual = Some "fail was called"
  ; expected = None }


(* ------------------ *)
(* Announce completion *)
(* ------------------ *)
let finish () =
  let open Private in
  let successes = List.filter !results ~f:(fun r -> r.success = Passed) in
  let fails = List.filter !results ~f:(fun r -> r.success = Failed) in
  let skips = List.filter !results ~f:(fun r -> r.success = Skipped) in
  let successCount = List.length successes |> string_of_int in
  let failCount = List.length fails |> string_of_int in
  let skipCount = List.length skips |> string_of_int in
  Js.log "\n\n" ;
  if fails = []
  then Js.log ("Passed " ^ successCount ^ " tests (" ^ skipCount ^ " skipped)")
  else
    Js.log
      ( "Failed "
      ^ failCount
      ^ " tests ("
      ^ successCount
      ^ " passed, "
      ^ skipCount
      ^ " skipped)" ) ;
  ()
