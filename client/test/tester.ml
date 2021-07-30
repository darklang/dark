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

  type 'a expectation =
    { actual : 'a
    ; equalityFn : 'a -> 'a -> bool
    ; printer : 'a -> string }
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


let print_test_end name (t : Private.t) : unit =
  let open Private in
  let shortName = String.slice ~from:0 ~to_:68 name in
  if t.success = Passed
  then Js.log @@ testIndent () ^ {j|✅|j} ^ " " ^ shortName
  else if t.success = Failed
  then (
    Js.log @@ testIndent () ^ {j|❌|j} ^ " " ^ name ;
    Js.log
    @@ testIndent ()
    ^ "Expected: "
    ^ Option.unwrap ~default:"None" t.expected ;
    Js.log
    @@ testIndent ()
    ^ "  Actual: "
    ^ Option.unwrap ~default:"None" t.actual )
  else print_test_skip name


(* ------------------ *)
(* Framework - test creation functions *)
(* ------------------ *)

let shouldRun name =
  match !pattern with
  | None ->
      true
  | Some pattern ->
      let fullname = String.join ~sep:" " (name :: !Private.categories) in
      Util.Regex.contains ~re:pattern fullname


let describe (name : string) (testFn : unit -> unit) : unit =
  let open Private in
  categories := name :: !categories ;
  if List.length !categories <= 1 || !verbose then print_category_start name ;
  testFn () ;
  match !categories with [] -> () | _ :: rest -> categories := rest


let test (name : string) (testFn : unit -> Private.t) : unit =
  let open Private in
  let run = shouldRun name in
  if run then runningTest := name else if !verbose then print_test_skip name ;
  let result =
    if run
    then
      try testFn ()
      with e ->
        let error =
          match e with
          | Failure msg ->
              "Failure: " ^ msg
          | _ ->
              Printexc.to_string e
        in
        { categories = !categories
        ; name = !runningTest
        ; success = Failed
        ; actual = Some error
        ; expected = None }
    else
      { categories = !categories
      ; name = !runningTest
      ; success = Skipped
      ; actual = None
      ; expected = None }
  in
  if run && (result.success = Failed || !verbose)
  then print_test_end name result ;
  results := result :: !results


let testAll (name : string) (items : 'a list) (testFn : 'a -> Private.t) : unit
    =
  items
  |> List.forEach ~f:(fun item ->
         let name' = {j|$name  - $item|j} in
         test name' (fun () -> testFn item))


(* ------------------ *)
(* Framework - test evaluation functions *)
(* ------------------ *)
let expect (actual : 'a) =
  { Private.actual
  ; equalityFn = ( = )
  ; printer = Js.Json.stringifyAny >> Option.valueExn }


let toEqual (expected : 'a) (e : 'a Private.expectation) =
  let open Private in
  if e.equalityFn e.actual expected
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
    ; actual = Some (e.printer e.actual)
    ; expected = Some (e.printer expected) }


(** withEquality replaces the equality function used for comparing the expected
 * and actual values with the given equalityFn (it defaults to ( = )).
 *
 * Eg:
      expect actualExpr
      |> withEquality FluidExpression.testEqualIgnoringIds
      |> toEqual expectedExpr
 *)
let withEquality (equalityFn : 'a -> 'a -> bool) (e : 'a Private.expectation) :
    'a Private.expectation =
  {e with equalityFn}


let withPrinter (printer : 'a -> string) (e : 'a Private.expectation) :
    'a Private.expectation =
  {e with printer}


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


let skip () : Private.t =
  let open Private in
  { categories = !categories
  ; name = !runningTest
  ; success = Skipped
  ; actual = None
  ; expected = None }


(* ------------------ *)
(* Announce completion *)
(* ------------------ *)

let successes () =
  let open Private in
  List.filter !results ~f:(fun r -> r.success = Passed)


let fails () =
  let open Private in
  List.filter !results ~f:(fun r -> r.success = Failed)


let skips () =
  let open Private in
  List.filter !results ~f:(fun r -> r.success = Skipped)


let finish () =
  let open Private in
  let successes = successes () in
  let fails = fails () in
  let skips = skips () in
  let successCount = List.length successes |> string_of_int in
  let failCount = List.length fails |> string_of_int in
  let skipCount = List.length skips |> string_of_int in
  Js.log "\n\n" ;
  if fails = []
  then (
    Js.log ("Passed " ^ successCount ^ " tests (" ^ skipCount ^ " skipped)") ;
    exit 0 )
  else (
    Js.log "Failures:" ;
    fails
    |> List.forEach ~f:(fun {name; _} ->
           Js.log @@ testIndent () ^ {j|❌|j} ^ " " ^ name) ;
    Js.log "" ;
    Js.log
      ( "Failed "
      ^ failCount
      ^ " tests ("
      ^ successCount
      ^ " passed, "
      ^ skipCount
      ^ " skipped)" ) ;
    exit 1 )
