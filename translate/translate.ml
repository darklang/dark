open Core_kernel

(* Notes on Aeson
 *
 * If only a single constructor, it's unwrapped.
 *
 * Sum types are object with "tag" and "contents".
 * Constructors with objects have "tag" and everything else become other keys.
 * *)

type atodo = int [@@deriving to_yojson, show]
let atodo_of_yojson json =
  failwith ("todo: we've hit atodo: " ^ (Yojson.Safe.to_string json))
type btodo = int [@@deriving to_yojson, show]
let btodo_of_yojson json =
  failwith ("todo: we've hit btodo: " ^ (Yojson.Safe.to_string json))
type ctodo = int [@@deriving to_yojson, show]
let ctodo_of_yojson json =
  failwith ("todo: we've hit ctodo: " ^ (Yojson.Safe.to_string json))
type dtodo = int [@@deriving to_yojson, show]
let dtodo_of_yojson json =
  failwith ("todo: we've hit dtodo: " ^ (Yojson.Safe.to_string json))
type etodo = int [@@deriving to_yojson, show]
let etodo_of_yojson json =
  failwith ("todo: we've hit etodo: " ^ (Yojson.Safe.to_string json))
type ftodo = int [@@deriving to_yojson, show]
let ftodo_of_yojson json =
  failwith ("todo: we've hit ftodo: " ^ (Yojson.Safe.to_string json))
type gtodo = int [@@deriving to_yojson, show]
let gtodo_of_yojson json =
  failwith ("todo: we've hit gtodo: " ^ (Yojson.Safe.to_string json))
type htodo = int [@@deriving to_yojson, show]
let htodo_of_yojson json =
  failwith ("todo: we've hit htodo: " ^ (Yojson.Safe.to_string json))
type itodo = int [@@deriving to_yojson, show]
let itodo_of_yojson json =
  failwith ("todo: we've hit itodo: " ^ (Yojson.Safe.to_string json))
type jtodo = int [@@deriving to_yojson, show]
let jtodo_of_yojson json =
  failwith ("todo: we've hit jtodo: " ^ (Yojson.Safe.to_string json))
type ktodo = int [@@deriving to_yojson, show]
let ktodo_of_yojson json =
  failwith ("todo: we've hit ktodo: " ^ (Yojson.Safe.to_string json))
type ltodo = int [@@deriving to_yojson, show]
let ltodo_of_yojson json =
  failwith ("todo: we've hit ltodo: " ^ (Yojson.Safe.to_string json))

type position = { line: int
                ; column: int
                }[@@deriving yojson, show]
type region = { start : position
              ; end_ : position [@key "end"]
              } [@@deriving yojson, show]

type comment
  = BlockComment of string list
  | LineComment of string
  | CommentTrickOpener
  | CommentTrickCloser
  | CommentTrickBlock of string
  [@@deriving yojson, show]

(* The unit type in aeson *)
type hUnit = unit list
  [@@deriving yojson, show]

type comments = comment list [@@deriving yojson, show]
type markdown_blocks = btodo [@@deriving yojson, show]
type 'a located = (region * 'a) [@@deriving yojson, show]
type pattern = dtodo [@@deriving yojson, show]
type 'a preCommented = (comments * 'a) [@@deriving yojson, show]
type 'a postCommented = 'a * comments [@@deriving yojson, show]
type 'a commented = comments * 'a * comments [@@deriving yojson, show]
type 'a keywordCommented = (comments * comments * 'a) [@@deriving yojson, show]
type expr = ktodo [@@deriving yojson, show]
type uppercaseIdentifier = string [@@deriving yojson, show]
type lowercaseIdentifier = string [@@deriving yojson, show]
type symbolIdentifier = string [@@deriving yojson, show]

type ('a, 'b) map = ('a * 'b) list [@@deriving yojson, show]

type ('k, 'v) commentedMap = ('k, 'v commented) map
  [@@deriving yojson, show]

type 'a listing
  = ExplicitListing of ('a * bool)
  | OpenListing of hUnit commented
  | ClosedListing
  [@@deriving yojson, show]

type value
  = Value of lowercaseIdentifier
  | OpValue of symbolIdentifier
  | Union of (uppercaseIdentifier postCommented) * ((uppercaseIdentifier, unit) commentedMap) listing
  [@@deriving yojson, show]

type detailedListing =
  { values: (lowercaseIdentifier, hUnit) commentedMap
  ; operators: (symbolIdentifier, hUnit) commentedMap
  ; types: (uppercaseIdentifier, (comments * (uppercaseIdentifier * unit) listing)) commentedMap
  }
  [@@deriving yojson, show]

type importMethod =
  { alias: (comments * uppercaseIdentifier preCommented) option
  ; exposedVars: (comments * detailedListing listing preCommented)
  } [@@deriving yojson, show]

type sourceTag
  = Normal
  | Effect of comments
  | Port of comments
[@@deriving yojson, show]

type header =
  { srcTag : sourceTag
  ; name : uppercaseIdentifier list commented
  ; moduleSettings : ftodo option
  ; exports : detailedListing listing keywordCommented
  } [@@deriving yojson, show]

type ref_
  = VarRef of (uppercaseIdentifier list) * lowercaseIdentifier
  | TagRef of (uppercaseIdentifier list) * uppercaseIdentifier
  | OpRef of symbolIdentifier
                 [@@deriving yojson, show]

type type_ = etodo [@@deriving yojson, show]

type declaration
  = Definition of pattern * (pattern preCommented list) * comments * expr
  | TypeAnnotation of (ref_ postCommented) * (type_ preCommented)
  | Datatype of atodo
      (* { nameWithArgs :: Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier) *)
      (* , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier Type) *)
      (* } *)
  | TypeAlias of ctodo
      (*   Comments *)
      (* (Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier)) *)
      (* (PreCommented Type) *)
  (* | PortAnnotation (Commented LowercaseIdentifier) Comments Type *)
  (* | PortDefinition (Commented LowercaseIdentifier) Comments Expression.Expr *)
  (* | Fixity Assoc Comments Int Comments Var.Ref *)
  (* | Fixity_0_19 (PreCommented Assoc) (PreCommented Int) (Commented SymbolIdentifier) (PreCommented LowercaseIdentifier) *)

[@@deriving yojson, show]

type 'a topLevelStructure = Entry of 'a located
                          | BodyComment of comment
                          | DocComment of markdown_blocks
[@@deriving yojson, show]

type module_ =
  { initialComments : comments
  ; header : header
  ; docs : markdown_blocks option located
  ; imports : (((uppercaseIdentifier list), (comments * importMethod)) map) preCommented
  ; body : declaration topLevelStructure list
  }
[@@deriving yojson, show]

let rec preprocess ~(print:bool) ?(indent=2) (json: Yojson.Safe.json) : Yojson.Safe.json =
  let preprocess = preprocess ~print ~indent:(indent + 2) in
  let p arg = if print then prerr_endline ((String.make indent ' ') ^ arg) in
  match json with
  | `Assoc [("tag", `String tag)] ->
    p (tag ^ "(0C)");
    `List [`String tag]

  | `Assoc [("tag", `String tag); ("contents", `List contents)]
  | `Assoc [("contents", `List contents); ("tag", `String tag)] ->
    let count = List.length contents |> string_of_int in
    p (tag ^ " (" ^ count ^ "C):");
    `List (`String tag :: `List (List.map ~f:preprocess contents) :: [])

  | `Assoc [("tag", `String tag); ("contents", contents)]
  | `Assoc [("contents", contents); ("tag", `String tag)] ->
    p (tag ^ "(1C)");
    `List [`String tag; preprocess contents]

  | `Assoc (("tag", `String tag) :: rest) ->
    let count = List.length rest |> string_of_int in
    p (tag ^ "(J" ^ count ^ ")");
    `List [`String tag; `Assoc (List.map rest ~f:(fun (k,v) -> (k, preprocess v)))]

  | `List l ->
    let count = List.length l |> string_of_int in
    if count = "0"
    then
      (p "[]";
      `List [])
    else
      (
      p ("List/Tuple (" ^ count ^ ")");
      p ("[");
      let body = List.map ~f:preprocess l in
      p ("]");
      `List body
    )

  | `Assoc a ->
    (* let names = a |> List.map ~f:Tuple.T2.get1 |> String.concat ~sep:", " in *)
    (* let count = List.length a |> string_of_int in *)
    p ("{");
    let body = List.map a ~f:(fun (k,v) -> p k; (k, preprocess v)) in
    p ("}");
    `Assoc body

  | `String s ->
    p (s);
    json

  | `Bool s ->
    p (string_of_bool s);
    json

  | `Tuple s ->
    p "tuple";
    json

  | `Intlit s ->
    p "intlit";
    json

  | `Variant s ->
    p "variant";
    json

  | `Float s ->
    p (string_of_float s);
    json

  | `Int s ->
    p (string_of_int s);
    json

  | `Null ->
    p "null";
    json

let _ =
  try
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "--clean"
    then
      In_channel.stdin
      |> Yojson.Safe.from_channel
      |> preprocess ~print:true
      |> Yojson.Safe.to_string
      |> print_endline
    else
      In_channel.stdin
      |> Yojson.Safe.from_channel
      (* |> preprocess *)
      |> module__of_yojson
      (* |> export_of_yojson *)
      |> Result.ok_or_failwith
      |> show_module_
      |> Str.global_replace (Str.regexp "Translate\\.") ""
      (* |> show_export *)
      |> print_endline
  with e ->
    print_endline (Exn.to_string e)



