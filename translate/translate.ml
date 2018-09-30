open Core_kernel

(* Notes on Aeson
 *
 * If only a single constructor, it's unwrapped.
 *
 * Sum types are object with "tag" and "contents".
 *
 * Constructors with objects have "tag" and everything else become other keys.
 *
 * Derived yojson behaves differently depending on how we define the types. If
 * you define a constructor with a tuple of 2 types, it will generate ["Name",
 * arg1, arg2]. If instead of define a constructor with 1 type, and that type
 * is a tuple, it will generate ["Name", [[arg1, arg2]]]. Aeson treats these
 * situations the same. *)

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

type comments = comment list
[@@deriving yojson, show]

(* Aeson generated empty list for unit type *)
type hUnit = unit list
  [@@deriving yojson, show]

(* Aeson and Yojson behave differently for these, so don't use them directly IN CONSTRUCTORS. Instead inline them. I think they're fine to use in non-constructors.
 * *)
type 'a located = (region * 'a) [@@deriving yojson, show]
type 'a preCommented = (comments * 'a) [@@deriving yojson, show]
type 'a postCommented = 'a * comments [@@deriving yojson, show]
type 'a commented = comments * 'a * comments [@@deriving yojson, show]
type 'a keywordCommented = (comments * comments * 'a) [@@deriving yojson, show]



type markdown_blocks = btodo [@@deriving yojson, show]
type pattern = dtodo [@@deriving yojson, show]
type expr = ktodo [@@deriving yojson, show]
type uppercaseIdentifier = string [@@deriving yojson, show]
type lowercaseIdentifier = string [@@deriving yojson, show]
type symbolIdentifier = string [@@deriving yojson, show]

type ('a, 'b) map = ('a * 'b) list [@@deriving yojson, show]

type ('k, 'v) commentedMap = ('k, 'v commented) map
  [@@deriving yojson, show]

type 'a listing
  = ExplicitListing of ('a * bool)
  | OpenListing of comments * hUnit * comments
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

type type__ = ctodo
[@@deriving yojson, show]

type type_ = type__ located
[@@deriving yojson, show]

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

type 'a topLevelStructure = Entry of region * 'a
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

let rec preprocess (json: Yojson.Safe.json) : Yojson.Safe.json =
  match json with
  | `Assoc [("tag", `String tag)] ->
    `List [`String tag]

  | `Assoc [("tag", `String tag); ("contents", `List contents)]
  | `Assoc [("contents", `List contents); ("tag", `String tag)] ->
    `List (`String tag :: (List.map ~f:preprocess contents))

  | `Assoc [("tag", `String tag); ("contents", contents)]
  | `Assoc [("contents", contents); ("tag", `String tag)] ->
    `List [`String tag; preprocess contents]

  | `Assoc (("tag", `String tag) :: rest) ->
    `List [`String tag; `Assoc (List.map rest ~f:(fun (k,v) -> (k, preprocess v)))]

  | `List l ->
    `List (List.map ~f:preprocess l)

  | `Assoc a ->
    `Assoc (List.map a ~f:(fun (k,v) -> (k, preprocess v)))

  | json ->
    json

let _ =
  try
    if Array.length Sys.argv > 1 && Sys.argv.(1) = "--clean"
    then
      In_channel.stdin
      |> Yojson.Safe.from_channel
      |> preprocess
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



