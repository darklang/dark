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
 * situations the same.
 *
 * The solution is to never use tuples in a constructor: always use another
 * type which is defined as that tuple.
 *)

module Util = Yojson.Basic.Util
type bjs = Yojson.Basic.json

exception E of string * bjs
type 'a r = ('a, (string * bjs)) Result.t

let err msg what json =
  raise (E (("Error: " ^ msg ^ " (" ^ what ^ ")"), json))


(* -------------------- *)
(* TODOs *)
(* -------------------- *)
let todofn letter json = err "TODO" letter json
let atodo json = todofn "a" json
let btodo json = todofn "b" json
let ctodo json = todofn "c" json
let dtodo json = todofn "d" json
let etodo json = todofn "e" json
let ftodo json = todofn "f" json
let gtodo json = todofn "g" json
let htodo json = todofn "h" json
let itodo json = todofn "i" json
let jtodo json = todofn "j" json
let ktodo json = todofn "k" json
let ltodo json = todofn "l" json
let todo name json = todofn name json

type todo = int [@@deriving show]

(* -------------------- *)
(* Extracting JSON *)
(* -------------------- *)
let listJ (f: (bjs -> 'a)) (j: bjs) : 'a list =
  j
  |> Util.to_list
  |> List.map ~f

let intJ = Util.to_int
let stringJ = Util.to_string
let floatJ = Util.to_float
let boolJ  = Util.to_bool
let optionJ = Util.to_option
let unitJ j =
  if j = `List []
  then ()
  else err "unit" "Not unit" j

let charJ j : char =
  match j with
  | `String "" -> err "char" "empty string" j
  | `String s -> String.get s 0
  | _ -> err "char"  "not a string" j


let pairJ (f1: bjs -> 'a) (f2: bjs -> 'b) (j: bjs) : ('a * 'b) =
  match j with
  | `List [a; b] ->
    (f1 a, f2 b)
  | _ -> err "pairJ" "Not a pair" j

let tripleJ (f1: bjs -> 'a) (f2: bjs -> 'b) (f3: bjs -> 'c) (j: bjs)
  : ('a * 'b * 'c) =
  match j with
  | `List [a; b; c] ->
    (f1 a, f2 b, f3 c)
  | _ -> err "triple" "Not a triple" j

let quadrupleJ (f1: bjs -> 'a) (f2: bjs -> 'b) (f3: bjs -> 'c) (f4: bjs -> 'd) (j: bjs)
  : ('a * 'b * 'c * 'd) =
  match j with
  | `List [a; b; c; d] ->
    (f1 a, f2 b, f3 c, f4 d)
  | _ -> err "quadruple" "Not a quadruple" j



let expect_tag (name: string) (j: bjs) =
  match Util.member "tag" j with
  | `Null -> err "Got null instead of tag name" name j
  | `String found ->
    if found = name
    then ()
    else err ("Expected " ^ name) found j
  | _ -> err "More than one match" name j

let member name (j: bjs) : bjs =
  Util.member name j

let constructor (name: string) (const: 'a -> 'b) (f: bjs -> 'a) (j: bjs) : 'b r =
  match Util.member "tag" j with
  | `String found ->
    if found = name
    then
      Result.Ok (const (f (Util.member "contents" j)))
    else
      Result.Error (("Expected " ^ name), j)
  | _ -> err "Found non-string" name j


let recordConstructor (name: string) (const: 'a -> 'b) (f: bjs -> 'a) (j: bjs) : 'b r =
  match Util.member "tag" j with
  | `String found ->
    if found = name
    then
      Result.Ok (const (f j))
    else
      Result.Error (("Expected " ^ name), j)
  | _ -> err "Found non-string" name j

let orConstructor (name: string) (const: 'a -> 'b) (f: bjs -> 'a) (j: bjs) (result: 'b r) : 'b r =
  match result with
  | Ok _ -> result
  | Error _ -> constructor name const f j

let orRecordConstructor (name: string) (const: 'a -> 'b) (f: bjs -> 'a) (j: bjs) (result: 'b r) : 'b r =
  match result with
  | Ok _ -> result
  | Error _ -> recordConstructor name const f j



let orFail (msg: string) (j: bjs) (r: 'a r) : 'a =
  match r with
  | Ok a -> a
  | Error _ ->
    err "fail" msg j


(* -------------------- *)
(* Elm-format types *)
(* -------------------- *)

type position = { line: int
                ; column: int
                }[@@deriving show]
type region = { start : position
              ; end_ : position
              } [@@deriving show]
let positionJ json : position =
  { line = json |> member "line" |> intJ
  ; column = json |> member "column" |> intJ
  }
let regionJ json =
  { start = json |> member "start" |> positionJ
  ; end_ = json |> member "end" |> positionJ
  }

type 'a located = (region * 'a) [@@deriving show]
let locatedJ (f: bjs -> 'a) j : 'a located =
  pairJ regionJ f j



type comment
  = BlockComment of string list
  | LineComment of string
  | CommentTrickOpener
  | CommentTrickCloser
  | CommentTrickBlock of string
  [@@deriving show]

type comments = comment list
[@@deriving show]


type 'a preCommented = (comments * 'a) [@@deriving show]
type 'a postCommented = 'a * comments [@@deriving show]
type 'a commented = comments * 'a * comments [@@deriving show]
type 'a keywordCommented = (comments * comments * 'a) [@@deriving show]
type 'a withEol = ('a * string option) [@@deriving show]

let commentJ (j: bjs) : comment =
  constructor "BlockComment" (fun x -> BlockComment x) (listJ stringJ) j
  |> orConstructor "LineComment" (fun x -> LineComment x) stringJ j
  |> orConstructor "CommentTrickOpener" (fun x -> CommentTrickOpener) ident j
  |> orConstructor "CommentTrickCloser" (fun x -> CommentTrickCloser) ident j
  |> orConstructor "CommentTrickBlock" (fun x -> CommentTrickBlock x) stringJ j
  |> orFail "comment" j

let commentsJ = listJ commentJ

let preCommentedJ (f: bjs -> 'a) (j: bjs) : 'a preCommented =
  pairJ commentsJ f j

let postCommentedJ (f: bjs -> 'a) (j: bjs) : 'a postCommented =
  pairJ f commentsJ j

let commentedJ (f: bjs -> 'a) (j: bjs) : 'a commented =
  tripleJ commentsJ f commentsJ j

let keywordCommentedJ (f: bjs -> 'a) (j: bjs) : 'a keywordCommented =
  tripleJ commentsJ commentsJ f j

let withEolJ (f: bjs -> 'a) j : 'a withEol =
  pairJ f (optionJ stringJ) j

(* Identifiers *)
type uppercaseIdentifier = string [@@deriving show]
type lowercaseIdentifier = string [@@deriving show]
type symbolIdentifier = string [@@deriving show]
let lowercaseIdentifierJ = stringJ
let uppercaseIdentifierJ = stringJ
let symbolIdentifierJ = stringJ

(* Maps *)
type ('a, 'b) map = ('a * 'b) list [@@deriving show]

type ('k, 'v) commentedMap = ('k, 'v commented) map
  [@@deriving show]

let mapJ (f1: bjs -> 'a) (f2: bjs -> 'b) json : ('a, 'b) map =
  listJ (pairJ f1 f2) json

let commentedMapJ (f1: bjs -> 'a) (f2: bjs -> 'b) json
  : ('a, 'b) commentedMap =
  listJ (pairJ f1 (commentedJ f2)) json



type multiline
  = JoinAll
  | SplitAll
  [@@deriving show]

let multilineJ j : multiline =
  match j with
  | `String "JoinAll" -> JoinAll
  | `String "SplitAll" -> SplitAll
  | _ -> err "multiline" "no matched" j

type functionApplicationMultiline
  = FASplitFirst
  | FAJoinFirst of multiline
  [@@deriving show]

let functionApplicationMultilineJ j =
  constructor "FASplitFirst" (fun a -> FASplitFirst) ident j
  |> orConstructor "FAJoinFirst" (fun t -> FAJoinFirst t) multilineJ j
  |> orFail "functionApplicationMultiline" j

type intRepresentation
  = DecimalInt
  | HexadecimalInt
  [@@deriving show]

let intRepresentationJ j : intRepresentation =
  match j with
  | `String "DecimalInt" -> DecimalInt
  | `String "HexadecimalInt" -> HexadecimalInt
  | _ -> err "intRepresentation" "no matched" j

type floatRepresentation
  = DecimalFloat
  | ExponentFloat
  [@@deriving show]

let floatRepresentationJ j : floatRepresentation =
  match j with
  | `String "DecimalFloat" -> DecimalFloat
  | `String "ExponentFloat" -> ExponentFloat
  | _ -> err "floatRepresentation" "no matched" j


type literal
  = IntNum of (int * intRepresentation)
  | FloatNum of (float * floatRepresentation)
  | Chr of char
  | Str of (string * bool)
  | Boolean of bool
  [@@deriving show]

let literalJ j =
  constructor "IntNum" (fun (a,b) -> IntNum (a,b)) (pairJ intJ intRepresentationJ) j
  |> orConstructor "FloatNum" (fun (a,b) -> FloatNum (a,b)) (pairJ floatJ floatRepresentationJ) j
  |> orConstructor "Chr" (fun a -> Chr a) charJ j
  |> orConstructor "Str" (fun (a,b) -> Str (a,b)) (pairJ stringJ boolJ) j
  |> orConstructor "Boolean" (fun a -> Boolean a) boolJ j
  |> orFail "literal" j

type varref = (uppercaseIdentifier list) * lowercaseIdentifier
  [@@deriving show]

type tagref = (uppercaseIdentifier list) * uppercaseIdentifier
  [@@deriving show]

type ref_
  = VarRef of varref
  | TagRef of tagref
  | OpRef of symbolIdentifier
[@@deriving show]

let varRefJ j =
  pairJ (listJ uppercaseIdentifierJ) lowercaseIdentifierJ j

let tagRefJ j =
  pairJ (listJ uppercaseIdentifierJ) uppercaseIdentifierJ j

let ref_J (j: bjs) : ref_ =
  constructor "VarRef" (fun d -> VarRef d) varRefJ j
  |> orConstructor "TagRef" (fun t -> TagRef t) tagRefJ j
  (* |> orConstructor "OpRef" (fun t -> OpRef t) itodo  j *)
  |> orFail "ref_" j

(* Patterns *)
type patternp
  = Anything
  | UnitPattern of comments
  | Literal of literal
  | VarPattern of lowercaseIdentifier
  (* | OpPattern SymbolIdentifier *)
  | Data of data
  (* | PatternParens (Commented Pattern) *)
  (* | Tuple [Commented Pattern] *)
  (* | EmptyListPattern Comments *)
  (* | List [Commented Pattern] *)
  (* | ConsPattern *)
  (*     { first :: (Pattern, Maybe String) *)
  (*     , rest :: [(Comments, Comments, Pattern, Maybe String)] *)
  (*     } *)
  (* | Record [Commented LowercaseIdentifier] *)
  (* | Alias (Pattern, Comments) (Comments, LowercaseIdentifier) *)
and data = uppercaseIdentifier list * (comments * pattern) list
and pattern = patternp located
[@@deriving show]

let rec patternpJ j : patternp =
  constructor "Anything" (fun a -> Anything) ident j
  |> orConstructor "UnitPattern" (fun t -> UnitPattern t) commentsJ j
  |> orConstructor "Literal" (fun t -> Literal t) literalJ j
  |> orConstructor "VarPattern" (fun t -> VarPattern t) lowercaseIdentifierJ j
  |> orConstructor "Data" (fun t -> Data t) dataJ j
  |> orFail "patternp" j
and dataJ j : data =
  pairJ
    (listJ uppercaseIdentifierJ)
    (listJ (pairJ commentsJ patternJ))
    j

and patternJ j : pattern =
  locatedJ patternpJ j


(* Expressions *)
type app =
  (expr * ((comments * expr) list) * functionApplicationMultiline)
and case =
  (expr commented * bool) * ((pattern commented * (comments * expr)) list)
and tuple = (expr commented) list * bool
and expr_
  = Unit of comments
  | App of app
  | Literal of literal
  | VarExpr of ref_

    (* | Unary UnaryOperator Expr *)
    (* | Binops Expr [(Comments, Var.Ref, Comments, Expr)] Bool *)
  | Parens of expr commented
    (*  *)
    (* | ExplicitList *)
    (*     { terms :: Sequence Expr *)
    (*     , trailingComments :: Comments *)
    (*     , forceMultiline :: ForceMultiline *)
    (*     } *)
    (* | Range (Commented Expr) (Commented Expr) Bool *)
    (*  *)
  | Tuple of tuple
    (* | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple *)
    (*  *)
    (* | Record *)
    (*     { base :: Maybe (Commented LowercaseIdentifier) *)
    (*     , fields :: Sequence (Pair LowercaseIdentifier Expr) *)
    (*     , trailingComments :: Comments *)
    (*     , forceMultiline :: ForceMultiline *)
    (*     } *)
    (* | Access Expr LowercaseIdentifier *)
    (* | AccessFunction LowercaseIdentifier *)
    (*  *)
    (* | Lambda [(Comments, Pattern.Pattern)] Comments Expr Bool *)
    (* | If IfClause [(Comments, IfClause)] (Comments, Expr) *)
    (* | Let [LetDeclaration] Comments Expr *)
  | Case of case
    (* -- for type checking and code gen only *)
    (* | GLShader String *)
and expr = expr_ located
[@@deriving show]

let rec appJ j : app =
  tripleJ exprJ (listJ (pairJ commentsJ exprJ)) functionApplicationMultilineJ j

and expr_J j : expr_ =
  constructor "App" (fun a -> App a) appJ j
  |> orConstructor "Unit" (fun a -> Unit a) commentsJ j
  |> orConstructor "Literal" (fun a -> Literal a) literalJ j
  |> orConstructor "VarExpr" (fun a -> VarExpr a) ref_J j
  |> orConstructor "Case" (fun a -> Case a) caseJ j
  |> orConstructor "Tuple" (fun a -> Tuple a) tupleJ j
  |> orConstructor "Parens" (fun a -> Parens a) (commentedJ exprJ) j
  |> orFail "expr_" j

and exprJ j : expr =
  locatedJ expr_J j

and caseJ j : case =
  pairJ
    (pairJ
      (commentedJ exprJ)
      boolJ)
    (listJ
      (pairJ
        (commentedJ patternJ)
        (pairJ commentsJ exprJ)))
    j

and tupleJ j : tuple =
  pairJ (listJ (commentedJ exprJ)) boolJ j






type markdown_blocks = todo [@@deriving show]
let markdown_blocksJ = ctodo


type 'a listing
  = ExplicitListing of 'a  * bool
  | OpenListing of unit commented
  | ClosedListing
  [@@deriving show]

let listingJ (f: bjs -> 'a) (j: bjs) : 'a listing =
  constructor "ExplicitListing" (fun (a, b) -> ExplicitListing (a ,b)) atodo j
  |> orConstructor "OpenListing" (fun t -> OpenListing t) (commentedJ unitJ) j
  |> orConstructor "ClosedListing" (fun t -> ClosedListing) ident j
  |> orFail "listing" j




type value
  = Value of lowercaseIdentifier
  | OpValue of symbolIdentifier
  | Union of (uppercaseIdentifier postCommented) * ((uppercaseIdentifier, unit) commentedMap) listing
  [@@deriving show]

type detailedListing =
  { values: (lowercaseIdentifier, unit) commentedMap
  ; operators: (symbolIdentifier, unit) commentedMap
  ; types: (uppercaseIdentifier, (comments * (uppercaseIdentifier * unit) listing)) commentedMap
  }
  [@@deriving show]

let detailedListingJ (j: bjs) : detailedListing =
  { values =
      j
      |> member "values"
      |> commentedMapJ lowercaseIdentifierJ unitJ
  ; operators =
      j
      |> member "operators"
      |> commentedMapJ symbolIdentifierJ unitJ
  ; types =
      j
      |> member "operators"
      |> commentedMapJ
           uppercaseIdentifierJ
           (pairJ
             commentsJ
             (listingJ (pairJ uppercaseIdentifierJ unitJ)))
  }


type importMethod =
  { alias: (comments * uppercaseIdentifier preCommented) option
  ; exposedVars: comments * detailedListing listing preCommented
  } [@@deriving show]

let importMethodJ (j: bjs) : importMethod =
  { alias =
      j
      |> member "alias"
      |> optionJ (pairJ commentsJ (preCommentedJ uppercaseIdentifierJ))
  ; exposedVars =
      j
      |> member "exposedVars"
      |> pairJ commentsJ (preCommentedJ (listingJ detailedListingJ))
  }


type sourceTag
  = Normal
  | Effect of comments
  | Port of comments
[@@deriving show]

let sourceTagJ (j: bjs) : sourceTag =
  constructor "Normal" (fun d -> Normal) ident j
  |> orConstructor "Effect" (fun t -> Effect t) commentsJ j
  |> orConstructor "Port" (fun t -> Port t) commentsJ j
  |> orFail "sourceTag" j



type header =
  { srcTag : sourceTag
  ; name : uppercaseIdentifier list commented
  ; moduleSettings : todo option
  ; exports : detailedListing listing keywordCommented
  } [@@deriving show]

let headerJ j : header =
  { srcTag = j |> member "srcTag" |> sourceTagJ
  ; name = j |> member "name" |> commentedJ (listJ uppercaseIdentifierJ)
  ; moduleSettings = j |> member "moduleSettings" |> optionJ ctodo
  ; exports = j |> member "exports" |> keywordCommentedJ (listingJ detailedListingJ)
  }


type forceMultiline = bool
[@@deriving show]

type typeConstructor
  = NamedConstructor of uppercaseIdentifier list
  | TupleConstructor of int
[@@deriving show]

type typeConstruction =
  typeConstructor * ((comments * type_) list)
and functionType =
  { first: type_ withEol
  ; rest: (comments * comments * type_ * string option) list
  ; forceMultiline: forceMultiline
  }
and typep
  = TypeConstruction of typeConstruction
  | FunctionType of functionType
and type_ = typep located

[@@deriving show]

type definition = pattern * (pattern preCommented list) * comments * expr
[@@deriving show]

type typeAnnotation = (ref_ postCommented) * (type_ preCommented)
[@@deriving show]

type declaration
  = Definition of definition
  | TypeAnnotation of typeAnnotation
  | Datatype of todo
      (* { nameWithArgs :: Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier) *)
      (* , tags :: OpenCommentedList (NameWithArgs UppercaseIdentifier Type) *)
      (* } *)
  | TypeAlias of todo
      (*   Comments *)
      (* (Commented (NameWithArgs UppercaseIdentifier LowercaseIdentifier)) *)
      (* (PreCommented Type) *)
  (* | PortAnnotation (Commented LowercaseIdentifier) Comments Type *)
  (* | PortDefinition (Commented LowercaseIdentifier) Comments Expression.Expr *)
  (* | Fixity Assoc Comments Int Comments Var.Ref *)
  (* | Fixity_0_19 (PreCommented Assoc) (PreCommented Int) (Commented SymbolIdentifier) (PreCommented LowercaseIdentifier) *)

[@@deriving show]

type 'a topLevelStructure = Entry of 'a located
                          | BodyComment of comment
                          | DocComment of markdown_blocks
[@@deriving show]

let topLevelStructureJ (f: bjs -> 'a) (j: bjs) : 'a topLevelStructure =
  constructor "Entry" (fun (x, y) -> Entry (x, y)) (locatedJ f) j
  |> orConstructor "BodyComment" (fun t -> BodyComment t) commentJ j
  (* |> orConstructor "DocComment" (fun x -> DocComment x) gtodo j *)
  |> orFail "topLevel" j

let typeConstructorJ (j: bjs) : typeConstructor =
  constructor "NamedConstructor" (fun d -> NamedConstructor d)
    (listJ uppercaseIdentifierJ) j
  |> orConstructor "TupleConstructor" (fun d -> TupleConstructor d) intJ j
  |> orFail "typeConstructor" j

let forceMultilineJ = boolJ

let rec typepJ (j: bjs) : typep =
  constructor "TypeConstruction" (fun d -> TypeConstruction d) typeConstructionJ j
  |> orRecordConstructor "FunctionType" (fun d -> FunctionType d) functionTypeJ j
  |> orFail "declaration" j
and type_J (j: bjs) : type_ =
  locatedJ typepJ j
and typeConstructionJ j =
  pairJ typeConstructorJ (listJ (pairJ commentsJ type_J)) j
and functionTypeJ j =
  expect_tag "FunctionType" j;
  { first = j |> member "first" |> withEolJ type_J
  ; rest = j
           |> member "rest"
           |> listJ
                (quadrupleJ
                   commentsJ
                   commentsJ
                   type_J
                   (optionJ stringJ))
  ; forceMultiline = j |> member "forceMultiline" |> forceMultilineJ
  }


let typeAnnotationJ j =
  pairJ (postCommentedJ ref_J) (preCommentedJ type_J) j

let definitionJ =
  quadrupleJ
    patternJ
    (listJ (preCommentedJ patternJ))
    commentsJ
    exprJ

let declarationJ (j: bjs) : declaration =
  constructor "Definition" (fun d -> Definition d) definitionJ j
  |> orConstructor "TypeAnnotation" (fun t -> TypeAnnotation t) typeAnnotationJ j
  (* |> orConstructor "Datatype" (fun d -> Datatype d) gtodo j *)
  |> orFail "declaration" j

type imports =
  (((uppercaseIdentifier list), (comments * importMethod)) map)
  preCommented
[@@deriving show]

type module_ =
  { initial_comments : comments
  ; header : header
  ; docs : markdown_blocks option located
  ; imports : imports
  ; body : declaration topLevelStructure list
  }
[@@deriving show]

let docsJ = locatedJ (optionJ (markdown_blocksJ))

let importsJ (j: bjs) : imports =
  preCommentedJ
    (mapJ
       (listJ uppercaseIdentifierJ)
       (pairJ
          commentsJ
          importMethodJ))
    j

let moduleJ json =
  { initial_comments = json |> member "initialComments" |> commentsJ
  ; header = json |> member "header" |> headerJ
  ; docs = json |> member "docs" |> docsJ
  ; imports = json |> member "imports" |> importsJ
  ; body = json |> member "body" |> listJ (topLevelStructureJ declarationJ)
  }
(* let rec preprocess (json: Yojson.Basic.json) : Yojson.Basic.json = *)
(*   match json with *)
(*   | `Assoc [("tag", `String tag)] -> *)
(*     `List [`String tag] *)
(*  *)
(*   | `Assoc [("tag", `String tag); ("contents", `List contents)] *)
(*   | `Assoc [("contents", `List contents); ("tag", `String tag)] -> *)
(*     `List (`String tag :: `List (List.map ~f:preprocess contents) :: []) *)
(*  *)
(*   | `Assoc [("tag", `String tag); ("contents", contents)] *)
(*   | `Assoc [("contents", contents); ("tag", `String tag)] -> *)
(*     `List [`String tag; preprocess contents] *)
(*  *)
(*   | `Assoc (("tag", `String tag) :: rest) -> *)
(*     `List [`String tag; `Assoc (List.map rest ~f:(fun (k,v) -> (k, preprocess v)))] *)
(*  *)
(*   | `List l -> *)
(*     `List (List.map ~f:preprocess l) *)
(*  *)
(*   | `Assoc a -> *)
(*     `Assoc (List.map a ~f:(fun (k,v) -> (k, preprocess v))) *)
(*  *)
(*   | json -> *)
(*     json *)
(*  *)
let _ =
  try
    In_channel.stdin
    |> Yojson.Basic.from_channel
    |> moduleJ
    |> show_module_
    |> Str.global_replace (Str.regexp "Translate\\.") ""
    |> print_endline
  with (E (msg, json)) ->
    Printexc.print_backtrace stderr;
    print_endline (Yojson.Basic.pretty_to_string json);
    prerr_endline msg;
    ()



