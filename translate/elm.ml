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
  match j with
  | `List l -> List.map ~f l
  | _ -> err "listJ" "not a list" j

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
  let m =
    try
      Util.member "tag" j
    with _  ->
      err "getting tag" name j
  in
  match m with
  | `String found ->
    if found = name
    then ()
    else err ("Expected " ^ name) found j
  | _ -> err "Expected string tag" name j

let member ?(nullable=false) name (f: bjs -> 'a) (j: bjs) : 'a =
  let v =
    try
      Util.member name j
    with e ->
      err "member" name j
  in
  if v = `Null && (not nullable)
  then err "no member" name j
  else
    f v

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
let positionJ j : position =
  { line = member "line" intJ j
  ; column = member "column" intJ j
  }
let regionJ j =
  { start = member "start" positionJ j
  ; end_ = member "end" positionJ j
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
type 'a sequence = (comments * 'a withEol preCommented) list [@@deriving show]

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

let sequenceJ (f: bjs -> 'a) j : 'a sequence =
  listJ (pairJ commentsJ (preCommentedJ (withEolJ f))) j


(* Identifiers *)
type uppercaseIdentifier = string [@@deriving show]
type lowercaseIdentifier = string [@@deriving show]
type symbolIdentifier = string [@@deriving show]
let lowercaseIdentifierJ = stringJ
let uppercaseIdentifierJ = stringJ
let symbolIdentifierJ = stringJ
let forceMultilineJ = boolJ

(* Maps *)
type ('a, 'b) map = ('a * 'b) list [@@deriving show]

type ('k, 'v) commentedMap = ('k, 'v commented) map
  [@@deriving show]

let mapJ (f1: bjs -> 'a) (f2: bjs -> 'b) json : ('a, 'b) map =
  listJ (pairJ f1 f2) json

let commentedMapJ (f1: bjs -> 'a) (f2: bjs -> 'b) json
  : ('a, 'b) commentedMap =
  listJ (pairJ f1 (commentedJ f2)) json


type unaryOperator =
  Negative
[@@deriving show]

type forceMultiline = bool
[@@deriving show]

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

type ('k, 'v) elmPair =
  { _key: 'k postCommented
  ; _value: 'v preCommented
  ; forceMultiline : forceMultiline
  }
  [@@deriving show]

let elmPairJ (kf: bjs -> 'k) (vf: bjs -> 'v) j : ('k, 'v) elmPair =
  { _key = member "_key" (postCommentedJ kf) j
  ; _value = member "_value" (preCommentedJ vf) j
  ; forceMultiline = member "forceMultiline" forceMultilineJ j
  }


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
  |> orConstructor "OpRef" (fun t -> OpRef t) symbolIdentifierJ j
  |> orFail "ref_" j

type typeConstructor
  = NamedConstructor of uppercaseIdentifier list
  | TupleConstructor of int
[@@deriving show]



(* Expressions *)
type app =
  (expr * ((comments * expr) list) * functionApplicationMultiline)
and case =
  (expr commented * bool) * ((pattern commented * (comments * expr)) list)
and tuple = (expr commented) list * bool
and let_ = (letDeclaration list * comments * expr)
and lambda = (comments * pattern) list * comments * expr * bool
and explicitList =
  { terms : expr sequence
  ; trailingComments : comments
  ; elForceMultiline : forceMultiline
  }

and record =
  { base : lowercaseIdentifier commented option
  ; fields : ((lowercaseIdentifier, expr) elmPair) sequence
  ; rTrailingComments : comments
  ; rForceMultiline : forceMultiline
  }

and ifClause = expr commented * expr commented

and consPattern =
  { cpFirst : pattern * string option
  ; cpRest : (comments * comments * pattern * string option) list
  }

and exprp
  = Unit of comments
  | App of app
  | ELiteral of literal
  | VarExpr of ref_

    (* | Unary UnaryOperator Expr *)
  | Binops of (expr * (comments * ref_ * comments * expr) list * bool)
  | Parens of expr commented

  | ExplicitList of explicitList
    (* | Range (Commented Expr) (Commented Expr) Bool *)
    (*  *)
  | TupleExpr of tuple
  | TupleFunction of int
    (*  *)
  | Record of record
  | Access of expr * lowercaseIdentifier
    (* | AccessFunction LowercaseIdentifier *)
    (*  *)
  | Lambda of lambda
  | If of (ifClause * ((comments * ifClause) list) * (comments * expr))
  | Let of let_
  | Case of case
    (* -- for type checking and code gen only *)
    (* | GLShader String *)
and expr = exprp located
and patternp
  = Anything
  | UnitPattern of comments
  | PLiteral of literal
  | VarPattern of lowercaseIdentifier
  (* | OpPattern SymbolIdentifier *)
  | Data of data
  (* | PatternParens (Commented Pattern) *)
  | TuplePattern of pattern commented list
  | EmptyListPattern of comments
  (* | List [Commented Pattern] *)
  | ConsPattern of consPattern
  (* | Record [Commented LowercaseIdentifier] *)
  (* | Alias (Pattern, Comments) (Comments, LowercaseIdentifier) *)
and data = uppercaseIdentifier list * (comments * pattern) list
and pattern = patternp located
and letDefinition =
  pattern * (comments * pattern) list * comments * expr
and letAnnotation =
  (ref_ * comments) * (comments * type_)
and letDeclaration
  = LetDefinition of letDefinition
  | LetAnnotation of letAnnotation
  | LetComment of comment
and typeConstruction =
  typeConstructor * ((comments * type_) list)
and functionType =
  { first: type_ withEol
  ; rest: (comments * comments * type_ * string option) list
  ; forceMultiline: forceMultiline
  }
and recordType =
  { rtBase : lowercaseIdentifier commented option
  ; rtFields : ((lowercaseIdentifier, type_) elmPair) sequence
  ; rtTrailingComments : comments
  ; rtForceMultiline : forceMultiline
  }

and typep
  = TypeConstruction of typeConstruction
  | FunctionType of functionType
  | TupleType of type_ withEol commented list
  | UnitType of comments
  | TypeVariable of lowercaseIdentifier
  | TypeParens of type_ commented
  | RecordType of recordType
and type_ = typep located



[@@deriving show]

let rec appJ j : app =
  tripleJ exprJ (listJ (pairJ commentsJ exprJ)) functionApplicationMultilineJ j

and exprpJ j : exprp =
  constructor "App" (fun a -> App a) appJ j
  |> orConstructor "Unit" (fun a -> Unit a) commentsJ j
  |> orConstructor "Literal" (fun a -> ELiteral a) literalJ j
  |> orConstructor "VarExpr" (fun a -> VarExpr a) ref_J j
  |> orConstructor "Case" (fun a -> Case a) caseJ j
  |> orConstructor "Tuple" (fun a -> TupleExpr a) tupleJ j
  |> orConstructor "TupleFunction" (fun a -> TupleFunction a) intJ j
  |> orConstructor "Parens" (fun a -> Parens a) (commentedJ exprJ) j
  |> orConstructor "Let" (fun a -> Let a) let_J j
  |> orConstructor "Binops" (fun a -> Binops a) binopsJ j

  |> orRecordConstructor "ExplicitList" (fun a -> ExplicitList a) explicitListJ j
  |> orConstructor "Access" (fun (a,b) -> Access (a,b)) (pairJ exprJ lowercaseIdentifierJ) j
  |> orConstructor "Lambda" (fun a -> Lambda a) lambdaJ j
  |> orConstructor "If" (fun a -> If a) ifJ j
  |> orRecordConstructor "Record" (fun a -> Record a) recordJ j
  |> orFail "exprp" j

and exprJ j : expr =
  locatedJ exprpJ j

and ifJ j =
  tripleJ
    ifClauseJ
    (listJ (pairJ commentsJ ifClauseJ))
    (pairJ commentsJ exprJ)
  j

and ifClauseJ j =
  pairJ
    (commentedJ exprJ)
    (commentedJ exprJ)
  j

and lambdaJ j : lambda =
  quadrupleJ
    (listJ (pairJ commentsJ patternJ))
    commentsJ
    exprJ
    boolJ
    j

and binopsJ j =
  tripleJ
    exprJ
    (listJ
       (quadrupleJ
          commentsJ
          ref_J
          commentsJ
          exprJ))
    boolJ
    j

and recordJ j : record =
  expect_tag "Record" j;
  { base = member "base" (optionJ (commentedJ lowercaseIdentifierJ)) j
  ; fields = member "fields" (sequenceJ (elmPairJ lowercaseIdentifierJ exprJ)) j
  ; rTrailingComments = member "trailingComments" commentsJ j
  ; rForceMultiline = member "forceMultiline" forceMultilineJ j
  }

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

and letDefinitionJ j : letDefinition =
  quadrupleJ
    patternJ
    (listJ (pairJ commentsJ patternJ))
    commentsJ
    exprJ
    j

and letAnnotationJ j : letAnnotation =
  pairJ
    (pairJ ref_J commentsJ)
    (pairJ commentsJ type_J)
    j

and letDeclarationJ j : letDeclaration =
  constructor "LetDefinition" (fun a -> LetDefinition a) letDefinitionJ j
  |> orConstructor "LetAnnotation" (fun a -> LetAnnotation a) letAnnotationJ j
  |> orConstructor "LetComment" (fun a -> LetComment a) commentJ j
  |> orFail "letDeclaration" j


and let_J j : let_ =
  tripleJ (listJ letDeclarationJ) commentsJ exprJ j

and explicitListJ j : explicitList =
  { terms = member "terms" (sequenceJ exprJ) j
  ; trailingComments = member "trailingComments" commentsJ j
  ; elForceMultiline = member "forceMultiline" forceMultilineJ j
  }

and consPatternJ j : consPattern =
  { cpFirst = member "first" (pairJ patternJ (optionJ stringJ)) j
  ; cpRest =
      member "rest"
        (listJ
           (quadrupleJ
             commentsJ
             commentsJ
             patternJ
             (optionJ stringJ)))
        j
  }

and patternpJ j : patternp =
  constructor "Anything" (fun a -> Anything) ident j
  |> orConstructor "UnitPattern" (fun t -> UnitPattern t) commentsJ j
  |> orConstructor "Literal" (fun t -> PLiteral t) literalJ j
  |> orConstructor "VarPattern" (fun t -> VarPattern t) lowercaseIdentifierJ j
  |> orConstructor "EmptyListPattern" (fun t -> EmptyListPattern t) commentsJ j
  |> orRecordConstructor "ConsPattern" (fun t -> ConsPattern t) consPatternJ j
  |> orConstructor "Data" (fun t -> Data t) dataJ j
  |> orConstructor "Tuple" (fun t -> TuplePattern t) (listJ (commentedJ patternJ)) j
  |> orFail "patternp" j

and dataJ j : data =
  pairJ
    (listJ uppercaseIdentifierJ)
    (listJ (pairJ commentsJ patternJ))
    j

and patternJ j : pattern =
  locatedJ patternpJ j

and typepJ (j: bjs) : typep =
  constructor "TypeConstruction" (fun d -> TypeConstruction d) typeConstructionJ j
  |> orRecordConstructor "FunctionType" (fun d -> FunctionType d) functionTypeJ j
  |> orConstructor "TupleType" (fun d -> TupleType d) (listJ (commentedJ (withEolJ type_J))) j
  |> orConstructor "UnitType" (fun d -> UnitType d) commentsJ j
  |> orConstructor "TypeVariable" (fun d -> TypeVariable d) lowercaseIdentifierJ j
  |> orFail "declaration" j

and type_J (j: bjs) : type_ =
  locatedJ typepJ j

and typeConstructionJ j =
  pairJ typeConstructorJ (listJ (pairJ commentsJ type_J)) j

and functionTypeJ j =
  expect_tag "FunctionType" j;
  { first = member "first" (withEolJ type_J) j
  ; rest =
      member "rest"
        (listJ
           (quadrupleJ
              commentsJ
              commentsJ
              type_J
              (optionJ stringJ)))
        j
  ; forceMultiline = member "forceMultiline" forceMultilineJ j
  }
and typeConstructorJ (j: bjs) : typeConstructor =
  constructor "NamedConstructor" (fun d -> NamedConstructor d)
    (listJ uppercaseIdentifierJ) j
  |> orConstructor "TupleConstructor" (fun d -> TupleConstructor d) intJ j
  |> orFail "typeConstructor" j






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
      member "values"
        (commentedMapJ lowercaseIdentifierJ unitJ)
        j
  ; operators =
      member "operators"
        (commentedMapJ symbolIdentifierJ unitJ)
        j
  ; types =
      member "types"
        (commentedMapJ
           uppercaseIdentifierJ
           (pairJ
              commentsJ
              (listingJ (pairJ uppercaseIdentifierJ unitJ))))
        j
  }


type importMethod =
  { alias: (comments * uppercaseIdentifier preCommented) option
  ; exposedVars: comments * detailedListing listing preCommented
  } [@@deriving show]

let importMethodJ (j: bjs) : importMethod =
  { alias =
      member ~nullable:true "alias"
        (optionJ (pairJ commentsJ (preCommentedJ uppercaseIdentifierJ)))
        j
  ; exposedVars =
      member "exposedVars"
        (pairJ commentsJ (preCommentedJ (listingJ detailedListingJ)))
        j
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
  { srcTag = member "srcTag" sourceTagJ j
  ; name = member "name" (commentedJ (listJ uppercaseIdentifierJ)) j
  ; moduleSettings = member ~nullable:true "moduleSettings" (optionJ ctodo) j
  ; exports =
      member "exports"
        (keywordCommentedJ (listingJ detailedListingJ))
        j
  }


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

let moduleJ j =
  { initial_comments = member "initialComments" commentsJ j
  ; header = member "header" headerJ j
  ; docs = member "docs" docsJ j
  ; imports = member "imports" importsJ j
  ; body = member "body" (listJ (topLevelStructureJ declarationJ)) j
  }


