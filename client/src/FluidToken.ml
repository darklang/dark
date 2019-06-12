open Tc
open Types
open Prelude

type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let tid (t : token) : id =
  match t with
  | TInteger (id, _)
  | TFloatWhole (id, _)
  | TFloatPoint id
  | TFloatFraction (id, _)
  | TTrue id
  | TFalse id
  | TNullToken id
  | TBlank id
  | TPlaceholder (_, id)
  | TPartial (id, _)
  | TPartialGhost (id, _)
  | TLetKeyword id
  | TLetAssignment id
  | TLetLHS (id, _)
  | TString (id, _)
  | TIfKeyword id
  | TIfThenKeyword id
  | TIfElseKeyword id
  | TBinOp (id, _)
  | TFieldOp id
  | TFieldName (id, _)
  | TVariable (id, _)
  | TFnName (id, _, _)
  | TLambdaVar (id, _, _)
  | TLambdaArrow id
  | TLambdaSymbol id
  | TLambdaSep id
  | TListOpen id
  | TListClose id
  | TListSep id
  | TThreadPipe (id, _)
  | TRecordOpen id
  | TRecordClose id
  | TRecordField (id, _, _)
  | TRecordSep (id, _)
  | TMatchSep id
  | TMatchKeyword id
  | TConstructorName (id, _)
  | TPatternBlank (_, id)
  | TPatternInteger (_, id, _)
  | TPatternVariable (_, id, _)
  | TPatternConstructorName (_, id, _)
  | TPatternString (_, id, _)
  | TPatternTrue (_, id)
  | TPatternFalse (_, id)
  | TPatternNullToken (_, id)
  | TPatternFloatWhole (_, id, _)
  | TPatternFloatPoint (_, id)
  | TPatternFloatFraction (_, id, _) ->
      id
  | TSep | TNewline | TIndented _ | TIndent _ | TIndentToHere _ ->
      ID "no-id"


let isTextToken token : bool =
  match token with
  | TInteger _
  | TLetLHS _
  | TBinOp _
  | TFieldName _
  | TVariable _
  | TConstructorName _
  | TFnName _
  | TBlank _
  | TPlaceholder _
  | TPartial _
  | TPartialGhost _
  | TRecordField _
  | TString _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TLambdaVar _
  | TFloatWhole _
  | TFloatPoint _
  | TFloatFraction _
  | TPatternInteger _
  | TPatternVariable _
  | TPatternConstructorName _
  | TPatternBlank _
  | TPatternString _
  | TPatternTrue _
  | TPatternFalse _
  | TPatternNullToken _
  | TPatternFloatWhole _
  | TPatternFloatPoint _
  | TPatternFloatFraction _ ->
      true
  | TListOpen _
  | TListClose _
  | TListSep _
  | TSep
  | TLetKeyword _
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TLetAssignment _
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TFieldOp _
  | TNewline
  | TIndented _
  | TIndentToHere _
  | TIndent _
  | TLambdaSymbol _
  | TLambdaSep _
  | TMatchKeyword _
  | TMatchSep _
  | TThreadPipe _
  | TLambdaArrow _ ->
      false


(* if the cursor is at the end of this token, we take it as editing this
* token, rather than writing the next token. *)
let isAppendable token : bool =
  match token with
  (* String should really be directly editable, but the extra quote at the end
   makes it not so. *)
  | TString _ | TPatternString _ ->
      false
  | _ ->
      isTextToken token


let isBlank t =
  match t with
  | TBlank _
  | TPlaceholder _
  | TRecordField (_, _, "")
  | TVariable (_, "")
  | TFieldName (_, "")
  | TLetLHS (_, "")
  | TLambdaVar (_, _, "")
  | TPartial (_, "")
  | TPatternBlank _ ->
      true
  | _ ->
      false


let isKeyword (t : token) =
  match t with
  | TLetKeyword _
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TMatchKeyword _ ->
      true
  | _ ->
      false


let isSkippable (token : token) : bool =
  match token with TIndent _ -> true | _ -> false


let isAtom (t : token) : bool =
  match t with
  | TMatchSep _ | TThreadPipe _ | TLambdaArrow _ ->
      true
  | _ ->
      isKeyword t || isBlank t


let isAutocompletable (t : token) : bool =
  match t with
  | TBlank _
  | TPlaceholder _
  | TPartial _
  | TPatternBlank _
  (* since patterns have no partial but commit as variables
   * automatically, allow intermediate variables to
   * be autocompletable to other expressions *)
  | TPatternVariable _ ->
      true
  | _ ->
      false


let toText (t : token) : string =
  let shouldntBeEmpty name =
    if name = ""
    then (
      Js.log2 "shouldn't be empty" (show_fluidToken t) ;
      "   " )
    else name
  in
  let canBeEmpty name = if name = "" then "   " else name in
  match t with
  | TInteger (_, i) ->
      shouldntBeEmpty i
  | TFloatWhole (_, w) ->
      shouldntBeEmpty w
  | TFloatPoint _ ->
      "."
  | TFloatFraction (_, f) ->
      f
  | TString (_, str) ->
      "\"" ^ str ^ "\""
  | TTrue _ ->
      "true"
  | TFalse _ ->
      "false"
  | TNullToken _ ->
      "null"
  | TBlank _ ->
      "   "
  | TPlaceholder ((name, tipe), _) ->
      " " ^ name ^ " : " ^ tipe ^ " "
  | TPartial (_, str) ->
      canBeEmpty str
  | TPartialGhost (_, str) ->
      shouldntBeEmpty str
  | TSep ->
      " "
  | TNewline ->
      "\n"
  | TLetKeyword _ ->
      "let "
  | TLetAssignment _ ->
      " = "
  | TLetLHS (_, name) ->
      canBeEmpty name
  | TIfKeyword _ ->
      "if "
  | TIfThenKeyword _ ->
      "then"
  | TIfElseKeyword _ ->
      "else"
  | TBinOp (_, op) ->
      shouldntBeEmpty op
  | TFieldOp _ ->
      "."
  | TFieldName (_, name) ->
      canBeEmpty name
  | TVariable (_, name) ->
      canBeEmpty name
  | TFnName (_, name, _) ->
      shouldntBeEmpty name
  | TLambdaVar (_, _, name) ->
      canBeEmpty name
  | TLambdaSymbol _ ->
      "\\"
  | TLambdaSep _ ->
      ", "
  | TLambdaArrow _ ->
      " -> "
  | TIndent indent ->
      shouldntBeEmpty (Caml.String.make indent ' ')
  (* We dont want this to be transparent, so have these make their presence
   * known *)
  | TIndented _ ->
      "TIndented"
  | TIndentToHere _ ->
      "TIndentToHere"
  | TListOpen _ ->
      "["
  | TListClose _ ->
      "]"
  | TListSep _ ->
      ","
  | TRecordOpen _ ->
      "{"
  | TRecordClose _ ->
      "}"
  | TRecordField (_, _, name) ->
      canBeEmpty name
  | TRecordSep _ ->
      ":"
  | TConstructorName (_, name) ->
      canBeEmpty name
  | TThreadPipe _ ->
      "|>"
  | TMatchKeyword _ ->
      "match "
  | TMatchSep _ ->
      "->"
  | TPatternInteger (_, _, i) ->
      shouldntBeEmpty i
  | TPatternFloatWhole (_, _, w) ->
      shouldntBeEmpty w
  | TPatternFloatPoint _ ->
      "."
  | TPatternFloatFraction (_, _, f) ->
      f
  | TPatternString (_, _, str) ->
      "\"" ^ str ^ "\""
  | TPatternTrue _ ->
      "true"
  | TPatternFalse _ ->
      "false"
  | TPatternNullToken _ ->
      "null"
  | TPatternBlank _ ->
      "   "
  | TPatternVariable (_, _, name) ->
      canBeEmpty name
  | TPatternConstructorName (_, _, name) ->
      canBeEmpty name


let toTestText (t : token) : string =
  match t with
  | TPlaceholder _ | TBlank _ ->
      "___"
  | TPartialGhost (_, str) ->
    ( match String.length str with
    | 0 ->
        "@EMPTY@"
    | 1 ->
        "@"
    | 2 ->
        "@@"
    | _ ->
        let str =
          str |> String.dropLeft ~count:1 |> String.dropRight ~count:1
        in
        "@" ^ str ^ "@" )
  | _ ->
      if isBlank t then "***" else toText t


let toTypeName (t : token) : string =
  match t with
  | TInteger _ ->
      "integer"
  | TFloatWhole _ ->
      "float-whole"
  | TFloatPoint _ ->
      "float-point"
  | TFloatFraction _ ->
      "float-fraction"
  | TString (_, _) ->
      "string"
  | TTrue _ ->
      "true"
  | TFalse _ ->
      "false"
  | TNullToken _ ->
      "null"
  | TBlank _ ->
      "blank"
  | TPlaceholder _ ->
      "placeholder"
  | TPartial _ ->
      "partial"
  | TPartialGhost _ ->
      "partial-ghost"
  | TLetKeyword _ ->
      "let-keyword"
  | TLetAssignment _ ->
      "let-assignment"
  | TLetLHS _ ->
      "let-lhs"
  | TSep ->
      "sep"
  | TIndented _ ->
      "indented"
  | TIndentToHere _ ->
      "indent-to-here"
  | TIndent _ ->
      "indent"
  | TNewline ->
      "newline"
  | TIfKeyword _ ->
      "if-keyword"
  | TIfThenKeyword _ ->
      "if-then-keyword"
  | TIfElseKeyword _ ->
      "if-else-keyword"
  | TBinOp _ ->
      "binop"
  | TFieldOp _ ->
      "field-op"
  | TFieldName _ ->
      "field-name"
  | TVariable _ ->
      "variable"
  | TFnName (_, _, _) ->
      "fn-name"
  | TLambdaVar (_, _, _) ->
      "lambda-var"
  | TLambdaSymbol _ ->
      "lambda-symbol"
  | TLambdaArrow _ ->
      "lambda-arrow"
  | TLambdaSep _ ->
      "lambda-sep"
  | TListOpen _ ->
      "list-open"
  | TListClose _ ->
      "list-close"
  | TListSep _ ->
      "list-sep"
  | TRecordOpen _ ->
      "record-open"
  | TRecordClose _ ->
      "record-close"
  | TRecordField _ ->
      "record-field"
  | TRecordSep _ ->
      "record-sep"
  | TConstructorName _ ->
      "constructor-name"
  | TThreadPipe _ ->
      "thread-pipe"
  | TMatchKeyword _ ->
      "match-keyword"
  | TMatchSep _ ->
      "match-sep"
  | TPatternBlank _ ->
      "pattern-blank"
  | TPatternInteger _ ->
      "pattern-integer"
  | TPatternVariable _ ->
      "pattern-variable"
  | TPatternConstructorName _ ->
      "pattern-constructor-name"
  | TPatternString _ ->
      "pattern-string"
  | TPatternTrue _ ->
      "pattern-true"
  | TPatternFalse _ ->
      "pattern-false"
  | TPatternNullToken _ ->
      "pattern-null"
  | TPatternFloatWhole _ ->
      "pattern-float-whole"
  | TPatternFloatPoint _ ->
      "pattern-float-point"
  | TPatternFloatFraction _ ->
      "pattern-float-fraction"


let toCategoryName (t : token) : string =
  match t with
  | TInteger _ | TString _ ->
      "literal"
  | TVariable _
  | TNewline
  | TSep
  | TBlank _
  | TPartial _
  | TPlaceholder _
  | TPartialGhost _ ->
      ""
  | TFloatWhole _ | TFloatPoint _ | TFloatFraction _ ->
      "float"
  | TTrue _ | TFalse _ ->
      "boolean"
  | TNullToken _ ->
      "null"
  | TFnName _ | TBinOp _ ->
      "function"
  | TLetKeyword _ | TLetAssignment _ | TLetLHS _ ->
      "let"
  | TIndented _ | TIndentToHere _ | TIndent _ ->
      "indent"
  | TIfKeyword _ | TIfThenKeyword _ | TIfElseKeyword _ ->
      "if"
  | TFieldOp _ | TFieldName _ ->
      "field"
  | TLambdaVar _ | TLambdaSymbol _ | TLambdaArrow _ | TLambdaSep _ ->
      "lambda"
  | TListOpen _ | TListClose _ | TListSep _ ->
      "list"
  | TThreadPipe _ ->
      "thread"
  | TConstructorName _ ->
      "constructor"
  | TRecordOpen _ | TRecordClose _ | TRecordField _ | TRecordSep _ ->
      "record"
  | TMatchKeyword _ | TMatchSep _ ->
      "match"
  | TPatternBlank _
  | TPatternInteger _
  | TPatternVariable _
  | TPatternConstructorName _
  | TPatternString _
  | TPatternTrue _
  | TPatternFalse _
  | TPatternNullToken _
  | TPatternFloatWhole _
  | TPatternFloatPoint _
  | TPatternFloatFraction _ ->
      "pattern"


let toCssClasses (t : token) : string list =
  let empty = if isBlank t then Some "fluid-empty" else None in
  let keyword = if isKeyword t then Some "fluid-keyword" else None in
  let typename = Some ("fluid-" ^ toTypeName t) in
  let category =
    let name = toCategoryName t in
    if name = "" then None else Some ("fluid-" ^ name)
  in
  [empty; keyword; typename; category] |> List.filterMap ~f:identity


let show_tokenInfo (ti : tokenInfo) =
  Printf.sprintf
    "(%d, %d), '%s', %s (%s)"
    ti.startPos
    ti.endPos
    (* ti.length *)
    (toText ti.token)
    (tid ti.token |> deID)
    (toTypeName ti.token)
