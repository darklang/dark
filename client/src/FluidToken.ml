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
  | TLambdaVar (id, _)
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
  | TPatternBlank id
  | TPatternPartial (id, _)
  | TPatternInteger (id, _)
  | TPatternVariable (id, _)
  | TPatternConstructorName (id, _)
  | TPatternString (id, _)
  | TPatternTrue id
  | TPatternFalse id
  | TPatternNullToken id
  | TPatternFloatWhole (id, _)
  | TPatternFloatPoint id
  | TPatternFloatFraction (id, _) ->
      id
  | TSep | TNewline | TIndented _ | TIndent _ | TIndentToHere _ ->
      ID "no-id"


let isBlank t =
  match t with
  | TBlank _
  | TPlaceholder _
  | TRecordField (_, _, "")
  | TFieldName (_, "")
  | TLetLHS (_, "")
  | TLambdaVar (_, "")
  | TPartial (_, "")
  | TPatternPartial (_, "")
  | TPatternBlank _ ->
      true
  | _ ->
      false


let isAutocompletable (t : token) : bool =
  match t with TBlank _ | TPlaceholder _ | TPartial _ -> true | _ -> false


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
  | TLambdaVar (_, name) ->
      canBeEmpty name
  | TLambdaSymbol _ ->
      "\\"
  | TLambdaSep _ ->
      " "
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
  | TPatternInteger (_, i) ->
      shouldntBeEmpty i
  | TPatternFloatWhole (_, w) ->
      shouldntBeEmpty w
  | TPatternFloatPoint _ ->
      "."
  | TPatternFloatFraction (_, f) ->
      f
  | TPatternString (_, str) ->
      "\"" ^ str ^ "\""
  | TPatternTrue _ ->
      "true"
  | TPatternFalse _ ->
      "false"
  | TPatternNullToken _ ->
      "null"
  | TPatternBlank _ ->
      "   "
  | TPatternPartial (_, str) ->
      canBeEmpty str
  | TPatternVariable (_, name) ->
      canBeEmpty name
  | TPatternConstructorName (_, name) ->
      canBeEmpty name


let toTestText (t : token) : string =
  match t with
  | TPatternBlank _ | TBlank _ ->
      "___"
  | TPatternPartial (_, str) | TPartial (_, str) ->
      str
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
  | TLambdaVar (_, _) ->
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
  | TPatternPartial _ ->
      "pattern-partial"
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
  | TVariable _ | TNewline | TSep | TBlank _ | TPartial _ | TPlaceholder _ ->
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
  | TPatternPartial _
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


let toCssClasses (t : token) : string =
  let keyword =
    match t with
    | TLetKeyword _
    | TIfKeyword _
    | TIfThenKeyword _
    | TIfElseKeyword _
    | TMatchKeyword _ ->
        "fluid-keyword"
    | _ ->
        ""
  in
  let empty =
    match t with
    | TLetLHS (_, "")
    | TFieldName (_, "")
    | TLambdaVar (_, "")
    | TPlaceholder _
    | TPatternBlank _
    | TBlank _
    | TRecordField (_, _, "") ->
        "fluid-empty"
    | _ ->
        ""
  in
  String.trim (keyword ^ " " ^ empty)
  ^ " fluid-"
  ^ toCategoryName t
  ^ " fluid-"
  ^ toTypeName t


let show_tokenInfo (ti : tokenInfo) =
  Printf.sprintf
    "(%d, %d), '%s', %s (%s)"
    ti.startPos
    ti.endPos
    (* ti.length *)
    (toText ti.token)
    (tid ti.token |> deID)
    (toTypeName ti.token)
