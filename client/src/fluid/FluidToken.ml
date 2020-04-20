open Prelude

type t = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let fakeid = ID.fromString "fake-id"

let tid (t : t) : ID.t =
  match t with
  | TInteger (id, _, _)
  | TFloatWhole (id, _, _)
  | TFloatPoint (id, _)
  | TFloatFractional (id, _, _)
  | TTrue (id, _)
  | TFalse (id, _)
  | TNullToken (id, _)
  | TBlank (id, _)
  | TPlaceholder {blankID = id; _}
  | TPartial (id, _, _)
  | TLeftPartial (id, _)
  | TRightPartial (id, _, _)
  | TPartialGhost (id, _, _)
  | TLetKeyword (id, _, _)
  | TLetAssignment (id, _, _)
  | TLetVarName (id, _, _, _)
  | TString (id, _, _)
  | TStringMLStart (id, _, _, _)
  | TStringMLMiddle (id, _, _, _)
  | TStringMLEnd (id, _, _, _)
  | TIfKeyword (id, _)
  | TIfThenKeyword (id, _)
  | TIfElseKeyword (id, _)
  | TBinOp (id, _, _)
  | TFieldOp (id, _, _)
  | TFieldName (id, _, _, _)
  | TFieldPartial (id, _, _, _, _)
  | TVariable (id, _, _)
  | TFnName (id, _, _, _, _)
  | TFnVersion (id, _, _, _)
  | TLambdaVar (id, _, _, _, _)
  | TLambdaArrow (id, _)
  | TLambdaSymbol (id, _)
  | TLambdaComma (id, _, _)
  | TListOpen id
  | TListClose id
  | TListComma (id, _)
  | TPipe (id, _, _, _)
  | TRecordOpen id
  | TRecordClose id
  | TRecordFieldname {recordID = id; _}
  | TRecordSep (id, _, _)
  | TConstructorName (id, _)
  | TMatchBranchArrow {matchID = id; _}
  | TMatchKeyword id
  | TPatternBlank (_, id, _)
  | TPatternInteger (_, id, _, _)
  | TPatternVariable (_, id, _, _)
  | TPatternConstructorName (_, id, _, _)
  | TPatternString {patternID = id; _}
  | TPatternTrue (_, id, _)
  | TPatternFalse (_, id, _)
  | TPatternNullToken (_, id, _)
  | TPatternFloatWhole (_, id, _, _)
  | TPatternFloatPoint (_, id, _)
  | TPatternFloatFractional (_, id, _, _)
  | TSep (id, _)
  | TParenOpen id
  | TParenClose id
  | TFlagWhenKeyword id
  | TFlagEnabledKeyword id
  | TNewline (Some (_, id, _)) ->
      id
  | TNewline None | TIndent _ ->
      fakeid


let analysisID (t : t) : ID.t =
  match t with
  | TLetVarName (_, id, _, _)
  | TLetKeyword (_, id, _)
  | TLetAssignment (_, id, _)
  | TRecordFieldname {exprID = id; _}
  | TLambdaVar (_, id, _, _, _)
  | TRecordSep (_, _, id)
  | TMatchBranchArrow {patternID = id; _} ->
      id
  | _ ->
      tid t


(* TODO(alice) merge these two functions? *)
let parentExprID (t : t) : ID.t =
  match t with TNewline (Some (_, id, _)) -> id | _ -> tid t


let parentBlockID (t : t) : ID.t option =
  match t with
  | TStringMLStart (id, _, _, _)
  | TStringMLMiddle (id, _, _, _)
  | TStringMLEnd (id, _, _, _)
  | TListOpen id
  | TListClose id
  | TListComma (id, _)
  | TRecordOpen id
  | TRecordSep (id, _, _)
  | TRecordClose id ->
      Some id
  | TBlank (_, pid)
  | TInteger (_, _, pid)
  | TString (_, _, pid)
  | TTrue (_, pid)
  | TFalse (_, pid)
  | TNullToken (_, pid)
  | TFloatWhole (_, _, pid)
  | TFloatPoint (_, pid)
  | TFloatFractional (_, _, pid)
  | TPartial (_, _, pid)
  | TRightPartial (_, _, pid)
  | TPartialGhost (_, _, pid)
  | TLetKeyword (_, _, pid)
  | TLetVarName (_, _, _, pid)
  | TLetAssignment (_, _, pid)
  | TIfKeyword (_, pid)
  | TIfThenKeyword (_, pid)
  | TIfElseKeyword (_, pid)
  | TBinOp (_, _, pid)
  | TFieldOp (_, _, pid)
  | TFieldName (_, _, _, pid)
  | TFieldPartial (_, _, _, _, pid)
  | TVariable (_, _, pid)
  | TLambdaComma (_, _, pid)
  | TLambdaArrow (_, pid)
  | TLambdaSymbol (_, pid)
  | TLambdaVar (_, _, _, _, pid)
  | TPipe (_, _, _, pid)
  | TSep (_, pid) ->
      pid
  | TRecordFieldname d ->
      d.parentBlockID
  | TNewline (Some (_, id, _)) ->
      Some id
  | TFnName _
  | TFnVersion _
  | TMatchKeyword _
  | TMatchBranchArrow _
  | TPatternVariable _
  | TPatternConstructorName _
  | TPatternInteger _
  | TPatternString _
  | TPatternTrue _
  | TPatternFalse _
  | TPatternNullToken _
  | TPatternFloatWhole _
  | TPatternFloatPoint _
  | TPatternFloatFractional _
  | TPatternBlank _
  | TConstructorName _
  | TParenOpen _
  | TParenClose _
  | TFlagWhenKeyword _
  | TFlagEnabledKeyword _
  | TNewline None
  | TIndent _
  | TPlaceholder _ ->
      None


let validID id = id <> fakeid

(* Tokens that are 'editable' are considered text tokens
 * If the cursor is to the left of a text token, then pressing a character
 * will append to the end of that token *)
let isTextToken t : bool =
  match t with
  | TInteger _
  | TLetVarName _
  | TBinOp _
  | TFieldName _
  | TFieldPartial _
  | TVariable _
  | TConstructorName _
  | TFnName _
  | TFnVersion _
  | TBlank _
  | TPlaceholder _
  | TPartial _
  | TRightPartial _
  | TLeftPartial _
  | TPartialGhost _
  | TRecordFieldname _
  | TString _
  | TStringMLStart _
  | TStringMLMiddle _
  | TStringMLEnd _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TLambdaVar _
  | TFloatWhole _
  | TFloatPoint _
  | TFloatFractional _
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
  | TPatternFloatFractional _ ->
      true
  | TListOpen _
  | TListClose _
  | TListComma (_, _)
  | TSep _
  | TLetKeyword _
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TLetAssignment _
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TFieldOp _
  | TNewline _
  | TIndent _
  | TLambdaSymbol _
  | TLambdaComma _
  | TMatchKeyword _
  | TMatchBranchArrow _
  | TPipe _
  | TLambdaArrow _
  | TParenOpen _
  | TParenClose _
  | TFlagWhenKeyword _
  | TFlagEnabledKeyword _ ->
      false


let isStringToken t : bool =
  match t with
  | TStringMLStart _ | TStringMLMiddle _ | TStringMLEnd _ | TString _ ->
      true
  | _ ->
      false


(* if the cursor is at the end of this token, we take it as editing this
* token, rather than writing the next token. *)
let isAppendable t : bool =
  match t with
  (* String should really be directly editable, but the extra quote at the end
   makes it not so; since there's no quote at the end of TStringMLStart or
   TStringMLMiddle, then they are appendable *)
  | TString _ | TPatternString _ | TStringMLEnd _ ->
      false
  | _ ->
      isTextToken t


let isBlank t =
  match t with
  | TBlank _
  | TPlaceholder _
  | TRecordFieldname {fieldName = ""; _}
  | TVariable (_, "", _)
  | TFieldName (_, _, "", _)
  | TFieldPartial (_, _, _, "", _)
  | TLetVarName (_, _, "", _)
  | TLambdaVar (_, _, _, "", _)
  | TPartial (_, "", _)
  | TRightPartial (_, "", _)
  | TLeftPartial (_, "")
  | TPatternBlank _ ->
      true
  | _ ->
      false


let isKeyword (t : t) =
  match t with
  | TLetKeyword _
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TMatchKeyword _
  | TFlagWhenKeyword _
  | TFlagEnabledKeyword _ ->
      true
  | _ ->
      false


let isSkippable (t : t) : bool = match t with TIndent _ -> true | _ -> false

let isAtom (t : t) : bool =
  match t with
  | TMatchBranchArrow _ | TPipe _ | TLambdaArrow _ ->
      true
  | _ ->
      isKeyword t || isBlank t


let isNewline (t : t) : bool = match t with TNewline _ -> true | _ -> false

let isLet (t : t) : bool =
  match t with TLetAssignment _ | TLetVarName _ -> true | _ -> false


let isAutocompletable (t : t) : bool =
  match t with
  | TBlank _
  | TPlaceholder _
  | TPartial _
  | TFieldPartial _
  | TRightPartial _
  | TLeftPartial _
  | TPatternBlank _
  (* since patterns have no partial but commit as variables
   * automatically, allow intermediate variables to
   * be autocompletable to other expressions *)
  | TPatternVariable _ ->
      true
  | _ ->
      false


(* Is this token something we can highlight as DError or DIncomplete? *)
let isErrorDisplayable (t : t) : bool =
  isTextToken t && match t with TFnVersion _ -> false | _ -> true


let isFieldPartial (t : t) : bool =
  match t with TFieldPartial _ -> true | _ -> false


let isMutlilineString (t : fluidToken) : bool =
  match t with
  | TStringMLStart _ | TStringMLMiddle _ | TStringMLEnd _ ->
      true
  | _ ->
      false


let isListSymbol (t : fluidToken) : bool =
  match t with TListOpen _ | TListClose _ | TListComma _ -> true | _ -> false


let toText (t : t) : string =
  let shouldntBeEmpty name =
    if name = ""
    then asserT ~debug:(show_fluidToken t) "shouldn't be empty" (name <> "") ;
    name
  in
  let canBeEmpty name = if name = "" then "   " else name in
  match t with
  | TInteger (_, i, _) ->
      shouldntBeEmpty i
  | TFloatWhole (_, w, _) ->
      shouldntBeEmpty w
  | TFloatPoint _ ->
      "."
  | TFloatFractional (_, f, _) ->
      f
  | TString (_, str, _) ->
      "\"" ^ str ^ "\""
  | TStringMLStart (_, str, _, _) ->
      "\"" ^ str
  | TStringMLMiddle (_, str, _, _) ->
      str
  | TStringMLEnd (_, str, _, _) ->
      str ^ "\""
  | TTrue _ ->
      "true"
  | TFalse _ ->
      "false"
  | TNullToken _ ->
      "null"
  | TBlank _ ->
      "   "
  | TPlaceholder {placeholder = {name; tipe}; _} ->
      " " ^ name ^ " : " ^ tipe ^ " "
  | TPartial (_, str, _) ->
      shouldntBeEmpty str
  | TRightPartial (_, str, _) ->
      shouldntBeEmpty str
  | TLeftPartial (_, str) ->
      shouldntBeEmpty str
  | TPartialGhost (_, str, _) ->
      shouldntBeEmpty str
  | TSep _ ->
      " "
  | TNewline _ ->
      "\n"
  | TLetKeyword _ ->
      "let "
  | TLetAssignment _ ->
      " = "
  | TLetVarName (_, _, name, _) ->
      canBeEmpty name
  | TIfKeyword _ ->
      "if "
  | TIfThenKeyword _ ->
      "then"
  | TIfElseKeyword _ ->
      "else"
  | TBinOp (_, op, _) ->
      shouldntBeEmpty op
  | TFieldOp _ ->
      "."
  | TFieldPartial (_, _, _, name, _) ->
      canBeEmpty name
  | TFieldName (_, _, name, _) ->
      (* Although we typically use TFieldPartial for empty fields, when
       * there's a new field we won't have a fieldname for it. *)
      canBeEmpty name
  | TVariable (_, name, _) ->
      canBeEmpty name
  | TFnName (_, _, displayName, _, _) | TFnVersion (_, _, displayName, _)
    ->
      shouldntBeEmpty displayName
  | TLambdaVar (_, _, _, name, _) ->
      canBeEmpty name
  | TLambdaSymbol _ ->
      "\\"
  | TLambdaComma _ ->
      ","
  | TLambdaArrow _ ->
      " -> "
  | TIndent indent ->
      shouldntBeEmpty (Caml.String.make indent ' ')
  (* We dont want this to be transparent, so have these make their presence
   * known *)
  | TListOpen _ ->
      "["
  | TListClose _ ->
      "]"
  | TListComma (_, _) ->
      ","
  | TRecordOpen _ ->
      "{"
  | TRecordClose _ ->
      "}"
  | TRecordFieldname f ->
      canBeEmpty f.fieldName
  | TRecordSep _ ->
      " : "
  | TConstructorName (_, name) ->
      canBeEmpty name
  | TPipe _ ->
      "|>"
  | TMatchKeyword _ ->
      "match "
  | TMatchBranchArrow _ ->
      " -> "
  | TPatternInteger (_, _, i, _) ->
      shouldntBeEmpty i
  | TPatternFloatWhole (_, _, w, _) ->
      shouldntBeEmpty w
  | TPatternFloatPoint _ ->
      "."
  | TPatternFloatFractional (_, _, f, _) ->
      f
  | TPatternString {str; _} ->
      "\"" ^ str ^ "\""
  | TPatternTrue _ ->
      "true"
  | TPatternFalse _ ->
      "false"
  | TPatternNullToken _ ->
      "null"
  | TPatternBlank _ ->
      "   "
  | TPatternVariable (_, _, name, _) ->
      canBeEmpty name
  | TPatternConstructorName (_, _, name, _) ->
      canBeEmpty name
  | TParenOpen _ ->
      "("
  | TParenClose _ ->
      ")"
  | TFlagWhenKeyword _ ->
      "when "
  | TFlagEnabledKeyword _ ->
      "enabled"


let toTestText (t : t) : string =
  let result =
    match t with
    | TPlaceholder {placeholder = {name; tipe}; _} ->
        let count = 1 + String.length name + 3 + String.length tipe + 1 in
        Caml.String.make count '_'
    | TBlank _ ->
        "___"
    | TPartialGhost (_, str, _) ->
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
  in
  asserT
    ~debug:t
    "wrong length toTestText"
    (String.length result = String.length (toText t)) ;
  result


let toIndex (t : t) : int option =
  match t with
  | TStringMLMiddle (_, _, index, _)
  | TLambdaVar (_, _, index, _, _)
  | TLambdaComma (_, index, _)
  | TPipe (_, _, index, _)
  | TRecordFieldname {index; _}
  | TRecordSep (_, index, _)
  | TListComma (_, index)
  | TNewline (Some (_, _, Some index))
  | TPatternBlank (_, _, index)
  | TPatternInteger (_, _, _, index)
  | TPatternVariable (_, _, _, index)
  | TPatternConstructorName (_, _, _, index)
  | TPatternString {branchIdx = index; _}
  | TPatternTrue (_, _, index)
  | TPatternFalse (_, _, index)
  | TPatternNullToken (_, _, index)
  | TPatternFloatWhole (_, _, _, index)
  | TPatternFloatPoint (_, _, index)
  | TPatternFloatFractional (_, _, _, index) ->
      Some index
  | _ ->
      None


let toParentID (t : t) : ID.t option =
  match t with
  | TRecordFieldname {recordID = id; _}
  | TPatternBlank (id, _, _)
  | TPatternInteger (id, _, _, _)
  | TPatternVariable (id, _, _, _)
  | TPatternConstructorName (id, _, _, _)
  | TPatternString {matchID = id; _}
  | TPatternTrue (id, _, _)
  | TPatternFalse (id, _, _)
  | TPatternNullToken (id, _, _)
  | TPatternFloatWhole (id, _, _, _)
  | TPatternFloatPoint (id, _, _)
  | TPatternFloatFractional (id, _, _, _) ->
      Some id
  | _ ->
      None


let toTypeName (t : t) : string =
  match t with
  | TInteger _ ->
      "integer"
  | TFloatWhole _ ->
      "float-whole"
  | TFloatPoint _ ->
      "float-point"
  | TFloatFractional _ ->
      "float-fractional"
  | TString _ ->
      "string"
  | TStringMLStart _ ->
      "string-ml-start"
  | TStringMLMiddle _ ->
      "string-ml-middle"
  | TStringMLEnd _ ->
      "string-ml-end"
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
  | TRightPartial _ ->
      "partial-right"
  | TLeftPartial _ ->
      "partial-left"
  | TPartialGhost _ ->
      "partial-ghost"
  | TLetKeyword _ ->
      "let-keyword"
  | TLetAssignment _ ->
      "let-assignment"
  | TLetVarName _ ->
      "let-var-name"
  | TSep _ ->
      "sep"
  | TIndent _ ->
      "indent"
  | TNewline _ ->
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
  | TFieldPartial _ ->
      "field-partial"
  | TVariable _ ->
      "variable"
  | TFnName _ ->
      "fn-name"
  | TFnVersion _ ->
      "fn-version"
  | TLambdaVar _ ->
      "lambda-var"
  | TLambdaSymbol _ ->
      "lambda-symbol"
  | TLambdaArrow _ ->
      "lambda-arrow"
  | TLambdaComma _ ->
      "lambda-comma"
  | TListOpen _ ->
      "list-open"
  | TListClose _ ->
      "list-close"
  | TListComma (_, _) ->
      "list-comma"
  | TRecordOpen _ ->
      "record-open"
  | TRecordClose _ ->
      "record-close"
  | TRecordFieldname _ ->
      "record-fieldname"
  | TRecordSep _ ->
      "record-sep"
  | TConstructorName _ ->
      "constructor-name"
  | TPipe _ ->
      "pipe-symbol"
  | TMatchKeyword _ ->
      "match-keyword"
  | TMatchBranchArrow _ ->
      "match-branch-arrow"
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
  | TPatternFloatFractional _ ->
      "pattern-float-fractional"
  | TParenOpen _ ->
      "paren-open"
  | TParenClose _ ->
      "paren-close"
  | TFlagWhenKeyword _ ->
      "ff-cond"
  | TFlagEnabledKeyword _ ->
      "ff-enabled"


let toCategoryName (t : t) : string =
  match t with
  | TInteger _ ->
      "integer"
  | TString _ | TStringMLStart _ | TStringMLMiddle _ | TStringMLEnd _ ->
      "string"
  | TVariable _ | TNewline _ | TSep _ | TBlank _ | TPlaceholder _ ->
      ""
  | TPartial _ | TRightPartial _ | TLeftPartial _ | TPartialGhost _ ->
      "partial"
  | TFloatWhole _ | TFloatPoint _ | TFloatFractional _ ->
      "float"
  | TTrue _ | TFalse _ ->
      "boolean"
  | TNullToken _ ->
      "null"
  | TFnName _ | TFnVersion _ | TBinOp _ ->
      "function"
  | TLetKeyword _ | TLetAssignment _ | TLetVarName _ ->
      "let"
  | TIndent _ ->
      "indent"
  | TIfKeyword _ | TIfThenKeyword _ | TIfElseKeyword _ ->
      "if"
  | TFieldOp _ | TFieldName _ | TFieldPartial _ ->
      "field"
  | TLambdaVar _ | TLambdaSymbol _ | TLambdaArrow _ | TLambdaComma _ ->
      "lambda"
  | TListOpen _ | TListClose _ | TListComma _ ->
      "list"
  | TPipe _ ->
      "pipe"
  | TConstructorName _ ->
      "constructor"
  | TRecordOpen _ | TRecordClose _ | TRecordFieldname _ | TRecordSep _ ->
      "record"
  | TMatchKeyword _ | TMatchBranchArrow _ ->
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
  | TPatternFloatFractional _ ->
      "pattern"
  | TParenOpen _ | TParenClose _ ->
      "paren"
  | TFlagWhenKeyword _ | TFlagEnabledKeyword _ ->
      "flag"

let toValue (t: t) : string =
    match t with
    | TInteger ( _ , v , _)
    | TString ( _ , v , _)
    | TStringMLStart ( _ , v , _ , _)
    | TStringMLMiddle (_, v , _ , _)
    | TStringMLEnd ( _ , v , _ , _)
    | TFloatWhole ( _ , v , _)
    | TFloatFractional ( _ , v , _)
    | TPartial ( _ , v , _)
    | TRightPartial ( _ , v , _)
    | TPartialGhost ( _ , v , _)
    | TLetVarName ( _ , _ , v , _)
    | TBinOp ( _ , v , _)
    | TFieldName (  _ , _ , v , _)
    | TFieldPartial ( _ , _  , _ , v , _)
    | TVariable ( _ , v , _)
    | TFnName ( _ , v  , _ , _ , _)
    | TFnVersion ( _ , _ , v , _)
    | TLambdaVar ( _ , _ , _ , v , _)
    | TPatternVariable ( _ , _ , v , _)
    | TPatternConstructorName ( _ , _ , v , _)
    | TPatternInteger ( _ , _ , v , _)
    | TPatternFloatWhole ( _ , _ , v , _)
    | TPatternFloatFractional ( _ , _ , v , _)
    | TConstructorName ( _ , v) -> v

    | TPatternString r -> r.str
    | TRecordFieldname r -> r.fieldName
  
    | TRecordSep _ 
    | TRecordClose  _
    | TMatchKeyword _
    | TMatchBranchArrow _
    | TLetAssignment _
    | TIfKeyword _
    | TIfThenKeyword  _
    | TIfElseKeyword _
    | TFieldOp _
    | TLambdaComma _
    | TLambdaArrow _
    | TLambdaSymbol _
    | TRecordOpen _
    | TListOpen _
    | TListClose _
    | TListComma _
    | TPipe _
    | TIndent _
    | TLetKeyword _
    | TFloatPoint _
    | TSep _
    | TPatternFloatPoint _
    | TPatternBlank _
    | TBlank _
    | TPlaceholder _
    | TParenOpen _
    | TParenClose _
    | TFlagWhenKeyword _
    | TNewline _ 
    | TFlagEnabledKeyword _
  
    | TPatternTrue _ -> "ptrue"
    | TPatternFalse _ -> "pfalse"
    | TPatternNullToken _ -> "pnull"
    | TTrue _ -> "true"
    | TFalse _ -> "false"
    | TNullToken _ -> "null"

let toDebugInfo (t : t) : string =
  match t with
  | TStringMLStart (_, _, offset, _)
  | TStringMLMiddle (_, _, offset, _)
  | TStringMLEnd (_, _, offset, _) ->
      "offset=" ^ string_of_int offset
  | TNewline (Some (_, pid, Some idx)) ->
      "parent=" ^ ID.toString pid ^ " idx=" ^ string_of_int idx
  | TNewline (Some (_, pid, None)) ->
      "parent=" ^ ID.toString pid ^ " idx=none"
  | TNewline None ->
      "no parent"
  | TPipe (_, idx, len, _) ->
      Printf.sprintf "idx=%d len=%d" idx len
  | TMatchBranchArrow {index = idx; _} ->
      "idx=" ^ string_of_int idx
  | TPatternBlank (mid, _, idx)
  | TPatternInteger (mid, _, _, idx)
  | TPatternVariable (mid, _, _, idx)
  | TPatternConstructorName (mid, _, _, idx)
  | TPatternString {matchID = mid; branchIdx = idx; _}
  | TPatternTrue (mid, _, idx)
  | TPatternFalse (mid, _, idx)
  | TPatternNullToken (mid, _, idx)
  | TPatternFloatWhole (mid, _, _, idx)
  | TPatternFloatPoint (mid, _, idx)
  | TPatternFloatFractional (mid, _, _, idx) ->
      "match=" ^ ID.toString mid ^ " idx=" ^ string_of_int idx
  | _ ->
      ""


let toCssClasses (t : t) : string list =
  let empty = if isBlank t then ["fluid-empty"] else [] in
  let keyword = if isKeyword t then ["fluid-keyword"] else [] in
  let typename = ["fluid-" ^ toTypeName t] in
  let category =
    let name = toCategoryName t in
    if name = "" then [] else ["fluid-category-" ^ name]
  in
  empty @ keyword @ typename @ category


let show_tokenInfo (ti : tokenInfo) =
  Html.dl
    []
    [ Html.dt [] [Html.text "pos"]
    ; Html.dd [] [Html.text (Printf.sprintf "(%d, %d)" ti.startPos ti.endPos)]
    ; Html.dt [] [Html.text "len"]
    ; Html.dd [] [Html.text (Printf.sprintf "%d" ti.length)]
    ; Html.dt [] [Html.text "tok"]
    ; Html.dd [] [Html.text (toText ti.token)]
    ; Html.dt [] [Html.text "id"]
    ; Html.dd [] [Html.text (tid ti.token |> ID.toString)]
    ; Html.dt [] [Html.text "aid"]
    ; Html.dd [] [Html.text (analysisID ti.token |> ID.toString)]
    ; Html.dt [] [Html.text "type"]
    ; Html.dd [] [Html.text (toTypeName ti.token)]
    ; Html.dt [] [Html.text "debug"]
    ; Html.dd [] [Html.text (toDebugInfo ti.token)] ]


(* Since tokens don't have unique IDs, it is hard to look at two tokens streams
 * and find which tokens represent the same thing. You can use toText and ID,
 * but that doesn't work where the content has changed, which is a thing we
 * want to check for. *)
let matches (t1 : t) (t2 : t) : bool =
  tid t1 = tid t2
  && toTypeName t1 = toTypeName t2
  && toIndex t1 = toIndex t2
  && toValue t1 = toValue t2
  && t1 <> (* Matches too many things *) TNewline None
