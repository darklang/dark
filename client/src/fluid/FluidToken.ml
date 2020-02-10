open Prelude

type t = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let fakeid : id = ID "fake-id"

let tid (t : t) : id =
  match t with
  | TInteger (id, _)
  | TFloatWhole (id, _)
  | TFloatPoint id
  | TFloatFractional (id, _)
  | TTrue id
  | TFalse id
  | TNullToken id
  | TBlank id
  | TPlaceholder {blankID = id; _}
  | TPartial (id, _)
  | TRightPartial (id, _)
  | TPartialGhost (id, _)
  | TLetKeyword (id, _)
  | TLetAssignment (id, _)
  | TLetVarName (id, _, _)
  | TString (id, _)
  | TStringMLStart (id, _, _, _)
  | TStringMLMiddle (id, _, _, _)
  | TStringMLEnd (id, _, _, _)
  | TIfKeyword id
  | TIfThenKeyword id
  | TIfElseKeyword id
  | TBinOp (id, _)
  | TFieldOp (id, _)
  | TFieldName (id, _, _)
  | TFieldPartial (id, _, _, _)
  | TVariable (id, _)
  | TFnName (id, _, _, _, _)
  | TFnVersion (id, _, _, _)
  | TLambdaVar (id, _, _, _)
  | TLambdaArrow id
  | TLambdaSymbol id
  | TLambdaComma (id, _)
  | TListOpen id
  | TListClose id
  | TListComma (id, _)
  | TPipe (id, _, _)
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
  | TSep id
  | TParenOpen id
  | TParenClose id
  | TNewline (Some (id, _, _)) ->
      id
  | TNewline None | TIndent _ ->
      fakeid


let analysisID (t : t) : id =
  match t with
  | TLetVarName (_, id, _)
  | TLetKeyword (_, id)
  | TLetAssignment (_, id)
  | TRecordFieldname {exprID = id; _}
  | TLambdaVar (_, id, _, _)
  | TRecordSep (_, _, id)
  | TMatchBranchArrow {patternID = id; _} ->
      id
  | _ ->
      tid t


let parentExprID (t : t) : id =
  match t with TNewline (Some (_, id, _)) -> id | _ -> tid t


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
  | TParenClose _ ->
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
  | TVariable (_, "")
  | TFieldName (_, _, "")
  | TFieldPartial (_, _, _, "")
  | TLetVarName (_, _, "")
  | TLambdaVar (_, _, _, "")
  | TPartial (_, "")
  | TRightPartial (_, "")
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
  | TMatchKeyword _ ->
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


let toText (t : t) : string =
  let shouldntBeEmpty name =
    if name = ""
    then asserT ~debug:(show_fluidToken t) "shouldn't be empty" (name <> "") ;
    name
  in
  let canBeEmpty name = if name = "" then "   " else name in
  match t with
  | TInteger (_, i) ->
      shouldntBeEmpty i
  | TFloatWhole (_, w) ->
      shouldntBeEmpty w
  | TFloatPoint _ ->
      "."
  | TFloatFractional (_, f) ->
      f
  | TString (_, str) ->
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
  | TPartial (_, str) ->
      shouldntBeEmpty str
  | TRightPartial (_, str) ->
      shouldntBeEmpty str
  | TPartialGhost (_, str) ->
      shouldntBeEmpty str
  | TSep _ ->
      " "
  | TNewline _ ->
      "\n"
  | TLetKeyword _ ->
      "let "
  | TLetAssignment _ ->
      " = "
  | TLetVarName (_, _, name) ->
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
  | TFieldPartial (_, _, _, name) ->
      canBeEmpty name
  | TFieldName (_, _, name) ->
      (* Although we typically use TFieldPartial for empty fields, when
       * there's a new field we won't have a fieldname for it. *)
      canBeEmpty name
  | TVariable (_, name) ->
      canBeEmpty name
  | TFnName (_, _, displayName, _, _) | TFnVersion (_, _, displayName, _) ->
      shouldntBeEmpty displayName
  | TLambdaVar (_, _, _, name) ->
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


let toTestText (t : t) : string =
  let result =
    match t with
    | TPlaceholder {placeholder = {name; tipe}; _} ->
        let count = 1 + String.length name + 3 + String.length tipe + 1 in
        Caml.String.make count '_'
    | TBlank _ ->
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
  in
  asserT
    ~debug:t
    "wrong length toTestText"
    (String.length result = String.length (toText t)) ;
  result


let toIndex (t : t) : int option =
  match t with
  | TStringMLMiddle (_, _, index, _)
  | TLambdaVar (_, _, index, _)
  | TLambdaComma (_, index)
  | TPipe (_, _, index)
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


let toParentID (t : t) : id option =
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
  | TLambdaVar (_, _, _, _) ->
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


let toCategoryName (t : t) : string =
  match t with
  | TInteger _ ->
      "integer"
  | TString _ | TStringMLStart _ | TStringMLMiddle _ | TStringMLEnd _ ->
      "string"
  | TVariable _ | TNewline _ | TSep _ | TBlank _ | TPlaceholder _ ->
      ""
  | TPartial _ | TRightPartial _ | TPartialGhost _ ->
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


let toDebugInfo (t : t) : string =
  match t with
  | TStringMLStart (_, _, offset, _)
  | TStringMLMiddle (_, _, offset, _)
  | TStringMLEnd (_, _, offset, _) ->
      "offset=" ^ string_of_int offset
  | TNewline (Some (_, pid, Some idx)) ->
      "parent=" ^ deID pid ^ " idx=" ^ string_of_int idx
  | TNewline (Some (_, pid, None)) ->
      "parent=" ^ deID pid ^ " idx=none"
  | TNewline None ->
      "no parent"
  | TPipe (_, idx, len) ->
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
      "match=" ^ deID mid ^ " idx=" ^ string_of_int idx
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
    ; Html.dd [] [Html.text (tid ti.token |> deID)]
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
  && t1 <> (* Matches too many things *) TNewline None
