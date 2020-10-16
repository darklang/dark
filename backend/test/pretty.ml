open Core_kernel
open Libshared
open Libexecution
open Tc

(* This file is copied from the frontend, for the purpose of pretty-printing
 * code for tests. It's a combination of Types.ml, FluidTokenizer.ml and
 * FluidPrinter.ml *)

let asserT msg ~debug v = ()
let recover msg ~debug v = v

module E = FluidExpression
module Pattern = FluidPattern


(* FluidUtil.ml *)
module FluidUtil = struct

  let splitFnName (fnName : string) : string option * string * string =
    let pattern = Re2.create_exn "^((\\w+)::)?([^_]+)(_v(\\d+))?$" in
    let mResult = Re2.find_submatches pattern fnName in
    match mResult with
    | Ok captures ->
        ( match captures with
        | [|_; _; mod_; Some fn; _; Some v|] ->
            (mod_, fn, v)
        | [|_; _; mod_; Some fn; _; None|] ->
            (mod_, fn, "0")
        | _ ->
            (None, "Error found while parsing name", "0"))
    | Error _ ->
        (None, fnName, "0")


  (* Get just the function mod and name *)
  let fnDisplayName (fnName : string) : string =
    let mod_, name, _ = splitFnName fnName in
    match mod_ with Some mod_ -> mod_ ^ "::" ^ name | None -> name


  (* Get just the function version *)
  let versionDisplayName (fnName : string) : string =
    let _, _, version = splitFnName fnName in
    (* For tests always show v0 *)
    "_v" ^ version

  let partialName = fnDisplayName

  let ghostPartialName (fnName : string) =
    partialName fnName ^ versionDisplayName fnName

  let fnDisplayNameWithVersion (fnName : string) =
    partialName fnName ^ versionDisplayName fnName
end

(* Types.ml *)
module ID = struct
  type t = Libexecution.Types.id
  let toString = Types.string_of_id
end

type parentBlockID = ID.t
type analysisID = ID.t

type placeholder =
  { name : string
  ; tipe : string }



type fluidToken =
  | TInteger of ID.t * string * parentBlockID option
  | TString of ID.t * string * parentBlockID option
  (* multi-line strings: ID.t *, segment, start offset, full-string *)
  | TStringMLStart of ID.t * string * int * string
  | TStringMLMiddle of ID.t * string * int * string
  | TStringMLEnd of ID.t * string * int * string
  | TBlank of ID.t * parentBlockID option
  | TPlaceholder of
      { blankID : ID.t
      ; fnID : ID.t
      ; parentBlockID : parentBlockID option
      ; placeholder : placeholder }
  | TTrue of ID.t * parentBlockID option
  | TFalse of ID.t * parentBlockID option
  | TNullToken of ID.t * parentBlockID option
  | TFloatWhole of ID.t * string * parentBlockID option
  | TFloatPoint of ID.t * parentBlockID option
  | TFloatFractional of ID.t * string * parentBlockID option
  (* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. *)
  | TPartial of ID.t * string * parentBlockID option
  (* A partial that extends out to the right. Used to create binops. *)
  (* A partial that preceeds an existing expression, used to wrap things in other things *)
  | TLeftPartial of ID.t * string * parentBlockID option
  | TRightPartial of ID.t * string * parentBlockID option
  (* When a partial used to be another thing, we want to show the name of the
   * old thing in a non-interactable way *)
  | TPartialGhost of ID.t * string * parentBlockID option
  (* the ID.t *here disambiguates with other separators for reflow *)
  | TSep of ID.t * parentBlockID option
  (* The first ID.t *is the ID.t *of the expression directly associated with the
   * newline. The second ID.t *is the ID.t *of that expression's parent. In an
   * expression with potentially many newlines (ie, a pipeline), the int holds
   * the relative line number (index) of this newline. *)
  | TNewline of (ID.t * ID.t * int option) option
  | TIndent of int
  | TLetKeyword of ID.t * analysisID * parentBlockID option
  (* Let-expr id * rhs id * varname *)
  | TLetVarName of ID.t * analysisID * string * parentBlockID option
  | TLetAssignment of ID.t * analysisID * parentBlockID option
  | TIfKeyword of ID.t * parentBlockID option
  | TIfThenKeyword of ID.t * parentBlockID option
  | TIfElseKeyword of ID.t * parentBlockID option
  | TBinOp of ID.t * string * parentBlockID option
  | TFieldOp of (* fieldAccess *) ID.t * (* lhs *) ID.t * parentBlockID option
  | TFieldName of
      ID.t (* fieldAccess *) * ID.t (* lhs *) * string * parentBlockID option
  | TFieldPartial of
      (* Partial ID, fieldAccess ID, analysisID (lhs), name *) ID.t
      * ID.t
      * ID.t
      * string
      * parentBlockID option
  | TVariable of ID.t * string * parentBlockID option
  (* ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'DB::getAll'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3'), sendToRail *)
  | TFnName of ID.t * string * string * string * FluidExpression.sendToRail
  (* ID.t, Partial name (The TFnName display name + TFnVersion display name ex:'DB::getAllv3'), Display name (the name that should be displayed ex:'v3'), fnName (Name for backend, Includes the underscore ex:'DB::getAll_v3') *)
  | TFnVersion of ID.t * string * string * string
  | TLambdaComma of ID.t * int * parentBlockID option
  | TLambdaArrow of ID.t * parentBlockID option
  | TLambdaSymbol of ID.t * parentBlockID option
  | TLambdaVar of ID.t * analysisID * int * string * parentBlockID option
  | TListOpen of ID.t * parentBlockID option
  | TListClose of ID.t * parentBlockID option
  | TListComma of ID.t * int
  (* 2nd int is the number of pipe segments there are *)
  | TPipe of ID.t * int * int * parentBlockID option
  | TRecordOpen of ID.t * parentBlockID option
  | TRecordFieldname of
      { recordID : ID.t
      ; exprID : ID.t
      ; parentBlockID : parentBlockID option
      ; index : int
      ; fieldName : string }
  | TRecordSep of ID.t * int * analysisID
  | TRecordClose of ID.t * parentBlockID option
  | TMatchKeyword of ID.t
  | TMatchBranchArrow of
      { matchID : ID.t
      ; patternID : ID.t
      ; index : int }
  (* for all these TPattern* variants:
   * - the first ID.t *is the match ID.t *
   * - the second ID.t *is the pattern ID.t *
   * - the final int is the index of the (pattern -> expr) *)
  | TPatternVariable of ID.t * ID.t * string * int
  | TPatternConstructorName of ID.t * ID.t * string * int
  | TPatternInteger of ID.t * ID.t * string * int
  | TPatternString of
      { matchID : ID.t
      ; patternID : ID.t
      ; str : string
      ; branchIdx : int }
  | TPatternTrue of ID.t * ID.t * int
  | TPatternFalse of ID.t * ID.t * int
  | TPatternNullToken of ID.t * ID.t * int
  | TPatternFloatWhole of ID.t * ID.t * string * int
  | TPatternFloatPoint of ID.t * ID.t * int
  | TPatternFloatFractional of ID.t * ID.t * string * int
  | TPatternBlank of ID.t * ID.t * int
  | TConstructorName of ID.t * string
  | TParenOpen of ID.t
  | TParenClose of ID.t
  | TFlagWhenKeyword of ID.t
  | TFlagEnabledKeyword of ID.t

and fluidTokenInfo =
  { startRow : int
  ; startCol : int
  ; startPos : int
  ; endPos : int
  ; length : int
  ; token : fluidToken }

type tokenInfo = fluidTokenInfo

type featureFlagTokenization =
  | FeatureFlagOnlyDisabled
      (** FeatureFlagOnlyDisabled is used in the main editor panel to only
          * show the flag's old code *)
  | FeatureFlagConditionAndEnabled
      (** FeatureFlagConditionAndEnabled is used in the secondary editor
          * panel for editing a flag's condition and new code *)

(* -------------------------------------- *)
(* FluidToken.ml *)
(* -------------------------------------- *)


module FluidToken = struct
  let fakeid = Types.id_of_int (-1)

  type tokenInfo = fluidTokenInfo

  let tid (t : fluidToken) : ID.t =
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
    | TLeftPartial (id, _, _)
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
    | TListOpen (id, _)
    | TListClose (id, _)
    | TListComma (id, _)
    | TPipe (id, _, _, _)
    | TRecordOpen (id, _)
    | TRecordClose (id, _)
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


  let analysisID (t : fluidToken) : ID.t =
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
  let parentExprID (t : fluidToken) : ID.t =
    match t with TNewline (Some (_, id, _)) -> id | _ -> tid t


  (* List literals, object literals, and multiline strings are blocks.
   This function returns the ID of the whole list, object, or string expression that this token belongs to, if it does indeed live inside a block.
  *)
  let parentBlockID (t : fluidToken) : ID.t option =
    match t with
    (* The first ID is the ID of the whole string expression *)
    | TStringMLStart (id, _, _, _)
    | TStringMLMiddle (id, _, _, _)
    | TStringMLEnd (id, _, _, _)
    (* The ID of a comma token is the ID of the whole list expression *)
    | TListComma (id, _)
    (* The first ID in the separator token is the ID of the whole obj expression *)
    | TRecordSep (id, _, _) ->
        Some id
    (* The reason { } and [ ] gets a parentBlockID is so if the list/object is empty, then it's not a multiline block. *)
    | TRecordOpen (_, pid)
    | TRecordClose (_, pid)
    | TListOpen (_, pid)
    | TListClose (_, pid)
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
    | TLeftPartial (_, _, pid)
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
  let isTextToken (t : fluidToken) : bool =
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
    | TLeftPartial (_, "", _)
    | TPatternBlank _ ->
        true
    | _ ->
        false


  let isKeyword (t : fluidToken) =
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


  (** [isWhitespace t] returns [true] if the token [t] is a whitespace token (separator, newline, indent)
   * and [false] otherwise. *)
  let isWhitespace (t : fluidToken) : bool =
    match t with
    | TSep _ | TNewline _ | TIndent _ ->
        true
    | TInteger _
    | TFloatWhole _
    | TFloatPoint _
    | TFloatFractional _
    | TTrue _
    | TFalse _
    | TNullToken _
    | TBlank _
    | TPlaceholder _
    | TPartial _
    | TLeftPartial _
    | TRightPartial _
    | TPartialGhost _
    | TLetKeyword _
    | TLetAssignment _
    | TLetVarName _
    | TString _
    | TStringMLStart _
    | TStringMLMiddle _
    | TStringMLEnd _
    | TIfKeyword _
    | TIfThenKeyword _
    | TIfElseKeyword _
    | TBinOp _
    | TFieldOp _
    | TFieldName _
    | TFieldPartial _
    | TVariable _
    | TFnName _
    | TFnVersion _
    | TLambdaVar _
    | TLambdaArrow _
    | TLambdaSymbol _
    | TLambdaComma _
    | TListOpen _
    | TListClose _
    | TListComma _
    | TPipe _
    | TRecordOpen _
    | TRecordClose _
    | TRecordFieldname _
    | TRecordSep _
    | TConstructorName _
    | TMatchBranchArrow _
    | TMatchKeyword _
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
    | TPatternFloatFractional _
    | TParenOpen _
    | TParenClose _
    | TFlagWhenKeyword _
    | TFlagEnabledKeyword _ ->
        false


  let isSkippable (t : fluidToken) : bool = match t with TIndent _ -> true | _ -> false

  let isAtom (t : fluidToken) : bool =
    match t with
    | TMatchBranchArrow _ | TPipe _ | TLambdaArrow _ ->
        true
    | _ ->
        isKeyword t || isBlank t


  let isNewline (t : fluidToken) : bool = match t with TNewline _ -> true | _ -> false

  let isLet (t : fluidToken) : bool =
    match t with TLetAssignment _ | TLetVarName _ -> true | _ -> false


  let isAutocompletable (t : fluidToken) : bool =
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
  let isErrorDisplayable (t : fluidToken) : bool =
    isTextToken t && match t with TFnVersion _ -> false | _ -> true


  let isFieldPartial (t : fluidToken) : bool =
    match t with TFieldPartial _ -> true | _ -> false


  let isMultilineString (t : fluidToken) : bool =
    match t with
    | TStringMLStart _ | TStringMLMiddle _ | TStringMLEnd _ ->
        true
    | _ ->
        false


  let isListSymbol (t : fluidToken) : bool =
    match t with TListOpen _ | TListClose _ | TListComma _ -> true | _ -> false


  let toText (t : fluidToken) : string =
    let shouldntBeEmpty name =
      name
    in
    let canBeEmpty name = if name = "" then "___" else name in
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
        let str = Core_kernel.String.substr_replace_all ~pattern:"\"" ~with_:"\\\"" str in
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
        "blank"
    | TPlaceholder {placeholder = {name; tipe}; _} ->
        "blank"
    | TPartial (_, str, _) ->
        shouldntBeEmpty str
    | TRightPartial (_, str, _) ->
        shouldntBeEmpty str
    | TLeftPartial (_, str, _) ->
        shouldntBeEmpty str
    | TPartialGhost (_, str, _) ->
        shouldntBeEmpty str
    | TSep _ ->
        " "
    | TNewline _ ->
        "\\n"
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
    | TFnName (_, _, displayName, _, _) | TFnVersion (_, _, displayName, _) ->
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
        ";"
    | TRecordOpen _ ->
        "{"
    | TRecordClose _ ->
        "}"
    | TRecordFieldname f ->
        canBeEmpty f.fieldName
    | TRecordSep _ ->
        " = "
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
        "blank"
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


  let toTestText (t : fluidToken) : string =
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
    (* asserT *)
    (*   ~debug:t *)
    (*   "wrong length toTestText" *)
    (*   (String.length result = String.length (toText t)) ; *)
    result


  let toIndex (t : fluidToken) : int option =
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


  let toParentID (t : fluidToken) : ID.t option =
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


  let toTypeName (t : fluidToken) : string =
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


  let toCategoryName (t : fluidToken) : string =
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


  let toDebugInfo (t : fluidToken) : string =
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


  let toCssClasses (t : fluidToken) : string list =
    let empty = if isBlank t then ["fluid-empty"] else [] in
    let keyword = if isKeyword t then ["fluid-keyword"] else [] in
    let typename = ["fluid-" ^ toTypeName t] in
    let category =
      let name = toCategoryName t in
      if name = "" then [] else ["fluid-category-" ^ name]
    in
    empty @ keyword @ typename @ category



  (* Since tokens don't have unique IDs, it is hard to look at two tokens streams
   * and find which tokens represent the same thing. You can use toText and ID,
   * but that doesn't work where the content has changed, which is a thing we
   * want to check for. *)
  let matches (t1 : fluidToken) (t2 : fluidToken) : bool =
    tid t1 = tid t2
    && toTypeName t1 = toTypeName t2
    && toIndex t1 = toIndex t2
    && t1 <> (* Matches too many things *) TNewline None


  (* Matches everything except parentBlockID *)
  let matchesContent (t1 : fluidToken) (t2 : fluidToken) : bool =
    match (t1, t2) with
    | ( TStringMLStart (id1, seg1, ind1, str1)
      , TStringMLStart (id2, seg2, ind2, str2) )
    | ( TStringMLMiddle (id1, seg1, ind1, str1)
      , TStringMLMiddle (id2, seg2, ind2, str2) )
    | TStringMLEnd (id1, seg1, ind1, str1), TStringMLEnd (id2, seg2, ind2, str2)
      ->
        id1 = id2 && seg1 = seg2 && ind1 = ind2 && str1 = str2
    | TListOpen (id1, _), TListOpen (id2, _)
    | TListClose (id1, _), TListClose (id2, _)
    | TRecordOpen (id1, _), TRecordOpen (id2, _)
    | TRecordClose (id1, _), TRecordClose (id2, _)
    | TTrue (id1, _), TTrue (id2, _)
    | TFalse (id1, _), TFalse (id2, _)
    | TNullToken (id1, _), TNullToken (id2, _)
    | TFloatPoint (id1, _), TFloatPoint (id2, _)
    | TLetKeyword (id1, _, _), TLetKeyword (id2, _, _)
    | TLetAssignment (id1, _, _), TLetAssignment (id2, _, _)
    | TIfKeyword (id1, _), TIfKeyword (id2, _)
    | TIfThenKeyword (id1, _), TIfThenKeyword (id2, _)
    | TIfElseKeyword (id1, _), TIfElseKeyword (id2, _)
    | TBlank (id1, _), TBlank (id2, _)
    | TLambdaArrow (id1, _), TLambdaArrow (id2, _)
    | TLambdaSymbol (id1, _), TLambdaSymbol (id2, _)
    | TSep (id1, _), TSep (id2, _)
    | TMatchKeyword id1, TMatchKeyword id2
    | TParenOpen id1, TParenOpen id2
    | TParenClose id1, TParenClose id2
    | TFlagWhenKeyword id1, TFlagWhenKeyword id2
    | TFlagEnabledKeyword id1, TFlagEnabledKeyword id2 ->
        id1 = id2
    | TListComma (id1, ind1), TListComma (id2, ind2)
    | TRecordSep (id1, ind1, _), TRecordSep (id2, ind2, _)
    | TLambdaComma (id1, ind1, _), TLambdaComma (id2, ind2, _) ->
        id1 = id2 && ind1 = ind2
    | TInteger (id1, val1, _), TInteger (id2, val2, _)
    | TFloatWhole (id1, val1, _), TFloatWhole (id2, val2, _)
    | TFloatFractional (id1, val1, _), TFloatFractional (id2, val2, _)
    | TPartial (id1, val1, _), TPartial (id2, val2, _)
    | TRightPartial (id1, val1, _), TRightPartial (id2, val2, _)
    | TPartialGhost (id1, val1, _), TPartialGhost (id2, val2, _)
    | TString (id1, val1, _), TString (id2, val2, _)
    | TLetVarName (id1, _, val1, _), TLetVarName (id2, _, val2, _)
    | TBinOp (id1, val1, _), TBinOp (id2, val2, _)
    | TVariable (id1, val1, _), TVariable (id2, val2, _)
    | TConstructorName (id1, val1), TConstructorName (id2, val2) ->
        id1 = id2 && val1 = val2
    | TFieldOp (id1, l1, _), TFieldOp (id2, l2, _) ->
        id1 = id2 && l1 = l2
    | TFieldName (id1, l1, val1, _), TFieldName (id2, l2, val2, _)
    | TFieldPartial (id1, l1, _, val1, _), TFieldPartial (id2, l2, _, val2, _) ->
        id1 = id2 && l1 = l2 && val1 = val2
    | TLambdaVar (id1, _, ind1, val1, _), TLambdaVar (id2, _, ind2, val2, _) ->
        id1 = id2 && ind1 = ind2 && val1 = val2
    | TPipe (id1, order1, nest1, _), TPipe (id2, order2, nest2, _) ->
        id1 = id2 && order1 = order2 && nest1 = nest2
    | TRecordFieldname d1, TRecordFieldname d2 ->
        d1.recordID = d2.recordID
        && d1.exprID = d2.exprID
        && d1.index = d2.index
        && d1.fieldName = d2.fieldName
    | TNewline props1, TNewline props2 ->
        props1 = props2
    | TMatchBranchArrow d1, TMatchBranchArrow d2 ->
        d1.matchID = d2.matchID
        && d1.patternID = d2.patternID
        && d1.index = d2.index
    | TPatternString d1, TPatternString d2 ->
        d1.matchID = d2.matchID
        && d1.patternID = d2.patternID
        && d1.str = d2.str
        && d1.branchIdx = d2.branchIdx
    | TFnName (id1, _, _, fullname1, rail1), TFnName (id2, _, _, fullname2, rail2)
      ->
        id1 = id2 && fullname1 = fullname2 && rail1 = rail2
    | TFnVersion (id1, _, _, fullname1), TFnVersion (id2, _, _, fullname2) ->
        id1 = id2 && fullname1 = fullname2
    | TPatternVariable (m1, p1, val1, ind1), TPatternVariable (m2, p2, val2, ind2)
    | ( TPatternConstructorName (m1, p1, val1, ind1)
      , TPatternConstructorName (m2, p2, val2, ind2) )
    | TPatternInteger (m1, p1, val1, ind1), TPatternInteger (m2, p2, val2, ind2)
      ->
        m1 = m2 && p1 = p2 && val1 = val2 && ind1 = ind2
    | TPatternTrue (p1, id1, ind1), TPatternTrue (p2, id2, ind2)
    | TPatternFalse (p1, id1, ind1), TPatternFalse (p2, id2, ind2)
    | TPatternNullToken (p1, id1, ind1), TPatternNullToken (p2, id2, ind2)
    | TPatternFloatPoint (p1, id1, ind1), TPatternFloatPoint (p2, id2, ind2)
    | TPatternBlank (p1, id1, ind1), TPatternBlank (p2, id2, ind2) ->
        p1 = p2 && id1 = id2 && ind1 = ind2
    | ( TPatternFloatWhole (p1, id1, val1, ind1)
      , TPatternFloatWhole (p2, id2, val2, ind2) )
    | ( TPatternFloatFractional (p1, id1, val1, ind1)
      , TPatternFloatFractional (p2, id2, val2, ind2) ) ->
        p1 = p2 && id1 = id2 && val1 = val2 && ind1 = ind2
    | TIndent ind1, TIndent ind2 ->
        ind1 = ind2
    | TPlaceholder d1, TPlaceholder d2 ->
        d1.blankID = d2.blankID
        && d1.fnID = d2.fnID
        && d1.placeholder = d2.placeholder
    | TInteger _, _
    | TString _, _
    | TStringMLStart _, _
    | TStringMLMiddle _, _
    | TStringMLEnd _, _
    | TBlank _, _
    | TPlaceholder _, _
    | TTrue _, _
    | TFalse _, _
    | TNullToken _, _
    | TFloatWhole _, _
    | TFloatPoint _, _
    | TFloatFractional _, _
    | TPartial _, _
    | TLeftPartial _, _
    | TRightPartial _, _
    | TPartialGhost _, _
    | TSep _, _
    | TNewline _, _
    | TIndent _, _
    | TLetKeyword _, _
    | TLetVarName _, _
    | TLetAssignment _, _
    | TIfKeyword _, _
    | TIfThenKeyword _, _
    | TIfElseKeyword _, _
    | TBinOp _, _
    | TFieldOp _, _
    | TFieldName _, _
    | TFieldPartial _, _
    | TVariable _, _
    | TFnName _, _
    | TFnVersion _, _
    | TLambdaComma _, _
    | TLambdaArrow _, _
    | TLambdaSymbol _, _
    | TLambdaVar _, _
    | TListOpen _, _
    | TListClose _, _
    | TListComma _, _
    | TPipe _, _
    | TRecordOpen _, _
    | TRecordFieldname _, _
    | TRecordSep _, _
    | TRecordClose _, _
    | TMatchKeyword _, _
    | TMatchBranchArrow _, _
    | TPatternVariable _, _
    | TPatternConstructorName _, _
    | TPatternInteger _, _
    | TPatternString _, _
    | TPatternTrue _, _
    | TPatternFalse _, _
    | TPatternNullToken _, _
    | TPatternFloatWhole _, _
    | TPatternFloatPoint _, _
    | TPatternFloatFractional _, _
    | TPatternBlank _, _
    | TConstructorName _, _
    | TParenOpen _, _
    | TParenClose _, _
    | TFlagWhenKeyword _, _
    | TFlagEnabledKeyword _, _ ->
        false
end
module T = FluidToken

(* -------------------------------------- *)
(* FluidTokenizer.ml *)
(* -------------------------------------- *)

module Builder = struct
  type t =
    { tokens : fluidToken list
          (** [tokens] list is kept reversed while being built up, as adding
            * things to the front of the list is an order of magnitude faster.
            * We were having large slowdowns on large handlers before this. *)
    ; indent : int  (** [indent] tracks the indent after a newline *)
    ; xPos : int option
          (** [xPos] tracks the indent for nesting.
            * `None` indicates it's ready to go after a newline *)
    ; ffTokenization : featureFlagTokenization }

  let rec endsInNewline (b : t) : bool =
    (* The latest token is on the front *)
    match b.tokens with
    | TNewline _ :: _ ->
        true
    | TIndent _ :: tail ->
        endsInNewline {b with tokens = tail}
    | _ ->
        false


  let empty =
    { tokens = []
    ; xPos = Some 0
    ; indent = 0
    ; ffTokenization = FeatureFlagOnlyDisabled }


  let lineLimit = 40000

  let strLimit = 40000 (* Just dont *)

  let listLimit = 40000

  let add (token : fluidToken) (b : t) : t =
    let tokenLength = token |> T.toText |> String.length in
    let newTokens, xPos =
      (* Add new tokens on the front *)
      if endsInNewline b
      then
        ( ( if b.indent <> 0
          then token :: TIndent b.indent :: b.tokens
          else token :: b.tokens )
        , Some (b.indent + tokenLength) )
      else
        let newXPos =
          match token with
          | TNewline _ ->
              None
          | _ ->
              let old = Option.withDefault b.xPos ~default:b.indent in
              Some (old + tokenLength)
        in
        (token :: b.tokens, newXPos)
    in
    {b with tokens = newTokens; xPos}


  let addIf (cond : bool) (token : fluidToken) (b : t) : t =
    if cond then add token b else b


  (* Take a list of 'a, and iterate through them, adding them to `b` by
   * calling `f` on them *)
  let addIter (xs : 'a list) ~(f : int -> 'a -> t -> t) (b : t) : t =
    List.foldl xs ~init:(b, 0) ~f:(fun x (b, i) -> (f i x b, i + 1))
    |> Tuple2.first


  let addMany (tokens : fluidToken list) (b : t) : t =
    List.foldl tokens ~init:b ~f:add


  (** [indentBy ~ident ~f b] calls [f] with a modified [b] having additional
    * indentation by [indent] characters (that is, b.indent + ~indent), then
    * returns the result of that [f b] invocation with the original indent of
    * [b] restored. *)
  let indentBy ~(indent : int) ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    {b with indent = b.indent + indent}
    |> f
    |> fun b -> {b with indent = oldIndent}


  let addNested ~(f : t -> t) (b : t) : t =
    let oldIndent = b.indent in
    let newIndent = Option.withDefault ~default:b.indent b.xPos in
    {b with indent = newIndent} |> f |> fun b -> {b with indent = oldIndent}


  let addNewlineIfNeeded (nlInfo : (ID.t * ID.t * int option) option) (b : t) :
      t =
    if endsInNewline b then b else add (TNewline nlInfo) b


  let asTokens (b : t) : fluidToken list =
    (* Tokens are stored reversed *)
    List.reverse b.tokens
end

let rec patternToToken (p : FluidPattern.t) ~(idx : int) : fluidToken list =
  match p with
  | FPVariable (mid, id, name) ->
      [TPatternVariable (mid, id, name, idx)]
  | FPConstructor (mid, id, name, args) ->
      let args =
        List.map args ~f:(fun a -> TSep (id, None) :: patternToToken a ~idx)
      in
      List.concat ([TPatternConstructorName (mid, id, name, idx)] :: args)
  | FPInteger (mid, id, i) ->
      [TPatternInteger (mid, id, i, idx)]
  | FPBool (mid, id, b) ->
      if b then [TPatternTrue (mid, id, idx)] else [TPatternFalse (mid, id, idx)]
  | FPString {matchID = mid; patternID = id; str} ->
      [TPatternString {matchID = mid; patternID = id; str; branchIdx = idx}]
  | FPFloat (mID, id, whole, fraction) ->
      let whole =
        if whole = "" then [] else [TPatternFloatWhole (mID, id, whole, idx)]
      in
      let fraction =
        if fraction = ""
        then []
        else [TPatternFloatFractional (mID, id, fraction, idx)]
      in
      whole @ [TPatternFloatPoint (mID, id, idx)] @ fraction
  | FPNull (mid, id) ->
      [TPatternNullToken (mid, id, idx)]
  | FPBlank (mid, id) ->
      [TPatternBlank (mid, id, idx)]


let rec toTokens' ?(parens=false) ?(parentID = None) (e : E.t) (b : Builder.t) : Builder.t =
  let open Builder in
  let ghostPartial id newName oldName =
    let ghostSuffix = String.dropLeft ~count:(String.length newName) oldName in
    if ghostSuffix = "" then [] else [TPartialGhost (id, ghostSuffix, None)]
  in
  (* placeholderFor = (id * string * int)
   * id: id of the placeholder-containing expr
   * string: name of the placeholder-containing expr
   * int: index of the placeholder within the expr's parameters
   *)
  let nest
      ?(placeholderFor : (ID.t * string * int) option = None)
      ?(parens=true)
      ~indent
      (e : E.t)
      (b : Builder.t) : Builder.t =
    let tokensFn b =
      match (e, placeholderFor) with
      | EBlank id, Some (fnID, fnname, pos) ->
          let name = None in
          ( match name with
          | None ->
              toTokens' e b
          | Some placeholder ->
              add
                (TPlaceholder
                   {blankID = id; parentBlockID = Some fnID; placeholder; fnID})
                b )
      | _ ->
          toTokens' ~parens e b
    in
    b |> indentBy ~indent ~f:(addNested ~f:tokensFn)
  in
  let addArgs (name : string) (id : ID.t) (args : E.t list) (b : Builder.t) :
      Builder.t =
    let args, offset =
      match args with EPipeTarget _ :: args -> (args, 1) | _ -> (args, 0)
    in
    let reflow =
      let tokens =
        args
        |> List.map ~f:(fun a -> toTokens' a Builder.empty)
        |> List.map ~f:Builder.asTokens
        |> List.concat
      in
      let length =
        tokens
        |> List.map ~f:(T.toText >> String.length)
        |> List.sum
        |> ( + ) (* separators, including at the front *) (List.length args)
        |> ( + ) (Option.withDefault ~default:0 b.xPos)
      in
      let tooLong = length > lineLimit in
      let needsNewlineBreak =
        (* newlines aren't disruptive in the last argument *)
        args
        |> List.init
        |> Option.withDefault ~default:[]
        |> List.map ~f:(fun a -> toTokens' a Builder.empty)
        |> List.map ~f:Builder.asTokens
        |> List.concat
        |> List.any ~f:(function TNewline _ -> true | _ -> false)
      in
      tooLong || needsNewlineBreak
    in
    b
    |> addIter args ~f:(fun i e b ->
           if reflow
           then
             b
             |> addNewlineIfNeeded (Some (id, id, Some (offset + i)))
             |> nest ~indent:2 ~placeholderFor:(Some (id, name, offset + i)) e
           else
             b
             |> add (TSep (E.toID e, None))
             |> nest ~indent:0 ~placeholderFor:(Some (id, name, offset + i)) e)
  in
  let id = E.toID e in
  let parens =
    (* Some things never need parens *)
    match e with
    | EInteger _ -> false
    | EString _ -> false
    | EFloat _ -> false
    | EBool _ -> false
    | EVariable _ -> false
    | ERecord _ -> false
    | EList _ -> false
    | EConstructor (_, _, []) -> false
    | EFnCall (_, _, [], _) -> false
    | _ -> parens
  in
  let b = if parens then b |> add (TParenOpen id) else b in
  let b =
    match e with
    | EInteger (id, i) ->
        b |> add (TInteger (id, i, parentID))
    | EBool (id, bool') ->
        b |> add (if bool' then TTrue (id, parentID) else TFalse (id, parentID))
    | ENull id ->
        b |> add (TNullToken (id, parentID))
    | EFloat (id, whole, fraction) ->
        let whole =
          if whole = "" then [] else [TFloatWhole (id, whole, parentID)]
        in
        let fraction =
          if fraction = "" then [] else [TFloatFractional (id, fraction, parentID)]
        in
        b |> addMany (whole @ [TFloatPoint (id, parentID)] @ fraction)
    | EBlank id ->
        b |> add (TBlank (id, parentID))
    | ELet (id, lhs, rhs, next) ->
        let rhsID = E.toID rhs in
        b
        |> add (TLetKeyword (id, rhsID, parentID))
        |> add (TLetVarName (id, rhsID, lhs, parentID))
        |> add (TLetAssignment (id, rhsID, parentID))
        |> addNested ~f:(toTokens' rhs)
        |> addNewlineIfNeeded (Some (E.toID next, id, None))
        |> addNested ~f:(toTokens' next)
    | EString (id, str) ->
        let strings =
          if String.length str > strLimit
          then String.segment ~size:strLimit str
          else [str]
        in
        ( match strings with
        | [] ->
            add (TString (id, "", parentID)) b
        | starting :: rest ->
          ( match List.reverse rest with
          | [] ->
              add (TString (id, str, parentID)) b
          | ending :: revrest ->
              b
              |> addNested ~f:(fun b ->
                     let endingOffset = strLimit * (List.length revrest + 1) in
                     b
                     |> add (TStringMLStart (id, starting, 0, str))
                     |> add (TNewline None)
                     |> addIter (List.reverse revrest) ~f:(fun i s b ->
                            b
                            |> add
                                 (TStringMLMiddle (id, s, strLimit * (i + 1), str))
                            |> add (TNewline None))
                     |> add (TStringMLEnd (id, ending, endingOffset, str))) ) )
    | EIf (id, cond, if', else') ->
        b
        |> add (TIfKeyword (id, parentID))
        |> addNested ~f:(toTokens' cond)
        |> addNewlineIfNeeded None
        |> add (TIfThenKeyword (id, parentID))
        |> addNewlineIfNeeded (Some (E.toID if', id, None))
        |> nest ~indent:2 if'
        |> addNewlineIfNeeded None
        |> add (TIfElseKeyword (id, parentID))
        |> add (TNewline (Some (E.toID else', id, None)))
        |> nest ~indent:2 else'
    | EBinOp (id, op, lexpr, rexpr, _ster) ->
        let start b =
          match lexpr with
          | EPipeTarget _ ->
              b
          | _ ->
              b
              |> nest ~indent:0 ~placeholderFor:(Some (id, op, 0)) lexpr
              |> add (TSep (E.toID lexpr, parentID))
        in
        b
        |> start
        |> addMany [TBinOp (id, op, parentID); TSep (id, parentID)]
        |> nest ~indent:0 ~placeholderFor:(Some (id, op, 1)) rexpr
    | EPartial (id, newName, EBinOp (_, oldName, lexpr, rexpr, _ster)) ->
        let ghost =
          ghostPartial id newName (FluidUtil.ghostPartialName oldName)
        in
        let start b =
          match lexpr with
          | EPipeTarget _ ->
              b
          | _ ->
              b
              |> nest ~indent:0 ~placeholderFor:(Some (id, oldName, 0)) lexpr
              |> add (TSep (E.toID lexpr, parentID))
        in
        b
        |> start
        |> add (TPartial (id, newName, parentID))
        |> addMany ghost
        |> add (TSep (id, parentID))
        |> nest ~indent:2 ~placeholderFor:(Some (id, oldName, 1)) rexpr
    | EFnCall (id, fnName, args, ster) ->
        let displayName = FluidUtil.fnDisplayName fnName in
        let versionDisplayName = FluidUtil.versionDisplayName fnName in
        let partialName = FluidUtil.fnDisplayNameWithVersion fnName in
        let versionToken =
          if versionDisplayName = ""
          then []
          else [TFnVersion (id, partialName, versionDisplayName, fnName)]
        in
        b
        |> add (TFnName (id, partialName, displayName, fnName, ster))
        |> addMany versionToken
        |> addArgs fnName id args
    | EPartial (id, newName, EFnCall (_, oldName, args, _)) ->
        let partial = TPartial (id, newName, parentID) in
        let newText = T.toText partial in
        let oldText = FluidUtil.ghostPartialName oldName in
        let ghost = ghostPartial id newText oldText in
        b |> add partial |> addMany ghost |> addArgs oldName id args
    | EConstructor (id, name, exprs) ->
        b |> add (TConstructorName (id, name)) |> addArgs name id exprs
    | EPartial (id, newName, EConstructor (_, oldName, exprs)) ->
        let partial = TPartial (id, newName, parentID) in
        let newText = T.toText partial in
        let ghost = ghostPartial id newText oldName in
        b |> add partial |> addMany ghost |> addArgs oldName id exprs
    | EFieldAccess (id, expr, fieldname) ->
        let lhsid = E.toID expr in
        b
        |> addNested ~f:(toTokens' expr ~parentID)
        |> addMany
             [ TFieldOp (id, lhsid, parentID)
             ; TFieldName (id, lhsid, fieldname, parentID) ]
    | EPartial (id, newFieldname, EFieldAccess (faID, expr, oldFieldname)) ->
        let lhsid = E.toID expr in
        let partial = TFieldPartial (id, faID, lhsid, newFieldname, parentID) in
        let newText = T.toText partial in
        let ghost = ghostPartial id newText oldFieldname in
        b
        |> addNested ~f:(toTokens' expr)
        |> addMany [TFieldOp (id, E.toID expr, parentID); partial]
        |> addMany ghost
    | EVariable (id, name) ->
        b |> add (TVariable (id, name, parentID))
    | ELambda (id, names, body) ->
        let isLast i = i = List.length names - 1 in
        b
        |> add (TLambdaSymbol (id, parentID))
        |> addIter names ~f:(fun i (aid, name) b ->
               b
               |> add (TLambdaVar (id, aid, i, name, parentID))
               |> addIf (not (isLast i)) (TLambdaComma (id, i, parentID))
               |> addIf (not (isLast i)) (TSep (aid, parentID)))
        |> add (TLambdaArrow (id, parentID))
        |> nest ~parens:false ~indent:2 body
    | EList (id, exprs) ->
        (*
           With each iteration of the list, we calculate the new line length, if we were to add this new item. If the new line length exceeds the limit, then we add a new line token and an indent by 1 first, before adding the tokenized item to the builder.
        *)
        let lastIndex = List.length exprs - 1 in
        let xOffset = b.xPos |> Option.withDefault ~default:0 in
        let pid = if lastIndex = -1 then None else Some id in
        b
        |> add (TListOpen (id, pid))
        |> addIter exprs ~f:(fun i e b' ->
               let currentLineLength =
                 let commaWidth = if i <> lastIndex then 1 else 0 in
                 (toTokens' e b').xPos
                 |> Option.map ~f:(fun x -> x - xOffset + commaWidth)
                 |> Option.withDefault ~default:commaWidth
               in
               (* Even if first element overflows, don't put it in a new line *)
               let isOverLimit = i > 0 && currentLineLength > listLimit in
               (* Indent after newlines to match the '[ ' *)
               let indent = if isOverLimit then 1 else 0 in
               b'
               |> addIf isOverLimit (TNewline None)
               |> indentBy ~indent ~f:(fun b' ->
                      b'
                      |> addNested ~f:(toTokens' ~parentID:(Some id) e)
                      |> addIf (i <> lastIndex) (TListComma (id, i))))
        |> add (TListClose (id, pid))
    | ERecord (id, fields) ->
        if fields = []
        then b |> addMany [TRecordOpen (id, None); TRecordClose (id, None)]
        else
          let parentBlockID = Some id in
          b
          |> add (TRecordOpen (id, parentBlockID))
          |> indentBy ~indent:2 ~f:(fun b ->
                 addIter fields b ~f:(fun i (fieldName, expr) b ->
                     let exprID = E.toID expr in
                     b
                     |> addNewlineIfNeeded (Some (id, id, Some i))
                     |> add
                          (TRecordFieldname
                             { recordID = id
                             ; exprID
                             ; index = i
                             ; fieldName
                             ; parentBlockID })
                     |> add (TRecordSep (id, i, exprID))
                     |> addNested ~f:(toTokens' ~parentID:(Some id) expr)))
          |> addMany
               [ TNewline (Some (id, id, Some (List.length fields)))
               ; TRecordClose (id, parentBlockID) ]
    | EPipe (id, exprs) ->
        let length = List.length exprs in
        ( match exprs with
        | [] ->
            recover "Empty pipe found" ~debug:e b
        | [single] ->
            recover "pipe with single entry found" ~debug:e (toTokens' single b)
        | head :: tail ->
            b
            |> addNested ~f:(toTokens' head)
            |> addNewlineIfNeeded (Some (E.toID head, id, Some 0))
            |> addIter tail ~f:(fun i e b ->
                   b
                   |> add (TPipe (id, i, length, parentID))
                   |> addNested ~f:(toTokens' ~parentID e)
                   |> addNewlineIfNeeded (Some (E.toID e, id, Some (i + 1))))
            |> addNewlineIfNeeded (Some (id, id, Some (List.length tail))) )
    | EPipeTarget _ ->
        recover "should never be making tokens for EPipeTarget" ~debug:e b
    | EMatch (id, mexpr, pairs) ->
        b
        |> add (TMatchKeyword id)
        |> addNested ~f:(toTokens' mexpr)
        |> indentBy ~indent:2 ~f:(fun b ->
               b
               |> addIter pairs ~f:(fun i (pattern, expr) b ->
                      b
                      |> addNewlineIfNeeded (Some (id, id, Some i))
                      |> addMany (patternToToken pattern ~idx:i)
                      |> add
                           (TMatchBranchArrow
                              { matchID = id
                              ; patternID = Pattern.toID pattern
                              ; index = i })
                      |> addNested ~f:(toTokens' expr))
               |> addNewlineIfNeeded (Some (id, id, Some (List.length pairs))))
    | EPartial (id, str, _) ->
        b |> add (TPartial (id, str, parentID))
    | ERightPartial (id, newOp, expr) ->
        b
        |> addNested ~f:(toTokens' expr)
        |> addMany [TSep (id, parentID); TRightPartial (id, newOp, parentID)]
    | ELeftPartial (id, str, expr) ->
        b
        |> add (TLeftPartial (id, str, parentID))
        |> addNested ~f:(toTokens' expr)
    | EFeatureFlag (id, _name, cond, disabled, enabled) ->
      (* Feature flag tokens are displayed in two different editor panels, so
       * they are built differently depending on the current builder option. *)
      ( match b.ffTokenization with
      | FeatureFlagOnlyDisabled ->
          b |> addNested ~f:(toTokens' disabled)
      | FeatureFlagConditionAndEnabled ->
          b
          |> add (TFlagWhenKeyword id)
          |> addNested ~f:(toTokens' cond)
          |> addNewlineIfNeeded None
          |> add (TFlagEnabledKeyword id)
          |> addNewlineIfNeeded (Some (E.toID enabled, id, None))
          |> nest ~indent:2 enabled )
  in
  if parens then b |> add (TParenClose id) else b



let infoize tokens : tokenInfo list =
  let row, col, pos = (ref 0, ref 0, ref 0) in
  List.map tokens ~f:(fun token ->
      let length = String.length (T.toText token) in
      let ti =
        { token
        ; startRow = !row
        ; startCol = !col
        ; startPos = !pos
        ; endPos = !pos + length
        ; length }
      in
      ( match token with
      | TNewline _ ->
          row := !row + 1 ;
          col := 0
      | _ ->
          col := !col + length ) ;
      pos := !pos + length ;
      ti)


let validateTokens (tokens : fluidToken list) : fluidToken list =
  List.iter tokens ~f:(fun t ->
      asserT "invalid token" (String.length (T.toText t) > 0) ~debug:t ;
      ()) ;
  tokens


(* Remove artifacts of the token generation process *)
let tidy (tokens : fluidToken list) : fluidToken list =
  tokens |> List.filter ~f:(function TIndent 0 -> false | _ -> true)


let tokenizeWithFFTokenization
    (ffTokenization : featureFlagTokenization) (e : FluidExpression.t) :
    tokenInfo list =
  {Builder.empty with ffTokenization}
  |> toTokens' e
  |> Builder.asTokens
  |> tidy
  |> validateTokens
  |> infoize


let tokenize : E.t -> FluidToken.tokenInfo list =
  tokenizeWithFFTokenization FeatureFlagOnlyDisabled

(* FluidPrint.ml *)
let tokensToString (tis : tokenInfo list) : string =
  tis |> List.map ~f:(fun ti -> T.toText ti.token) |> String.join ~sep:""

let eToHumanString (expr : E.t) : string =
  expr |> tokenize |> tokensToString
