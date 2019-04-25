(* open Webapi.Dom *)
open Tc
module K = FluidKeyboard
module Mouse = Tea.Mouse

(* Tea *)
module Cmd = Tea.Cmd

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events

(* -------------------- *)
(* Utils *)
(* -------------------- *)

let debugf ~(f : 'a -> 'b) (name : string) (value : 'a) : 'a =
  Js.log2 name (f value) ;
  value


let debug (name : string) (value : 'a) : 'a =
  Js.log2 name value ;
  value


let removeCharAt str offset : string =
  if offset < 0
  then str
  else
    String.slice ~from:0 ~to_:offset str
    ^ String.slice ~from:(offset + 1) ~to_:(String.length str) str


let isNumber (str : string) = Js.Re.test str [%re "/[0-9]+/"]

let isIdentifierChar (str : string) = Js.Re.test str [%re "/[_a-zA-Z0-9]+/"]

let isFnNameChar str =
  Js.Re.test str [%re "/[_:a-zA-Z0-9]/"] && String.length str = 1


exception FExc of string

let fail str = raise (FExc str)

(* -------------------- *)
(* Expressions *)
(* -------------------- *)

type id = string

type name =
  | B
  | F of string

type expr =
  | EInteger of id * int
  | EString of id * string
  | EBlank of id
  | ELet of id * name * expr * expr
  | EIf of id * expr * expr * expr
  | EBinOp of id * name * expr * expr
  | ELambda of id * name list * expr
  | EFieldAccess of id * expr * name
  | EVariable of id * string
  | EFnCall of id * name * expr list
  | EPartial of id * string
  | EList of id * expr list
  | ERecord of id * (name * expr) list

(* TODO: Stuff to add *)
(* match/patterns *)
(* thread *)
(* constructor *)
(* send to rail *)
(* bool, character, float, null *)
(* extra params in lambdas *)
(* remove B/F *)
(* feature flags (may punt) *)

let gid () = string_of_int (Random.int (4096 * 1024) |> abs)

let eid expr : id =
  match expr with
  | EInteger (id, _)
  | EString (id, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _)
  | EFnCall (id, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _)
  | EList (id, _)
  | ERecord (id, _)
  | EBinOp (id, _, _, _) ->
      id


let newB () = EBlank (gid ())

let complexExpr =
  EIf
    ( gid ()
    , EBinOp
        ( gid ()
        , F "||"
        , EBinOp
            ( gid ()
            , F "=="
            , EFieldAccess
                ( gid ()
                , EFieldAccess
                    (gid (), EVariable (gid (), "request"), F "headers")
                , F "origin" )
            , EString (gid (), "https://usealtitude.com") )
        , EBinOp
            ( gid ()
            , F "=="
            , EFieldAccess
                ( gid ()
                , EFieldAccess
                    (gid (), EVariable (gid (), "request"), F "headers")
                , F "origin" )
            , EString (gid (), "https://localhost:3000") ) )
    , ELet
        ( gid ()
        , B
        , newB ()
        , EFnCall (gid (), F "Http::Forbidden", [EInteger (gid (), 403)]) )
    , EFnCall (gid (), F "Http::Forbidden", []) )


let startingExpr = complexExpr

(* if request.headers.origin == "https://usealtitude.com" || request.headers.origin == "http://localhost:3000" *)
(*   let user = DB::query {                                        Users *)
(*                          token : request.headers.authorization *)
(*                                  |> String::split "Bearer " *)
(*                                  |> List::last *)
(*                        } *)
(*              |> List::head *)
(*   if user != null *)
(*     let newAirlines = List::foreach request.body.selectedAirlines *)
(*                       \var -> *)
(*                          DB::fetchOneBy var.name "name" Airlines *)
(*     let newOrigins = List::foreach request.body.selectedOrigins *)
(*                       \var -> *)
(*                          DB::fetchOneBy var.name "name" Airports *)
(*     let newDestinations = List::foreach request.body.selectedDestinations *)
(*                            \var -> *)
(*                               DB::fetchOneBy var.name "name" Airports *)
(*     let updatedUser = user *)
(*                       |> assoc "userAirlines" newAirlines *)
(*                       |> assoc "email" request.body.email *)
(*                       |> assoc "phoneNumber" request.body.phoneNumber *)
(*                       |> assoc "userOrigins" newOrigins *)
(*                       |> assoc "userDestinations" newDestinations *)
(*                       |> assoc "emailNotifications" request.body.emailNotifications *)
(*                       |> assoc "smsNotifications" request.body.smsNotifications *)
(*                       |> assoc "userAvailability" request.body.userAvailability *)
(*                       |> assoc "updatedAt" Date::now *)
(*     let update = DB::set updatedUser user.uuid Users *)
(*     Http::success { status : 200 } *)
(*   else *)
(*     Http::forbidden *)
(* else *)
(*   Http::forbidden *)
(*  *)

(* -------------------- *)
(* Tokens *)
(* -------------------- *)
type token =
  | TInteger of id * string
  | TString of id * string
  | TBlank of id
  (* If you're filling in an expr, but havent finished it. Not used for
   * non-expr names. *)
  | TPartial of id * string
  | TSep
  | TNewline
  (* All newlines in the nested tokens start indented to this position. *)
  | TIndentToHere of token list
  (* Increase the level of indentation for all these tokens. *)
  | TIndented of token list
  (* TIndentToHere and TIndented are preprocessed to the right indentation
   * and turned into TIndents *)
  | TIndent of int
  | TLetKeyword of id
  | TLetLHS of id * string
  | TLetAssignment of id
  | TIfKeyword of id
  | TIfThenKeyword of id
  | TIfElseKeyword of id
  | TBinOp of id * string
  | TFieldOp of id
  | TFieldName of id * string
  | TVariable of id * string
  | TFnName of id * string
  | TLambdaSep of id
  | TLambdaArrow of id
  | TLambdaSymbol of id
  | TLambdaVar of id * string
  | TListOpen of id
  | TListClose of id
  | TListSep of id
  | TRecordOpen of id
  | TRecordField of id * int * string
  | TRecordSep of id * int
  | TRecordClose of id

let isBlank t =
  match t with
  | TBlank _
  | TRecordField (_, _, "")
  | TFieldName (_, "")
  | TLetLHS (_, "")
  | TLambdaVar (_, "")
  | TPartial _ ->
      true
  | _ ->
      false


let isAutocompletable (t : token) : bool =
  match t with TBlank _ | TPartial _ -> true | _ -> false


let toText (t : token) : string =
  let failIfEmpty name =
    if name = "" then fail "shouldn't be empty" else name
  in
  let blankIfEmpty name = if name = "" then "   " else name in
  match t with
  | TInteger (_, i) ->
      failIfEmpty i
  | TString (_, str) ->
      "\"" ^ str ^ "\""
  | TBlank _ ->
      "   "
  | TPartial (_, str) ->
      failIfEmpty str
  | TSep ->
      " "
  | TNewline ->
      "\n"
  | TLetKeyword _ ->
      "let "
  | TLetAssignment _ ->
      " = "
  | TLetLHS (_, name) ->
      blankIfEmpty name
  | TIfKeyword _ ->
      "if "
  | TIfThenKeyword _ ->
      "then"
  | TIfElseKeyword _ ->
      "else"
  | TBinOp (_, op) ->
      failIfEmpty op
  | TFieldOp _ ->
      "."
  | TFieldName (_, name) ->
      blankIfEmpty name
  | TVariable (_, name) ->
      failIfEmpty name
  | TFnName (_, name) ->
      failIfEmpty name
  | TLambdaVar (_, name) ->
      blankIfEmpty name
  | TLambdaSymbol _ ->
      "\\"
  | TLambdaSep _ ->
      " "
  | TLambdaArrow _ ->
      " -> "
  | TIndent indent ->
      failIfEmpty (Caml.String.make indent ' ')
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
      blankIfEmpty name
  | TRecordSep _ ->
      ":"


let toTestText (t : token) : string =
  match t with
  | TBlank _ ->
      "___"
  | TPartial (_, str) ->
      str
  | _ ->
      if isBlank t then "***" else toText t


let toName (t : token) : string =
  match t with
  | TInteger _ ->
      "integer"
  | TString (_, _) ->
      "string"
  | TBlank _ ->
      "blank"
  | TPartial _ ->
      "partial"
  | TLetKeyword _ ->
      "let let-keyword keyword"
  | TLetAssignment _ ->
      "let let-assignment"
  | TLetLHS (_, "") ->
      "let let-lhs empty"
  | TLetLHS _ ->
      "let let-lhs"
  | TSep ->
      "sep"
  | TIndented _ ->
      "indented"
  | TIndentToHere _ ->
      "indented-at-place"
  | TIndent _ ->
      "indent"
  | TNewline ->
      "newline"
  | TIfKeyword _ ->
      "if if-keyword keyword"
  | TIfThenKeyword _ ->
      "if-then-keyword keyword"
  | TIfElseKeyword _ ->
      "if-else-keyword keyword"
  | TBinOp (_, _) ->
      "binop"
  | TFieldOp _ ->
      "field field-op"
  | TFieldName (_, "") ->
      "field field-name empty"
  | TFieldName (_, _) ->
      "field field-name"
  | TVariable (_, _) ->
      "variable"
  | TFnName (_, _) ->
      "fn fn-name"
  | TLambdaVar (_, "") ->
      "lambda lambda-var empty"
  | TLambdaVar (_, _) ->
      "lambda lambda-var"
  | TLambdaSymbol _ ->
      "lambda lambda-keyword keyword"
  | TLambdaArrow _ ->
      "lambda lambda-arrow"
  | TLambdaSep _ ->
      "lambda lambda-sep"
  | TListOpen _ ->
      "list list-open"
  | TListClose _ ->
      "list list-close"
  | TListSep _ ->
      "list list-sep"
  | TRecordOpen _ ->
      "record record-open"
  | TRecordClose _ ->
      "record record-close"
  | TRecordField (_, _, "") ->
      "record record-field empty"
  | TRecordField _ ->
      "record record-field"
  | TRecordSep _ ->
      "record record-sep"


let tid (t : token) : id =
  match t with
  | TInteger (id, _)
  | TBlank id
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
  | TFnName (id, _)
  | TLambdaVar (id, _)
  | TLambdaArrow id
  | TLambdaSymbol id
  | TLambdaSep id
  | TListOpen id
  | TListClose id
  | TListSep id
  | TRecordOpen id
  | TRecordClose id
  | TRecordField (id, _, _)
  | TRecordSep (id, _) ->
      id
  | TSep | TNewline | TIndented _ | TIndent _ | TIndentToHere _ ->
      "no-id"


type tokenInfo =
  { startRow : int
  ; startCol : int
  ; startPos : int
  ; endPos : int
  ; length : int
  ; token : token }

let show_tokenInfo (ti : tokenInfo) =
  Printf.sprintf
    "start: %d, end: %d, len: %d, id: %s, %s (%s)"
    ti.startPos
    ti.endPos
    ti.length
    (toText ti.token)
    (tid ti.token)
    (toName ti.token)


let rec toTokens' (e : expr) : token list =
  let nameToString b = match b with B -> "" | F name -> name in
  let nested b = TIndentToHere (toTokens' b) in
  match e with
  | EInteger (id, i) ->
      [TInteger (id, string_of_int i)]
  | EBlank id ->
      [TBlank id]
  | EPartial (id, str) ->
      [TPartial (id, str)]
  | ELet (id, lhs, rhs, next) ->
      [ TLetKeyword id
      ; TLetLHS (id, nameToString lhs)
      ; TLetAssignment id
      ; nested rhs
      ; TNewline
      ; nested next ]
  | EString (id, str) ->
      [TString (id, str)]
  | EIf (id, cond, if', else') ->
      [ TIfKeyword id
      ; nested cond
      ; TNewline
      ; TIfThenKeyword id
      ; TNewline
      ; TIndent 2
      ; nested if'
      ; TNewline
      ; TIfElseKeyword id
      ; TNewline
      ; TIndent 2
      ; nested else' ]
  | EBinOp (id, op, lexpr, rexpr) ->
      let opT = match op with B -> TBlank id | F name -> TBinOp (id, name) in
      [nested lexpr; TSep; opT; TSep; nested rexpr]
  | EFieldAccess (id, expr, fieldname) ->
      [nested expr; TFieldOp id; TFieldName (id, nameToString fieldname)]
  | EVariable (id, name) ->
      [TVariable (id, name)]
  | ELambda (id, names, body) ->
      let tnames =
        List.map names ~f:(fun name -> TLambdaVar (id, nameToString name))
        |> List.intersperse (TLambdaSep id)
      in
      [TLambdaSymbol id] @ tnames @ [TLambdaArrow id; nested body]
  | EFnCall (id, fnName, exprs) ->
      [TFnName (id, nameToString fnName)]
      @ (exprs |> List.map ~f:(fun e -> [TSep; nested e]) |> List.concat)
  | EList (id, exprs) ->
      let ts =
        exprs
        |> List.map ~f:(fun expr -> [nested expr; TListSep id])
        |> List.concat
        (* Remove the extra seperator *)
        |> List.dropRight ~count:1
      in
      [TListOpen id] @ ts @ [TListClose id]
  | ERecord (id, fields) ->
      if fields = []
      then [TRecordOpen id; TRecordClose id]
      else
        [ [TRecordOpen id]
        ; List.mapi fields ~f:(fun i (fname, expr) ->
              [ TNewline
              ; TIndentToHere
                  [ TIndent 2
                  ; TRecordField (id, i, nameToString fname)
                  ; TSep
                  ; TRecordSep (id, i)
                  ; TSep
                  ; nested expr ] ] )
          |> List.concat
        ; [TNewline; TRecordClose id] ]
        |> List.concat


(* TODO: we need some sort of reflow thing that handles line length. *)
let rec reflow ~(x : int) (startingTokens : token list) : int * token list =
  let startingX = x in
  List.foldl startingTokens ~init:(x, []) ~f:(fun t (x, old) ->
      let text = toText t in
      let len = String.length text in
      match t with
      | TIndented tokens ->
          let newX, newTokens = reflow ~x:(x + 2) tokens in
          (newX, old @ [TIndent (startingX + 2)] @ newTokens)
      | TIndentToHere tokens ->
          let newX, newTokens = reflow ~x tokens in
          (newX, old @ newTokens)
      | TNewline ->
          if startingX = 0
          then (startingX, old @ [t])
          else (startingX, old @ [t; TIndent startingX])
      | _ ->
          (x + len, old @ [t]) )


let infoize ~(pos : int) tokens : tokenInfo list =
  let row, col = (ref 0, ref 0) in
  let rec makeInfo p ts =
    match ts with
    | [] ->
        []
    | token :: rest ->
        let length = String.length (toText token) in
        let ti =
          { token
          ; startRow = !row
          ; startCol = !col
          ; startPos = p
          ; endPos = p + length
          ; length }
        in
        ( match token with
        | TNewline ->
            row := !row + 1 ;
            col := 0
        | _ ->
            col := !col + length ) ;
        ti :: makeInfo (p + length) rest
  in
  makeInfo pos tokens


let toTokens (e : expr) : tokenInfo list =
  e |> toTokens' |> reflow ~x:0 |> Tuple2.second |> infoize ~pos:0


let eToString (e : expr) : string =
  e
  |> toTokens
  |> List.map ~f:(fun ti -> toTestText ti.token)
  |> String.join ~sep:""


let eDebug (e : expr) : string =
  e |> eToString |> Regex.replace ~re:(Regex.regex " ") ~repl:"."


let eToStructure (e : expr) : string =
  e
  |> toTokens
  |> List.map ~f:(fun ti -> "<" ^ toName ti.token ^ ":" ^ toText ti.token ^ ">")
  |> String.join ~sep:""


(* -------------------- *)
(* TEA *)
(* -------------------- *)

type msg =
  | KeyPress of K.keyEvent
  | MouseClick
  | NoEvent

type model =
  { error : string option
  ; actions : string list
  ; expr : expr
  ; oldPos : int
  ; newPos : int
  ; upDownCol :
      int option
      (* When moving up or down, and going through whitespace, track the column
       * so we can go back to it *)
  ; acPos : int option
  ; lastKey : K.key }

let emptyM =
  { expr = startingExpr
  ; actions = []
  ; error = None
  ; oldPos = 0
  ; newPos = 0
  ; upDownCol = None
  ; acPos = Some 0
  ; lastKey = K.Escape }


(* -------------------- *)
(* Direct canvas interaction *)
(* -------------------- *)

external jsGetCursorPosition : unit -> int Js.Nullable.t = "getCursorPosition"
  [@@bs.val]

external jsSetCursorPosition : int -> unit = "setCursorPosition" [@@bs.val]

let getCursorPosition () : int option =
  jsGetCursorPosition () |> Js.Nullable.toOption


let setCursorPosition (v : int) : unit = jsSetCursorPosition v

let editorID = "editor"

let setPos offset =
  Tea.Cmd.call (fun _ ->
      (* We need to set this in the new frame, as updating sets the cursor to
       * the start of the DOM node. *)
      ignore
        (Web.Window.requestAnimationFrame (fun _ -> setCursorPosition offset)) ;
      () )


(* -------------------- *)
(* Autocomplete *)
(* -------------------- *)

type acEntry =
  | ACFunction of string * int
  | ACLet
  | ACIf
  | ACVariable of string

let acEntryIsVariable e = match e with ACVariable _ -> true | _ -> false

let acEntryToString (entry : acEntry) =
  match entry with
  | ACFunction (name, _) ->
      name
  | ACVariable name ->
      name
  | ACLet ->
      "let "
  | ACIf ->
      "if "


let functions =
  [ ACFunction ("Http::Forbidden", 0)
  ; ACFunction ("String::reverse", 1)
  ; ACFunction ("List::insert", 2)
  ; ACFunction ("List::range", 2)
  ; ACFunction ("List::map", 2)
  ; ACFunction ("DB::set", 3) ]


let acItems m =
  let vars =
    m.expr
    |> toTokens
    |> List.filterMap ~f:(fun ti ->
           match ti.token with
           | TLetLHS (_, x) ->
               Some (ACVariable x)
           | TLambdaVar (_, x) ->
               Some (ACVariable x)
           | _ ->
               None )
  in
  [ACVariable "request"; ACVariable "Users"] @ vars @ [ACLet; ACIf] @ functions


let acMatches (str : string) (entry : acEntry) =
  let str = String.trim str in
  String.contains
    ~substring:(String.toLower str)
    (entry |> acEntryToString |> String.toLower)


let acSelected (str : string) (m : model) : acEntry option =
  match m.acPos with
  | None ->
      None
  | Some index ->
      acItems m |> List.filter ~f:(acMatches str) |> List.getAt ~index


let acExpr (entry : acEntry) : expr * int =
  match entry with
  | ACFunction (name, count) ->
      let args = Belt.List.makeBy count (fun _ -> EBlank (gid ())) in
      (EFnCall (gid (), F name, args), String.length name + 1)
  | ACLet ->
      (ELet (gid (), B, newB (), newB ()), 4)
  | ACIf ->
      (EIf (gid (), newB (), newB (), newB ()), 3)
  | ACVariable name ->
      (EVariable (gid (), name), String.length name)


let isAutocompleting (ti : tokenInfo) (m : model) : bool =
  isAutocompletable ti.token
  && m.upDownCol = None
  && m.acPos <> None
  && m.newPos <= ti.endPos
  && m.newPos >= ti.startPos


(* -------------------- *)
(* Update *)
(* -------------------- *)

let recordAction ?(pos = -1000) (action : string) (m : model) : model =
  let action =
    if pos = -1000 then action else action ^ " " ^ string_of_int pos
  in
  {m with actions = m.actions @ [action]}


let isTextToken token : bool =
  match token with
  | TInteger _
  | TLetLHS _
  | TBinOp _
  | TFieldName _
  | TVariable _
  | TFnName _
  | TBlank _
  | TPartial _
  | TRecordField _
  | TString _
  | TLambdaVar _ ->
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
  | TLambdaArrow _ ->
      false


(* if the cursor is at the end of this token, we take it as editing this
* token, rather than writing the next token. *)
let isAppendable token : bool =
  match token with
  (* String should really be directly editable, but the extra quote at the end
   makes it not so. *)
  | TString _ ->
      false
  | _ ->
      isTextToken token


let isSkippable (token : token) : bool =
  match token with TIndent _ -> true | _ -> false


let isAtom (token : token) : bool =
  match token with
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TLetKeyword _
  | TLambdaArrow _ ->
      true
  | TListOpen _
  | TListClose _
  | TListSep _
  | TInteger _
  | TString _
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TFieldOp _
  | TFieldName _
  | TVariable _
  | TFnName _
  | TBlank _
  | TLetLHS _
  | TLetAssignment _
  | TRecordField _
  | TBinOp _
  | TSep
  | TNewline
  | TIndented _
  | TIndent _
  | TIndentToHere _
  | TPartial _
  | TLambdaSymbol _
  | TLambdaSep _
  | TLambdaVar _ ->
      false


let length (tokens : token list) : int =
  tokens |> List.map ~f:toText |> List.map ~f:String.length |> List.sum


(* Returns the token to the left and to the right. Ignores indent tokens *)

type neighbour =
  | L of token * tokenInfo
  | R of token * tokenInfo
  | No

let rec getTokensAtPosition
    ?(prev = None) ~(pos : int) (tokens : tokenInfo list) :
    tokenInfo option * tokenInfo option * tokenInfo option =
  (* Get the next token and the remaining tokens, skipping indents. *)
  let rec getNextToken (infos : tokenInfo list) :
      tokenInfo option * tokenInfo list =
    match infos with
    | ti :: rest ->
        if isSkippable ti.token then getNextToken rest else (Some ti, rest)
    | [] ->
        (None, [])
  in
  match getNextToken tokens with
  | None, _remaining ->
      (prev, None, None)
  | Some current, remaining ->
      if current.endPos > pos
      then
        let next, _ = getNextToken remaining in
        (prev, Some current, next)
      else getTokensAtPosition ~prev:(Some current) ~pos remaining


let getNeighbours ~(pos : int) (tokens : tokenInfo list) :
    neighbour * neighbour * tokenInfo option =
  let mPrev, mCurrent, mNext = getTokensAtPosition ~pos tokens in
  let toTheRight =
    match mCurrent with Some current -> R (current.token, current) | _ -> No
  in
  (* The token directly before the cursor (skipping whitespace) *)
  let toTheLeft =
    match (mPrev, mCurrent) with
    | Some prev, _ when prev.endPos >= pos ->
        L (prev.token, prev)
    (* The left might be separated by whitespace *)
    | Some prev, Some current when current.startPos >= pos ->
        L (prev.token, prev)
    | _, Some current ->
        L (current.token, current)
    | None, None ->
        No
    | _ ->
        Js.log "Unexpect toTheLeft node" ;
        No
  in
  (toTheLeft, toTheRight, mNext)


type gridPos =
  { row : int
  ; col : int }

(* Result will definitely be a valid position. *)
let gridFor ~(pos : int) (tokens : tokenInfo list) : gridPos =
  let ti =
    List.find tokens ~f:(fun ti -> ti.startPos <= pos && ti.endPos >= pos)
  in
  match ti with
  | Some ti ->
      if ti.token = TNewline
      then {row = ti.startRow + 1; col = 0}
      else {row = ti.startRow; col = ti.startCol + (pos - ti.startPos)}
  | _ ->
      {row = 0; col = 0}


(* Result will definitely be a valid position. *)
let posFor ~(row : int) ~(col : int) (tokens : tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let ti =
      List.find tokens ~f:(fun ti ->
          ti.startRow = row
          && ti.startCol <= col
          && ti.startCol + ti.length >= col )
    in
    match ti with
    | Some ti ->
        ti.startPos + (col - ti.startCol)
    | None ->
      (match List.last tokens with None -> 0 | Some last -> last.endPos)


(* Ensures that the position places will be valid within the code. When placed
 * in an indent, will be moved to the next char. Empty lines get moved to the
 * start. When placed past the end of a line, will go on the end of the last
 * item in it. *)
let adjustedPosFor ~(row : int) ~(col : int) (tokens : tokenInfo list) : int =
  if row < 0 || col < 0
  then 0
  else
    let thisRow = List.filter tokens ~f:(fun ti -> ti.startRow = row) in
    let ti =
      List.find thisRow ~f:(fun ti ->
          ti.startCol <= col && ti.startCol + ti.length > col )
    in
    match ti with
    | Some ti ->
        ti.startPos + (col - ti.startCol)
    | None ->
      ( match (List.head thisRow, List.last thisRow) with
      | Some h, _ when col < h.startCol ->
          h.startPos
      | _, Some l when col >= l.startCol ->
        ( match l.token with
        | TNewline ->
            l.startPos
        | _ ->
            l.startPos + l.length )
      | None, None ->
          posFor ~row ~col:0 tokens
      | _, _ ->
          fail "adjustedPosFor" )


(* ------------- *)
(* Getting expressions *)
(* ------------- *)
let rec findExpr (id : id) (expr : expr) : expr option =
  let fe = findExpr id in
  if eid expr = id
  then Some expr
  else
    match expr with
    | EInteger _ | EBlank _ | EString _ | EVariable _ | EPartial _ ->
        None
    | ELet (_, _, rhs, next) ->
        fe rhs |> Option.orElse (fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond |> Option.orElse (fe ifexpr) |> Option.orElse (fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr) ->
        fe lexpr |> Option.orElse (fe rexpr)
    | EFieldAccess (_, expr, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields
        |> List.map ~f:Tuple2.second
        |> List.filterMap ~f:fe
        |> List.head
    | EFnCall (_, _, exprs) | EList (_, exprs) ->
        List.filterMap ~f:fe exprs |> List.head


let isEmpty (e : expr) : bool =
  let isBlank e = match e with EBlank _ -> true | _ -> false in
  match e with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (k, v) -> k = B && not (isBlank v))
      |> List.isEmpty
  | EList (_, l) ->
      l |> List.filter ~f:(not << isBlank) |> List.isEmpty
  | _ ->
      false


let exprIsEmpty (id : id) (m : model) : bool =
  match findExpr id m.expr with Some expr -> isEmpty expr | _ -> false


let findParent (id : id) (expr : expr) : expr option =
  let rec findParent' ~(parent : expr option) (id : id) (expr : expr) :
      expr option =
    let fp = findParent' ~parent:(Some expr) id in
    if eid expr = id
    then parent
    else
      match expr with
      | EInteger _ | EBlank _ | EString _ | EVariable _ | EPartial _ ->
          None
      | ELet (_, _, rhs, next) ->
          fp rhs |> Option.orElse (fp next)
      | EIf (_, cond, ifexpr, elseexpr) ->
          fp cond |> Option.orElse (fp ifexpr) |> Option.orElse (fp elseexpr)
      | EBinOp (_, _, lexpr, rexpr) ->
          fp lexpr |> Option.orElse (fp rexpr)
      | EFieldAccess (_, expr, _) | ELambda (_, _, expr) ->
          fp expr
      | ERecord (_, fields) ->
          fields
          |> List.map ~f:Tuple2.second
          |> List.filterMap ~f:fp
          |> List.head
      | EFnCall (_, _, exprs) | EList (_, exprs) ->
          List.filterMap ~f:fp exprs |> List.head
  in
  findParent' ~parent:None id expr


(* ------------- *)
(* Replacing expressions *)
(* ------------- *)
(* f needs to call recurse or it won't go far *)
let recurse ~(f : expr -> expr) (expr : expr) : expr =
  match expr with
  | EInteger _ | EBlank _ | EString _ | EVariable _ | EPartial _ ->
      expr
  | ELet (id, name, rhs, next) ->
      ELet (id, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr) ->
      EBinOp (id, op, f lexpr, f rexpr)
  | EFieldAccess (id, expr, fieldname) ->
      EFieldAccess (id, f expr, fieldname)
  | EFnCall (id, name, exprs) ->
      EFnCall (id, name, List.map ~f exprs)
  | ELambda (id, names, expr) ->
      ELambda (id, names, f expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | ERecord (id, fields) ->
      ERecord (id, List.map ~f:(fun (name, expr) -> (name, f expr)) fields)


let wrap ~(f : expr -> expr) (id : id) (m : model) : model =
  let rec run e = if id = eid e then f e else recurse ~f:run e in
  {m with expr = run m.expr}


let replaceExpr ~(newExpr : expr) (id : id) (m : model) : model =
  wrap id m ~f:(fun _ -> newExpr)


let removeField (id : id) (m : model) : model =
  wrap id m ~f:(fun e ->
      match e with
      | EFieldAccess (_, faExpr, _) ->
          faExpr
      | _ ->
          fail "not a fieldAccess" )


let removeRecordField (id : id) (index : int) (m : model) : model =
  wrap id m ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.removeAt ~index fields)
      | _ ->
          fail "not a record field" )


let exprToFieldAccess (id : id) (m : model) : model =
  wrap id m ~f:(fun e -> EFieldAccess (gid (), e, B))


let removeEmptyExpr (id : id) (m : model) =
  wrap id m ~f:(fun expr ->
      match expr with
      | ELet (_, B, EBlank _, e) ->
          e
      | EIf (_, EBlank _, EBlank _, EBlank _) ->
          newB ()
      | ELambda (_, _, EBlank _) ->
          newB ()
      | _ ->
          expr )


let replaceString (str : string) (id : id) (m : model) : model =
  wrap id m ~f:(fun e ->
      match e with
      | EInteger (id, _) ->
          if str = ""
          then EBlank id
          else
            let value = try int_of_string str with _ -> 0 in
            EInteger (id, value)
      | EString (id, _) ->
          EString (id, str)
      | EVariable (id, _) ->
          if str = "" then EBlank id else EPartial (id, str)
      | EPartial (id, _) ->
          if str = "" then EBlank id else EPartial (id, str)
      | EFieldAccess (id, expr, _) ->
          if str = ""
          then EFieldAccess (id, expr, B)
          else EFieldAccess (id, expr, F str)
      | ELet (id, _, rhs, next) ->
          if str = ""
          then ELet (id, B, rhs, next)
          else ELet (id, F str, rhs, next)
      | ELambda (id, vars, expr) ->
          let rest = List.tail vars |> Option.withDefault ~default:[] in
          if str = ""
          then ELambda (id, B :: rest, expr)
          else ELambda (id, F str :: rest, expr)
      | _ ->
          fail "not a string type" )


let replaceRecordField ~index (str : string) (id : id) (m : model) : model =
  wrap id m ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          let fields =
            List.updateAt fields ~index ~f:(fun (_, expr) -> (F str, expr))
          in
          ERecord (id, fields)
      | _ ->
          fail "not a record" )


(* Supports the various different tokens replacing their string contents.
 * Doesn't do movement. *)
let replaceStringToken ~(f : string -> string) (token : token) (m : model) :
    model =
  match token with
  | TString (id, str) ->
      replaceExpr id ~newExpr:(EString (gid (), f str)) m
  | TRecordField (id, index, str) ->
      replaceRecordField ~index (f str) id m
  | TInteger (id, str)
  | TVariable (id, str)
  | TPartial (id, str)
  | TLetLHS (id, str)
  | TLambdaVar (id, str)
  | TFieldName (id, str) ->
      replaceString (f str) id m
  | _ ->
      fail "not supported by replaceToken"


let insertInList ~(index : int) ~(newExpr : expr) (id : id) (m : model) : model
    =
  wrap id m ~f:(fun expr ->
      match expr with
      | EList (id, exprs) ->
          EList (id, List.insertAt ~index ~value:newExpr exprs)
      | _ ->
          fail "not a list" )


(* Add a blank after the expr indicated by id, which we presume is in a list *)
let addBlankToList (id : id) (m : model) : model =
  let parent = findParent id m.expr in
  match parent with
  | Some (EList (pID, exprs)) ->
      wrap pID m ~f:(fun expr ->
          match List.findIndex ~f:(fun e -> eid e = id) exprs with
          | Some index ->
              EList
                ( pID
                , List.insertAt
                    ~index:(index + 1)
                    ~value:(EBlank (gid ()))
                    exprs )
          | _ ->
              expr )
  | _ ->
      m


(* Add a row to the record *)
let addRecordRowToFront (id : id) (m : model) : model =
  wrap id m ~f:(fun expr ->
      match expr with
      | ERecord (id, fields) ->
          ERecord (id, (B, newB ()) :: fields)
      | _ ->
          fail "Not a record" )


let convertToBinOp (char : char option) (id : id) (m : model) : model =
  match char with
  | None ->
      m
  | Some c ->
      wrap id m ~f:(fun expr ->
          EBinOp (gid (), F (String.fromChar c), expr, newB ()) )


let moveToNextNonWhitespaceToken ~pos (m : model) : model =
  let m = recordAction ~pos "moveToNextNonWhitespaceToken" m in
  let tokens = toTokens m.expr in
  let rec getNextWS tokens =
    match tokens with
    | [] ->
        pos
    | ti :: rest ->
      ( match ti.token with
      | TSep | TNewline | TIndent _ | TIndentToHere _ | TIndented _ ->
          getNextWS rest
      | _ ->
          if pos > ti.startPos then getNextWS rest else ti.startPos )
  in
  let newPos = getNextWS tokens in
  {m with newPos; upDownCol = None}


let moveToEnd (ti : tokenInfo) (m : model) : model =
  let m = recordAction "moveToEnd" m in
  {m with newPos = ti.endPos - 1; upDownCol = None}


let moveToStart (ti : tokenInfo) (m : model) : model =
  let m = recordAction ~pos:ti.startPos "moveToStart" m in
  {m with newPos = ti.startPos; upDownCol = None}


let moveToAfter (ti : tokenInfo) (m : model) : model =
  let m = recordAction ~pos:ti.endPos "moveToAfter" m in
  {m with newPos = ti.endPos; upDownCol = None}


let moveOneLeft (pos : int) (m : model) : model =
  let m = recordAction ~pos "moveOneLeft" m in
  {m with newPos = max 0 (pos - 1); upDownCol = None}


let moveOneRight (pos : int) (m : model) : model =
  let m = recordAction ~pos "moveOneRight" m in
  {m with newPos = pos + 1; upDownCol = None}


let moveTo (newPos : int) (m : model) : model =
  let m = recordAction ~pos:newPos "moveTo" m in
  {m with newPos}


(* TODO: rewrite nextBlank like prevBlank *)
let moveToNextBlank ~(pos : int) (m : model) : model =
  let m = recordAction ~pos "moveToNextBlank" m in
  let tokens = toTokens m.expr in
  let rec getNextBlank pos' tokens' =
    match tokens' with
    | [] ->
        (* Wrap, unless we've already wrapped *)
        if pos' = -1 then 0 else getNextBlank (-1) tokens
    | ti :: rest ->
        if isBlank ti.token && ti.startPos > pos'
        then ti.startPos
        else getNextBlank pos' rest
  in
  let newPos = getNextBlank pos tokens in
  {m with newPos; upDownCol = None}


let moveToPrevBlank ~(pos : int) (m : model) : model =
  let m = recordAction ~pos "moveToPrevBlank" m in
  let tokens =
    toTokens m.expr |> List.filter ~f:(fun ti -> isBlank ti.token)
  in
  let rec getPrevBlank pos' tokens' =
    match tokens' with
    | [] ->
      (match List.last tokens with None -> 0 | Some ti -> ti.startPos)
    | ti :: rest ->
        if ti.endPos < pos' then ti.startPos else getPrevBlank pos' rest
  in
  let newPos = getPrevBlank pos (List.reverse tokens) in
  {m with newPos; upDownCol = None}


let acMoveUp (m : model) : model =
  let m = recordAction "acMoveUp" m in
  let acPos =
    match m.acPos with
    | None ->
        Some 0
    | Some current ->
        Some (max 0 (current - 1))
  in
  {m with acPos; upDownCol = None}


let acMoveDown (m : model) : model =
  let m = recordAction "acMoveDown" m in
  let acPos =
    match m.acPos with
    | None ->
        Some 0
    | Some current ->
        Some (min (current + 1) (List.length (acItems m) - 1))
  in
  {m with acPos; upDownCol = None}


let report (e : string) (m : model) =
  let m = recordAction "report" m in
  {m with error = Some e}


let acEnter (ti : tokenInfo) (str : string) (m : model) : model =
  let m = recordAction "acEnter" m in
  match acSelected str m with
  | None ->
      m
  | Some entry ->
      let newExpr, length = acExpr entry in
      let id = tid ti.token in
      {m with acPos = None}
      |> replaceExpr ~newExpr id
      (* TODO: the correct thing is to decide on where to go based on context: Enter
      * stops at the end, space goes one space ahead, tab goes to next blank
      * *)
      |> moveTo (ti.startPos + length)


let acCompleteField (ti : tokenInfo) (str : string) (m : model) : model =
  match acSelected str m with
  | None ->
      m
  | Some entry ->
      let newExpr, length = acExpr entry in
      let newExpr = EFieldAccess (gid (), newExpr, B) in
      let length = length + 1 in
      {m with acPos = None}
      |> replaceExpr ~newExpr (tid ti.token)
      |> moveTo (ti.startPos + length)


let doBackspace ~(pos : int) (ti : tokenInfo) (m : model) : model =
  let m = recordAction "doBackspace" m in
  let left m = moveOneLeft (min pos ti.endPos) m in
  let offset =
    match ti.token with
    | TString _ ->
        pos - ti.startPos - 2
    | _ ->
        pos - ti.startPos - 1
  in
  let newID = gid () in
  match ti.token with
  | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ ->
      moveToStart ti m
  | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ ->
      let newM = removeEmptyExpr (tid ti.token) m in
      if newM.expr = m.expr then m else newM |> moveToStart ti
  | TString (id, "") ->
      m |> replaceExpr id ~newExpr:(EBlank newID) |> left
  | (TRecordOpen id | TListOpen id) when exprIsEmpty id m ->
      m |> replaceExpr id ~newExpr:(EBlank newID) |> left
  | TRecordField (id, i, "") when pos = ti.startPos ->
      m
      |> removeRecordField id i
      |> left
      |> fun m -> moveOneLeft (m.newPos - 1) m
  | TBinOp _
  | TBlank _
  | TFnName _
  | TIndent _
  | TIndentToHere _
  | TIndented _
  | TLetAssignment _
  | TListClose _
  | TListOpen _
  | TListSep _
  | TNewline
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TSep
  | TLambdaSep _ ->
      left m
  | TFieldOp id ->
      m |> removeField id |> left
  | TString _
  | TRecordField _
  | TInteger _
  | TVariable _
  | TPartial _
  | TFieldName _
  | TLetLHS _
  | TLambdaVar _ ->
      let f str = removeCharAt str offset in
      replaceStringToken ~f ti.token m |> left


let doDelete ~(pos : int) (ti : tokenInfo) (m : model) : model =
  let m = recordAction "doDelete" m in
  let left m = moveOneLeft pos m in
  let offset = pos - ti.startPos in
  let newID = gid () in
  match ti.token with
  | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ ->
      m
  | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ ->
      let newM = removeEmptyExpr (tid ti.token) m in
      if newM.expr = m.expr then m else newM
  | (TListOpen id | TRecordOpen id) when exprIsEmpty id m ->
      replaceExpr id ~newExpr:(newB ()) m
  | TBinOp _
  | TBlank _
  | TFnName _
  | TIndent _
  | TIndentToHere _
  | TIndented _
  | TLetAssignment _
  | TListClose _
  | TListOpen _
  | TListSep _
  | TNewline
  | TRecordClose _
  | TRecordOpen _
  | TRecordSep _
  | TSep
  | TLambdaSep _ ->
      m
  | TFieldOp id ->
      removeField id m
  | TString (id, str) ->
      let target m =
        (* if we're in front of the quotes vs within it *)
        if offset == 0 then m else left m
      in
      if str = ""
      then m |> replaceExpr id ~newExpr:(EBlank newID) |> target
      else
        let str = removeCharAt str (offset - 1) in
        replaceExpr id ~newExpr:(EString (newID, str)) m
  | TRecordField _
  | TInteger _
  | TVariable _
  | TPartial _
  | TFieldName _
  | TLetLHS _
  | TLambdaVar _ ->
      let f str = removeCharAt str offset in
      replaceStringToken ~f ti.token m


let doLeft ~(pos : int) (ti : tokenInfo) (m : model) : model =
  let m = recordAction ~pos "doLeft" m in
  if isAtom ti.token
  then moveToStart ti m
  else moveOneLeft (min pos ti.endPos) m


let doRight
    ~(pos : int) ~(next : tokenInfo option) (current : tokenInfo) (m : model) :
    model =
  let m = recordAction ~pos "doRight" m in
  match current.token with
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TLetKeyword _
  | TLambdaArrow _ ->
    ( match next with
    | None ->
        moveToAfter current m
    | Some nInfo ->
        moveToStart nInfo m )
  | TIndent _
  | TIndented _
  | TIndentToHere _
  | TInteger _
  | TString _
  | TFieldOp _
  | TFieldName _
  | TVariable _
  | TFnName _
  | TBlank _
  | TLetLHS _
  | TLetAssignment _
  | TBinOp _
  | TSep
  | TListOpen _
  | TListClose _
  | TListSep _
  | TNewline
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TPartial _
  | TRecordField _
  | TLambdaVar _
  | TLambdaSymbol _
  | TLambdaSep _ ->
    ( match next with
    | Some n when pos + 1 >= current.endPos ->
        moveToStart n m
    | _ ->
        (* When we're in whitespace, current is the next non-whitespace. So we
         * don't want to use pos, we want to use the startPos of current. *)
        let startingPos = max pos (current.startPos - 1) in
        moveOneRight startingPos m )


let doUp ~(pos : int) (m : model) : model =
  let m = recordAction "doUp" m in
  let tokens = toTokens m.expr in
  let {row; col} = gridFor ~pos tokens in
  let col = match m.upDownCol with None -> col | Some savedCol -> savedCol in
  if row = 0
  then moveTo 0 m
  else
    let pos = adjustedPosFor ~row:(row - 1) ~col tokens in
    moveTo pos {m with upDownCol = Some col}


let doDown ~(pos : int) (m : model) : model =
  let m = recordAction "doDown" m in
  let tokens = toTokens m.expr in
  let {row; col} = gridFor ~pos tokens in
  let col = match m.upDownCol with None -> col | Some savedCol -> savedCol in
  let pos = adjustedPosFor ~row:(row + 1) ~col tokens in
  moveTo pos {m with upDownCol = Some col}


let isRealCharacter (letter : string) : bool = String.length letter = 1

let doInsert' ~pos (letter : char) (ti : tokenInfo) (m : model) : model =
  let m = recordAction "doInsert" m in
  let m = {m with upDownCol = None} in
  let letterStr = String.fromChar letter in
  let offset =
    match ti.token with
    | TString _ ->
        pos - ti.startPos - 1
    | _ ->
        pos - ti.startPos
  in
  let newID = gid () in
  let newExpr =
    if letter = '"'
    then EString (newID, "")
    else if letter = '['
    then EList (newID, [])
    else if letter = '{'
    then ERecord (newID, [])
    else if letter = '\\'
    then ELambda (newID, [B], EBlank (gid ()))
    else if letter = ','
    then EBlank newID (* new separators *)
    else if isNumber letterStr
    then EInteger (newID, letterStr |> int_of_string)
    else EPartial (newID, letterStr)
  in
  match ti.token with
  | (TFieldName (id, _) | TVariable (id, _))
    when pos = ti.endPos && letter = '.' ->
      m |> exprToFieldAccess id |> moveOneRight pos
  (* replace blank *)
  | TBlank id ->
      m |> replaceExpr id ~newExpr |> moveTo (ti.startPos + 1)
  (* lists *)
  | TListOpen id ->
      m |> insertInList ~index:0 id ~newExpr |> moveTo (ti.startPos + 2)
  (* Ignore invalid situations *)
  | TString _ when offset < 0 ->
      m
  | TInteger _ when not (isNumber letterStr) ->
      m
  | TVariable _ when not (isIdentifierChar letterStr) ->
      m
  | TLetLHS _ when not (isIdentifierChar letterStr) ->
      m
  | TFieldName _ when not (isIdentifierChar letterStr) ->
      m
  | TLambdaVar _ when not (isIdentifierChar letterStr) ->
      m
  | TFnName _ when not (isFnNameChar letterStr) ->
      m
  (* Do the insert *)
  | TLetLHS (_, "") ->
      let f str = String.insertAt ~index:offset ~insert:letterStr str in
      replaceStringToken ~f ti.token m |> moveTo (ti.startPos + 1)
  | TRecordField _
  | TFieldName _
  | TVariable _
  | TPartial _
  | TInteger _
  | TString _
  | TLetLHS _
  | TLambdaVar _ ->
      let f str = String.insertAt ~index:offset ~insert:letterStr str in
      replaceStringToken ~f ti.token m |> moveOneRight pos
  (* do nothing *)
  | TNewline
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TFieldOp _
  | TFnName _
  | TLetKeyword _
  | TLetAssignment _
  | TBinOp _
  | TSep
  | TIndented _
  | TIndentToHere _
  | TListClose _
  | TListSep _
  | TIndent _
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TLambdaSymbol _
  | TLambdaArrow _
  | TLambdaSep _ ->
      m


let doInsert ~pos (letter : char option) (ti : tokenInfo) (m : model) : model =
  match letter with None -> m | Some letter -> doInsert' ~pos letter ti m


let updateKey (key : K.key) (m : model) : model =
  let pos = m.newPos in
  let keyChar = K.toChar key in
  let tokens = toTokens m.expr in
  (* These might be the same token *)
  let toTheLeft, toTheRight, mNext = getNeighbours ~pos tokens in
  let onEdge =
    match (toTheLeft, toTheRight) with
    | L (lt, lti), R (rt, rti) ->
        (lt, lti) <> (rt, rti)
    | _ ->
        true
  in
  let newM =
    (* TODO: When changing TVariable and TFieldName and probably TFnName we
     * should convert them to a partial which retains the old object *)
    match (key, toTheLeft, toTheRight) with
    (* Deleting *)
    | K.Backspace, L (TString _, ti), _ when pos = ti.endPos ->
        (* Backspace should move into a string, not delete it *)
        moveOneLeft pos m
    | K.Backspace, _, R (TRecordField (_, _, ""), ti) ->
        doBackspace ~pos ti m
    | K.Backspace, L (_, ti), _ ->
        doBackspace ~pos ti m
    | K.Delete, _, R (_, ti) ->
        doDelete ~pos ti m
    (* Tab to next blank *)
    | K.Tab, _, R (_, ti)
      when exprIsEmpty (tid ti.token) m || not (isAutocompleting ti m) ->
        moveToNextBlank ~pos m
    | K.Tab, L (_, ti), _
      when exprIsEmpty (tid ti.token) m || not (isAutocompleting ti m) ->
        moveToNextBlank ~pos m
    | K.ShiftTab, _, R (_, ti)
      when exprIsEmpty (tid ti.token) m || not (isAutocompleting ti m) ->
        moveToPrevBlank ~pos m
    | K.ShiftTab, L (_, ti), _
      when exprIsEmpty (tid ti.token) m || not (isAutocompleting ti m) ->
        moveToPrevBlank ~pos m
    (* Autocomplete menu *)
    (* Note that these are spelt out explicitly on purpose, else they'll
     * trigger on the wrong element sometimes. *)
    | K.Escape, L (_, ti), _ when isAutocompleting ti m ->
        {m with acPos = None}
    | K.Escape, _, R (_, ti) when isAutocompleting ti m ->
        {m with acPos = None}
    | K.Up, _, R (_, ti) when isAutocompleting ti m ->
        acMoveUp m
    | K.Up, L (_, ti), _ when isAutocompleting ti m ->
        acMoveUp m
    | K.Down, _, R (_, ti) when isAutocompleting ti m ->
        acMoveDown m
    | K.Down, L (_, ti), _ when isAutocompleting ti m ->
        acMoveDown m
    (* Autocomplete finish *)
    | K.Enter, L (TPartial (_, str), ti), _
    | K.Enter, _, R (TPartial (_, str), ti)
    | K.Space, L (TPartial (_, str), ti), _
    | K.Space, _, R (TPartial (_, str), ti)
    | K.Tab, L (TPartial (_, str), ti), _
    | K.Tab, _, R (TPartial (_, str), ti) ->
        acEnter ti str m
    | K.Enter, L (TBlank _, ti), _
    | K.Enter, _, R (TBlank _, ti)
    | K.Space, L (TBlank _, ti), _
    | K.Space, _, R (TBlank _, ti)
    | K.Tab, L (TBlank _, ti), _
    | K.Tab, _, R (TBlank _, ti) ->
        acEnter ti "" m
    (* Special autocomplete entries *)
    (* press dot while in a variable entry *)
    | K.Period, L (TPartial (_, str), ti), _
      when Option.map ~f:acEntryIsVariable (acSelected str m) = Some true ->
        acCompleteField ti str m
    (* TODO: press comma while in an expr in a list *)
    (* TODO: press comma while in an expr in a record *)
    (* TODO: press equals when in a let *)
    (* TODO: press colon when in a record field *)
    (* Left/Right movement *)
    | K.Left, L (_, ti), _ ->
        doLeft ~pos ti m
    | K.Right, _, R (_, ti) ->
        doRight ~pos ~next:mNext ti m
    | K.Up, _, _ ->
        doUp ~pos m
    | K.Down, _, _ ->
        doDown ~pos m
    | K.Space, _, R (TSep, _) ->
        moveOneRight pos m
    (* list-specific insertions *)
    | K.Comma, L (TListOpen _, toTheLeft), _ ->
        doInsert ~pos keyChar toTheLeft m
    | K.Comma, L (t, ti), _ ->
        if onEdge
        then addBlankToList (tid t) m |> moveOneRight ti.endPos
        else doInsert ~pos keyChar ti m
    | K.RightCurlyBrace, _, R (TRecordClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close curly to go over the last curly *)
        moveOneRight pos m
    (* Record-specific insertions *)
    | K.Enter, L (TRecordOpen id, _), _ ->
        addRecordRowToFront id m |> moveToNextNonWhitespaceToken ~pos
    | K.RightSquareBracket, _, R (TListClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close square to go over the last square *)
        moveOneRight pos m
    (* Lambda-specific insertions *)
    (* String-specific insertions *)
    | K.DoubleQuote, _, R (TString _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing quote to go over the last quote *)
        moveOneRight pos m
    (* Field access *)
    | K.Period, L (TVariable _, toTheLeft), _
    | K.Period, L (TFieldName _, toTheLeft), _
      when onEdge ->
        doInsert ~pos keyChar toTheLeft m
    (* Binop specific *)
    | K.Percent, L (_, toTheLeft), _
    | K.Minus, L (_, toTheLeft), _
    | K.Plus, L (_, toTheLeft), _
    | K.Multiply, L (_, toTheLeft), _
    | K.ForwardSlash, L (_, toTheLeft), _
      when onEdge ->
        convertToBinOp keyChar (tid toTheLeft.token) m
    (* End of line *)
    | K.Enter, _, R (TNewline, _) ->
        moveOneRight pos m
    (* Let specific *)
    | K.Equals, _, R (TLetAssignment _, toTheRight) ->
        moveTo toTheRight.endPos m
    (* Rest of Insertions *)
    | _, L (TListOpen _, toTheLeft), R (TListClose _, _) ->
        doInsert ~pos keyChar toTheLeft m
    | _, L (_, toTheLeft), _ when isAppendable toTheLeft.token ->
        doInsert ~pos keyChar toTheLeft m
    | _, _, R (TListOpen _, _) ->
        m
    | _, _, R (_, toTheRight) ->
        doInsert ~pos keyChar toTheRight m
    | _ ->
        (* Unknown *)
        report ("Unknown action: " ^ K.toName key) m
  in
  (* Fix up autocomplete *)
  let newM =
    let oldTextToken =
      match (toTheLeft, toTheRight) with
      | _, R (t, _) when isTextToken t && isAutocompletable t ->
          Some t
      | L (t, _), _ when isTextToken t && isAutocompletable t ->
          Some t
      | _ ->
          None
    in
    let newLeft, newRight, _ =
      getNeighbours ~pos:newM.newPos (toTokens newM.expr)
    in
    let newTextToken =
      match (newLeft, newRight) with
      | _, R (t, _) when isTextToken t && isAutocompletable t ->
          Some t
      | L (t, _), _ when isTextToken t && isAutocompletable t ->
          Some t
      | _ ->
          None
    in
    match (oldTextToken, newTextToken) with
    | Some tOld, Some tNew when tOld <> tNew ->
        {newM with acPos = Some 0}
    | None, Some _ ->
        {newM with acPos = Some 0}
    | _, None ->
        {newM with acPos = None}
    | _ ->
        newM
  in
  newM


let update (m : model) (msg : msg) =
  let m = {m with error = None; oldPos = m.newPos} in
  match msg with
  | NoEvent ->
      (m, Cmd.none)
  | MouseClick ->
    ( match getCursorPosition () with
    | Some newPos ->
        ({m with newPos}, Cmd.none)
    | None ->
        ({m with error = Some "found no pos"}, Cmd.none) )
  | KeyPress {key} ->
      let m = {m with lastKey = key; actions = []} in
      let newM = updateKey key m in
      (* These might be the same token *)
      let cmd =
        if newM.expr <> m.expr || newM.oldPos <> newM.newPos
        then setPos newM.newPos
        else Cmd.none
      in
      (newM, cmd)


(* -------------------- *)
(* View *)
(* -------------------- *)
let toHtml (m : model) (l : tokenInfo list) : msg Html.html list =
  List.map l ~f:(fun ti ->
      let dropdown () =
        Html.div
          [Attrs.id "dropdown"]
          [ Html.ul
              []
              ( acItems m
              |> List.filter ~f:(acMatches (toText ti.token))
              |> List.indexedMap ~f:(fun i entry ->
                     let class' =
                       if Some i = m.acPos
                       then [Attrs.class' "selected"]
                       else []
                     in
                     let attrs = class' in
                     Html.li attrs [Html.text (acEntryToString entry)] ) ) ]
      in
      let element nested =
        let content = toText ti.token in
        let tokenName = toName ti.token in
        let idclasses = [("id-" ^ tid ti.token, true)] in
        Html.span
          (* ~key:(tid ti.token ^ toName ti.token) *)
          [Attrs.classList (("entry", true) :: (tokenName, true) :: idclasses)]
          ([Html.text content] @ nested)
      in
      if isAutocompleting ti m then element [dropdown ()] else element [] )


let view (m : model) : msg Html.html =
  let tokens = toTokens m.expr in
  let actions =
    [ Html.div
        []
        ( [Html.text "actions: "]
        @ ( m.actions
          |> List.map ~f:(fun action -> [action; ", "])
          |> List.concat
          |> List.dropRight ~count:1
          |> List.map ~f:Html.text ) ) ]
  in
  let posDiv =
    let oldGrid = gridFor ~pos:m.oldPos tokens in
    let newGrid = gridFor ~pos:m.newPos tokens in
    [ Html.div
        []
        [ Html.text (string_of_int m.oldPos)
        ; Html.text " -> "
        ; Html.text (string_of_int m.newPos) ]
    ; Html.div
        []
        [ Html.text (oldGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (oldGrid.row |> string_of_int)
        ; Html.text " -> "
        ; Html.text (newGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (newGrid.row |> string_of_int) ]
    ; Html.div
        []
        [ Html.text "acPos: "
        ; Html.text
            ( m.acPos
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; Html.div
        []
        [ Html.text "lastKey: "
        ; Html.text
            ( K.toName m.lastKey
            ^ ", "
            ^ ( K.toChar m.lastKey
              |> Option.map ~f:String.fromChar
              |> Option.withDefault ~default:"unknown" ) ) ] ]
  in
  let tokenDiv =
    let prev, current, next = getTokensAtPosition tokens ~pos:m.newPos in
    let p =
      match prev with Some prev -> show_tokenInfo prev | None -> "none"
    in
    let c =
      match current with
      | Some current ->
          show_tokenInfo current
      | None ->
          "none"
    in
    let n =
      match next with Some next -> show_tokenInfo next | None -> "none"
    in
    [ Html.text ("prev: " ^ p)
    ; Html.br []
    ; Html.text ("current: " ^ c)
    ; Html.br []
    ; Html.text ("next: " ^ n) ]
  in
  let status = List.concat [posDiv; tokenDiv; actions] in
  let editor =
    let event ~(key : string) (event : string) : msg Vdom.property =
      let decodeNothing =
        let open Tea.Json.Decoder in
        succeed NoEvent
      in
      Html.onWithOptions
        ~key
        event
        {stopPropagation = false; preventDefault = true}
        decodeNothing
    in
    Html.div
      [ Attrs.id editorID
      ; Vdom.prop "contentEditable" "true"
      ; Attrs.autofocus true
      ; Attrs.spellcheck false
      ; event ~key:"keydown" "keydown"
      (* ; event ~key:"keyup" "keyup" *)
       ]
      (m.expr |> toTokens |> toHtml m)
  in
  Html.div [Attrs.id "app"] (editor :: status)

(* -------------------- *)
(* Scaffolidng *)
(* -------------------- *)

(* let registerGlobalDirect name tagger = *)
(*   let open Vdom in *)
(*   let enableCall callbacks_base = *)
(*     let callbacks = ref callbacks_base in *)
(*     let fn ev = Some (tagger (Obj.magic ev)) in *)
(*     let handler = EventHandlerCallback (name, fn) in *)
(*     let elem = Web_node.document_node in *)
(*     let cache = eventHandler_Register callbacks elem name handler in *)
(*     fun () -> ignore (eventHandler_Unregister elem name cache) *)
(*   in *)
(*   Tea_sub.registration name enableCall *)
(*  *)

(* let subscriptions (_m : model) : msg Tea.Sub.t = *)
(*   let keySubs = [Keyboard.downs (fun x -> KeyPress x)] in *)
(*   let mouseSubs = [Mouse.ups (fun _ -> MouseClick)] in *)
(*   let events = [registerGlobalDirect "SaveEditor" (fun _ -> SaveEditor)] in *)
(*   Tea.Sub.batch (List.concat [keySubs; mouseSubs; events]) *)
(*  *)
