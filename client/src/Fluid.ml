(* open Webapi.Dom *)
open Tc
open Types
open Prelude
module K = FluidKeyboard
module Mouse = Tea.Mouse
module Regex = Util.Regex
module TL = Toplevel

(* Tea *)
module Cmd = Tea.Cmd

module Html = struct
  include Tea.Html

  type 'a html = 'a Vdom.t
end

module Attrs = Tea.Html2.Attributes
module Events = Tea.Html2.Events
module AC = FluidAutocomplete
module Token = FluidToken

(* -------------------- *)
(* Utils *)
(* -------------------- *)

let removeCharAt str offset : string =
  if offset < 0
  then str
  else
    String.slice ~from:0 ~to_:offset str
    ^ String.slice ~from:(offset + 1) ~to_:(String.length str) str


let isNumber (str : string) = Js.Re.test_ [%re "/[0-9]+/"] str

let isIdentifierChar (str : string) = Js.Re.test_ [%re "/[_a-zA-Z0-9]+/"] str

let isFnNameChar str =
  Js.Re.test_ [%re "/[_:a-zA-Z0-9]/"] str && String.length str = 1


let safe_int_of_string (s : string) : int =
  (* 2,147,483,647 *)
  try String.left ~count:10 s |> int_of_string with _ ->
    String.left ~count:9 s |> int_of_string


exception FExc of string

let fail str = raise (FExc str)

(* -------------------- *)
(* Expressions *)
(* -------------------- *)

type ast = fluidExpr [@@deriving show]

type state = Types.fluidState

let rec fromExpr (s : state) (expr : Types.expr) : fluidExpr =
  let varToName var =
    match var with Blank _ -> "" | Partial (_, name) | F (_, name) -> name
  in
  let parseString str :
      [> `Bool of bool
      | `Int of int
      | `Null
      | `Float of string * string
      | `Unknown ] =
    let asBool =
      if str = "true"
      then Some (`Bool true)
      else if str = "false"
      then Some (`Bool false)
      else if str = "null"
      then Some `Null
      else None
    in
    let asInt = try Some (`Int (int_of_string str)) with _ -> None in
    let asFloat =
      try
        (* for the exception *)
        ignore (float_of_string str) ;
        match String.split ~on:"." str with
        | [whole; fraction] ->
            Some (`Float (whole, fraction))
        | _ ->
            None
      with _ -> None
    in
    let asString =
      if String.startsWith ~prefix:"\"" str && String.endsWith ~suffix:"\"" str
      then
        Some
          (`String
            (str |> String.dropLeft ~count:1 |> String.dropRight ~count:1))
      else None
    in
    asInt
    |> Option.or_ asString
    |> Option.or_ asBool
    |> Option.or_ asFloat
    |> Option.withDefault ~default:`Unknown
  in
  let fromExpr = fromExpr s in
  match expr with
  | Blank id ->
      EBlank id
  | Partial (id, v) ->
      EPartial (id, v)
  | F (id, nExpr) ->
    ( match nExpr with
    | Let (name, rhs, body) ->
        ELet (id, Blank.toID name, varToName name, fromExpr rhs, fromExpr body)
    | Variable varname ->
        EVariable (id, varname)
    | If (cond, thenExpr, elseExpr) ->
        EIf (id, fromExpr cond, fromExpr thenExpr, fromExpr elseExpr)
    | ListLiteral exprs ->
        EList (id, List.map ~f:fromExpr exprs)
    | ObjectLiteral pairs ->
        ERecord
          ( id
          , List.map pairs ~f:(fun (k, v) ->
                (Blank.toID k, varToName k, fromExpr v) ) )
    | FieldAccess (expr, field) ->
        EFieldAccess (id, fromExpr expr, Blank.toID field, varToName field)
    | FnCall (name, args, ster) ->
        let args = List.map ~f:fromExpr args in
        let fnCall = EFnCall (id, varToName name, args, ster) in
        let fn =
          List.find s.ac.functions ~f:(fun fn -> fn.fnName = varToName name)
        in
        ( match fn with
        | Some fn when fn.fnInfix ->
          ( match args with
          | [a; b] ->
              EBinOp (id, varToName name, a, b, ster)
          | _ ->
              fnCall )
        | _ ->
            fnCall )
    | Thread exprs ->
        EThread (id, List.map ~f:fromExpr exprs)
    | Lambda (varnames, exprs) ->
        ELambda
          ( id
          , List.map varnames ~f:(fun var -> (Blank.toID var, varToName var))
          , fromExpr exprs )
    | Value str ->
      ( match parseString str with
      | `Bool b ->
          EBool (id, b)
      | `Int i ->
          EInteger (id, i)
      | `String s ->
          EString (id, s)
      | `Null ->
          ENull id
      | `Float (whole, fraction) ->
          EFloat (id, whole, fraction)
      | `Unknown ->
          Js.log2 "Getting old Value that we coudln't parse" expr ;
          EOldExpr expr )
    | Constructor (name, exprs) ->
        EConstructor
          (id, Blank.toID name, varToName name, List.map ~f:fromExpr exprs)
    | Match (mexpr, pairs) ->
        let mid = id in
        let rec fromPattern (p : pattern) : fluidPattern =
          match p with
          | Blank id ->
              FPBlank (mid, id)
          | Partial (id, name) ->
              FPPartial (mid, id, name)
          | F (id, np) ->
            ( match np with
            | PVariable name ->
                FPVariable (mid, id, name)
            | PConstructor (name, patterns) ->
                FPConstructor (mid, id, name, List.map ~f:fromPattern patterns)
            | PLiteral str ->
              ( match parseString str with
              | `Bool b ->
                  FPBool (mid, id, b)
              | `Int i ->
                  FPInteger (mid, id, i)
              | `String s ->
                  FPString (mid, id, s)
              | `Null ->
                  FPNull (mid, id)
              | `Float (whole, fraction) ->
                  FPFloat (mid, id, whole, fraction)
              | `Unknown ->
                  Js.log2
                    "Getting old pattern literal that we couldn't parse"
                    p ;
                  FPOldPattern (mid, p) ) )
        in
        let pairs =
          List.map pairs ~f:(fun (p, e) -> (fromPattern p, fromExpr e))
        in
        EMatch (id, fromExpr mexpr, pairs)
    | FeatureFlag _ ->
        EOldExpr expr )


let literalToString
    (v : [> `Bool of bool | `Int of int | `Null | `Float of string * string]) :
    string =
  match v with
  | `Int i ->
      Int.toString i
  | `String str ->
      "\"" ^ str ^ "\""
  | `Bool b ->
      if b then "true" else "false"
  | `Null ->
      "null"
  | `Float (whole, fraction) ->
      whole ^ "." ^ fraction


let rec toPattern (p : fluidPattern) : pattern =
  match p with
  | FPVariable (_, id, var) ->
      F (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      F (id, PConstructor (name, List.map ~f:toPattern patterns))
  | FPInteger (_, id, i) ->
      F (id, PLiteral (literalToString (`Int i)))
  | FPBool (_, id, b) ->
      F (id, PLiteral (literalToString (`Bool b)))
  | FPString (_, id, str) ->
      F (id, PLiteral (literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      F (id, PLiteral (literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      F (id, PLiteral (literalToString `Null))
  | FPBlank (_, id) ->
      Blank id
  | FPPartial (_, id, str) ->
      Partial (id, str)
  | FPOldPattern (_, pattern) ->
      pattern


let rec toExpr (expr : fluidExpr) : Types.expr =
  (* TODO: remove any new generation (gid ()) from this fn, save the old
   * ones instead *)
  match expr with
  | EInteger (id, num) ->
      F (id, Value (literalToString (`Int num)))
  | EString (id, str) ->
      F (id, Value (literalToString (`String str)))
  | EFloat (id, whole, fraction) ->
      F (id, Value (literalToString (`Float (whole, fraction))))
  | EBool (id, b) ->
      F (id, Value (literalToString (`Bool b)))
  | ENull id ->
      F (id, Value (literalToString `Null))
  | EVariable (id, var) ->
      F (id, Variable var)
  | EFieldAccess (id, obj, fieldID, fieldname) ->
      F (id, FieldAccess (toExpr obj, F (fieldID, fieldname)))
  | EFnCall (id, name, args, ster) ->
      F
        ( id
        , FnCall
            (F (ID (deID id ^ "_name"), name), List.map ~f:toExpr args, ster)
        )
  | EBinOp (id, name, arg1, arg2, ster) ->
      F
        ( id
        , FnCall
            (F (ID (deID id ^ "_name"), name), [toExpr arg1; toExpr arg2], ster)
        )
  | ELambda (id, vars, body) ->
      F
        ( id
        , Lambda
            ( List.map vars ~f:(fun (vid, var) -> Types.F (vid, var))
            , toExpr body ) )
  | EBlank id ->
      Blank id
  | ELet (id, lhsID, lhs, rhs, body) ->
      F (id, Let (F (lhsID, lhs), toExpr rhs, toExpr body))
  | EIf (id, cond, thenExpr, elseExpr) ->
      F (id, If (toExpr cond, toExpr thenExpr, toExpr elseExpr))
  | EPartial (id, str) ->
      Partial (id, str)
  | EList (id, exprs) ->
      F (id, ListLiteral (List.map ~f:toExpr exprs))
  | ERecord (id, pairs) ->
      F
        ( id
        , ObjectLiteral
            (List.map pairs ~f:(fun (id, k, v) -> (Types.F (id, k), toExpr v)))
        )
  | EThread (id, exprs) ->
      F (id, Thread (List.map ~f:toExpr exprs))
  | EConstructor (id, nameID, name, exprs) ->
      F (id, Constructor (F (nameID, name), List.map ~f:toExpr exprs))
  | EMatch (id, mexpr, pairs) ->
      let pairs = List.map pairs ~f:(fun (p, e) -> (toPattern p, toExpr e)) in
      F (id, Match (toExpr mexpr, pairs))
  | EOldExpr expr ->
      expr


let eid expr : id =
  match expr with
  | EOldExpr expr ->
      Blank.toID expr
  | EInteger (id, _)
  | EString (id, _)
  | EBool (id, _)
  | ENull id
  | EFloat (id, _, _)
  | EVariable (id, _)
  | EFieldAccess (id, _, _, _)
  | EFnCall (id, _, _, _)
  | ELambda (id, _, _)
  | EBlank id
  | ELet (id, _, _, _, _)
  | EIf (id, _, _, _)
  | EPartial (id, _)
  | EList (id, _)
  | ERecord (id, _)
  | EThread (id, _)
  | EBinOp (id, _, _, _, _)
  | EConstructor (id, _, _, _)
  | EMatch (id, _, _) ->
      id


let pid pattern : id =
  match pattern with
  | FPVariable (_, id, _)
  | FPConstructor (_, id, _, _)
  | FPInteger (_, id, _)
  | FPBool (_, id, _)
  | FPString (_, id, _)
  | FPFloat (_, id, _, _)
  | FPNull (_, id)
  | FPBlank (_, id)
  | FPPartial (_, id, _) ->
      id
  | FPOldPattern (_, pattern) ->
      Blank.toID pattern


let pmid pattern : id =
  match pattern with
  | FPVariable (mid, _, _)
  | FPConstructor (mid, _, _, _)
  | FPInteger (mid, _, _)
  | FPBool (mid, _, _)
  | FPString (mid, _, _)
  | FPFloat (mid, _, _, _)
  | FPNull (mid, _)
  | FPBlank (mid, _)
  | FPPartial (mid, _, _) ->
      mid
  | FPOldPattern (mid, _) ->
      mid


let newB () = EBlank (gid ())

(* -------------------- *)
(* Tokens *)
(* -------------------- *)
type token = Types.fluidToken

type tokenInfo = Types.fluidTokenInfo

let rec patternToToken (p : fluidPattern) : fluidToken list =
  match p with
  | FPVariable (mid, id, name) ->
      [TPatternVariable (mid, id, name)]
  | FPConstructor (mid, id, name, args) ->
      let args = List.map args ~f:(fun a -> TSep :: patternToToken a) in
      List.concat ([TPatternConstructorName (mid, id, name)] :: args)
  | FPInteger (mid, id, i) ->
      [TPatternInteger (mid, id, string_of_int i)]
  | FPBool (mid, id, b) ->
      if b then [TPatternTrue (mid, id)] else [TPatternFalse (mid, id)]
  | FPString (mid, id, s) ->
      [TPatternString (mid, id, s)]
  | FPFloat (mid, id, whole, fraction) ->
      [ TPatternFloatWhole (mid, id, whole)
      ; TPatternFloatPoint (mid, id)
      ; TPatternFloatFraction (mid, id, fraction) ]
  | FPNull (mid, id) ->
      [TPatternNullToken (mid, id)]
  | FPBlank (mid, id) ->
      [TPatternBlank (mid, id)]
  | FPPartial (mid, id, str) ->
      [TPatternPartial (mid, id, str)]
  | FPOldPattern (mid, op) ->
      [TPatternPartial (mid, Blank.toID op, "TODO: old pattern")]


let rec toTokens' (s : state) (e : ast) : token list =
  let nested ?(placeholderFor : (string * int) option = None) b =
    let tokens =
      match (b, placeholderFor) with
      | EBlank id, Some (fnname, pos) ->
          let name =
            s.ac.functions
            |> List.find ~f:(fun f -> f.fnName = fnname)
            |> Option.andThen ~f:(fun fn ->
                   List.getAt ~index:pos fn.fnParameters )
            |> Option.map ~f:(fun p ->
                   (p.paramName, Runtime.tipe2str p.paramTipe) )
          in
          ( match name with
          | None ->
              toTokens' s b
          | Some placeholder ->
              [TPlaceholder (placeholder, id)] )
      | _ ->
          toTokens' s b
    in
    TIndentToHere tokens
  in
  match e with
  | EInteger (id, i) ->
      [TInteger (id, string_of_int i)]
  | EBool (id, b) ->
      if b then [TTrue id] else [TFalse id]
  | ENull id ->
      [TNullToken id]
  | EFloat (id, whole, fraction) ->
      let whole = if whole = "" then [] else [TFloatWhole (id, whole)] in
      let fraction =
        if fraction = "" then [] else [TFloatFraction (id, fraction)]
      in
      whole @ [TFloatPoint id] @ fraction
  | EBlank id ->
      [TBlank id]
  | EPartial (id, str) ->
      [TPartial (id, str)]
  | ELet (id, _, lhs, rhs, next) ->
      [ TLetKeyword id
      ; TLetLHS (id, lhs)
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
  | EBinOp (id, op, lexpr, rexpr, _ster) ->
      [ nested ~placeholderFor:(Some (op, 0)) lexpr
      ; TSep
      ; TBinOp (id, op)
      ; TSep
      ; nested ~placeholderFor:(Some (op, 1)) rexpr ]
  | EFieldAccess (id, expr, _, fieldname) ->
      [nested expr; TFieldOp id; TFieldName (id, fieldname)]
  | EVariable (id, name) ->
      [TVariable (id, name)]
  | ELambda (id, names, body) ->
      let tnames =
        List.map names ~f:(fun (_, name) -> TLambdaVar (id, name))
        |> List.intersperse (TLambdaSep id)
      in
      [TLambdaSymbol id] @ tnames @ [TLambdaArrow id; nested body]
  | EFnCall (id, fnName, exprs, ster) ->
      [TFnName (id, fnName, ster)]
      @ ( exprs
        |> List.indexedMap ~f:(fun i e ->
               [TSep; nested ~placeholderFor:(Some (fnName, i)) e] )
        |> List.concat )
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
        ; List.mapi fields ~f:(fun i (_, fname, expr) ->
              [ TNewline
              ; TIndentToHere
                  [ TIndent 2
                  ; TRecordField (id, i, fname)
                  ; TSep
                  ; TRecordSep (id, i)
                  ; TSep
                  ; nested expr ] ] )
          |> List.concat
        ; [TNewline; TRecordClose id] ]
        |> List.concat
  | EThread (id, exprs) ->
    ( match exprs with
    | [] ->
        Js.log2 "Empty thread found" (show_fluidExpr e) ;
        []
    | [single] ->
        Js.log2 "Thread with single entry found" (show_fluidExpr single) ;
        [nested single]
    | head :: tail ->
        [ nested head
        ; TNewline
        ; TIndentToHere
            ( tail
            |> List.indexedMap ~f:(fun i e ->
                   let thread =
                     [TIndentToHere [TThreadPipe (id, i); nested e]]
                   in
                   if i == 0 then thread else TNewline :: thread )
            |> List.concat ) ] )
  | EConstructor (id, _, name, exprs) ->
      [TConstructorName (id, name)]
      @ (exprs |> List.map ~f:(fun e -> [TSep; nested e]) |> List.concat)
  | EMatch (id, mexpr, pairs) ->
      [ [TMatchKeyword id; nested mexpr]
      ; List.map pairs ~f:(fun (pattern, expr) ->
            [TNewline; TIndent 2]
            @ patternToToken pattern
            @ [TSep; TMatchSep (pid pattern); TSep; nested expr] )
        |> List.concat ]
      |> List.concat
  | EOldExpr expr ->
      [TPartial (Blank.toID expr, "TODO: oldExpr")]


(* TODO: we need some sort of reflow thing that handles line length. *)
let rec reflow ~(x : int) (startingTokens : token list) : int * token list =
  let startingX = x in
  List.foldl startingTokens ~init:(x, []) ~f:(fun t (x, old) ->
      let text = Token.toText t in
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
        let length = String.length (Token.toText token) in
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


let toTokens (s : state) (e : ast) : tokenInfo list =
  e |> toTokens' s |> reflow ~x:0 |> Tuple2.second |> infoize ~pos:0


let eToString (s : state) (e : ast) : string =
  e
  |> toTokens s
  |> List.map ~f:(fun ti -> Token.toTestText ti.token)
  |> String.join ~sep:""


let eToStructure (s : state) (e : fluidExpr) : string =
  e
  |> toTokens s
  |> List.map ~f:(fun ti ->
         "<" ^ Token.toTypeName ti.token ^ ":" ^ Token.toText ti.token ^ ">" )
  |> String.join ~sep:""


(* -------------------- *)
(* Patterns *)
(* -------------------- *)
let pToString (p : fluidPattern) : string =
  p
  |> patternToToken
  |> List.map ~f:(fun t -> Token.toTestText t)
  |> String.join ~sep:""


(* -------------------- *)
(* Direct canvas interaction *)
(* -------------------- *)

external jsGetCursorPosition : unit -> int Js.Nullable.t = "getCursorPosition"
  [@@bs.val] [@@bs.scope "window"]

external jsSetCursorPosition : int -> unit = "setCursorPosition"
  [@@bs.val] [@@bs.scope "window"]

let getCursorPosition () : int option =
  jsGetCursorPosition () |> Js.Nullable.toOption


let setCursorPosition (v : int) : unit = jsSetCursorPosition v

let editorID = "fluid-editor"

let setBrowserPos offset =
  Tea.Cmd.call (fun _ ->
      (* We need to set this in the new frame, as updating sets the cursor to
       * the start of the DOM node. *)
      ignore
        (Web.Window.requestAnimationFrame (fun _ -> setCursorPosition offset)) ;
      () )


(* -------------------- *)
(* Autocomplete *)
(* -------------------- *)

let acToExpr (entry : Types.fluidAutocompleteItem) : fluidExpr * int =
  match entry with
  | FACFunction fn ->
      let count = List.length fn.fnParameters in
      let name = fn.fnName in
      let r =
        if List.member ~value:fn.fnReturnTipe Runtime.errorRailTypes
        then Types.Rail
        else Types.NoRail
      in
      let args = List.initialize count (fun _ -> EBlank (gid ())) in
      (EFnCall (gid (), name, args, r), String.length name + 1)
  | FACKeyword KLet ->
      (ELet (gid (), gid (), "", newB (), newB ()), 4)
  | FACKeyword KIf ->
      (EIf (gid (), newB (), newB (), newB ()), 3)
  | FACKeyword KLambda ->
      (ELambda (gid (), [(gid (), "")], newB ()), 1)
  | FACKeyword KMatch ->
      let matchID = gid () in
      (EMatch (matchID, newB (), [(FPBlank (matchID, gid ()), newB ())]), 6)
  | FACVariable name ->
      (EVariable (gid (), name), String.length name)
  | FACLiteral "true" ->
      (EBool (gid (), true), 4)
  | FACLiteral "false" ->
      (EBool (gid (), false), 5)
  | FACLiteral "null" ->
      (ENull (gid ()), 4)
  | FACConstructorName (name, argCount) ->
      let argCount = List.initialize argCount (fun _ -> EBlank (gid ())) in
      (EConstructor (gid (), gid (), name, argCount), 1 + String.length name)
  | _ ->
      let str =
        "TODO: autocomplete result for "
        ^ Types.show_fluidAutocompleteItem entry
      in
      (EPartial (gid (), str), String.length str)


let initAC (s : state) (m : Types.model) : state = {s with ac = AC.init m}

let isAutocompleting (ti : tokenInfo) (s : state) : bool =
  Token.isAutocompletable ti.token
  && s.upDownCol = None
  && s.ac.index <> None
  && s.newPos <= ti.endPos
  && s.newPos >= ti.startPos


let recordAction ?(pos = -1000) (action : string) (s : state) : state =
  let action =
    if pos = -1000 then action else action ^ " " ^ string_of_int pos
  in
  {s with actions = s.actions @ [action]}


let setPosition ?(resetUD = false) (s : state) (pos : int) : state =
  let s = recordAction ~pos "setPosition" s in
  let upDownCol = if resetUD then None else s.upDownCol in
  {s with newPos = pos; upDownCol}


(* -------------------- *)
(* Update *)
(* -------------------- *)

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
  | TPatternPartial _
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


let isSkippable (token : token) : bool =
  match token with TIndent _ -> true | _ -> false


let isAtom (token : token) : bool =
  match token with
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TLetKeyword _
  | TMatchSep _
  | TMatchKeyword _
  | TThreadPipe _
  | TPlaceholder _
  | TBlank _
  | TLambdaArrow _
  | TPatternBlank _
  | TPatternPartial _ ->
      true
  | TListOpen _
  | TListClose _
  | TListSep _
  | TInteger _
  | TFloatWhole _
  | TFloatPoint _
  | TFloatFraction _
  | TString _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TRecordOpen _
  | TRecordClose _
  | TRecordSep _
  | TFieldOp _
  | TFieldName _
  | TVariable _
  | TFnName _
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
  | TLambdaVar _
  | TConstructorName _
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
      false


let length (tokens : token list) : int =
  tokens |> List.map ~f:Token.toText |> List.map ~f:String.length |> List.sum


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
    | Some prev, None ->
        (* Last position in the ast *)
        L (prev.token, prev)
    | None, None ->
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
  | None ->
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
let rec findExpr (id : id) (expr : fluidExpr) : fluidExpr option =
  let fe = findExpr id in
  if eid expr = id
  then Some expr
  else
    match expr with
    | EInteger _
    | EBlank _
    | EString _
    | EVariable _
    | EPartial _
    | EBool _
    | ENull _
    | EFloat _ ->
        None
    | ELet (_, _, _, rhs, next) ->
        fe rhs |> Option.orElse (fe next)
    | EIf (_, cond, ifexpr, elseexpr) ->
        fe cond |> Option.orElse (fe ifexpr) |> Option.orElse (fe elseexpr)
    | EBinOp (_, _, lexpr, rexpr, _) ->
        fe lexpr |> Option.orElse (fe rexpr)
    | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
        fe expr
    | ERecord (_, fields) ->
        fields |> List.map ~f:Tuple3.third |> List.filterMap ~f:fe |> List.head
    | EMatch (_, _, pairs) ->
        pairs |> List.map ~f:Tuple2.second |> List.filterMap ~f:fe |> List.head
    | EFnCall (_, _, exprs, _)
    | EList (_, exprs)
    | EConstructor (_, _, _, exprs)
    | EThread (_, exprs) ->
        List.filterMap ~f:fe exprs |> List.head
    | EOldExpr _ ->
        None


let isEmpty (e : fluidExpr) : bool =
  let isBlank e = match e with EBlank _ -> true | _ -> false in
  match e with
  | EBlank _ ->
      true
  | ERecord (_, []) ->
      true
  | ERecord (_, l) ->
      l
      |> List.filter ~f:(fun (_, k, v) -> k = "" && not (isBlank v))
      |> List.isEmpty
  | EList (_, l) ->
      l |> List.filter ~f:(not << isBlank) |> List.isEmpty
  | _ ->
      false


let exprIsEmpty (id : id) (ast : ast) : bool =
  match findExpr id ast with Some e -> isEmpty e | _ -> false


let findParent (id : id) (ast : ast) : fluidExpr option =
  let rec findParent' ~(parent : fluidExpr option) (id : id) (expr : fluidExpr)
      : fluidExpr option =
    let fp = findParent' ~parent:(Some expr) id in
    if eid expr = id
    then parent
    else
      match expr with
      | EInteger _
      | EBlank _
      | EString _
      | EVariable _
      | EPartial _
      | EBool _
      | ENull _
      | EFloat _ ->
          None
      | ELet (_, _, _, rhs, next) ->
          fp rhs |> Option.orElse (fp next)
      | EIf (_, cond, ifexpr, elseexpr) ->
          fp cond |> Option.orElse (fp ifexpr) |> Option.orElse (fp elseexpr)
      | EBinOp (_, _, lexpr, rexpr, _) ->
          fp lexpr |> Option.orElse (fp rexpr)
      | EFieldAccess (_, expr, _, _) | ELambda (_, _, expr) ->
          fp expr
      | EMatch (_, _, pairs) ->
          pairs
          |> List.map ~f:Tuple2.second
          |> List.filterMap ~f:fp
          |> List.head
      | ERecord (_, fields) ->
          fields
          |> List.map ~f:Tuple3.third
          |> List.filterMap ~f:fp
          |> List.head
      | EFnCall (_, _, exprs, _)
      | EList (_, exprs)
      | EConstructor (_, _, _, exprs)
      | EThread (_, exprs) ->
          List.filterMap ~f:fp exprs |> List.head
      | EOldExpr _ ->
          None
  in
  findParent' ~parent:None id ast


(* ------------- *)
(* Replacing expressions *)
(* ------------- *)
(* f needs to call recurse or it won't go far *)
let recurse ~(f : fluidExpr -> fluidExpr) (expr : fluidExpr) : fluidExpr =
  match expr with
  | EInteger _
  | EBlank _
  | EString _
  | EVariable _
  | EPartial _
  | EBool _
  | ENull _
  | EFloat _ ->
      expr
  | ELet (id, lhsID, name, rhs, next) ->
      ELet (id, lhsID, name, f rhs, f next)
  | EIf (id, cond, ifexpr, elseexpr) ->
      EIf (id, f cond, f ifexpr, f elseexpr)
  | EBinOp (id, op, lexpr, rexpr, ster) ->
      EBinOp (id, op, f lexpr, f rexpr, ster)
  | EFieldAccess (id, expr, fieldID, fieldname) ->
      EFieldAccess (id, f expr, fieldID, fieldname)
  | EFnCall (id, name, exprs, ster) ->
      EFnCall (id, name, List.map ~f exprs, ster)
  | ELambda (id, names, expr) ->
      ELambda (id, names, f expr)
  | EList (id, exprs) ->
      EList (id, List.map ~f exprs)
  | EMatch (id, mexpr, pairs) ->
      EMatch
        (id, f mexpr, List.map ~f:(fun (name, expr) -> (name, f expr)) pairs)
  | ERecord (id, fields) ->
      ERecord
        (id, List.map ~f:(fun (id, name, expr) -> (id, name, f expr)) fields)
  | EThread (id, exprs) ->
      EThread (id, List.map ~f exprs)
  | EConstructor (id, nameID, name, exprs) ->
      EConstructor (id, nameID, name, List.map ~f exprs)
  | EOldExpr _ ->
      expr


let wrap ~(f : fluidExpr -> fluidExpr) (id : id) (ast : ast) : ast =
  let rec run e = if id = eid e then f e else recurse ~f:run e in
  run ast


let replaceExpr ~(newExpr : fluidExpr) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun _ -> newExpr)


let wrapPattern
    ~(f : fluidPattern -> fluidPattern) (matchID : id) (patID : id) (ast : ast)
    : ast =
  wrap matchID ast ~f:(fun m ->
      match m with
      | EMatch (matchID, expr, pairs) ->
          let newPairs =
            List.map pairs ~f:(fun (pat, expr) ->
                if pid pat = patID then (f pat, expr) else (pat, expr) )
          in
          EMatch (matchID, expr, newPairs)
      | _ ->
          m )


let replacePattern
    ~(newPat : fluidPattern) (matchID : id) (patID : id) (ast : ast) : ast =
  wrapPattern matchID patID ast ~f:(fun _ -> newPat)


let replacePatternString (str : string) (matchID : id) (patID : id) (ast : ast)
    : ast =
  wrapPattern matchID patID ast ~f:(fun p ->
      match p with
      | FPInteger (_, id, _) ->
          if str = ""
          then FPBlank (matchID, id)
          else
            let value = try safe_int_of_string str with _ -> 0 in
            FPInteger (matchID, id, value)
      | FPString (_, id, _) ->
          FPString (matchID, id, str)
      | FPVariable (_, id, _) ->
          if str = ""
          then FPBlank (matchID, id)
          else FPPartial (matchID, id, str)
      | FPPartial (_, id, _) ->
          if str = ""
          then FPBlank (matchID, id)
          else FPPartial (matchID, id, str)
      | _ ->
          fail ("not a string type: " ^ show_fluidPattern p) )


let removeField (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | EFieldAccess (_, faExpr, _, _) ->
          faExpr
      | _ ->
          fail "not a fieldAccess" )


let removeRecordField (id : id) (index : int) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          ERecord (id, List.removeAt ~index fields)
      | _ ->
          fail "not a record field" )


let exprToFieldAccess (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e -> EFieldAccess (gid (), e, gid (), ""))


let removeEmptyExpr (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | ELet (_, _, "", EBlank _, body) ->
          body
      | EIf (_, EBlank _, EBlank _, EBlank _) ->
          newB ()
      | ELambda (_, _, EBlank _) ->
          newB ()
      | EMatch (_, EBlank _, pairs)
        when List.all pairs ~f:(fun (p, e) ->
                 match (p, e) with FPBlank _, EBlank _ -> true | _ -> false )
        ->
          newB ()
      | _ ->
          e )


let replaceFieldName (str : string) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | EFieldAccess (id, expr, fieldID, _) ->
          EFieldAccess (id, expr, fieldID, str)
      | _ ->
          fail "not a field" )


let replaceLamdaVar (str : string) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      (* TODO: rename in vars other than the first *)
      | ELambda (id, vars, expr) ->
          let rest = List.tail vars |> Option.withDefault ~default:[] in
          ELambda (id, (gid (), str) :: rest, expr)
      | _ ->
          fail "not a lamda" )


let replaceLetLHS (str : string) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | ELet (id, lhsID, _, rhs, next) ->
          ELet (id, lhsID, str, rhs, next)
      | _ ->
          fail "not a let" )


let replaceRecordField ~index (str : string) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun e ->
      match e with
      | ERecord (id, fields) ->
          let fields =
            List.updateAt fields ~index ~f:(fun (id, _, expr) -> (id, str, expr)
            )
          in
          ERecord (id, fields)
      | _ ->
          fail "not a record" )


(* Slightly modified version of `AST.uses` (pre-fluid code) *)
let rec modifyVariableOccurences
    (str : string) (ast : ast) ~(f : id -> ast -> ast) : ast =
  let u = modifyVariableOccurences str ~f in
  match ast with
  | EBlank _
  | EInteger _
  | EBool _
  | EString _
  | EOldExpr _
  | EFloat _
  | EPartial _
  | ENull _ ->
      ast
  | EVariable (id, potential) ->
      if potential = str then f id ast else ast
  | ELet (id, id', lhs, rhs, body) ->
      if str = lhs (* if variable name is rebound *)
      then ast
      else ELet (id, id', lhs, u rhs, u body)
  | EIf (id, cond, ifbody, elsebody) ->
      EIf (id, u cond, u ifbody, u elsebody)
  | EFnCall (id, name, exprs, stor) ->
      let exprs = exprs |> List.map ~f:u in
      EFnCall (id, name, exprs, stor)
  | EConstructor (id, id', name, exprs) ->
      let exprs = exprs |> List.map ~f:u in
      EConstructor (id, id', name, exprs)
  | ELambda (id, vars, lexpr) ->
      if List.map ~f:Tuple2.second vars |> List.member ~value:str
         (* if variable name is rebound *)
      then ast
      else ELambda (id, vars, u lexpr)
  | EThread (id, exprs) ->
      let exprs = exprs |> List.map ~f:u in
      EThread (id, exprs)
  | EFieldAccess (id, obj, id', name) ->
      EFieldAccess (id, u obj, id', name)
  | EList (id, exprs) ->
      let exprs = exprs |> List.map ~f:u in
      EList (id, exprs)
  | ERecord (id, triples) ->
      let triples =
        triples |> List.map ~f:(fun (id, name, expr) -> (id, name, u expr))
      in
      ERecord (id, triples)
  | EMatch (id, cond, pairs) ->
      let pairs =
        List.map
          ~f:(fun (pat, expr) ->
            if Pattern.hasVariableNamed str (toPattern pat)
            then (pat, expr)
            else (pat, u expr) )
          pairs
      in
      EMatch (id, u cond, pairs)
  | EBinOp (id, name, lhs, rhs, stor) ->
      EBinOp (id, name, u lhs, u rhs, stor)


(* Supports the various different tokens replacing their string contents.
 * Doesn't do movement. *)
let replaceStringToken ~(f : string -> string) (token : token) (ast : ast) :
    fluidExpr =
  match token with
  | TString (id, str) ->
      replaceExpr id ~newExpr:(EString (id, f str)) ast
  | TPatternString (mID, id, str) ->
      replacePattern mID id ~newPat:(FPString (mID, id, f str)) ast
  | TPatternInteger (mID, id, str) ->
      let str = f str in
      if str = ""
      then EBlank id
      else
        let value = try safe_int_of_string str with _ -> 0 in
        replacePattern mID id ~newPat:(FPInteger (mID, id, value)) ast
  | TPatternNullToken (mID, id) ->
      let str = f "null" in
      let newExpr = FPPartial (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternTrue (mID, id) ->
      let str = f "true" in
      let newExpr = FPPartial (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternFalse (mID, id) ->
      let str = f "false" in
      let newExpr = FPPartial (mID, gid (), str) in
      replacePattern mID id ~newPat:newExpr ast
  | TPatternVariable (mID, id, str) ->
      let str = f str in
      if str = ""
      then EBlank id
      else
        let newExpr = FPPartial (mID, gid (), str) in
        replacePattern mID id ~newPat:newExpr ast
  | TRecordField (id, index, str) ->
      replaceRecordField ~index (f str) id ast
  | TLetLHS (id, str) ->
      replaceLetLHS (f str) id ast
  | TLambdaVar (id, str) ->
      replaceLamdaVar (f str) id ast
  | TInteger (id, str) ->
      let str = f str in
      if str = ""
      then EBlank id
      else
        let value = try safe_int_of_string str with _ -> 0 in
        replaceExpr id ~newExpr:(EInteger (id, value)) ast
  | TVariable (id, str) ->
      let str = f str in
      let newExpr = if str = "" then EBlank id else EPartial (id, str) in
      replaceExpr id ~newExpr ast
  | TPartial (id, str) ->
      let str = f str in
      let newExpr = if str = "" then EBlank id else EPartial (id, str) in
      replaceExpr id ~newExpr ast
  | TFieldName (id, str) ->
      replaceFieldName (f str) id ast
  | TTrue id ->
      let str = f "true" in
      let newExpr = EPartial (gid (), str) in
      replaceExpr id ~newExpr ast
  | TFalse id ->
      let str = f "false" in
      let newExpr = EPartial (gid (), str) in
      replaceExpr id ~newExpr ast
  | TNullToken id ->
      let str = f "null" in
      let newExpr = EPartial (gid (), str) in
      replaceExpr id ~newExpr ast
  | _ ->
      fail "not supported by replaceToken"


let replaceFloatWhole (str : string) (id : id) (ast : ast) : fluidExpr =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EFloat (id, _, fraction) ->
          EFloat (id, str, fraction)
      | _ ->
          fail "not a float" )


let replaceFloatFraction (str : string) (id : id) (ast : ast) : fluidExpr =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EFloat (id, whole, _) ->
          EFloat (id, whole, str)
      | _ ->
          fail "not a float" )


let insertAtFrontOfFloatFraction (letter : string) (id : id) (ast : ast) :
    fluidExpr =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EFloat (id, whole, fraction) ->
          EFloat (id, whole, letter ^ fraction)
      | _ ->
          fail "not a float" )


let insertInList ~(index : int) ~(newExpr : fluidExpr) (id : id) (ast : ast) :
    ast =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EList (id, exprs) ->
          EList (id, List.insertAt ~index ~value:newExpr exprs)
      | _ ->
          fail "not a list" )


(* Add a blank after the expr indicated by id, which we presume is in a list *)
let addBlankToList (id : id) (ast : ast) : ast =
  let parent = findParent id ast in
  match parent with
  | Some (EList (pID, exprs)) ->
      wrap pID ast ~f:(fun expr ->
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
      ast


(* Add a row to the record *)
let addRecordRowToFront (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun expr ->
      match expr with
      | ERecord (id, fields) ->
          ERecord (id, (gid (), "", newB ()) :: fields)
      | _ ->
          fail "Not a record" )


let convertToBinOp (char : char option) (id : id) (ast : ast) : ast =
  (* TODO: does it go on the error rail? *)
  match char with
  | None ->
      ast
  | Some c ->
      wrap id ast ~f:(fun expr ->
          EBinOp (gid (), String.fromChar c, expr, newB (), NoRail) )


let convertIntToFloat (offset : int) (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EInteger (_, i) ->
          let str = Int.toString i in
          let whole, fraction = String.splitAt ~index:offset str in
          EFloat (gid (), whole, fraction)
      | _ ->
          fail "Not an int" )


let removePointFromFloat (id : id) (ast : ast) : ast =
  wrap id ast ~f:(fun expr ->
      match expr with
      | EFloat (_, whole, fraction) ->
          let i = safe_int_of_string (whole ^ fraction) in
          EInteger (gid (), i)
      | _ ->
          fail "Not an int" )


let moveToNextNonWhitespaceToken ~pos (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToNextNonWhitespaceToken" s in
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
  let newPos = getNextWS (toTokens s ast) in
  setPosition ~resetUD:true s newPos


let moveToEnd (ti : tokenInfo) (s : state) : state =
  let s = recordAction "moveToEnd" s in
  setPosition ~resetUD:true s (ti.endPos - 1)


let moveToStart (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~pos:ti.startPos "moveToStart" s in
  setPosition ~resetUD:true s ti.startPos


let moveToAfter (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~pos:ti.endPos "moveToAfter" s in
  setPosition ~resetUD:true s ti.endPos


let moveOneLeft (pos : int) (s : state) : state =
  let s = recordAction ~pos "moveOneLeft" s in
  setPosition ~resetUD:true s (max 0 (pos - 1))


let moveOneRight (pos : int) (s : state) : state =
  let s = recordAction ~pos "moveOneRight" s in
  setPosition ~resetUD:true s (pos + 1)


let moveTo (newPos : int) (s : state) : state =
  let s = recordAction ~pos:newPos "moveTo" s in
  setPosition s newPos


let getNextBlank (pos : int) (tokens : tokenInfo list) : int =
  let rec f pos tokens' =
    match tokens' with
    | [] ->
        (* Wrap, unless we've already wrapped *)
        if pos = -1 then 0 else f (-1) tokens
    | ti :: rest ->
        if Token.isBlank ti.token && ti.startPos > pos
        then ti.startPos
        else f pos rest
  in
  f pos tokens


(* TODO: rewrite nextBlank like prevBlank *)
let moveToNextBlank ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToNextBlank" s in
  let tokens = toTokens s ast in
  let newPos = getNextBlank pos tokens in
  setPosition ~resetUD:true s newPos


let moveToPrevBlank ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction ~pos "moveToPrevBlank" s in
  let tokens =
    toTokens s ast |> List.filter ~f:(fun ti -> Token.isBlank ti.token)
  in
  let rec getPrevBlank pos' tokens' =
    match tokens' with
    | [] ->
      (match List.last tokens with None -> 0 | Some ti -> ti.startPos)
    | ti :: rest ->
        if ti.endPos < pos' then ti.startPos else getPrevBlank pos' rest
  in
  let newPos = getPrevBlank pos (List.reverse tokens) in
  setPosition ~resetUD:true s newPos


let acSetIndex (i : int) (s : state) : state =
  {s with ac = {s.ac with index = Some i}; upDownCol = None}


let acMoveToStart (s : state) : state = {s with ac = {s.ac with index = Some 0}}

let acClear (s : state) : state = {s with ac = {s.ac with index = None}}

let acMoveUp (s : state) : state =
  let s = recordAction "acMoveUp" s in
  let index =
    match s.ac.index with None -> 0 | Some current -> max 0 (current - 1)
  in
  acSetIndex index s


let acMoveDown (s : state) : state =
  let s = recordAction "acMoveDown" s in
  let index =
    match s.ac.index with
    | None ->
        0
    | Some current ->
        min (current + 1) (List.length (AC.allCompletions s.ac) - 1)
  in
  acSetIndex index s


let report (e : string) (s : state) =
  let s = recordAction "report" s in
  {s with error = Some e}


let acEnter (ti : tokenInfo) (ast : ast) (s : state) (key : K.key) :
    ast * state =
  let s = recordAction "acEnter" s in
  match AC.highlighted s.ac with
  | None ->
      (ast, s)
  | Some entry ->
      (* TODO: the correct thing is to decide on where to go based
       * on context: Enter stops at the end, space goes one space
       * ahead, tab goes to next blank *)
      let newExpr, length = acToExpr entry in
      let id = Token.tid ti.token in
      let newAST = replaceExpr ~newExpr id ast in
      let tokens = toTokens s newAST in
      let offset = getNextBlank s.newPos tokens in
      let newState =
        match key with
        | K.Tab ->
            moveTo offset (acClear s)
        | K.Enter ->
            moveTo (ti.startPos + length) (acClear s)
        | K.Space ->
            let newPos =
              if offset > ti.startPos + length + 1
              then ti.startPos + length + 1
              else offset
            in
            moveTo newPos (acClear s)
        | _ ->
            s
      in
      (newAST, newState)


let acCompleteField (ti : tokenInfo) (ast : ast) (s : state) : ast * state =
  let s = recordAction "acCompleteField" s in
  match AC.highlighted s.ac with
  | None ->
      (ast, s)
  | Some entry ->
      let newExpr, length = acToExpr entry in
      let newExpr = EFieldAccess (gid (), newExpr, gid (), "") in
      let length = length + 1 in
      let newState = moveTo (ti.startPos + length) (acClear s) in
      let newAST = replaceExpr ~newExpr (Token.tid ti.token) ast in
      (newAST, newState)


let doBackspace ~(pos : int) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction "doBackspace" s in
  let left s = moveOneLeft (min pos ti.endPos) s in
  let offset =
    match ti.token with
    | TPatternString _ | TString _ ->
        pos - ti.startPos - 2
    | _ ->
        pos - ti.startPos - 1
  in
  let newID = gid () in
  match ti.token with
  | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ | TMatchSep _ ->
      (ast, moveToStart ti s)
  | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ | TMatchKeyword _ ->
      let newAST = removeEmptyExpr (Token.tid ti.token) ast in
      if newAST = ast then (ast, s) else (newAST, moveToStart ti s)
  | TString (id, "") ->
      (replaceExpr id ~newExpr:(EBlank newID) ast, left s)
  | TPatternString (mID, id, "") ->
      (replacePattern mID id ~newPat:(FPBlank (mID, newID)) ast, left s)
  | (TRecordOpen id | TListOpen id) when exprIsEmpty id ast ->
      (replaceExpr id ~newExpr:(EBlank newID) ast, left s)
  | TRecordField (id, i, "") when pos = ti.startPos ->
      ( removeRecordField id i ast
      , s |> left |> fun s -> moveOneLeft (s.newPos - 1) s )
  | TBinOp _
  | TBlank _
  | TPlaceholder _
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
  | TThreadPipe _
  | TConstructorName _
  | TLambdaSep _ ->
      (ast, left s)
  | TFieldOp id ->
      (removeField id ast, left s)
  | TFloatPoint id ->
      (removePointFromFloat id ast, left s)
  | TString _
  | TPatternString _
  | TRecordField _
  | TInteger _
  | TTrue _
  | TFalse _
  | TPatternTrue _
  | TPatternFalse _
  | TNullToken _
  | TVariable _
  | TPartial _
  | TFieldName _
  | TLetLHS _
  | TPatternInteger _
  | TPatternNullToken _
  | TPatternVariable _
  | TLambdaVar _ ->
      let f str = removeCharAt str offset in
      (replaceStringToken ~f ti.token ast, left s)
  | TFloatWhole (id, str) ->
      let str = removeCharAt str offset in
      (replaceFloatWhole str id ast, left s)
  | TFloatFraction (id, str) ->
      let str = removeCharAt str offset in
      (replaceFloatFraction str id ast, left s)
  | TPatternBlank _
  | TPatternPartial _
  | TPatternConstructorName _
  | TPatternFloatWhole _
  | TPatternFloatPoint _
  | TPatternFloatFraction _ ->
      (ast, left s)


let doDelete ~(pos : int) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction "doDelete" s in
  let left s = moveOneLeft pos s in
  let offset = pos - ti.startPos in
  let newID = gid () in
  let f str = removeCharAt str offset in
  match ti.token with
  | TIfThenKeyword _ | TIfElseKeyword _ | TLambdaArrow _ | TMatchSep _ ->
      (ast, s)
  | TIfKeyword _ | TLetKeyword _ | TLambdaSymbol _ | TMatchKeyword _ ->
      (removeEmptyExpr (Token.tid ti.token) ast, s)
  | (TListOpen id | TRecordOpen id) when exprIsEmpty id ast ->
      (replaceExpr id ~newExpr:(newB ()) ast, s)
  | TBinOp _
  | TBlank _
  | TPlaceholder _
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
  | TThreadPipe _
  | TLambdaSep _ ->
      (ast, s)
  | TFieldOp id ->
      (removeField id ast, s)
  | TFloatPoint id ->
      (removePointFromFloat id ast, s)
  | TString (id, str) ->
      let target s =
        (* if we're in front of the quotes vs within it *)
        if offset == 0 then s else left s
      in
      if str = ""
      then (ast |> replaceExpr id ~newExpr:(EBlank newID), target s)
      else
        let str = removeCharAt str (offset - 1) in
        (replaceExpr id ~newExpr:(EString (newID, str)) ast, s)
  | TPatternString (mID, id, str) ->
      let target s =
        (* if we're in front of the quotes vs within it *)
        if offset == 0 then s else left s
      in
      if str = ""
      then
        (ast |> replacePattern mID id ~newPat:(FPBlank (mID, newID)), target s)
      else
        let str = removeCharAt str (offset - 1) in
        (replacePattern mID id ~newPat:(FPString (mID, newID, str)) ast, s)
  | TRecordField _
  | TInteger _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TVariable _
  | TPartial _
  | TFieldName _
  | TLetLHS _
  | TPatternInteger _
  | TPatternNullToken _
  | TPatternTrue _
  | TPatternFalse _
  | TPatternVariable _
  | TLambdaVar _ ->
      (replaceStringToken ~f ti.token ast, s)
  | TFloatWhole (id, str) ->
      (replaceFloatWhole (f str) id ast, s)
  | TFloatFraction (id, str) ->
      (replaceFloatFraction (f str) id ast, s)
  | TConstructorName _ ->
      (ast, s)
  | TPatternBlank _
  | TPatternPartial _
  | TPatternConstructorName _
  | TPatternFloatWhole _
  | TPatternFloatPoint _
  | TPatternFloatFraction _ ->
      (ast, s)


let doLeft ~(pos : int) (ti : tokenInfo) (s : state) : state =
  let s = recordAction ~pos "doLeft" s in
  if isAtom ti.token
  then moveToStart ti s
  else moveOneLeft (min pos ti.endPos) s


let doRight
    ~(pos : int) ~(next : tokenInfo option) (current : tokenInfo) (s : state) :
    state =
  let s = recordAction ~pos "doRight" s in
  match current.token with
  | TIfKeyword _
  | TIfThenKeyword _
  | TIfElseKeyword _
  | TLetKeyword _
  | TPlaceholder _
  | TBlank _
  | TLambdaArrow _
  | TMatchSep _
  | TMatchKeyword _ ->
    ( match next with
    | None ->
        moveToAfter current s
    | Some nInfo ->
        moveToStart nInfo s )
  | TIndent _
  | TIndented _
  | TIndentToHere _
  | TInteger _
  | TFloatWhole _
  | TFloatPoint _
  | TFloatFraction _
  | TString _
  | TTrue _
  | TFalse _
  | TNullToken _
  | TFieldOp _
  | TFieldName _
  | TVariable _
  | TFnName _
  | TConstructorName _
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
  | TThreadPipe _
  | TLambdaVar _
  | TLambdaSymbol _
  | TLambdaSep _
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
    ( match next with
    | Some n when pos + 1 >= current.endPos ->
        moveToStart n s
    | _ ->
        (* When we're in whitespace, current is the next non-whitespace. So we
         * don't want to use pos, we want to use the startPos of current. *)
        let startingPos = max pos (current.startPos - 1) in
        moveOneRight startingPos s )


let doUp ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction "doUp" s in
  let tokens = toTokens s ast in
  let {row; col} = gridFor ~pos tokens in
  let col = match s.upDownCol with None -> col | Some savedCol -> savedCol in
  if row = 0
  then moveTo 0 s
  else
    let pos = adjustedPosFor ~row:(row - 1) ~col tokens in
    moveTo pos {s with upDownCol = Some col}


let doDown ~(pos : int) (ast : ast) (s : state) : state =
  let s = recordAction "doDown" s in
  let tokens = toTokens s ast in
  let {row; col} = gridFor ~pos tokens in
  let col = match s.upDownCol with None -> col | Some savedCol -> savedCol in
  let pos = adjustedPosFor ~row:(row + 1) ~col tokens in
  moveTo pos {s with upDownCol = Some col}


let isRealCharacter (letter : string) : bool = String.length letter = 1

let doInsert' ~pos (letter : char) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  let s = recordAction "doInsert" s in
  let s = {s with upDownCol = None} in
  let letterStr = String.fromChar letter in
  let offset =
    match ti.token with
    | TString _ | TPatternString _ ->
        pos - ti.startPos - 1
    | _ ->
        pos - ti.startPos
  in
  let right = moveOneRight pos s in
  let f str = String.insertAt ~index:offset ~insert:letterStr str in
  let newID = gid () in
  let newExpr =
    if letter = '"'
    then EString (newID, "")
    else if letter = '['
    then EList (newID, [])
    else if letter = '{'
    then ERecord (newID, [])
    else if letter = '\\'
    then ELambda (newID, [(gid (), "")], EBlank (gid ()))
    else if letter = ','
    then EBlank newID (* new separators *)
    else if isNumber letterStr
    then EInteger (newID, letterStr |> safe_int_of_string)
    else EPartial (newID, letterStr)
  in
  match ti.token with
  | (TFieldName (id, _) | TVariable (id, _))
    when pos = ti.endPos && letter = '.' ->
      (exprToFieldAccess id ast, right)
  (* replace blank *)
  | TBlank id | TPlaceholder (_, id) ->
      (replaceExpr id ~newExpr ast, moveTo (ti.startPos + 1) s)
  (* lists *)
  | TListOpen id ->
      (insertInList ~index:0 id ~newExpr ast, moveTo (ti.startPos + 2) s)
  (* Ignore invalid situations *)
  | TString _ when offset < 0 ->
      (ast, s)
  | TPatternString _ when offset < 0 ->
      (ast, s)
  | TInteger _ when not (isNumber letterStr) ->
      (ast, s)
  | TInteger _ when '0' = letter && offset = 0 ->
      (ast, s)
  | TPatternInteger _ when not (isNumber letterStr) ->
      (ast, s)
  | TPatternInteger _ when '0' = letter && offset = 0 ->
      (ast, s)
  | TFloatWhole _ when not (isNumber letterStr) ->
      (ast, s)
  | TFloatWhole _ when '0' = letter && offset = 0 ->
      (ast, s)
  | TFloatFraction _ when not (isNumber letterStr) ->
      (ast, s)
  | TPatternFloatWhole _ when not (isNumber letterStr) ->
      (ast, s)
  | TPatternFloatWhole _ when '0' = letter && offset = 0 ->
      (ast, s)
  | TPatternFloatFraction _ when not (isNumber letterStr) ->
      (ast, s)
  | TVariable _ when not (isIdentifierChar letterStr) ->
      (ast, s)
  | TPatternVariable _ when not (isIdentifierChar letterStr) ->
      (ast, s)
  | TLetLHS _ when not (isIdentifierChar letterStr) ->
      (ast, s)
  | TFieldName _ when not (isIdentifierChar letterStr) ->
      (ast, s)
  | TLambdaVar _ when not (isIdentifierChar letterStr) ->
      (ast, s)
  | TFnName _ when not (isFnNameChar letterStr) ->
      (ast, s)
  (* Do the insert *)
  | TLetLHS (_, "") ->
      (replaceStringToken ~f ti.token ast, moveTo (ti.startPos + 1) s)
  | TRecordField _
  | TFieldName _
  | TVariable _
  | TPartial _
  | TString _
  | TPatternString _
  | TLetLHS _
  | TTrue _
  | TFalse _
  | TPatternTrue _
  | TPatternFalse _
  | TNullToken _
  | TPatternNullToken _
  | TPatternVariable _
  | TLambdaVar _ ->
      (replaceStringToken ~f ti.token ast, right)
  | TPatternInteger (_, _, i) | TInteger (_, i) ->
      let newLength =
        f i |> safe_int_of_string |> string_of_int |> String.length
      in
      let move = if newLength > offset then right else s in
      (replaceStringToken ~f ti.token ast, move)
  | TFloatWhole (id, str) ->
      (replaceFloatWhole (f str) id ast, right)
  | TFloatFraction (id, str) ->
      (replaceFloatFraction (f str) id ast, right)
  | TFloatPoint id ->
      (insertAtFrontOfFloatFraction letterStr id ast, right)
  | TPatternPartial _
  | TPatternConstructorName _
  | TPatternFloatPoint _
  | TPatternFloatWhole _
  | TPatternFloatFraction _ ->
      (ast, s)
  | TPatternBlank (mID, pID) ->
      let newPat =
        if letter = '"'
        then FPString (mID, newID, "")
        else if isNumber letterStr
        then FPInteger (mID, newID, letterStr |> safe_int_of_string)
        else FPPartial (mID, newID, letterStr)
      in
      (replacePattern mID pID ~newPat ast, moveTo (ti.startPos + 1) s)
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
  | TThreadPipe _
  | TLambdaSymbol _
  | TLambdaArrow _
  | TConstructorName _
  | TLambdaSep _
  | TMatchSep _
  | TMatchKeyword _ ->
      (ast, s)


let doInsert
    ~pos (letter : char option) (ti : tokenInfo) (ast : ast) (s : state) :
    ast * state =
  match letter with
  | None ->
      (ast, s)
  | Some letter ->
      doInsert' ~pos letter ti ast s


let maybeOpenCmd (m : Types.model) : Types.modification =
  let getCurrentToken tokens =
    let _, mCurrent, _ = getTokensAtPosition tokens ~pos:m.fluidState.newPos in
    mCurrent
  in
  Toplevel.selected m
  |> Option.andThen ~f:(fun tl ->
         TL.rootExpr tl
         |> Option.andThen ~f:(fun ast -> Some (fromExpr m.fluidState ast))
         |> Option.andThen ~f:(fun ast -> Some (toTokens m.fluidState ast))
         |> Option.withDefault ~default:[]
         |> getCurrentToken
         |> Option.andThen ~f:(fun ti ->
                let id = Token.tid ti.token in
                Some (FluidCommandsFor (tl.id, id)) ) )
  |> Option.withDefault ~default:NoChange


let updateKey (key : K.key) (ast : ast) (s : state) : ast * state =
  let pos = s.newPos in
  let keyChar = K.toChar key in
  let tokens = toTokens s ast in
  (* These might be the same token *)
  let toTheLeft, toTheRight, mNext = getNeighbours ~pos tokens in
  let onEdge =
    match (toTheLeft, toTheRight) with
    | L (lt, lti), R (rt, rti) ->
        (lt, lti) <> (rt, rti)
    | _ ->
        true
  in
  let newAST, newState =
    (* TODO: When changing TVariable and TFieldName and probably TFnName we
     * should convert them to a partial which retains the old object *)
    match (key, toTheLeft, toTheRight) with
    (* Deleting *)
    | K.Backspace, L (TPatternString _, ti), _
    | K.Backspace, L (TString _, ti), _
      when pos = ti.endPos ->
        (* Backspace should move into a string, not delete it *)
        (ast, moveOneLeft pos s)
    | K.Backspace, _, R (TRecordField (_, _, ""), ti) ->
        doBackspace ~pos ti ast s
    | K.Backspace, L (_, ti), _ ->
        doBackspace ~pos ti ast s
    | K.Delete, _, R (_, ti) ->
        doDelete ~pos ti ast s
    (* Autocomplete menu *)
    (* Note that these are spelt out explicitly on purpose, else they'll
     * trigger on the wrong element sometimes. *)
    | K.Escape, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acClear s)
    | K.Escape, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acClear s)
    | K.Up, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | K.Up, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveUp s)
    | K.Down, _, R (_, ti) when isAutocompleting ti s ->
        (ast, acMoveDown s)
    | K.Down, L (_, ti), _ when isAutocompleting ti s ->
        (ast, acMoveDown s)
    (* Autocomplete finish *)
    | K.Enter, L (TPartial (_, _), ti), _
    | K.Enter, _, R (TPartial (_, _), ti)
    | K.Space, L (TPartial (_, _), ti), _
    | K.Space, _, R (TPartial (_, _), ti)
    | K.Tab, L (TPartial (_, _), ti), _
    | K.Tab, _, R (TPartial (_, _), ti)
    | K.Enter, L (TBlank _, ti), _
    | K.Enter, _, R (TBlank _, ti)
    | K.Space, L (TBlank _, ti), _
    | K.Space, _, R (TBlank _, ti)
    | K.Tab, L (TBlank _, ti), _
    | K.Tab, _, R (TBlank _, ti) ->
        acEnter ti ast s key
    (* Special autocomplete entries *)
    (* press dot while in a variable entry *)
    | K.Period, L (TPartial _, ti), _
      when Option.map ~f:AC.isVariable (AC.highlighted s.ac) = Some true ->
        acCompleteField ti ast s
    (* Tab to next blank *)
    | K.Tab, _, R (_, ti)
      when exprIsEmpty (Token.tid ti.token) ast || not (isAutocompleting ti s)
      ->
        (ast, moveToNextBlank ~pos ast s)
    | K.Tab, L (_, ti), _
      when exprIsEmpty (Token.tid ti.token) ast || not (isAutocompleting ti s)
      ->
        (ast, moveToNextBlank ~pos ast s)
    | K.ShiftTab, _, R (_, ti)
      when exprIsEmpty (Token.tid ti.token) ast || not (isAutocompleting ti s)
      ->
        (ast, moveToPrevBlank ~pos ast s)
    | K.ShiftTab, L (_, ti), _
      when exprIsEmpty (Token.tid ti.token) ast || not (isAutocompleting ti s)
      ->
        (ast, moveToPrevBlank ~pos ast s)
    (* TODO: press comma while in an expr in a list *)
    (* TODO: press comma while in an expr in a record *)
    (* TODO: press equals when in a let *)
    (* TODO: press colon when in a record field *)
    (* Left/Right movement *)
    | K.Left, L (_, ti), _ ->
        (ast, doLeft ~pos ti s)
    | K.Right, _, R (_, ti) ->
        (ast, doRight ~pos ~next:mNext ti s)
    | K.Up, _, _ ->
        (ast, doUp ~pos ast s)
    | K.Down, _, _ ->
        (ast, doDown ~pos ast s)
    | K.Space, _, R (TSep, _) ->
        (ast, moveOneRight pos s)
    (* list-specific insertions *)
    | K.Comma, L (TListOpen _, toTheLeft), _ ->
        doInsert ~pos keyChar toTheLeft ast s
    | K.Comma, L (t, ti), _ ->
        if onEdge
        then (addBlankToList (Token.tid t) ast, moveOneRight ti.endPos s)
        else doInsert ~pos keyChar ti ast s
    | K.RightCurlyBrace, _, R (TRecordClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close curly to go over the last curly *)
        (ast, moveOneRight pos s)
    (* Record-specific insertions *)
    | K.Enter, L (TRecordOpen id, _), _ ->
        let newAST = addRecordRowToFront id ast in
        (newAST, moveToNextNonWhitespaceToken ~pos newAST s)
    | K.RightSquareBracket, _, R (TListClose _, ti) when pos = ti.endPos - 1 ->
        (* Allow pressing close square to go over the last square *)
        (ast, moveOneRight pos s)
    (* Lambda-specific insertions *)
    (* String-specific insertions *)
    | K.DoubleQuote, _, R (TPatternString _, ti)
    | K.DoubleQuote, _, R (TString _, ti)
      when pos = ti.endPos - 1 ->
        (* Allow pressing quote to go over the last quote *)
        (ast, moveOneRight pos s)
    (* Field access *)
    | K.Period, L (TVariable _, toTheLeft), _
    | K.Period, L (TFieldName _, toTheLeft), _
      when onEdge ->
        doInsert ~pos keyChar toTheLeft ast s
    (* Int to float *)
    | K.Period, L (TInteger (id, _), ti), _ ->
        let offset = pos - ti.startPos in
        (convertIntToFloat offset id ast, moveOneRight pos s)
    (* Binop specific *)
    | K.Percent, L (_, toTheLeft), _
    | K.Minus, L (_, toTheLeft), _
    | K.Plus, L (_, toTheLeft), _
    | K.Multiply, L (_, toTheLeft), _
    | K.ForwardSlash, L (_, toTheLeft), _
    | K.LessThan, L (_, toTheLeft), _
    | K.GreaterThan, L (_, toTheLeft), _
      when onEdge ->
        ( convertToBinOp keyChar (Token.tid toTheLeft.token) ast
        , s |> moveTo (pos + 3) )
    (* End of line *)
    | K.Enter, _, R (TNewline, _) ->
        (ast, moveOneRight pos s)
    (* Let specific *)
    | K.Equals, _, R (TLetAssignment _, toTheRight) ->
        (ast, moveTo toTheRight.endPos s)
    (* Rest of Insertions *)
    | _, L (TListOpen _, toTheLeft), R (TListClose _, _) ->
        doInsert ~pos keyChar toTheLeft ast s
    | _, L (_, toTheLeft), _ when isAppendable toTheLeft.token ->
        doInsert ~pos keyChar toTheLeft ast s
    | _, _, R (TListOpen _, _) ->
        (ast, s)
    | _, _, R (_, toTheRight) ->
        doInsert ~pos keyChar toTheRight ast s
    | _ ->
        (* Unknown *)
        (ast, report ("Unknown action: " ^ K.toName key) s)
  in
  (newAST, newState)


let updateAutocomplete m tlid ast s : fluidState =
  let tokens = toTokens s ast in
  let ti =
    let toTheLeft, toTheRight, _ = getNeighbours ~pos:s.newPos tokens in
    match (toTheLeft, toTheRight) with
    | _, R (_, ti)
      when isTextToken ti.token && Token.isAutocompletable ti.token ->
        Some ti
    | L (_, ti), _
      when isTextToken ti.token && Token.isAutocompletable ti.token ->
        Some ti
    | _ ->
        None
  in
  match ti with
  | Some ti ->
      let m = TL.withAST m tlid (toExpr ast) in
      let newAC = AC.regenerate m s.ac (tlid, ti) in
      {s with ac = newAC}
  | None ->
      s


let updateMsg m tlid (ast : ast) (msg : Types.msg) (s : fluidState) :
    ast * fluidState =
  (* TODO: The state should be updated from the last request, and so this
   * shouldn't be necessary, but the tests don't work without it *)
  let s = updateAutocomplete m tlid ast s in
  let newAST, newState =
    match msg with
    | FluidMouseClick ->
      ( match getCursorPosition () with
      | Some newPos ->
          let lastPos =
            toTokens s ast
            |> List.last
            |> Option.map ~f:(fun ti -> ti.endPos)
            |> Option.withDefault ~default:0
          in
          let newPos = if newPos > lastPos then lastPos else newPos in
          (ast, setPosition s newPos)
      | None ->
          (ast, {s with error = Some "found no pos"}) )
    | FluidKeyPress {key} ->
        let s = {s with lastKey = key} in
        updateKey key ast s
    | _ ->
        (ast, s)
  in
  let newState = updateAutocomplete m tlid newAST newState in
  (newAST, newState)


let update (m : Types.model) (msg : Types.msg) : Types.modification =
  let s = m.fluidState in
  let s = {s with error = None; oldPos = s.newPos; actions = []} in
  match msg with
  | FluidKeyPress {key; metaKey; ctrlKey; shiftKey}
    when (metaKey || ctrlKey) && key = K.Letter 'z' ->
      KeyPress.undo_redo m shiftKey
  | FluidKeyPress {key; metaKey; ctrlKey; shiftKey}
    when (metaKey || ctrlKey) && key = K.Letter 'z' ->
      KeyPress.undo_redo m shiftKey
  | FluidKeyPress {key; altKey} when altKey && key = K.Letter 'x' ->
      maybeOpenCmd m
  | FluidKeyPress ke when m.fluidState.cp.show = true ->
      FluidCommands.updateCmds m ke
  | _ ->
    ( match Toplevel.selected m with
    | None ->
        Types.NoChange
    | Some tl ->
      ( match TL.rootExpr tl with
      | None ->
          NoChange
      | Some ast ->
          let ast = ast |> fromExpr s in
          let newAST, newState = updateMsg m tl.id ast msg s in
          let cmd =
            let astCmd =
              if newAST <> ast || newState.oldPos <> newState.newPos
              then [setBrowserPos newState.newPos]
              else []
            in
            let acCmd =
              match newState.ac.index with
              | Some index ->
                  [FluidAutocomplete.focusItem index]
              | None ->
                  []
            in
            let commands = acCmd @ astCmd in
            if List.isEmpty commands then Cmd.none else Cmd.batch commands
          in
          let astMod =
            if ast <> newAST
            then
              let asExpr = toExpr newAST in
              Many
                [ Types.TweakModel (fun m -> TL.withAST m tl.id asExpr)
                ; Toplevel.setSelectedAST m asExpr ]
            else Types.NoChange
          in
          Types.Many
            [ Types.TweakModel (fun m -> {m with fluidState = newState})
            ; astMod
            ; Types.MakeCmd cmd ] ) )


(* -------------------- *)
(* View *)
(* -------------------- *)
let viewAutocomplete (ac : Types.fluidAutocompleteState) : Types.msg Html.html
    =
  let toList acis class' index =
    List.indexedMap
      ~f:(fun i item ->
        let highlighted = index = i in
        let name = AC.asName item in
        Html.li
          [ Attrs.classList
              [ ("autocomplete-item", true)
              ; ("fluid-selected", highlighted)
              ; (class', true) ]
          ; ViewUtils.nothingMouseEvent "mouseup"
          ; ViewEntry.defaultPasteHandler
          ; ViewUtils.nothingMouseEvent "mousedown"
          ; ViewUtils.eventNoPropagation ~key:("ac-" ^ name) "click" (fun _ ->
                AutocompleteClick name ) ]
          [ Html.text name
          ; Html.span [Html.class' "types"] [Html.text <| AC.asTypeString item]
          ] )
      acis
  in
  let index = ac.index |> Option.withDefault ~default:(-1) in
  let invalidIndex = index - List.length ac.completions in
  let autocompleteList =
    toList ac.completions "valid" index
    @ toList ac.invalidCompletions "invalid" invalidIndex
  in
  Html.div [Attrs.id "fluid-dropdown"] [Html.ul [] autocompleteList]


let viewCopyButton tlid value : msg Html.html =
  Html.div
    [ Html.class' "copy-value"
    ; Html.title "Copy this expression's value to the clipboard"
    ; ViewUtils.eventNoPropagation
        "click"
        ~key:("copylivevalue-" ^ showTLID tlid)
        (fun _ -> ClipboardCopyLivevalue value) ]
    [ViewUtils.fontAwesome "copy"]


let viewLiveValue ~tlid ~currentResults ti : Types.msg Html.html =
  match ti.token with
  | TSep | TNewline | TIndented _ | TIndent _ | TIndentToHere _ ->
      Vdom.noNode
  | _ ->
      let id = Token.tid ti.token in
      let liveValueString =
        StrDict.get ~key:(deID id) currentResults.liveValues
        |> Option.map ~f:Runtime.toRepr
        |> Option.withDefault ~default:"<loading>"
      in
      Html.div
        [ Html.class' "live-value"
        ; Vdom.prop "contentEditable" "true"
        ; Attrs.autofocus false
        ; Attrs.spellcheck false ]
        [Html.text liveValueString; viewCopyButton tlid liveValueString]


let viewErrorIndicator ~currentResults ti : Types.msg Html.html =
  let sentToRail id =
    let dv =
      StrDict.get ~key:(deID id) currentResults.liveValues
      |> Option.withDefault ~default:DNull
    in
    match dv with
    | DErrorRail (DOption OptNothing) | DErrorRail (DResult (ResError _)) ->
        true
    | _ ->
        false
  in
  match ti.token with
  | TFnName (id, _, Rail) ->
      Html.span
        [Html.class' "error-indicator"]
        [ Html.span
            [ Html.class' "error-icon"
            ; Vdom.prop "data-sent-to-rail" (sentToRail id |> string_of_bool)
            ]
            [] ]
  | _ ->
      Vdom.noNode


let toHtml ~tlid ~currentResults ~state (l : tokenInfo list) :
    Types.msg Html.html list =
  let displayedLv = ref false in
  List.map l ~f:(fun ti ->
      let dropdown () =
        if state.cp.show && Some (Token.tid ti.token) = state.cp.cmdOnID
        then FluidCommands.viewCommandPalette state.cp
        else if isAutocompleting ti state
        then viewAutocomplete state.ac
        else Vdom.noNode
      in
      let liveValue () = viewLiveValue ~tlid ~currentResults ti in
      let errorIndicator = viewErrorIndicator ~currentResults ti in
      let element nested =
        let content = Token.toText ti.token in
        let classes = Token.toCssClasses ti.token in
        let idclasses = [("id-" ^ deID (Token.tid ti.token), true)] in
        Html.span
          [ Attrs.classList
              (("fluid-entry", true) :: (classes, true) :: idclasses) ]
          ([Html.text content] @ nested)
      in
      let liveValue =
        if state.newPos <= ti.endPos
           && state.newPos >= ti.startPos
           && not !displayedLv
        then (
          displayedLv := true ;
          liveValue () )
        else Vdom.noNode
      in
      [element [dropdown (); liveValue]; errorIndicator] )
  |> List.flatten


let viewAST
    ~(tlid : tlid)
    ~(currentResults : analysisResults)
    ~(state : state)
    (ast : ast) : Types.msg Html.html =
  let cmdOpen = FluidCommands.isOpenOnTL state.cp tlid in
  let event ~(key : string) (event : string) : Types.msg Vdom.property =
    let decodeNothing =
      let open Tea.Json.Decoder in
      succeed Types.IgnoreMsg
    in
    Html.onWithOptions
      ~key
      event
      {stopPropagation = false; preventDefault = not cmdOpen}
      decodeNothing
  in
  let eventKey = "keydown" ^ show_tlid tlid ^ string_of_bool cmdOpen in
  Html.div
    [ Attrs.id editorID
    ; Vdom.prop "contentEditable" "true"
    ; Attrs.autofocus true
    ; Attrs.spellcheck false
    ; event ~key:eventKey "keydown"
    (* ; event ~key:"keyup" "keyup" *)
     ]
    (ast |> toTokens state |> toHtml ~tlid ~currentResults ~state)


let viewStatus (ast : ast) (s : state) : Types.msg Html.html =
  let tokens = toTokens s ast in
  let posDiv =
    let oldGrid = gridFor ~pos:s.oldPos tokens in
    let newGrid = gridFor ~pos:s.newPos tokens in
    [ Html.div
        []
        [ Html.text (string_of_int s.oldPos)
        ; Html.text " -> "
        ; Html.text (string_of_int s.newPos) ]
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
        [ Html.text "acIndex: "
        ; Html.text
            ( s.ac.index
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; Html.div
        []
        [ Html.text "lastKey: "
        ; Html.text
            ( K.toName s.lastKey
            ^ ", "
            ^ ( K.toChar s.lastKey
              |> Option.map ~f:String.fromChar
              |> Option.withDefault ~default:"" ) ) ] ]
  in
  let tokenDiv =
    let prev, current, next = getTokensAtPosition tokens ~pos:s.newPos in
    let p =
      match prev with Some prev -> Token.show_tokenInfo prev | None -> "none"
    in
    let c =
      match current with
      | Some current ->
          Token.show_tokenInfo current
      | None ->
          "none"
    in
    let n =
      match next with Some next -> Token.show_tokenInfo next | None -> "none"
    in
    [ Html.text ("prev: " ^ p)
    ; Html.br []
    ; Html.text ("current: " ^ c)
    ; Html.br []
    ; Html.text ("next: " ^ n) ]
  in
  let actions =
    [ Html.div
        []
        ( [Html.text "Actions: "]
        @ ( s.actions
          |> List.map ~f:(fun action -> [action; ", "])
          |> List.concat
          |> List.dropRight ~count:1
          |> List.map ~f:Html.text ) ) ]
  in
  let error =
    [ Html.div
        []
        [ Html.text
            ( Option.map s.error ~f:(fun e -> "Errors: " ^ e)
            |> Option.withDefault ~default:"none" ) ] ]
  in
  let status = List.concat [posDiv; tokenDiv; actions; error] in
  Html.div [Attrs.id "fluid-status"] status

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
