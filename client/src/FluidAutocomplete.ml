open Tc
open Types

(* Dark *)
module P = Pointer
module RT = Runtime
module TL = Toplevel
module B = Blank
module Regex = Util.Regex

type autocomplete = fluidAutocompleteState [@@deriving show]

type autocompleteItem = fluidAutocompleteItem [@@deriving show]

type tokenInfo = fluidTokenInfo [@@deriving show]

let focusItem (i : int) : msg Tea.Cmd.t =
  Tea_task.attempt
    (fun _ -> IgnoreMsg)
    (Tea_task.nativeBinding (fun _ ->
         let open Webapi.Dom in
         let open Native.Ext in
         let container = Document.getElementById "fluid-dropdown" document in
         let nthChild =
           querySelector
             ("#fluid-dropdown ul li:nth-child(" ^ string_of_int (i + 1) ^ ")")
         in
         match (container, nthChild) with
         | Some el, Some li ->
             let cRect = getBoundingClientRect el in
             let cBottom = rectBottom cRect in
             let cTop = rectTop cRect in
             let liRect = getBoundingClientRect li in
             let liBottom = rectBottom liRect in
             let liTop = rectTop liRect in
             let liHeight = rectHeight liRect in
             if liBottom +. liHeight > cBottom
             then
               let offset = float_of_int (offsetTop li) in
               let padding = rectHeight cRect -. (liHeight *. 2.0) in
               Element.setScrollTop el (offset -. padding)
             else if liTop -. liHeight < cTop
             then
               let offset = float_of_int (offsetTop li) in
               Element.setScrollTop el (offset -. liHeight)
             else ()
         | _, _ ->
             () ))


(* ---------------------------- *)
(* display *)
(* ---------------------------- *)
let asName (aci : autocompleteItem) : string =
  match aci with
  | FACFunction {fnName} ->
      fnName
  | FACField name ->
      name
  | FACVariable name ->
      name
  | FACLiteral lit ->
      lit
  | FACConstructorName (name, _) ->
      name
  | FACKeyword k ->
    ( match k with
    | KLet ->
        "let"
    | KIf ->
        "if"
    | KLambda ->
        "lambda"
    | KMatch ->
        "match" )
  | FACPattern p ->
    ( match p with
    | FPVariable (_, _, name) | FPConstructor (_, _, name, _) ->
        name
    | FPInteger (_, _, v) ->
        string_of_int v
    | FPBool (_, _, v) ->
        string_of_bool v
    | FPString (_, _, v) ->
        v
    | FPFloat (_, _, v, v') ->
        v ^ "." ^ v'
    | FPNull _ ->
        "null"
    | FPBlank _ ->
        "_"
    | FPOldPattern _ ->
        "TODO: oldPattern" )


let asTypeString (item : autocompleteItem) : string =
  match item with
  | FACFunction f ->
      f.fnParameters
      |> List.map ~f:(fun x -> x.paramTipe)
      |> List.map ~f:RT.tipe2str
      |> String.join ~sep:", "
      |> fun s -> "(" ^ s ^ ") ->  " ^ RT.tipe2str f.fnReturnTipe
  | FACField _ ->
      "field"
  | FACVariable _ | FACPattern (FPVariable _) ->
      "variable"
  | FACConstructorName (name, _) | FACPattern (FPConstructor (_, _, name, _))
    ->
      if name = "Just"
      then "(any) -> option"
      else if name = "Nothing"
      then "option"
      else if name = "Ok" || name = "Error"
      then "(any) -> result"
      else ""
  | FACLiteral lit ->
      let tipe =
        lit
        |> Decoders.parseDvalLiteral
        |> Option.withDefault ~default:DIncomplete
        |> RT.typeOf
        |> RT.tipe2str
      in
      tipe ^ " literal"
  | FACPattern (FPString _) ->
      "string literal"
  | FACPattern (FPInteger _) ->
      "integer literal"
  | FACPattern (FPBool _) ->
      "boolean literal"
  | FACPattern (FPFloat _) ->
      "float literal"
  | FACKeyword _ ->
      "keyword"
  | FACPattern (FPNull _) ->
      "null"
  | FACPattern (FPBlank _) ->
      "_"
  | FACPattern (FPOldPattern _) ->
      "TODO: oldPattern"


let asString (aci : autocompleteItem) : string = asName aci ^ asTypeString aci

(* ---------------------------- *)
(* Utils *)
(* ---------------------------- *)

let isVariable (aci : autocompleteItem) : bool =
  match aci with FACVariable _ -> true | _ -> false


(* ---------------------------- *)
(* External: utils *)
(* ---------------------------- *)
let findFunction (a : autocomplete) (name : string) : function_ option =
  List.find ~f:(fun f -> f.fnName = name) a.functions


let allCompletions (a : autocomplete) : autocompleteItem list =
  a.completions @ a.invalidCompletions


let highlighted (a : autocomplete) : autocompleteItem option =
  Option.andThen a.index ~f:(fun index -> List.getAt ~index (allCompletions a))


let rec containsOrdered (needle : string) (haystack : string) : bool =
  match String.uncons needle with
  | Some (c, newneedle) ->
      let char = String.fromChar c in
      String.contains ~substring:char haystack
      && containsOrdered
           newneedle
           ( haystack
           |> String.split ~on:char
           |> List.drop ~count:1
           |> String.join ~sep:char )
  | None ->
      true


let dvalFields (dv : dval) : string list =
  match dv with DObj dict -> StrDict.keys dict | _ -> []


let findCompatibleThreadParam ({fnParameters} : function_) (tipe : tipe) :
    parameter option =
  fnParameters
  |> List.head
  |> Option.andThen ~f:(fun fst ->
         if RT.isCompatible fst.paramTipe tipe then Some fst else None )


let findParamByType ({fnParameters} : function_) (tipe : tipe) :
    parameter option =
  fnParameters |> List.find ~f:(fun p -> RT.isCompatible p.paramTipe tipe)


let dvalForTarget (m : model) (tl : toplevel) (ti : tokenInfo) : dval option =
  let ast = tl |> TL.asHandler |> Option.map ~f:(fun x -> x.ast) in
  match ast with
  | Some ast ->
      let id = FluidToken.tid ti.token in
      AST.find id ast
      |> Option.andThen ~f:(fun pd -> AST.getValueParent pd ast)
      |> Option.map ~f:P.toID
      |> Option.andThen ~f:(Analysis.getCurrentLiveValue m tl.id)
      (* don't filter on incomplete values *)
      |> Option.andThen ~f:(fun dv_ ->
             if dv_ = DIncomplete then None else Some dv_ )
  | None ->
      None


let isThreadMember (tl : toplevel) (ti : tokenInfo) =
  let id = FluidToken.tid ti.token in
  TL.asHandler tl
  |> Option.map ~f:(fun x -> x.ast)
  |> Option.andThen ~f:(AST.findParentOfWithin_ id)
  |> Option.map ~f:(fun e ->
         match e with F (_, Thread _) -> true | _ -> false )
  |> Option.withDefault ~default:false


let paramTipeForTarget (a : autocomplete) (tl : toplevel) (ti : tokenInfo) :
    tipe =
  let id = FluidToken.tid ti.token in
  TL.asHandler tl
  |> Option.map ~f:(fun x -> x.ast)
  |> Option.andThen ~f:(fun ast -> AST.getParamIndex ast id)
  |> Option.andThen ~f:(fun (name, index) ->
         a.functions
         |> List.find ~f:(fun f -> name = f.fnName)
         |> Option.map ~f:(fun x -> x.fnParameters)
         |> Option.andThen ~f:(List.getAt ~index)
         |> Option.map ~f:(fun x -> x.paramTipe) )
  |> Option.withDefault ~default:TAny


let matchesTypes
    (isThreadMemberVal : bool) (paramTipe : tipe) (dv : dval option) :
    function_ -> bool =
 fun fn ->
  let matchesReturnType = RT.isCompatible fn.fnReturnTipe paramTipe in
  let matchesParamType =
    match dv with
    | Some dval ->
        if isThreadMemberVal
        then None <> findCompatibleThreadParam fn (RT.typeOf dval)
        else None <> findParamByType fn (RT.typeOf dval)
    | None ->
        true
  in
  matchesReturnType && matchesParamType


(* ------------------------------------ *)
(* Dynamic Items *)
(* ------------------------------------ *)

let qLiteral (s : string) : autocompleteItem option =
  if Runtime.isStringLiteral s
  then
    if Runtime.isValidDisplayString s
    then Some (FACLiteral (Runtime.convertDisplayStringToLiteral s))
    else None
  else if Decoders.isLiteralRepr s
  then Some (FACLiteral s)
  else if String.length s > 0
  then
    if String.startsWith ~prefix:(String.toLower s) "false"
    then Some (FACLiteral "false")
    else if String.startsWith ~prefix:(String.toLower s) "true"
    then Some (FACLiteral "true")
    else if String.startsWith ~prefix:(String.toLower s) "null"
    then Some (FACLiteral "null")
    else None
  else None


let matcher
    (tipeConstraintOnTarget : tipe)
    (dbnames : string list)
    (matchTypesOfFn : tipe -> function_ -> bool)
    (item : autocompleteItem) =
  match item with
  | FACFunction fn ->
      matchTypesOfFn tipeConstraintOnTarget fn
  | FACVariable var ->
      if List.member ~value:var dbnames
      then match tipeConstraintOnTarget with TDB -> true | _ -> false
      else true
  | FACConstructorName (name, _) ->
    ( match tipeConstraintOnTarget with
    | TOption ->
        name = "Just" || name = "Nothing"
    | TResult ->
        name = "Ok" || name = "Error"
    | TAny ->
        true
    | _ ->
        false )
  | _ ->
      true


type query = tlid * tokenInfo

type fullQuery = toplevel * tokenInfo * dval option * string

let toQueryString (ti : tokenInfo) : string =
  if FluidToken.isBlank ti.token then "" else FluidToken.toText ti.token


(* ------------------------------------ *)
(* Create the list *)
(* ------------------------------------ *)

let generate (m : model) (a : autocomplete) ((tl, ti, _, _) : fullQuery) :
    autocomplete =
  let varnames, _dval =
    let id = FluidToken.tid ti.token in
    (Analysis.getCurrentAvailableVarnames m tl id, dvalForTarget m tl ti)
  in
  let fields =
    []
    (*     match dval with *)
    (* | Some dv when RT.typeOf dv = TObj -> *)
    (*   ( match a.target with *)
    (*   | Some (_, pd) when P.typeOf pd = Field -> *)
    (*       List.map ~f:(fun x -> ACField x) (dvalFields dv) *)
    (*   | _ -> *)
    (*       [] ) *)
    (* | _ -> *)
    (* [] *)
  in
  (* let isExpression = *)
  (*   match a.target with Some (_, p) -> P.typeOf p = Expr | None -> false *)
  (* in *)
  (* functions *)
  let funcList = a.functions in
  (* let funcList = if isExpression then a.functions else [] in *)
  let functions = List.map ~f:(fun x -> FACFunction x) funcList in
  let constructors =
    [ FACConstructorName ("Just", 1)
    ; FACConstructorName ("Nothing", 0)
    ; FACConstructorName ("Ok", 1)
    ; FACConstructorName ("Error", 1) ]
  in
  let extras =
    []
    (* match a.target with *)
    (* | Some (_, p) -> *)
    (*   ( match P.typeOf p with *)
    (*   (* autocomplete HTTP verbs if the handler is in the HTTP event space *) *)
    (*   | Pattern -> *)
    (*     ( match dval with *)
    (*     | Some dv when RT.typeOf dv = TResult -> *)
    (*         [FACConstructorName "Ok"; FACConstructorName "Error"] *)
    (*     | Some dv when RT.typeOf dv = TOption -> *)
    (*         [FACConstructorName "Just"; FACConstructorName "Nothing"] *)
    (*     | _ -> *)
    (*         constructors ) *)
    (*   | _ -> *)
    (*       [] ) *)
    (* | _ -> *)
    (*     [] *)
  in
  let open Prelude in
  let pmid = gid () in
  let patterns =
    match ti.token with
    | TPatternBlank _ | TPatternVariable _ ->
        (* if a pattern is in the autocomplete already, don't bother creating
         * new FACPatterns with different pmids and pids *)
        if List.any
             ~f:(fun v -> match v with FACPattern _ -> true | _ -> false)
             a.allCompletions
        then a.allCompletions
        else
          [ FPBool (pmid, gid (), true)
          ; FPBool (pmid, gid (), false)
          ; FPConstructor (pmid, gid (), "Just", [FPBlank (pmid, gid ())])
          ; FPConstructor (pmid, gid (), "Nothing", [])
          ; FPConstructor (pmid, gid (), "Ok", [FPBlank (pmid, gid ())])
          ; FPConstructor (pmid, gid (), "Error", [FPBlank (pmid, gid ())])
          ; FPNull (pmid, gid ()) ]
          @ List.map ~f:(fun var -> FPVariable (pmid, gid (), var)) varnames
          |> List.map ~f:(fun x -> FACPattern x)
    | _ ->
        []
  in
  let exprs =
    (* if isExpression *)
    (* then *)
    let varnames = List.map ~f:(fun x -> FACVariable x) varnames in
    let keywords =
      List.map ~f:(fun x -> FACKeyword x) [KLet; KIf; KLambda; KMatch]
    in
    let literals =
      List.map ~f:(fun x -> FACLiteral x) ["true"; "false"; "null"]
    in
    varnames @ constructors @ keywords @ literals @ functions
    (* else [] *)
  in
  let items = extras @ exprs @ fields in
  if patterns == []
  then {a with allCompletions = items}
  else
    { a with
      allCompletions = patterns
    ; index = Some (a.index |> Option.withDefault ~default:0) }


let filter
    (m : model)
    (a : autocomplete)
    (candidates0 : autocompleteItem list)
    ((tl, ti, dval, queryString) : fullQuery) :
    autocompleteItem list * autocompleteItem list =
  let lcq = queryString |> String.toLower in
  let stringify i =
    (if 1 >= String.length lcq then asName i else asString i)
    |> Regex.replace ~re:(Regex.regex {js|⟶|js}) ~repl:"->"
  in
  (* split into different lists *)
  let candidates1, notSubstring =
    List.partition
      ~f:(stringify >> String.toLower >> String.contains ~substring:lcq)
      candidates0
  in
  let startsWith, candidates2 =
    List.partition
      ~f:(stringify >> String.startsWith ~prefix:queryString)
      candidates1
  in
  let startsWithCI, candidates3 =
    List.partition
      ~f:(stringify >> String.toLower >> String.startsWith ~prefix:lcq)
      candidates2
  in
  let substring, substringCI =
    List.partition
      ~f:(stringify >> String.contains ~substring:queryString)
      candidates3
  in
  let stringMatch, _notMatched =
    List.partition
      ~f:(asName >> String.toLower >> containsOrdered lcq)
      notSubstring
  in
  let allMatches =
    [startsWith; startsWithCI; substring; substringCI; stringMatch]
    |> List.concat
  in
  (* Now split list by type validity *)
  let dbnames = TL.allDBNames m.toplevels in
  let isThreadMemberVal = isThreadMember tl ti in
  let tipeConstraintOnTarget = paramTipeForTarget a tl ti in
  let matchTypesOfFn pt = matchesTypes isThreadMemberVal pt dval in
  List.partition
    ~f:(matcher tipeConstraintOnTarget dbnames matchTypesOfFn)
    allMatches


let refilter
    (m : model)
    ((tl, ti, _, queryString) as query : fullQuery)
    (old : autocomplete) : autocomplete =
  (* add or replace the literal the user is typing to the completions *)
  let newCompletions, invalidCompletions =
    filter m old old.allCompletions query
  in
  let oldHighlight = highlighted old in
  let allCompletions = newCompletions @ invalidCompletions in
  let newCount = List.length allCompletions in
  let oldHighlightNewPos =
    oldHighlight
    |> Option.andThen ~f:(fun oh -> List.elemIndex ~value:oh allCompletions)
  in
  let oldQueryString =
    match old.query with Some (_, ti) -> toQueryString ti | _ -> ""
  in
  let index =
    (* Clear the highlight conditions *)
    if queryString = ""
       (* when we had previously highlighted something due to any actual match *)
       (* or this condition previously held and nothing has changed *)
       && (old.index = None || oldQueryString <> "")
    then None
    else
      (* If an entry is highlighted, and you press another *)
      (* valid key for that entry, keep it highlighted *)
      match oldHighlightNewPos with
      | Some i ->
          Some i
      (* If an entry vanishes, highlight 0 *)
      | None ->
          (* if nothing matches, highlight nothing *)
          if newCount = 0
          then
            None
            (* we matched something but its gone, go to top of *)
            (* list *)
          else Some 0
  in
  { old with
    index
  ; query = Some (tl.id, ti)
  ; completions = newCompletions
  ; invalidCompletions }


let regenerate (m : model) (a : autocomplete) ((tlid, ti) : query) :
    autocomplete =
  let tl = TL.getExn m tlid in
  let queryString = toQueryString ti in
  let dval = dvalForTarget m tl ti in
  let query = (tl, ti, dval, queryString) in
  generate m a query |> refilter m query


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let reset (m : model) : autocomplete =
  let userFunctionMetadata =
    m.userFunctions
    |> List.map ~f:(fun x -> x.ufMetadata)
    |> List.filterMap ~f:Functions.ufmToF
  in
  let functions =
    m.builtInFunctions
    |> List.filter ~f:(fun f ->
           (not f.fnDeprecated) || Refactor.usedFn m f.fnName )
  in
  let functions = functions @ userFunctionMetadata in
  {Defaults.defaultModel.fluidState.ac with functions}


let init m = reset m

let numCompletions (a : autocomplete) : int =
  List.length a.completions + List.length a.invalidCompletions


let selectDown (a : autocomplete) : autocomplete =
  match a.index with
  | Some index ->
      let max_ = numCompletions a in
      let max = max max_ 1 in
      let new_ = (index + 1) mod max in
      {a with index = Some new_}
  | None ->
      a


let selectUp (a : autocomplete) : autocomplete =
  match a.index with
  | Some index ->
      let max = numCompletions a - 1 in
      {a with index = Some (if index <= 0 then max else index - 1)}
  | None ->
      a


let rec documentationForItem (aci : autocompleteItem) : string option =
  match aci with
  | FACFunction f ->
      let desc =
        if String.length f.fnDescription <> 0
        then f.fnDescription
        else "Function call with no description"
      in
      let desc = if f.fnDeprecated then "DEPRECATED: " ^ desc else desc in
      Some desc
  | FACConstructorName ("Just", _) ->
      Some "An Option containing a value"
  | FACConstructorName ("Nothing", _) ->
      Some "An Option representing Nothing"
  | FACConstructorName ("Ok", _) ->
      Some "A successful Result containing a value"
  | FACConstructorName ("Error", _) ->
      Some "A Result representing a failure"
  | FACConstructorName (name, _) ->
      Some ("TODO: this should never occur: the constructor " ^ name)
  | FACField fieldname ->
      Some ("The '" ^ fieldname ^ "' field of the object")
  | FACVariable var ->
      if String.isCapitalized var
      then Some ("The database '" ^ var ^ "'")
      else Some ("The variable '" ^ var ^ "'")
  | FACLiteral lit ->
      Some ("The literal value '" ^ lit ^ "'")
  | FACKeyword KLet ->
      Some "A `let` expression allows you assign a variable to an expression"
  | FACKeyword KIf ->
      Some "An `if` expression allows you to branch on a boolean condition"
  | FACKeyword KLambda ->
      Some
        "A `lambda` creates an anonymous function. This is most often used for iterating through lists"
  | FACKeyword KMatch ->
      Some
        "A `match` expression allows you to pattern match on a value, and return different expressions based on many possible conditions"
  | FACPattern p ->
    ( match p with
    | FPConstructor (_, _, name, args) ->
        documentationForItem (FACConstructorName (name, List.length args))
    | FPVariable (_, _, name) ->
        documentationForItem (FACVariable name)
    | FPInteger (_, _, var) ->
        documentationForItem (FACLiteral (string_of_int var))
    | FPBool (_, _, var) ->
        documentationForItem (FACLiteral (string_of_bool var))
    | FPString (_, _, var) ->
        documentationForItem (FACLiteral var)
    | FPFloat (_, _, var, var') ->
        let var = var ^ "." ^ var' in
        documentationForItem (FACLiteral var)
    | FPNull _ ->
        Some "A 'null' literal"
    | FPBlank _ ->
        Some "A Blank allows you to match any value"
    | FPOldPattern _ ->
        Some "TODO: oldPattern" )
