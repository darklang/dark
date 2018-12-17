open! Porting
open Types

(* Dark *)
module P = Pointer
module RT = Runtime
module TL = Toplevel

(* ---------------------------- *)
(* Focus *)
(* ---------------------------- *)
(* show the prev 5 *)
(* obvi this should use getClientBoundingBox, but that's tough in Elm *)
let height (i : int) : int = if i < 4 then 0 else 14 * (i - 4)

let focusItem (i : int) : msg Tea.Cmd.t =
  Tea_task.attempt
    (fun x -> FocusAutocompleteItem x)
    (Tea_task.nativeBinding (fun _ ->
         let open Webapi.Dom in
         match Document.getElementById "autocomplete-holder" document with
         | Some el ->
             Element.setScrollTop el (i |> height |> float_of_int)
         | None ->
             () ))


(* ---------------------------- *)
(* display *)
(* ---------------------------- *)
let asName (aci : autocompleteItem) : string =
  match aci with
  | ACFunction {fnName} ->
      fnName
  | ACField name ->
      name
  | ACVariable name ->
      name
  | ACExtra name ->
      name
  | ACCommand command ->
      ":" ^ command.commandName
  | ACLiteral lit ->
      lit
  | ACOmniAction ac ->
    ( match ac with
    | NewDB name ->
        "Create new database: " ^ name
    | NewHandler ->
        "Create new handler"
    | NewFunction maybeName ->
      ( match maybeName with
      | Some name ->
          "Create new function: " ^ name
      | None ->
          "Create new function" )
    | NewHTTPHandler ->
        "Create new HTTP handler"
    | NewHTTPRoute name ->
        "Create new HTTP handler for " ^ name
    | NewEventSpace name ->
        "Create new " ^ name ^ " handler" )
  | ACConstructorName name ->
      if name = "Just" then "Just ______" else name
  | ACKeyword k ->
    ( match k with
    | KLet ->
        "let"
    | KIf ->
        "if"
    | KLambda ->
        "lambda"
    | KMatch ->
        "match" )


let asTypeString (item : autocompleteItem) : string =
  match item with
  | ACFunction f ->
      f.fnParameters
      |> List.map (fun x -> x.paramTipe)
      |> List.map RT.tipe2str
      |> String.join ", "
      |> fun s -> "(" ^ s ^ ") ->  " ^ RT.tipe2str f.fnReturnTipe
  | ACField _ ->
      "field"
  | ACVariable _ ->
      "variable"
  | ACExtra _ ->
      ""
  | ACCommand _ ->
      ""
  | ACConstructorName _ ->
      "option"
  | ACLiteral lit ->
      let tipe =
        lit
        |> Decoders.parseDvalLiteral
        |> Option.withDefault DIncomplete
        |> RT.typeOf
        |> RT.tipe2str
      in
      tipe ^ " literal"
  | ACOmniAction _ ->
      ""
  | ACKeyword _ ->
      "keyword"


let asString (aci : autocompleteItem) : string = asName aci ^ asTypeString aci

(* ---------------------------- *)
(* External: utils *)
(* ---------------------------- *)
let findFunction (a : autocomplete) (name : string) : function_ option =
  List.find (fun f -> f.fnName = name) a.functions


let isStringEntry (a : autocomplete) : bool = String.startsWith "\"" a.value

let isLargeStringEntry (a : autocomplete) : bool =
  isStringEntry a && String.contains "\n" a.value


let isSmallStringEntry (a : autocomplete) : bool =
  isStringEntry a && not (isLargeStringEntry a)


let highlighted (a : autocomplete) : autocompleteItem option =
  List.getAt a.index a.completions


let getValue (a : autocomplete) : string =
  match highlighted a with Some item -> asName item | None -> a.value


let rec sharedPrefix2 (l : string) (r : string) : string =
  match (String.uncons l, String.uncons r) with
  | Some (l1, lrest), Some (r1, rrest) ->
      if l1 = r1 then String.fromChar l1 ^ sharedPrefix2 lrest rrest else ""
  | _ ->
      ""


let sharedPrefixList (strs : string list) : string =
  match List.head strs with
  | None ->
      ""
  | Some s ->
      List.foldl sharedPrefix2 s strs


(* Find the shared prefix of all the possible suggestions (eg "List::") *)
let sharedPrefix (a : autocomplete) : string =
  a.completions |> List.map asName |> sharedPrefixList


let rec containsOrdered (needle : string) (haystack : string) : bool =
  match String.uncons needle with
  | Some (c, newneedle) ->
      let char = String.fromChar c in
      String.contains char haystack
      && containsOrdered
           newneedle
           (haystack |> String.split char |> List.drop 1 |> String.join char)
  | None ->
      true


(* returns (indent, suggestion, search), where: *)
(* - indent is the string that occurs before the match *)
(* - suggestion is the match rewritten with the search *)
(* - search is the search rewritten to match the suggestion *)
(* Returns no suggestion or indent for an OmniAction *)
let compareSuggestionWithActual (a : autocomplete) (actual : string) :
    string * string * string =
  match highlighted a with
  | Some (ACOmniAction _) ->
      ("", "", actual)
  | _ ->
      let suggestion = sharedPrefix a in
      ( match
          Js.String.indexOf (String.toLower actual) (String.toLower suggestion)
        with
      | -1 ->
          ("", suggestion, actual)
      | index ->
          let prefix = String.slice 0 index suggestion in
          let suffix =
            String.slice
              (index + String.length actual)
              (String.length suggestion)
              suggestion
          in
          (prefix, prefix ^ actual ^ suffix, actual) )


let nonAdminFunctions (fns : function_ list) : function_ list = fns

let dvalFields (dv : dval) : autocompleteItem list =
  match dv with
  | DObj dict ->
      StrDict.keys dict |> List.map (fun x -> ACField x)
  | _ ->
      []


let findCompatibleThreadParam ({fnParameters} : function_) (tipe : tipe) :
    parameter option =
  fnParameters
  |> List.head
  |> Option.andThen (fun fst ->
         if RT.isCompatible fst.paramTipe tipe then Some fst else None )


let findParamByType ({fnParameters} : function_) (tipe : tipe) :
    parameter option =
  fnParameters |> List.find (fun p -> RT.isCompatible p.paramTipe tipe)


let dvalForTarget (m : model) (target : target option) =
  match target with
  | None ->
      None
  | Some (tlid, p) ->
      TL.get m tlid
      |> Option.andThen TL.asHandler
      |> Option.map (fun x -> x.ast)
      |> Option.andThen (AST.getValueParent p)
      |> Option.map P.toID
      |> Option.andThen (Analysis.getCurrentLiveValue m tlid)
      (* don't filter on incomplete values *)
      |> Option.andThen (fun dv_ ->
             if dv_ = DIncomplete then None else Some dv_ )


let isThreadMember (m : model) (target : target option) =
  match target with
  | None ->
      false
  | Some (tlid, p) ->
      TL.get m tlid
      |> Option.andThen TL.asHandler
      |> Option.map (fun x -> x.ast)
      |> Option.andThen (AST.findParentOfWithin_ (P.toID p))
      |> Option.map (fun e ->
             match e with F (_, Thread _) -> true | _ -> false )
      |> Option.withDefault false


let paramTipeForTarget (m : model) (target : target option) =
  match target with
  | None ->
      None
  | Some (tlid, p) ->
      TL.get m tlid
      |> Option.andThen TL.asHandler
      |> Option.map (fun x -> x.ast)
      |> Option.andThen (fun ast -> AST.getParamIndex ast (P.toID p))
      |> Option.andThen (fun (name, index) ->
             m.complete.functions
             |> List.find (fun f -> name = f.fnName)
             |> Option.map (fun x -> x.fnParameters)
             |> Option.andThen (List.getAt index)
             |> Option.map (fun x -> x.paramTipe) )


let matchesType (m : model) (target : target option) : function_ -> bool =
  let dv = dvalForTarget m target in
  let matchesReturnType fn =
    match paramTipeForTarget m target with
    | Some t ->
        RT.isCompatible fn.fnReturnTipe t
    | None ->
        true
  in
  let matchesParamType fn =
    match dv with
    | Some dv ->
        if isThreadMember m target
        then None <> findCompatibleThreadParam fn (RT.typeOf dv)
        else None <> findParamByType fn (RT.typeOf dv)
    | None ->
        true
  in
  fun fn -> matchesReturnType fn && matchesParamType fn


(* ------------------------------------ *)
(* Dynamic Items *)
(* ------------------------------------ *)

let qLiteral (s : string) : autocompleteItem option =
  if Decoders.isLiteralString s
  then Some (ACLiteral s)
  else if String.length s > 0
  then
    if String.startsWith (String.toLower s) "false"
    then Some (ACLiteral "false")
    else if String.startsWith (String.toLower s) "true"
    then Some (ACLiteral "true")
    else if String.startsWith (String.toLower s) "null"
    then Some (ACLiteral "null")
    else None
  else None


let qNewDB (s : string) : autocompleteItem option =
  if String.length s >= 3
     && Util.reExactly "[A-Z][a-zA-Z0-9_-]+" s
     (* annoying to offer a DB when looking for HTTP handler *)
     && s <> "HTTP"
     && s <> "HTT"
  then Some (ACOmniAction (NewDB s))
  else None


let qHTTPHandler (s : string) : autocompleteItem option =
  if String.length s = 0 then Some (ACOmniAction NewHTTPHandler) else None


let qHandler (s : string) : autocompleteItem option =
  if String.length s = 0 then Some (ACOmniAction NewHandler) else None


let qFunction (s : string) : autocompleteItem option =
  if String.length s = 0
  then Some (ACOmniAction (NewFunction None))
  else if Util.reExactly "[a-zA-Z_][a-zA-Z0-9_]*" s
  then Some (ACOmniAction (NewFunction (Some s)))
  else None


let qHTTPRoute (s : string) : autocompleteItem option =
  if String.startsWith "/" s
  then Some (ACOmniAction (NewHTTPRoute s))
  else None


let qEventSpace (s : string) : autocompleteItem option =
  if Util.reExactly "[A-Z]+" s
  then Some (ACOmniAction (NewEventSpace s))
  else None


let isDynamicItem (item : autocompleteItem) : bool =
  match item with ACLiteral _ -> true | ACOmniAction _ -> true | _ -> false


let isStaticItem (item : autocompleteItem) : bool = not (isDynamicItem item)

let toDynamicItems (isOmni : bool) (query : string) : autocompleteItem list =
  let items =
    if isOmni
    then [qNewDB; qHandler; qFunction; qHTTPHandler; qHTTPRoute; qEventSpace]
    else [qLiteral]
  in
  items |> List.filterMap (fun aci -> aci query)


let withDynamicItems
    (target : target option) (query : string) (acis : autocompleteItem list) :
    autocompleteItem list =
  let new_ = toDynamicItems (target = None) query in
  let withoutDynamic = List.filter isStaticItem acis in
  new_ @ withoutDynamic


let paramFor (m : model) (tlid : tlid) (id : id) : parameter option =
  TL.get m tlid
  |> Option.andThen TL.asHandler
  |> Option.map (fun x -> x.ast)
  |> Option.andThen (fun ast -> AST.getParamIndex ast id)
  |> Option.andThen (fun (name, index) ->
         m.complete.functions
         |> List.find (fun f -> name = f.fnName)
         |> Option.map (fun x -> x.fnParameters)
         |> Option.andThen (List.getAt index) )


let paramForTarget (m : model) (a : autocomplete) : parameter option =
  match a.target with
  | None ->
      None
  | Some (tlid, p) ->
      paramFor m tlid (P.toID p)


(* ------------------------------------ *)
(* Create the list *)
(* ------------------------------------ *)
let generateFromModel (m : model) (a : autocomplete) : autocompleteItem list =
  let dv = dvalForTarget m a.target in
  let fields =
    match dv with
    | Some dv ->
      ( match (a.target, RT.typeOf dv) with
      | Some (_, p), TObj ->
          if P.typeOf p = Field then dvalFields dv else []
      | _ ->
          [] )
    | None ->
        []
  in
  let isExpression =
    match a.target with Some (_, p) -> P.typeOf p = Expr | None -> false
  in
  (* functions *)
  let funcList = if isExpression then a.functions else [] in
  let functions = List.map (fun x -> ACFunction x) funcList in
  let extras =
    match a.target with
    | Some (tlid, p) ->
      ( match P.typeOf p with
      (* autocomplete HTTP verbs if the handler is in the HTTP event space *)
      | EventModifier ->
        ( match TL.spaceOf (TL.getTL m tlid) with
        | Some HSHTTP ->
            ["GET"; "POST"; "PUT"; "DELETE"; "PATCH"]
        | Some HSCron ->
            [ "Daily"
            ; "Weekly"
            ; "Fortnightly"
            ; "Every 1hr"
            ; "Every 12hrs"
            ; "Every 1min" ]
        | Some HSOther ->
            []
        | Some HSEmpty ->
            []
        | None ->
            [] )
      | EventSpace ->
          ["HTTP"; "CRON"]
      | DBColType ->
          let builtins =
            [ "String"
            ; "Int"
            ; "Boolean"
            ; "Float"
            ; "Title"
            ; "Url"
            ; "Date"
            ; "Password"
            ; "UUID" ]
          in
          let compound = List.map (fun s -> "[" ^ s ^ "]") builtins in
          builtins @ compound
      | ParamTipe ->
          [ "Any"
          ; "String"
          ; "Int"
          ; "Boolean"
          ; "Float"
          ; "Date"
          ; "Obj"
          ; "Block"
          ; "Char"
          ; "List" ]
      | _ ->
          [] )
    | _ ->
        []
  in
  let exprs =
    if isExpression
    then
      let constructors =
        [ACConstructorName "Just"; ACConstructorName "Nothing"]
      in
      let varnames =
        Analysis.currentVarnamesFor m a.target
        |> List.map (fun x -> ACVariable x)
      in
      let keywords =
        List.map (fun x -> ACKeyword x) [KLet; KIf; KLambda; KMatch]
      in
      constructors @ keywords @ varnames @ functions
    else []
  in
  let regular = List.map (fun x -> ACExtra x) extras @ exprs @ fields in
  let commands = List.map (fun x -> ACCommand x) Commands.commands in
  if a.isCommandMode then commands else regular


let filter
    (m : model)
    (target : target option)
    (list : autocompleteItem list)
    (query : string) : autocompleteItem list =
  let lcq = query |> String.toLower in
  let stringify i =
    (if 1 >= String.length lcq then asName i else asString i)
    |> Regex.replace {js|⟶|js} "->"
  in
  (* split into different lists *)
  let dynamic, candidates0 = List.partition isDynamicItem list in
  let candidates1, notSubstring =
    List.partition
      (stringify >> String.toLower >> String.contains lcq)
      candidates0
  in
  let startsWith, candidates2 =
    List.partition (stringify >> String.startsWith query) candidates1
  in
  let startsWithCI, candidates3 =
    List.partition
      (stringify >> String.toLower >> String.startsWith lcq)
      candidates2
  in
  let substring, substringCI =
    List.partition (stringify >> String.contains query) candidates3
  in
  let stringMatch =
    List.filter (asName >> String.toLower >> containsOrdered lcq) notSubstring
  in
  let allMatches =
    [dynamic; startsWith; startsWithCI; substring; substringCI; stringMatch]
    |> List.concat
  in
  (* Now split list by type validity *)
  let matchFn = matchesType m target in
  List.filter (function ACFunction fn -> matchFn fn | _ -> true) allMatches


let init (fns : function_ list) (isAdmin : bool) : autocomplete =
  let functions = if isAdmin then fns else nonAdminFunctions fns in
  { functions
  ; admin = isAdmin
  ; completions = []
  ; allCompletions = []
  ; index = -1
  ; value = ""
  ; prevValue = ""
  ; target = None
  ; isCommandMode = false }


let refilter (query : string) (m : model) (old : autocomplete) : autocomplete =
  (* add or replace the literal the user is typing to the completions *)
  let fudgedCompletions =
    withDynamicItems old.target query old.allCompletions
  in
  let newCompletions = filter m old.target fudgedCompletions query in
  let newCount = newCompletions |> List.length in
  let oldHighlight = highlighted old in
  let oldHighlightNewPos =
    oldHighlight |> Option.andThen (fun oh -> List.elemIndex oh newCompletions)
  in
  let index =
    (* Clear the highlight conditions *)
    if query = ""
       (* when we had previously highlighted something due to any actual match *)
       && ( (old.index <> -1 && old.value <> query)
          (* or this condition previously held and nothing has changed *)
          || old.index = -1 )
    then -1
    else
      (* If an entry is highlighted, and you press another *)
      (* valid key for that entry, keep it highlighted *)
      match oldHighlightNewPos with
      | Some i ->
          i
      (* If an entry vanishes, highlight 0 *)
      | None ->
          (* if nothing matches, highlight nothing *)
          if newCount = 0
          then
            -1
            (* we matched something but its gone, go to top of *)
            (* list *)
          else 0
  in
  { old with
    index; completions = newCompletions; value = query; prevValue = old.value
  }


let regenerate (m : model) (a : autocomplete) : autocomplete =
  {a with allCompletions = generateFromModel m a} |> refilter a.value m


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let empty : autocomplete = init [] false

let reset (m : model) (a : autocomplete) : autocomplete =
  let userFunctionMetadata =
    m.userFunctions
    |> List.map (fun x -> x.ufMetadata)
    |> List.filterMap Functions.ufmToF
  in
  let unusedDeprecatedFns = Refactor.unusedDeprecatedFunctions m in
  let functions =
    m.builtInFunctions
    |> List.filter (fun f ->
           not
             (List.member
                f.fnName
                (List.map (fun x -> x.fnName) userFunctionMetadata)) )
    |> List.filter (fun f -> not (StrSet.member f.fnName unusedDeprecatedFns))
    |> List.append userFunctionMetadata
  in
  init functions a.admin |> regenerate m


let numCompletions (a : autocomplete) : int = a.completions |> List.length

let selectDown (a : autocomplete) : autocomplete =
  let max_ = numCompletions a in
  let max = max max_ 1 in
  let new_ = (a.index + 1) mod max in
  {a with index = new_}


let selectUp (a : autocomplete) : autocomplete =
  let max = numCompletions a - 1 in
  {a with index = (if a.index <= 0 then max else a.index - 1)}


(* Implementation: *)
(* n The autocomplete list should include: *)
(*    y all imported functions *)
(*    y restricted by types that are allowed *)
(*    y allowed field names *)
(*    n library names *)
(*    y case-insensitive *)
(* n order by most likely, offer other alternatives below *)
(*   n slight typos *)
(*   n slight typeos *)
(* y Press enter to select *)
(* y Press right to fill as much as is definitive *)
(*  *)
let setQuery (q : string) (m : model) (a : autocomplete) : autocomplete =
  refilter q m a


let appendQuery (str : string) (m : model) (a : autocomplete) : autocomplete =
  let q =
    if isStringEntry a
    then String.dropRight 1 a.value ^ str ^ "\""
    else a.value ^ str
  in
  setQuery q m a


let documentationForItem (aci : autocompleteItem) : string option =
  match aci with
  | ACFunction f ->
      if String.length f.fnDescription <> 0 then Some f.fnDescription else None
  | ACCommand c ->
      Some (c.doc ^ " (" ^ c.shortcut ^ ")")
  | _ ->
      None


let setTarget (m : model) (t : (tlid * pointerData) option) (a : autocomplete)
    : autocomplete =
  {a with target = t} |> regenerate m


(* ------------------------------------ *)
(* Commands *)
(* ------------------------------------ *)
let enableCommandMode (a : autocomplete) : autocomplete =
  {a with isCommandMode = true}


let update (m : model) (mod_ : autocompleteMod) (a : autocomplete) :
    autocomplete =
  match mod_ with
  | ACSetQuery str ->
      setQuery str m a
  | ACAppendQuery str ->
      appendQuery str m a
  | ACReset ->
      reset m a
  | ACSelectDown ->
      selectDown a
  | ACSelectUp ->
      selectUp a
  | ACSetTarget target ->
      setTarget m target a
  | ACRegenerate ->
      regenerate m a
  | ACEnableCommandMode ->
      enableCommandMode a


(* --------------------------- *)
(* Modifications *)
(* --------------------------- *)
let selectSharedPrefix (ac : autocomplete) : modification =
  let sp = sharedPrefix ac in
  if sp = "" then NoChange else AutocompleteMod (ACSetQuery sp)
