open Prelude
open Tc
open Types

(* Dark *)
module P = Pointer
module RT = Runtime
module TL = Toplevel
module B = BlankOr
module Regex = Util.Regex
module TD = TLIDDict

(* ---------------------------- *)
(* Focus *)
(* ---------------------------- *)
(* show the prev 5 *)
(* obvi this should use getClientBoundingBox, but that's tough in Elm *)
let height (i : int) : int = if i < 4 then 0 else 14 * (i - 4)

let focusItem (i : int) : msg Tea.Cmd.t =
  Tea_task.attempt
    (fun _ -> IgnoreMsg)
    (Tea_task.nativeBinding (fun _ ->
         let open Webapi.Dom in
         match Document.getElementById "autocomplete-holder" document with
         | Some el ->
             Element.setScrollTop el (i |> height |> float_of_int)
         | None ->
             ()))


(* ---------------------------- *)
(* display *)
(* ---------------------------- *)
let asName (aci : autocompleteItem) : string =
  match aci with
  | ACOmniAction ac ->
    ( match ac with
    | NewDB maybeName ->
      ( match maybeName with
      | Some name ->
          "New DB named " ^ name
      | None ->
          "New DB" )
    | NewWorkerHandler maybeName ->
      ( match maybeName with
      | Some name ->
          "New WORKER handler named " ^ name
      | None ->
          "New WORKER handler" )
    | NewFunction maybeName ->
      ( match maybeName with
      | Some name ->
          "New function named " ^ name
      | None ->
          "New function" )
    | NewHTTPHandler maybeName ->
      ( match maybeName with
      | Some name ->
          "New HTTP handler named " ^ name
      | None ->
          "New HTTP handler" )
    | NewCronHandler maybeName ->
      ( match maybeName with
      | Some name ->
          "New CRON handler named " ^ name
      | None ->
          "New CRON handler" )
    | NewReplHandler maybeName ->
      ( match maybeName with
      | Some name ->
          "New REPL named " ^ name
      | None ->
          "New REPL handler" )
    | NewGroup maybeName ->
      ( match maybeName with
      | Some name ->
          "New group named " ^ name
      | None ->
          "New group" )
    | Goto (_, _, desc, _) ->
        desc )
  | ACHTTPModifier name ->
      name
  | ACHTTPRoute name ->
      name
  | ACWorkerName name ->
      name
  | ACReplName name ->
      name
  | ACCronName name ->
      name
  | ACCronTiming timing ->
      timing
  | ACEventSpace space ->
      space
  | ACDBColType tipe ->
      tipe
  | ACParamTipe tipe ->
      RT.tipe2str tipe
  | ACDBName name ->
      name
  | ACDBColName name
  | ACEventModifier name
  | ACFnName name
  | ACParamName name
  | ACTypeName name
  | ACTypeFieldName name
  | ACGroupName name ->
      name
  | ACTypeFieldTipe tipe ->
      RT.tipe2str tipe


let asTypeString (item : autocompleteItem) : string =
  match item with
  | ACOmniAction _ ->
      ""
  | ACHTTPModifier _ ->
      "method"
  | ACHTTPRoute _ ->
      "route"
  | ACWorkerName _ ->
      "worker name"
  | ACReplName _ ->
      "REPL name"
  | ACCronName _ ->
      "cron job"
  | ACCronTiming _ ->
      "interval"
  | ACEventSpace _ ->
      "event space"
  | ACDBColType _ ->
      "type"
  | ACParamTipe _ ->
      "param type"
  | ACDBName _ ->
      "name"
  | ACDBColName _ ->
      "column name"
  | ACEventModifier _ ->
      "event modifier"
  | ACFnName _ ->
      "function name"
  | ACParamName _ ->
      "param name"
  | ACTypeName _ ->
      "type name"
  | ACTypeFieldName _ ->
      "type field name"
  | ACGroupName _ ->
      "group name"
  | ACTypeFieldTipe tipe ->
    ( match tipe with
    | TUserType (_, v) ->
        "version " ^ string_of_int v
    | _ ->
        "builtin" )


let asTypeClass (item : autocompleteItem) : string =
  match item with
  | ACOmniAction (NewDB _)
  | ACOmniAction (NewFunction _)
  | ACOmniAction (NewHTTPHandler _)
  | ACOmniAction (NewWorkerHandler _)
  | ACOmniAction (NewCronHandler _)
  | ACOmniAction (NewReplHandler _)
  | ACOmniAction (NewGroup _) ->
      "new-tl"
  | ACOmniAction (Goto (_, _, _, true)) ->
      "found-in"
  | ACOmniAction (Goto (_, _, _, false)) ->
      "jump-to"
  | _ ->
      ""


let asString (aci : autocompleteItem) : string = asName aci ^ asTypeString aci

(* ---------------------------- *)
(* External: utils *)
(* ---------------------------- *)

(* Return different type if possible *)
let highlighted (a : autocomplete) : autocompleteItem option =
  List.getAt ~index:a.index a.completions


let getValue (a : autocomplete) : string =
  match highlighted a with Some item -> asName item | None -> a.value


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


(* ------------------------------------ *)
(* Validators *)
(* ------------------------------------ *)

(*
  urls
  From https://www.w3.org/Addressing/URL/5_URI_BNF.html
  path = void | xpalphas [ / path ]
  xalpha = alpha | digit | safe | extra | escape
  xalphas = xalpha [ xalphas ]
  xpalpha = xalpha | +
  xpalphas = xpalpha [ xpalpha ]
  alpha = [a-zA-Z]
  digit = [0-9]
  safe = $ | - | _ | @ | . | &
  extra = ! | * | <doublequote> | ' | ( | ) | ,
  reserved = = | ; | / | # | ? | : | space
  escape = % hex hex
*)
(* let urlPathSafeCharacters = "[-a-zA-Z0-9$_@.&!*\"'(),%/]" *)
(* let nonUrlPathSafeCharacters = "[^-a-zA-Z0-9$_@.&!*\"'(),%/]" *)
(* let urlPathValidator = "[-a-zA-Z0-9$_@.&!*\"'(),%/]+" *)

(* allow : for parameter names. TODO: do better job parsing here *)

let nonEventNameSafeCharacters = "[^-a-zA-Z0-9$_@.&!*\"'(),%/:]"

let httpNameValidator = "/[-a-zA-Z0-9$_@.&!*\"'(),%/:]*"

let eventNameValidator = "[-a-zA-Z0-9$_@.&!*\"'(),%/:]+"

let varnameValidator = "[a-z_][a-zA-Z0-9_]*"

let dbColTypeValidator = "\\[?[A-Z]\\w+\\]?"

let dbColNameValidator = "\\w+"

let dbNameValidator = "[A-Z][a-zA-Z0-9_]*"

let groupNameValidator = "[A-Z][a-zA-Z0-9_]*"

let eventModifierValidator = "[a-zA-Z_][\\sa-zA-Z0-9_]*"

let httpVerbValidator = "[A-Z]+"

let eventSpaceValidator = "(CRON|HTTP|REPL|WORKER)"

let cronValidatorPattern =
  "(Daily|Weekly|Fortnightly|Every 1hr|Every 12hrs|Every 1min)"


let fieldNameValidator = ".+"

let fnNameValidator = "[a-z][a-zA-Z0-9_]*"

(* NB: disallowing inital-capitals also defends against having a collision
 * between a function param name and a db name *)
let paramNameValidator = "[a-z][a-zA-Z0-9_]*"

let typeNameValidator = dbNameValidator

let paramTypeValidator = "[A-Za-z0-9_]*"

(* Adding a more clear error message for invalid cron intervals *)
let cronIntervalValidator (name : string) : string option =
  if Regex.exactly ~re:cronValidatorPattern name
  then None
  else Some (name ^ " is an invalid CRON interval")


let assertValid pattern value : string =
  asserT
    "Failed validator"
    (Regex.exactly ~re:pattern value)
    ~debug:(pattern, value) ;
  value


let validateHttpNameValidVarnames (httpName : string) =
  let route_variables (route : string) : string list =
    route
    |> String.split ~on:"/"
    |> List.filter ~f:(fun x -> String.length x > 0)
    |> List.filter ~f:(fun x -> String.startsWith ~prefix:":" x)
    |> List.map ~f:(fun x -> String.dropLeft ~count:1 x)
  in
  if route_variables httpName
     |> List.all ~f:(fun v -> Regex.exactly ~re:varnameValidator v)
  then None
  else Some ("route variables must match /" ^ varnameValidator ^ "/")


let validateFnParamNameFree (tl : toplevel) (value : string) : string option =
  match tl with
  | TLFunc fn ->
      let params = Functions.allParamNames fn in
      if List.member ~value params
      then Some ("`" ^ value ^ "` is already declared. Use another name.")
      else None
  | _ ->
      None


(* ------------------------------------ *)
(* Omniactions *)
(* ------------------------------------ *)

let rec stripCharsFromFront (disallowed : string) (s : string) : string =
  match String.uncons s with
  | None ->
      s
  | Some (c, rest) ->
      let needle = String.fromChar c in
      if Regex.contains ~re:(Regex.regex disallowed) needle
      then stripCharsFromFront disallowed rest
      else s


let stripChars (disallowed : string) (s : string) : string =
  Regex.replace ~re:(Regex.regex disallowed) ~repl:"" s


let removeExtraSlashes (s : string) : string =
  let s = Regex.replace ~re:(Regex.regex "/+") ~repl:"/" s in
  let s =
    if s <> "/" && String.endsWith ~suffix:"/" s
    then String.dropRight ~count:1 s
    else s
  in
  s


let cleanEventName (s : string) : string =
  s |> stripChars nonEventNameSafeCharacters |> removeExtraSlashes


let cleanHTTPname (s : string) : string =
  "/" ^ s |> stripChars nonEventNameSafeCharacters |> removeExtraSlashes


let cleanDBName (s : string) : string =
  s
  |> stripChars "[^a-zA-Z0-9_]"
  |> stripCharsFromFront "[^a-zA-Z]"
  |> String.capitalize


let cleanGroupName (s : string) : string =
  s
  |> stripChars "[^a-zA-Z0-9_]"
  |> stripCharsFromFront "[^A-Z]"
  |> String.capitalize


let qNewDB (s : string) : omniAction =
  let name = cleanDBName s in
  if name = ""
  then NewDB None
  else NewDB (Some (assertValid dbNameValidator name))


let qFunction (s : string) : omniAction =
  let name =
    s
    |> stripChars "[^a-zA-Z0-9_]"
    |> stripCharsFromFront "[^a-zA-Z]"
    |> String.uncapitalize
  in
  if name = ""
  then NewFunction None
  else NewFunction (Some (assertValid fnNameValidator name))


let qWorkerHandler (s : string) : omniAction =
  let name = s |> cleanEventName |> String.uncapitalize in
  if name = ""
  then NewWorkerHandler None
  else NewWorkerHandler (Some (assertValid eventNameValidator name))


let qCronHandler (s : string) : omniAction =
  let name = s |> cleanEventName |> String.uncapitalize in
  if name = ""
  then NewCronHandler None
  else NewCronHandler (Some (assertValid eventNameValidator name))


let qReplHandler (s : string) : omniAction =
  let name = s |> cleanEventName |> String.uncapitalize in
  if name = ""
  then NewReplHandler None
  else NewReplHandler (Some (assertValid eventNameValidator name))


let qGroup (s : string) : omniAction =
  let name = cleanGroupName s in
  if name = ""
  then NewGroup None
  else NewGroup (Some (assertValid groupNameValidator name))


let qHTTPHandler (s : string) : omniAction =
  let name = cleanEventName s in
  if name = ""
  then NewHTTPHandler None
  else if String.startsWith ~prefix:"/" name
  then NewHTTPHandler (Some (assertValid httpNameValidator name))
  else NewHTTPHandler (Some (assertValid httpNameValidator ("/" ^ name)))


let handlerDisplayName (h : handler) : string =
  let space =
    h.spec.space
    |> B.toOption
    |> Option.map ~f:(fun x -> x ^ "::")
    |> Option.withDefault ~default:""
  in
  let name = h.spec.name |> B.toOption |> Option.withDefault ~default:"" in
  let modi =
    h.spec.modifier
    |> B.toOption
    |> Option.map ~f:(fun x -> if x = "_" then "" else " - " ^ x)
    |> Option.withDefault ~default:""
  in
  space ^ name ^ modi


let fnDisplayName (f : userFunction) : string =
  f.ufMetadata.ufmName
  |> B.toOption
  |> Option.withDefault ~default:"undefinedFunction"


let foundHandlerOmniAction (h : handler) : omniAction =
  let name = "Found in " ^ handlerDisplayName h in
  Goto (FocusedHandler (h.hTLID, true), h.hTLID, name, true)


let foundFnOmniAction (f : userFunction) : omniAction =
  let name = "Found in function " ^ fnDisplayName f in
  Goto (FocusedFn f.ufTLID, f.ufTLID, name, true)


let qSearch (m : model) (s : string) : omniAction list =
  if String.length s > 3
  then
    let maxResults = 20 in
    let results =
      TLIDDict.toList m.searchCache
      |> List.filterMap ~f:(fun (tlid, code) ->
             if String.contains ~substring:s code
             then
               TLIDDict.get ~tlid m.handlers
               |> Option.map ~f:foundHandlerOmniAction
               |> Option.orElse
                    ( TLIDDict.get ~tlid m.userFunctions
                    |> Option.map ~f:foundFnOmniAction )
             else None)
    in
    if List.length results > maxResults
    then List.take ~count:maxResults results
    else results
  else []


let isDynamicItem (item : autocompleteItem) : bool =
  match item with
  | ACOmniAction (Goto (_, _, _, dyna)) ->
      dyna
  | ACOmniAction _ ->
      true
  | ACEventSpace _ ->
      false (* false because we want the static items to be first *)
  | ACHTTPRoute _ ->
      false
  | ACWorkerName _ ->
      true
  | ACDBName _ ->
      true
  | _ ->
      false


let isStaticItem (item : autocompleteItem) : bool = not (isDynamicItem item)

let toDynamicItems
    (m : model)
    (space : handlerSpace option)
    (target : target option)
    (q : string) : autocompleteItem list =
  match target with
  | None ->
      (* omnicompletion *)
      let standard =
        [ qHTTPHandler q
        ; qNewDB q
        ; qFunction q
        ; qWorkerHandler q
        ; qCronHandler q
        ; qReplHandler q ]
        @ qSearch m q
      in
      (* Creating a group Spec: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.sny6o08h9gc2 *)
      let all =
        if VariantTesting.variantIsActive m GroupVariant
        then standard @ [qGroup q]
        else standard
      in
      List.map ~f:(fun o -> ACOmniAction o) all
  | Some (_, PEventName _) ->
    ( match space with
    | Some HSHTTP ->
        [ACHTTPRoute (cleanHTTPname q)]
    | Some HSCron ->
        if q = "" then [] else [ACCronName (cleanEventName q)]
    | Some HSRepl ->
        if q = "" then [] else [ACReplName (cleanEventName q)]
    | _ ->
        if q = "" then [] else [ACWorkerName (cleanEventName q)] )
  | Some (_, PDBName _) ->
      if q == "" then [] else [ACDBName (cleanDBName q)]
  | _ ->
      []


let withDynamicItems
    (m : model)
    (target : target option)
    (query : string)
    (acis : autocompleteItem list) : autocompleteItem list =
  let space =
    target
    |> Option.map ~f:Tuple2.first
    |> Option.andThen ~f:(TL.get m)
    |> Option.andThen ~f:TL.spaceOf
  in
  let new_ = toDynamicItems m space target query in
  let withoutDynamic = List.filter ~f:isStaticItem acis in
  List.uniqueBy ~f:asName (new_ @ withoutDynamic)


let tlGotoName (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      "Jump to handler: " ^ handlerDisplayName h
  | TLDB db ->
      "Jump to DB: "
      ^ (db.dbName |> B.toOption |> Option.withDefault ~default:"Unnamed DB")
  | TLGroup g ->
      "Jump to Group: "
      ^ (g.gName |> B.toOption |> Option.withDefault ~default:"Undefined")
  | TLFunc _ ->
      recover "can't goto function" ~debug:tl "<invalid state>"
  | TLTipe _ ->
      recover "can't goto tipe " ~debug:tl "<invalid state>"


let tlDestinations (m : model) : autocompleteItem list =
  let tls =
    m
    |> TL.structural
    |> TD.values
    |> List.sortBy ~f:tlGotoName
    |> List.map ~f:(fun tl ->
           Goto (TL.asPage tl true, TL.id tl, tlGotoName tl, false))
  in
  let ufs =
    m.userFunctions
    |> TD.filterMapValues ~f:(fun fn ->
           let name = "Jump to function: " ^ fnDisplayName fn in
           Some (Goto (FocusedFn fn.ufTLID, fn.ufTLID, name, false)))
  in
  List.map ~f:(fun x -> ACOmniAction x) (tls @ ufs)


(* ------------------------------------ *)
(* Create the list *)
(* ------------------------------------ *)
let generate (m : model) (a : autocomplete) : autocomplete =
  let space =
    a.target
    |> Option.map ~f:Tuple2.first
    |> Option.andThen ~f:(TL.get m)
    |> Option.andThen ~f:TL.spaceOf
  in
  let entries =
    match a.target with
    | Some (_, p) ->
      ( match P.typeOf p with
      (* autocomplete HTTP verbs if the handler is in the HTTP event space *)
      | EventModifier ->
        ( match space with
        | Some HSHTTP ->
            [ ACHTTPModifier "GET"
            ; ACHTTPModifier "POST"
            ; ACHTTPModifier "PUT"
            ; ACHTTPModifier "DELETE"
            ; ACHTTPModifier "PATCH" ]
        | Some HSCron ->
            [ ACCronTiming "Daily"
            ; ACCronTiming "Weekly"
            ; ACCronTiming "Fortnightly"
            ; ACCronTiming "Every 1hr"
            ; ACCronTiming "Every 12hrs"
            ; ACCronTiming "Every 1min" ]
        | None | Some HSRepl | Some HSDeprecatedOther | Some HSWorker ->
            [] )
      | EventName ->
        ( match space with
        | Some HSHTTP ->
            let fourOhFourList =
              m.f404s
              |> List.uniqueBy ~f:(fun f404 -> f404.path)
              |> List.sortBy ~f:(fun f404 -> f404.path)
              |> List.filterMap ~f:(fun f404 ->
                     if f404.path != "/"
                     then Some (ACHTTPRoute (cleanHTTPname f404.path))
                     else None)
            in
            fourOhFourList
        | _ ->
            [] )
      | EventSpace ->
          (* Other spaces aren't allowed anymore *)
          [ ACEventSpace "HTTP"
          ; ACEventSpace "CRON"
          ; ACEventSpace "WORKER"
          ; ACEventSpace "REPL" ]
      | DBColType ->
          let builtins =
            [ "String"
            ; "Int"
            ; "Boolean"
            ; "Float"
            ; "Password"
            ; "Date"
            ; "UUID"
            ; "Dict" ]
          in
          let compound = List.map ~f:(fun s -> "[" ^ s ^ "]") builtins in
          List.map ~f:(fun x -> ACDBColType x) (builtins @ compound)
      | ParamTipe ->
          let userTypes =
            m.userTipes
            |> TD.filterMapValues ~f:UserTypes.toTUserType
            |> List.map ~f:(fun t -> ACParamTipe t)
          in
          [ ACParamTipe TAny
          ; ACParamTipe TStr
          ; ACParamTipe TInt
          ; ACParamTipe TBool
          ; ACParamTipe TFloat
          ; ACParamTipe TDate
          ; ACParamTipe TObj
          ; ACParamTipe TBlock
          ; ACParamTipe TPassword
          ; ACParamTipe TUuid
          ; ACParamTipe TList ]
          @ userTypes
      | TypeFieldTipe ->
          [ ACTypeFieldTipe TStr
          ; ACTypeFieldTipe TInt
          ; ACTypeFieldTipe TBool
          ; ACTypeFieldTipe TFloat
          ; ACTypeFieldTipe TDate
          ; ACTypeFieldTipe TPassword
          ; ACTypeFieldTipe TUuid ]
      | _ ->
          [] )
    | _ ->
        []
  in
  let items = if a.target = None then tlDestinations m else entries in
  {a with allCompletions = items}


let filter (list : autocompleteItem list) (query : string) :
    autocompleteItem list =
  let lcq = query |> String.toLower in
  let stringify i =
    (if 1 >= String.length lcq then asName i else asString i)
    |> Regex.replace ~re:(Regex.regex {js|⟶|js}) ~repl:"->"
  in
  (* HACK: dont show Gotos when the query is "" *)
  let list =
    List.filter list ~f:(function
        | ACOmniAction (Goto _) ->
            query <> ""
        | _ ->
            true)
  in
  (* split into different lists *)
  let dynamic, candidates0 = List.partition ~f:isDynamicItem list in
  let candidates1, notSubstring =
    List.partition
      ~f:(stringify >> String.toLower >> String.contains ~substring:lcq)
      candidates0
  in
  let startsWith, candidates2 =
    List.partition ~f:(stringify >> String.startsWith ~prefix:query) candidates1
  in
  let startsWithCI, candidates3 =
    List.partition
      ~f:(stringify >> String.toLower >> String.startsWith ~prefix:lcq)
      candidates2
  in
  let substring, substringCI =
    List.partition
      ~f:(stringify >> String.contains ~substring:query)
      candidates3
  in
  let stringMatch, _notMatched =
    List.partition
      ~f:(asName >> String.toLower >> containsOrdered lcq)
      notSubstring
  in
  let allMatches =
    [dynamic; startsWith; startsWithCI; substring; substringCI; stringMatch]
    |> List.concat
  in
  allMatches


let refilter (m : model) (query : string) (old : autocomplete) : autocomplete =
  (* add or replace the literal the user is typing to the completions *)
  let fudgedCompletions =
    withDynamicItems m old.target query old.allCompletions
  in
  let newCompletions = filter fudgedCompletions query in
  let allCompletions = newCompletions in
  let newCount = List.length allCompletions in
  let index =
    (* Clear the highlight conditions *)
    if query = ""
       (* when we had previously highlighted something due to any actual match *)
       && ( (old.index <> -1 && old.value <> query)
          (* or this condition previously held and nothing has changed *)
          || old.index = -1 )
       (* if nothing matches, highlight nothing *)
       || newCount = 0
    then -1
    else 0
  in
  { old with
    index
  ; completions = newCompletions
  ; value = query
  ; prevValue = old.value }


let regenerate (m : model) (a : autocomplete) : autocomplete =
  generate m a |> refilter m a.value


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let reset (m : model) : autocomplete =
  let admin = m.isAdmin in
  { Defaults.defaultModel.complete with
    admin
  ; visible = VariantTesting.defaultAutocompleteVisible m }
  |> regenerate m


let init m = reset m

let numCompletions (a : autocomplete) : int = List.length a.completions

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
let setQuery (m : model) (q : string) (a : autocomplete) : autocomplete =
  refilter m q a


let appendQuery (m : model) (str : string) (a : autocomplete) : autocomplete =
  let q = a.value ^ str in
  setQuery m q a


let documentationForItem (aci : autocompleteItem) : string option =
  match aci with
  | ACOmniAction _ ->
      None
  | ACHTTPModifier verb ->
      Some ("Make this handler match the " ^ verb ^ " HTTP verb")
  | ACCronTiming timing ->
      Some ("Request this handler to trigger " ^ timing)
  | ACEventSpace "HTTP" ->
      Some "This handler will respond to HTTP requests"
  | ACEventSpace "CRON" ->
      Some "This handler will periodically trigger"
  | ACEventSpace "WORKER" ->
      Some "This handler will run emitted events in the background"
  | ACEventSpace "REPL" ->
      Some "This handler allows you run code in it"
  | ACEventSpace _ ->
      Some
        "This handler is deprecated. You should create a new WORKER handler, copy the code over, and change your `emit` calls to point to the new WORKER"
  | ACReplName name ->
      Some ("A REPL named " ^ name)
  | ACWorkerName name ->
      Some ("Respond to events emitted to " ^ name)
  | ACCronName _ ->
      Some "Name of your CRON job"
  | ACHTTPRoute name ->
      Some ("Handle HTTP requests made to " ^ name)
  | ACDBName name ->
      Some ("Set the DB's name to " ^ name)
  | ACDBColType tipe ->
      Some ("This field will be a " ^ tipe)
  | ACParamTipe tipe ->
      Some ("This parameter will be a " ^ RT.tipe2str tipe)
  | ACTypeFieldTipe tipe ->
      Some ("This parameter will be a " ^ RT.tipe2str tipe)
  | ACDBColName name ->
      Some ("Set the DB's column name to" ^ name)
  | ACEventModifier name ->
      Some ("Set event modifier to " ^ name)
  | ACFnName fnName ->
      Some ("Set function name to " ^ fnName)
  | ACParamName paramName ->
      Some ("Set param name to " ^ paramName)
  | ACTypeName typeName ->
      Some ("Set type name to " ^ typeName)
  | ACGroupName groupName ->
      Some ("Set group name to " ^ groupName)
  | ACTypeFieldName _ ->
      None


let setTarget (m : model) (t : target option) (a : autocomplete) : autocomplete
    =
  {a with target = t} |> regenerate m


let setVisible (visible : bool) (a : autocomplete) : autocomplete =
  {a with visible}


(* ------------------------------------ *)
(* Commands *)
(* ------------------------------------ *)

let update (m : model) (mod_ : autocompleteMod) (a : autocomplete) :
    autocomplete =
  match mod_ with
  | ACSetQuery str ->
      setQuery m str a
  | ACAppendQuery str ->
      appendQuery m str a
  | ACReset ->
      reset m
  | ACSelectDown ->
      selectDown a
  | ACSelectUp ->
      selectUp a
  | ACSetTarget target ->
      setTarget m target a
  | ACRegenerate ->
      regenerate m a
  | ACSetVisible visible ->
      setVisible visible a


(* Checks to see if autocomplete or command palette is opened
 * but not omnibox since it's not scrollable
 *)
let isOpened (ac : autocomplete) : bool = Option.isSome ac.target

let isOmnibox (ac : autocomplete) : bool = ac.target = None && ac.visible
