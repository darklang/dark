open Tc
open Types

(* Dark *)
module P = Pointer
module RT = Runtime
module TL = Toplevel
module B = Blank
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
  | ACCommand command ->
      ":" ^ command.commandName
  | ACLiteral lit ->
      lit
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
    | Goto (_, _, desc) ->
        desc )
  | ACConstructorName name ->
      name
  | ACKeyword k ->
    ( match k with
    | KLet ->
        "let"
    | KIf ->
        "if"
    | KLambda ->
        "lambda"
    | KMatch ->
        "match"
    | KThread ->
        "|>" )
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
  | ACExpr name
  | ACDBColName name
  | ACVarBind name
  | ACEventModifier name
  | ACKey name
  | ACFFMsg name
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
  | ACFunction f ->
      f.fnParameters
      |> List.map ~f:(fun x -> x.paramTipe)
      |> List.map ~f:RT.tipe2str
      |> String.join ~sep:", "
      |> fun s -> "(" ^ s ^ ") ->  " ^ RT.tipe2str f.fnReturnTipe
  | ACField _ ->
      "field"
  | ACVariable _ ->
      "variable"
  | ACCommand _ ->
      ""
  | ACConstructorName name ->
      if name = "Just"
      then "(any) -> option"
      else if name = "Nothing"
      then "option"
      else if name = "Ok" || name = "Error"
      then "(any) -> result"
      else ""
  | ACLiteral lit ->
      let tipe =
        lit
        |> Decoders.parseDvalLiteral
        |> Option.withDefault ~default:DIncomplete
        |> RT.typeOf
        |> RT.tipe2str
      in
      tipe ^ " literal"
  | ACOmniAction _ ->
      ""
  | ACKeyword _ ->
      "keyword"
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
  | ACExpr _ ->
      "expression"
  | ACDBColName _ ->
      "column name"
  | ACVarBind _ ->
      "var"
  | ACEventModifier _ ->
      "event modifier"
  | ACKey _ ->
      "key"
  | ACFFMsg _ ->
      "feature flag message"
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


let asString (aci : autocompleteItem) : string = asName aci ^ asTypeString aci

(* ---------------------------- *)
(* External: utils *)
(* ---------------------------- *)
let findFunction (a : autocomplete) (name : string) : function_ option =
  List.find ~f:(fun f -> f.fnName = name) a.functions


let isStringEntry (a : autocomplete) : bool =
  String.startsWith ~prefix:"\"" a.value


let isLargeStringEntry (a : autocomplete) : bool =
  isStringEntry a && String.contains ~substring:"\n" a.value


let isSmallStringEntry (a : autocomplete) : bool =
  isStringEntry a && not (isLargeStringEntry a)


(* Return different type if possible *)
let highlighted (a : autocomplete) : autocompleteItem option =
  List.getAt ~index:a.index (a.completions @ a.invalidCompletions)


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


let dvalForTarget (m : model) ((tlid, pd) : target) : dval option =
  TL.get m tlid
  |> Option.andThen ~f:TL.getAST
  |> Option.andThen ~f:(AST.getValueParent pd)
  |> Option.map ~f:P.toID
  |> Option.andThen ~f:(Analysis.getCurrentLiveValue m tlid)
  (* don't filter on incomplete values *)
  |> Option.andThen ~f:(fun dv_ -> if dv_ = DIncomplete then None else Some dv_)


let isThreadMember (m : model) ((tlid, pd) : target) =
  TL.get m tlid
  |> Option.andThen ~f:TL.getAST
  |> Option.andThen ~f:(AST.findParentOfWithin_ (P.toID pd))
  |> Option.map ~f:(fun e ->
         match e with F (_, Thread _) -> true | _ -> false )
  |> Option.withDefault ~default:false


let paramTipeForTarget (m : model) ((tlid, pd) : target) : tipe option =
  TL.get m tlid
  |> Option.andThen ~f:TL.getAST
  |> Option.andThen ~f:(fun ast -> AST.getParamIndex ast (P.toID pd))
  |> Option.andThen ~f:(fun (name, index) ->
         m.complete.functions
         |> List.find ~f:(fun f -> name = f.fnName)
         |> Option.map ~f:(fun x -> x.fnParameters)
         |> Option.andThen ~f:(List.getAt ~index)
         |> Option.map ~f:(fun x -> x.paramTipe) )


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
    then Some (ACLiteral (Runtime.convertDisplayStringToLiteral s))
    else None
  else if Decoders.isLiteralRepr s
  then Some (ACLiteral s)
  else if String.length s > 0
  then
    if String.startsWith ~prefix:(String.toLower s) "false"
    then Some (ACLiteral "false")
    else if String.startsWith ~prefix:(String.toLower s) "true"
    then Some (ACLiteral "true")
    else if String.startsWith ~prefix:(String.toLower s) "null"
    then Some (ACLiteral "null")
    else None
  else None


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

let varnamePatternValidator = varnameValidator

let constructorPatternValidator = "[A-Z_][a-zA-Z0-9_]*"

let constructorNameValidator = "Just|Nothing|Ok|Error"

let dbColTypeValidator = "\\[?[A-Z]\\w+\\]?"

let dbColNameValidator = "\\w+"

let dbNameValidator = "[A-Z][a-zA-Z0-9_]*"

let groupNameValidator = "[A-Z][a-zA-Z0-9_]*"

let eventModifierValidator = "[a-zA-Z_][\\sa-zA-Z0-9_]*"

let httpVerbValidator = "[A-Z]+"

let eventSpaceValidator = "(CRON|HTTP|REPL|WORKER)"

let fieldNameValidator = ".+"

let keynameValidator = ".+"

let fnNameValidator = "[a-z][a-zA-Z0-9_]*"

(* NB: disallowing inital-capitals also defends against having a collision
 * between a function param name and a db name *)
let paramNameValidator = "[a-z][a-zA-Z0-9_]*"

let typeNameValidator = dbNameValidator

let paramTypeValidator = "[A-Za-z0-9_]*"

let assertValid pattern value : string =
  if Regex.exactly ~re:pattern value
  then value
  else Debug.crash ("Failed validator: " ^ pattern ^ ", " ^ value)


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


let isDynamicItem (item : autocompleteItem) : bool =
  match item with
  | ACLiteral _ ->
      true
  | ACOmniAction (Goto _) ->
      false
  | ACOmniAction _ ->
      true
  | ACEventSpace _ ->
      false (* false because we want the static items to be first *)
  | ACHTTPRoute _ ->
      true
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
      in
      (* Creating a group Spec: https://docs.google.com/document/d/19dcGeRZ4c7PW9hYNTJ9A7GsXkS2wggH2h2ABqUw7R6A/edit#heading=h.sny6o08h9gc2 *)
      let all =
        if VariantTesting.variantIsActive m GroupVariant
        then standard @ [qGroup q]
        else standard
      in
      List.map ~f:(fun o -> ACOmniAction o) all
  | Some (_, PExpr _) ->
      Option.values [qLiteral q]
  | Some (_, PField _) ->
      [ACField q]
  | Some (_, PEventName _) ->
    ( match space with
    | Some HSHTTP ->
        [ACHTTPRoute (cleanHTTPname q)]
    | Some HSCron ->
        [ACCronName (cleanEventName q)]
    | Some HSRepl ->
        [ACReplName (cleanEventName q)]
    | _ ->
        [ACWorkerName (cleanEventName q)] )
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
  withoutDynamic @ new_


let fnGotoName (name : string) : string = "Just to function: " ^ name

let tlGotoName (tl : toplevel) : string =
  match tl with
  | TLHandler h ->
      "Jump to handler: "
      ^ (h.spec.space |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ "::"
      ^ (h.spec.name |> B.toMaybe |> Option.withDefault ~default:"Undefined")
      ^ " - "
      ^ ( h.spec.modifier
        |> B.toMaybe
        |> Option.withDefault ~default:"Undefined" )
  | TLDB db ->
      "Jump to DB: "
      ^ (db.dbName |> B.toMaybe |> Option.withDefault ~default:"Unnamed DB")
  | TLGroup g ->
      "Jump to Group: "
      ^ (g.gName |> B.toMaybe |> Option.withDefault ~default:"Undefined")
  | TLFunc _ ->
      Debug.crash "cannot happen"
  | TLTipe _ ->
      Debug.crash "cannot happen"


let tlDestinations (m : model) : autocompleteItem list =
  let tls =
    m
    |> TL.structural
    |> TD.values
    |> List.sortBy ~f:tlGotoName
    |> List.map ~f:(fun tl -> Goto (TL.asPage tl true, TL.id tl, tlGotoName tl))
  in
  let ufs =
    m.userFunctions
    |> TD.filterMapValues ~f:(fun fn ->
           match fn.ufMetadata.ufmName with
           | Blank _ ->
               None
           | F (_, name) ->
               Some (Goto (FocusedFn fn.ufTLID, fn.ufTLID, fnGotoName name)) )
  in
  List.map ~f:(fun x -> ACOmniAction x) (tls @ ufs)


let matcher
    (tipeConstraintOnTarget : tipe)
    (dbnames : string list)
    (matchTypesOfFn : tipe -> function_ -> bool)
    (item : autocompleteItem) =
  match item with
  | ACFunction fn ->
      matchTypesOfFn tipeConstraintOnTarget fn
  | ACVariable var ->
      if List.member ~value:var dbnames
      then match tipeConstraintOnTarget with TDB -> true | _ -> false
      else true
  | ACConstructorName name ->
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
  let varnames =
    a.target
    |> Option.andThen ~f:(fun (tlid, pd) ->
           TL.get m tlid |> Option.map ~f:(fun tl -> (tl, Pointer.toID pd)) )
    |> Option.map ~f:(fun (tl, id) ->
           Analysis.getCurrentAvailableVarnames m tl id )
    |> Option.withDefault ~default:[]
  in
  let dval = Option.andThen ~f:(dvalForTarget m) a.target in
  let fields =
    match dval with
    | Some dv when RT.typeOf dv = TObj ->
      ( match a.target with
      | Some (_, pd) when P.typeOf pd = Field ->
          List.map ~f:(fun x -> ACField x) (dvalFields dv)
      | _ ->
          [] )
    | _ ->
        []
  in
  let isExpression =
    match a.target with Some (_, p) -> P.typeOf p = Expr | None -> false
  in
  (* functions *)
  let funcList = if isExpression then a.functions else [] in
  let functions = List.map ~f:(fun x -> ACFunction x) funcList in
  let constructors =
    [ ACConstructorName "Just"
    ; ACConstructorName "Nothing"
    ; ACConstructorName "Ok"
    ; ACConstructorName "Error" ]
  in
  let extras =
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
      | Pattern ->
        ( match dval with
        | Some dv when RT.typeOf dv = TResult ->
            [ACConstructorName "Ok"; ACConstructorName "Error"]
        | Some dv when RT.typeOf dv = TOption ->
            [ACConstructorName "Just"; ACConstructorName "Nothing"]
        | _ ->
            constructors )
      | _ ->
          [] )
    | _ ->
        []
  in
  let exprs =
    if isExpression
    then
      let varnames = List.map ~f:(fun x -> ACVariable x) varnames in
      let keywords =
        List.map ~f:(fun x -> ACKeyword x) [KLet; KIf; KLambda; KMatch]
      in
      varnames @ constructors @ keywords @ functions
    else []
  in
  let items =
    if a.isCommandMode
    then List.map ~f:(fun x -> ACCommand x) Commands.commands
    else if a.target = None
    then tlDestinations m
    else extras @ exprs @ fields
  in
  {a with allCompletions = items; targetDval = dval}


let filter
    (m : model)
    (a : autocomplete)
    (list : autocompleteItem list)
    (query : string) : autocompleteItem list * autocompleteItem list =
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
            true )
  in
  (* split into different lists *)
  let dynamic, candidates0 = List.partition ~f:isDynamicItem list in
  let candidates1, notSubstring =
    List.partition
      ~f:(stringify >> String.toLower >> String.contains ~substring:lcq)
      candidates0
  in
  let startsWith, candidates2 =
    List.partition
      ~f:(stringify >> String.startsWith ~prefix:query)
      candidates1
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
  (* Now split list by type validity *)
  let dbnames = TL.allDBNames m.dbs in
  let isThreadMemberVal =
    Option.map ~f:(isThreadMember m) a.target
    |> Option.withDefault ~default:false
  in
  let tipeConstraintOnTarget =
    a.target
    |> Option.andThen ~f:(paramTipeForTarget m)
    |> Option.withDefault ~default:TAny
  in
  let matchTypesOfFn pt = matchesTypes isThreadMemberVal pt a.targetDval in
  List.partition
    ~f:(matcher tipeConstraintOnTarget dbnames matchTypesOfFn)
    allMatches


let refilter (m : model) (query : string) (old : autocomplete) : autocomplete =
  (* add or replace the literal the user is typing to the completions *)
  let fudgedCompletions =
    if old.isCommandMode
    then List.filter ~f:isStaticItem old.allCompletions
    else withDynamicItems m old.target query old.allCompletions
  in
  let newCompletions, invalidCompletions =
    filter m old fudgedCompletions query
  in
  let oldHighlight = highlighted old in
  let allCompletions = newCompletions @ invalidCompletions in
  let newCount = List.length allCompletions in
  let oldHighlightNewPos =
    oldHighlight
    |> Option.andThen ~f:(fun oh -> List.elemIndex ~value:oh allCompletions)
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
    index
  ; completions = newCompletions
  ; invalidCompletions
  ; value = query
  ; prevValue = old.value }


let regenerate (m : model) (a : autocomplete) : autocomplete =
  generate m a |> refilter m a.value


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let reset (m : model) : autocomplete =
  let userFunctionMetadata =
    m.userFunctions
    |> TD.mapValues ~f:(fun x -> x.ufMetadata)
    |> List.filterMap ~f:Functions.ufmToF
  in
  let functions =
    m.builtInFunctions
    |> List.filter ~f:(fun f ->
           (not f.fnDeprecated) || Refactor.usedFn m f.fnName )
  in
  let admin = m.isAdmin in
  let functions = functions @ userFunctionMetadata in
  { Defaults.defaultModel.complete with
    admin; functions; visible = VariantTesting.defaultAutocompleteVisible m }
  |> regenerate m


let init m = reset m

let numCompletions (a : autocomplete) : int =
  List.length a.completions + List.length a.invalidCompletions


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
  let q =
    if isStringEntry a
    then String.dropRight ~count:1 a.value ^ str ^ "\""
    else a.value ^ str
  in
  setQuery m q a


let documentationForItem (aci : autocompleteItem) : string option =
  match aci with
  | ACFunction f ->
      let desc =
        if String.length f.fnDescription <> 0
        then f.fnDescription
        else "Function call with no description"
      in
      let desc = if f.fnDeprecated then "DEPRECATED: " ^ desc else desc in
      Some desc
  | ACCommand c ->
      Some (c.doc ^ " (" ^ c.shortcut ^ ")")
  | ACConstructorName "Just" ->
      Some "An Option containing a value"
  | ACConstructorName "Nothing" ->
      Some "An Option representing Nothing"
  | ACConstructorName "Ok" ->
      Some "A successful Result containing a value"
  | ACConstructorName "Error" ->
      Some "A Result representing a failure"
  | ACConstructorName name ->
      Some ("TODO: this should never occur: the constructor " ^ name)
  | ACField fieldname ->
      Some ("The '" ^ fieldname ^ "' field of the object")
  | ACVariable var ->
      if String.isCapitalized var
      then Some ("The datastore '" ^ var ^ "'")
      else Some ("The variable '" ^ var ^ "'")
  | ACLiteral lit ->
      Some ("The literal value '" ^ lit ^ "'")
  | ACKeyword KLet ->
      Some "A `let` expression allows you assign a variable to an expression"
  | ACKeyword KIf ->
      Some "An `if` expression allows you to branch on a boolean condition"
  | ACKeyword KLambda ->
      Some
        "A `lambda` creates an anonymous function. This is most often used for iterating through lists"
  | ACKeyword KMatch ->
      Some
        "A `match` expression allows you to pattern match on a value, and return different expressions based on many possible conditions"
  | ACKeyword KThread ->
      Some
        "The `|>` (pipe) expression takes the result of this expression, and puts it as the first argument to the next expression. Think of it as method chaining (a.b().c())"
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
      Some "This handler will run emited events in the background"
  | ACEventSpace "REPL" ->
      Some "This handler allows you run code in it"
  | ACEventSpace _ ->
      Some
        "This handler is deprecated. You should create a new WORKER handler, copy the code over, and change your `emit` calls to point to the new WORKER"
  | ACExpr _ ->
      Some "An expression"
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
  | ACVarBind str ->
      Some ("Set variable name to " ^ str)
  | ACEventModifier name ->
      Some ("Set event modifier to " ^ name)
  | ACKey key ->
      Some ("Set key to " ^ key)
  | ACFFMsg msg ->
      Some ("Set feature flag message to " ^ msg)
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
let enableCommandMode (a : autocomplete) : autocomplete =
  {a with isCommandMode = true}


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
  | ACEnableCommandMode ->
      enableCommandMode a
  | ACSetVisible visible ->
      setVisible visible a


(* Checks to see if autocomplete or command palette is opened
 * but not omnibox since it's not scrollable
*)
let isOpened (ac : autocomplete) : bool =
  Option.isSome ac.target || ac.isCommandMode
