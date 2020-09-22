open Prelude
module RT = Runtime
module TL = Toplevel
module Regex = Util.Regex

type t = fluidAutocompleteState [@@deriving show]

type item = fluidAutocompleteItem [@@deriving show]

type data = fluidAutocompleteData [@@deriving show]

type props = {functions : Types.functionsType}

type tokenInfo = fluidTokenInfo [@@deriving show]

let focusItem (i : int) : msg Tea.Cmd.t =
  Tea_task.attempt
    (fun _ -> IgnoreMsg "fluid-autocomplete-focus")
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
             ()))


(* ---------------------------- *)
(* display *)
(* ---------------------------- *)
let asName (aci : item) : string =
  match aci with
  | FACFunction fn ->
      fn.fnName
  | FACField name ->
      name
  | FACVariable (name, _) ->
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
        "match"
    | KPipe ->
        "|>" )
  | FACPattern p ->
    ( match p with
    | FPAVariable (_, _, name) | FPAConstructor (_, _, name, _) ->
        name
    | FPABool (_, _, v) ->
        string_of_bool v
    | FPANull _ ->
        "null" )
  | FACCreateFunction (name, _, _) ->
      "Create new function: " ^ name


(* Return the string types of the item's arguments and return types. If the
 * item is not a function, the return type will still be used, and might not be
 * a real type, sometimes it's a hint such as "variable". *)
let asTypeStrings (item : item) : string list * string =
  match item with
  | FACFunction f ->
      f.fnParameters
      |> List.map ~f:(fun x -> x.paramTipe)
      |> List.map ~f:RT.tipe2str
      |> fun s -> (s, RT.tipe2str f.fnReturnTipe)
  | FACField _ ->
      ([], "field")
  | FACVariable (_, odv) ->
      odv
      |> Option.map ~f:(fun dv -> dv |> RT.typeOf |> RT.tipe2str)
      |> Option.withDefault ~default:"variable"
      |> fun r -> ([], r)
  | FACPattern (FPAVariable _) ->
      ([], "variable")
  | FACConstructorName (name, _) | FACPattern (FPAConstructor (_, _, name, _))
    ->
      if name = "Just"
      then (["any"], "option")
      else if name = "Nothing"
      then ([], "option")
      else if name = "Ok" || name = "Error"
      then (["any"], "result")
      else ([], "unknown")
  | FACLiteral lit ->
      let tipe =
        lit
        |> Decoders.parseDvalLiteral
        |> Option.withDefault ~default:(DIncomplete SourceNone)
        |> RT.typeOf
        |> RT.tipe2str
      in
      ([], tipe ^ " literal")
  | FACPattern (FPABool _) ->
      ([], "boolean literal")
  | FACKeyword _ ->
      ([], "keyword")
  | FACPattern (FPANull _) ->
      ([], "null")
  | FACCreateFunction _ ->
      ([], "")


(* Used for matching, not for displaying to users *)
let asMatchingString (aci : item) : string =
  let argTypes, returnType = asTypeStrings aci in
  let typeString = String.join ~sep:", " argTypes ^ " -> " ^ returnType in
  asName aci ^ typeString


(* ---------------------------- *)
(* Utils *)
(* ---------------------------- *)

let isVariable (aci : item) : bool =
  match aci with FACVariable _ -> true | _ -> false


let isField (aci : item) : bool =
  match aci with FACField _ -> true | _ -> false


let isFnCall (aci : item) : bool =
  match aci with FACFunction _ -> true | _ -> false


let isCreateFn (aci : item) : bool =
  match aci with FACCreateFunction _ -> true | _ -> false


let item (data : data) : item = data.item

(* ---------------------------- *)
(* External: utils *)
(* ---------------------------- *)

(* Return the item that is highlighted (at a.index position in the
 * list), along with whether that it is a valid autocomplete option right now. *)
let highlightedWithValidity (a : t) : data option =
  Option.andThen a.index ~f:(fun index -> List.getAt ~index a.completions)


(* Return the item that is highlighted (at a.index position in the
 * list). *)
let highlighted (a : t) : item option =
  highlightedWithValidity a |> Option.map ~f:(fun d -> d.item)


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
(* Type checking *)
(* ------------------------------------ *)

(* Return the value being piped into the token at ti, if there is one *)
let findPipedDval (m : model) (tl : toplevel) (ti : tokenInfo) : dval option =
  let id =
    TL.getAST tl
    |> Option.andThen ~f:(AST.pipePrevious (FluidToken.tid ti.token))
    |> Option.map ~f:FluidExpression.toID
  in
  let tlid = TL.id tl in
  Analysis.getSelectedTraceID m tlid
  |> Option.andThen2 id ~f:(Analysis.getLiveValue m)
  |> Option.andThen ~f:(fun dv ->
         match dv with DIncomplete _ -> None | _ -> Some dv)


(* Return the fields of the object being referenced at ti, if there is one *)
let findFields (m : model) (tl : toplevel) (ti : tokenInfo) : string list =
  let tlid = TL.id tl in
  let id =
    match ti.token with
    | TFieldOp (_, lhsID, _)
    | TFieldName (_, lhsID, _, _)
    | TFieldPartial (_, _, lhsID, _, _) ->
        lhsID
    | _ ->
        FluidToken.tid ti.token
  in
  Analysis.getSelectedTraceID m tlid
  |> Option.andThen ~f:(Analysis.getLiveValue m id)
  |> Option.map ~f:(fun dv ->
         match dv with DObj dict -> StrDict.keys dict | _ -> [])
  |> Option.withDefault ~default:[]


let findExpectedType
    (functions : function_ list) (tl : toplevel) (ti : tokenInfo) :
    TypeInformation.t =
  let id = FluidToken.tid ti.token in
  let default = TypeInformation.default in
  TL.getAST tl
  |> Option.andThen ~f:(AST.getParamIndex id)
  |> Option.andThen ~f:(fun (name, index) ->
         functions
         |> List.find ~f:(fun f -> name = f.fnName)
         |> Option.map ~f:(fun fn ->
                let param = List.getAt ~index fn.fnParameters in
                let returnType =
                  Option.map param ~f:(fun p -> p.paramTipe)
                  |> Option.withDefault ~default:default.returnType
                in
                let paramName =
                  Option.map param ~f:(fun p -> p.paramName)
                  |> Option.withDefault ~default:default.paramName
                in
                ({fnName = fn.fnName; returnType; paramName} : TypeInformation.t)))
  |> Option.withDefault ~default


(* Checks whether an autocomplete item matches the expected types *)
let typeCheck
    (pipedType : tipe option)
    (expectedReturnType : TypeInformation.t)
    (item : item) : data =
  let valid = {item; validity = FACItemValid} in
  let invalidFirstArg tipe = {item; validity = FACItemInvalidPipedArg tipe} in
  let invalidReturnType =
    {item; validity = FACItemInvalidReturnType expectedReturnType}
  in
  let expectedReturnType = expectedReturnType.returnType in
  match item with
  | FACFunction fn ->
      if not (RT.isCompatible fn.fnReturnTipe expectedReturnType)
      then invalidReturnType
      else (
        match (List.head fn.fnParameters, pipedType) with
        | Some param, Some pipedType ->
            if RT.isCompatible param.paramTipe pipedType
            then valid
            else invalidFirstArg pipedType
        | None, Some pipedType ->
            (* if it takes no arguments, piping into it is invalid *)
            invalidFirstArg pipedType
        | _ ->
            valid )
  | FACVariable (_, dval) ->
    ( match dval with
    | Some dv ->
        if RT.isCompatible (Runtime.typeOf dv) expectedReturnType
        then valid
        else invalidReturnType
    | None ->
        valid )
  | FACConstructorName (name, _) ->
    ( match expectedReturnType with
    | TOption ->
        if name = "Just" || name = "Nothing" then valid else invalidReturnType
    | TResult ->
        if name = "Ok" || name = "Error" then valid else invalidReturnType
    | TAny ->
        valid
    | _ ->
        invalidReturnType )
  | _ ->
      valid


type query = TLID.t * tokenInfo [@@deriving show]

type fullQuery =
  { tl : toplevel
  ; ti : tokenInfo
  ; fieldList : string list
  ; pipedDval : dval option
  ; queryString : string }

let toQueryString (ti : tokenInfo) : string =
  if FluidToken.isBlank ti.token then "" else FluidToken.toText ti.token


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let init : t = Defaults.defaultModel.fluidState.ac

(* ------------------------------------ *)
(* Create the list *)
(* ------------------------------------ *)

let secretToACItem (s : SecretTypes.t) : fluidAutocompleteItem =
  let asDval = DStr (Util.obscureString s.secretValue) in
  FACVariable (s.secretName, Some asDval)


let lookupIsInQuery (tl : toplevel) ti =
  let isQueryFn name =
    [ "DB::query_v4"
    ; "DB::queryWithKey_v3"
    ; "DB::queryOne_v3"
    ; "DB::queryOne_v4"
    ; "DB::queryOneWithKey_v3"
    ; "DB::queryCount" ]
    |> List.any ~f:(fun q -> q = name)
  in
  let ast' = TL.getAST tl in
  match ast' with
  | None ->
      false
  | Some ast ->
      FluidAST.ancestors (FluidToken.tid ti.token) ast
      |> List.find ~f:(function
             | FluidExpression.EFnCall (_, name, _, _) ->
                 isQueryFn name
             | _ ->
                 false)
      |> Option.is_some


let filterToDbSupportedFns isInQuery functions =
  if not isInQuery
  then functions
  else
    functions
    |> List.filter ~f:(fun f ->
           match f with FACFunction fn -> fn.fnIsSupportedInQuery | _ -> false)


let generateExprs (m : model) (props : props) (tl : toplevel) ti =
  let isInQuery = lookupIsInQuery tl ti in
  let functions' =
    Functions.asFunctions props.functions
    |> List.map ~f:(fun x -> FACFunction x)
  in
  let functions = filterToDbSupportedFns isInQuery functions' in
  let constructors =
    if not isInQuery
    then
      [ FACConstructorName ("Just", 1)
      ; FACConstructorName ("Nothing", 0)
      ; FACConstructorName ("Ok", 1)
      ; FACConstructorName ("Error", 1) ]
    else []
  in
  let id = FluidToken.tid ti.token in
  let varnames =
    Analysis.getSelectedTraceID m (TL.id tl)
    |> Option.map ~f:(Analysis.getAvailableVarnames m tl id)
    |> Option.withDefault ~default:[]
    |> List.map ~f:(fun (varname, dv) -> FACVariable (varname, dv))
  in
  let keywords =
    if not isInQuery
    then List.map ~f:(fun x -> FACKeyword x) [KLet; KIf; KLambda; KMatch; KPipe]
    else List.map ~f:(fun x -> FACKeyword x) [KLet; KPipe]
  in
  let literals =
    List.map ~f:(fun x -> FACLiteral x) ["true"; "false"; "null"]
  in
  let secrets = List.map m.secrets ~f:secretToACItem in
  varnames @ constructors @ literals @ keywords @ functions @ secrets


let generatePatterns ti a queryString : item list =
  let alreadyHasPatterns =
    List.any
      ~f:(fun v -> match v with {item = FACPattern _; _} -> true | _ -> false)
      a.completions
  in
  let newStandardPatterns mid =
    (* if patterns are in the autocomplete already, don't bother creating
        * new FACPatterns with different mids and pids *)
    ( if alreadyHasPatterns
    then a.completions |> List.map ~f:(fun {item; _} -> item)
    else
      [ FPABool (mid, gid (), true)
      ; FPABool (mid, gid (), false)
      ; FPAConstructor (mid, gid (), "Just", [FPBlank (mid, gid ())])
      ; FPAConstructor (mid, gid (), "Nothing", [])
      ; FPAConstructor (mid, gid (), "Ok", [FPBlank (mid, gid ())])
      ; FPAConstructor (mid, gid (), "Error", [FPBlank (mid, gid ())])
      ; FPANull (mid, gid ()) ]
      |> List.map ~f:(fun p -> FACPattern p) )
    |> List.filter ~f:(fun c ->
           (* filter out old query string variable *)
           match c with FACPattern (FPAVariable _) -> false | _ -> true)
  in
  let isInvalidPatternVar str =
    [""; "Just"; "Nothing"; "Ok"; "Error"; "true"; "false"; "null"]
    |> List.member ~value:str
    || str
       |> String.dropRight ~count:(String.length str - 1)
       |> String.isCapitalized
  in
  let newQueryVariable mid =
    (* no Query variable if the query is empty or equals to standard
     * constructor or boolean name *)
    if isInvalidPatternVar queryString
    then []
    else [FACPattern (FPAVariable (mid, gid (), queryString))]
  in
  match ti.token with
  | TPatternBlank (mid, _, _) | TPatternVariable (mid, _, _, _) ->
      newQueryVariable mid @ newStandardPatterns mid
  | _ ->
      []


let generateCommands _name _tlid _id =
  (* Disable for now, this is really annoying *)
  (* [FACCreateFunction (name, tlid, id)] *)
  []


let generateFields fieldList = List.map ~f:(fun x -> FACField x) fieldList

let generate (m : model) (props : props) (a : t) (query : fullQuery) : item list
    =
  let tlid = TL.id query.tl in
  match query.ti.token with
  | TPatternBlank _ | TPatternVariable _ ->
      generatePatterns query.ti a query.queryString
  | TFieldName _ | TFieldPartial _ ->
      generateFields query.fieldList
  | TLeftPartial _ ->
      (* Left partials can ONLY be if/let/match for now *)
      [FACKeyword KLet; FACKeyword KIf; FACKeyword KMatch]
  | TPartial (id, name, _) ->
      generateExprs m props query.tl query.ti @ generateCommands name tlid id
  | _ ->
      generateExprs m props query.tl query.ti


let filter
    (functions : function_ list) (candidates0 : item list) (query : fullQuery) :
    data list =
  let stripColons = Regex.replace ~re:(Regex.regex "::") ~repl:"" in
  let lcq = query.queryString |> String.toLower |> stripColons in
  let stringify i =
    (if 1 >= String.length lcq then asName i else asMatchingString i)
    |> Regex.replace ~re:(Regex.regex {js|⟶|js}) ~repl:"->"
    |> stripColons
  in
  (* split into different lists *)
  let candidates1, notSubstring =
    List.partition
      ~f:(stringify >> String.toLower >> String.contains ~substring:lcq)
      candidates0
  in
  let startsWith, candidates2 =
    List.partition
      ~f:(stringify >> String.startsWith ~prefix:query.queryString)
      candidates1
  in
  let startsWithCI, candidates3 =
    List.partition
      ~f:(stringify >> String.toLower >> String.startsWith ~prefix:lcq)
      candidates2
  in
  let substring, substringCI =
    List.partition
      ~f:(stringify >> String.contains ~substring:query.queryString)
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
  let pipedType = Option.map ~f:RT.typeOf query.pipedDval in
  let expectedTypeInfo = findExpectedType functions query.tl query.ti in
  List.map allMatches ~f:(typeCheck pipedType expectedTypeInfo)


let refilter (props : props) (query : fullQuery) (old : t) (items : item list) :
    t =
  (* add or replace the literal the user is typing to the completions *)
  let newCompletions =
    filter (Functions.asFunctions props.functions) items query
  in
  let oldHighlight = highlighted old in
  let newCount = List.length newCompletions in
  let oldHighlightNewIndex =
    oldHighlight
    |> Option.andThen ~f:(fun oh ->
           List.elemIndex
             ~value:oh
             (List.map ~f:(fun {item; _} -> item) newCompletions))
  in
  let oldQueryString =
    match old.query with Some (_, ti) -> toQueryString ti | _ -> ""
  in
  let isFieldPartial =
    match query.ti.token with TFieldPartial _ -> true | _ -> false
  in
  let index =
    if isFieldPartial
    then
      if query.queryString = "" && query.queryString <> oldQueryString
      then
        (* Show autocomplete - the first item - when there's no text. If we
         * just deleted the text, reset to the top. But only reset on change
         * - we want the arrow keys to work *)
        Some 0
      else if oldQueryString = "" && old.index = Some 0
      then
        (* If we didn't actually select the old value, don't cling to it. *)
        Some 0
      else if Option.isSome oldHighlightNewIndex
      then
        (* Otherwise we did select something, so let's find it. *)
        oldHighlightNewIndex
      else (* Always show fields. *)
        Some 0
    else if query.queryString = "" || newCount = 0
    then (* Do nothing if no queryString or autocomplete list *)
      None
    else if oldQueryString = query.queryString
    then
      (* If we didn't change anything, don't change anything *)
      match oldHighlightNewIndex with
      | Some newIndex ->
          Some newIndex
      | None ->
          None
    else (* If an entry vanishes, highlight 0 *)
      Some 0
  in
  {index; query = Some (TL.id query.tl, query.ti); completions = newCompletions}


(* Regenerate calls generate, except that it adapts the result using the
 * existing state (mostly putting the index in the right place. *)
let regenerate (m : model) (a : t) ((tlid, ti) : query) : t =
  match TL.get m tlid with
  | None ->
      init
  | Some tl ->
      let props = {functions = m.functions} in
      let queryString = toQueryString ti in
      let fieldList = findFields m tl ti in
      let pipedDval = findPipedDval m tl ti in
      let query = {tl; ti; fieldList; pipedDval; queryString} in
      let items = generate m props a query in
      refilter props query a items


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)

let numCompletions (a : t) : int = List.length a.completions

let selectDown (a : t) : t =
  match a.index with
  | Some index ->
      let max_ = numCompletions a in
      let max = max max_ 1 in
      let new_ = (index + 1) mod max in
      {a with index = Some new_}
  | None ->
      a


let selectUp (a : t) : t =
  match a.index with
  | Some index ->
      let max = numCompletions a - 1 in
      {a with index = Some (if index <= 0 then max else index - 1)}
  | None ->
      a


let isOpened (ac : fluidAutocompleteState) : bool = Option.isSome ac.index

let typeErrorDoc ({item; validity} : data) : msg Vdom.t =
  let _types = asTypeStrings item in
  let _validity = validity in
  match validity with
  | FACItemValid ->
      Vdom.noNode
  | FACItemInvalidPipedArg tipe ->
      let acFunction = asName item in
      let acFirstArgType = asTypeStrings item |> Tuple2.first |> List.head in
      let typeInfo =
        match acFirstArgType with
        | None ->
            [Html.text " takes no arguments."]
        | Some tipeStr ->
            [ Html.text " takes a "
            ; Html.span [Html.class' "type"] [Html.text tipeStr]
            ; Html.text " as its first argument." ]
      in
      Html.div
        []
        ( [ Html.span [Html.class' "err"] [Html.text "Type error: "]
          ; Html.text "A value of type "
          ; Html.span [Html.class' "type"] [Html.text (RT.tipe2str tipe)]
          ; Html.text " is being piped into this function call, but "
          ; Html.span [Html.class' "fn"] [Html.text acFunction] ]
        @ typeInfo )
  | FACItemInvalidReturnType {fnName; paramName; returnType} ->
      let acFunction = asName item in
      let acReturnType = asTypeStrings item |> Tuple2.second in
      Html.div
        []
        [ Html.span [Html.class' "err"] [Html.text "Type error: "]
        ; Html.span [Html.class' "fn"] [Html.text fnName]
        ; Html.text " expects "
        ; Html.span [Html.class' "param"] [Html.text paramName]
        ; Html.text " to be a "
        ; Html.span [Html.class' "type"] [Html.text (RT.tipe2str returnType)]
        ; Html.text ", but "
        ; Html.span [Html.class' "fn"] [Html.text acFunction]
        ; Html.text " returns a "
        ; Html.span [Html.class' "type"] [Html.text acReturnType] ]


let rec documentationForItem ({item; validity} : data) : 'a Vdom.t list option =
  let p (text : string) = Html.p [] [Html.text text] in
  let typeDoc = typeErrorDoc {item; validity} in
  let simpleDoc (text : string) = Some [p text; typeDoc] in
  let deprecated = Html.span [Html.class' "err"] [Html.text "DEPRECATED: "] in
  match item with
  | FACFunction f ->
      let desc =
        if String.length f.fnDescription <> 0
        then f.fnDescription
        else "Function call with no description"
      in
      let desc = PrettyDocs.convert desc in
      let desc = if f.fnDeprecated then deprecated :: desc else desc in
      Some (desc @ [ViewErrorRailDoc.hintForFunction f None; typeDoc])
  | FACConstructorName ("Just", _) ->
      simpleDoc "An Option containing a value"
  | FACConstructorName ("Nothing", _) ->
      simpleDoc "An Option representing Nothing"
  | FACConstructorName ("Ok", _) ->
      simpleDoc "A successful Result containing a value"
  | FACConstructorName ("Error", _) ->
      simpleDoc "A Result representing a failure"
  | FACConstructorName (name, _) ->
      simpleDoc ("TODO: this should never occur: the constructor " ^ name)
  | FACField fieldname ->
      simpleDoc ("The '" ^ fieldname ^ "' field of the object")
  | FACVariable (var, _) ->
      if String.isCapitalized var
      then simpleDoc ("The datastore '" ^ var ^ "'")
      else simpleDoc ("The variable '" ^ var ^ "'")
  | FACLiteral lit ->
      simpleDoc ("The literal value '" ^ lit ^ "'")
  | FACKeyword KLet ->
      simpleDoc
        "A `let` expression allows you assign a variable to an expression"
  | FACKeyword KIf ->
      simpleDoc "An `if` expression allows you to branch on a boolean condition"
  | FACKeyword KLambda ->
      simpleDoc
        "A `lambda` creates an anonymous function. This is most often used for iterating through lists"
  | FACKeyword KMatch ->
      simpleDoc
        "A `match` expression allows you to pattern match on a value, and return different expressions based on many possible conditions"
  | FACKeyword KPipe ->
      simpleDoc "Pipe into another expression"
  | FACPattern pat ->
    ( match pat with
    | FPAConstructor (_, _, name, args) ->
        documentationForItem
          {item = FACConstructorName (name, List.length args); validity}
    | FPAVariable (_, _, name) ->
        documentationForItem {item = FACVariable (name, None); validity}
    | FPABool (_, _, var) ->
        documentationForItem {item = FACLiteral (string_of_bool var); validity}
    | FPANull _ ->
        simpleDoc "A 'null' literal" )
  | FACCreateFunction _ ->
      None


let updateAutocompleteVisibility (m : model) : model =
  let oldTlid =
    match m.fluidState.ac.query with
    | Some (tlid, _) ->
        Some tlid
    | None ->
        CursorState.tlidOf m.cursorState
  in
  let newTlid = CursorState.tlidOf m.cursorState in
  if isOpened m.fluidState.ac && oldTlid <> newTlid
  then
    let newAc = init in
    {m with fluidState = {m.fluidState with ac = newAc}}
  else m
