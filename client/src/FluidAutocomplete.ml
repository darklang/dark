open Tc
open Types
open Prelude

(* Dark *)
module P = Pointer
module RT = Runtime
module TL = Toplevel
module B = Blank
module Regex = Util.Regex
module TD = TLIDDict

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
        "match"
    | KThread ->
        "|>" )
  | FACPattern p ->
    ( match p with
    | FPAVariable (_, _, name) | FPAConstructor (_, _, name, _) ->
        name
    | FPABool (_, _, v) ->
        string_of_bool v
    | FPANull _ ->
        "null" )


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
  | FACVariable _ | FACPattern (FPAVariable _) ->
      "variable"
  | FACConstructorName (name, _) | FACPattern (FPAConstructor (_, _, name, _))
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
  | FACPattern (FPABool _) ->
      "boolean literal"
  | FACKeyword _ ->
      "keyword"
  | FACPattern (FPANull _) ->
      "null"


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


let dvalForToken (m : model) (tl : toplevel) (ti : tokenInfo) : dval option =
  let tlid = TL.id tl in
  let id =
    match ti.token with
    | TFieldName (_, fieldID, _) ->
        fieldID
    | _ ->
        FluidToken.tid ti.token
  in
  (* TODO: missing function *)
  match TL.getAST tl with
  | Some ast ->
      AST.find id ast
      |> Option.andThen ~f:(fun pd -> AST.getValueParent pd ast)
      |> Option.map ~f:P.toID
      |> Option.andThen ~f:(Analysis.getCurrentLiveValue m tlid)
      (* don't filter on incomplete values *)
      |> Option.andThen ~f:(fun dv_ ->
             if dv_ = DIncomplete then None else Some dv_ )
  | None ->
      None


let isThreadMember (tl : toplevel) (ti : tokenInfo) =
  let id = FluidToken.tid ti.token in
  TL.getAST tl
  |> Option.andThen ~f:(AST.findParentOfWithin_ id)
  |> Option.map ~f:(fun e ->
         match e with F (_, Thread _) -> true | _ -> false )
  |> Option.withDefault ~default:false


let paramTipeForTarget (a : autocomplete) (tl : toplevel) (ti : tokenInfo) :
    tipe =
  let id = FluidToken.tid ti.token in
  TL.getAST tl
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
let generateExprs m tl a ti =
  let functions = List.map ~f:(fun x -> FACFunction x) a.functions in
  let constructors =
    [ FACConstructorName ("Just", 1)
    ; FACConstructorName ("Nothing", 0)
    ; FACConstructorName ("Ok", 1)
    ; FACConstructorName ("Error", 1) ]
  in
  let varnames =
    ti.token
    |> FluidToken.tid
    |> Analysis.getCurrentAvailableVarnames m tl
    |> List.map ~f:(fun x -> FACVariable x)
  in
  let keywords =
    List.map ~f:(fun x -> FACKeyword x) [KLet; KIf; KLambda; KMatch; KThread]
  in
  let literals =
    List.map ~f:(fun x -> FACLiteral x) ["true"; "false"; "null"]
  in
  varnames @ constructors @ literals @ keywords @ functions


let generatePatterns ti a queryString =
  let alreadyHasPatterns =
    List.any
      ~f:(fun v -> match v with FACPattern _ -> true | _ -> false)
      a.allCompletions
  in
  let newStandardPatterns mid =
    (* if patterns are in the autocomplete already, don't bother creating
        * new FACPatterns with different mids and pids *)
    ( if alreadyHasPatterns
    then a.allCompletions
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
           match c with FACPattern (FPAVariable _) -> false | _ -> true )
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
  | TPatternBlank (mid, _) | TPatternVariable (mid, _, _) ->
      newQueryVariable mid @ newStandardPatterns mid
  | _ ->
      []


let generateFields dval =
  match dval with
  | Some dv when RT.typeOf dv = TObj ->
      List.map ~f:(fun x -> FACField x) (dvalFields dv)
  | _ ->
      []


let generate
    (m : model) (a : autocomplete) ((tl, ti, dval, queryString) : fullQuery) :
    autocomplete =
  let items =
    match ti.token with
    | TPatternBlank _ | TPatternVariable _ ->
        generatePatterns ti a queryString
    | TFieldName _ ->
        generateFields dval
    | _ ->
        generateExprs m tl a ti
  in
  { a with
    allCompletions = items
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
  let dbnames = TL.allDBNames m.dbs in
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
  let oldHighlightNewIndex =
    oldHighlight
    |> Option.andThen ~f:(fun oh -> List.elemIndex ~value:oh allCompletions)
  in
  let oldQueryString =
    match old.query with Some (_, ti) -> toQueryString ti | _ -> ""
  in
  let index =
    if queryString = ""
    then None
    else
      (* If an entry is highlighted, and you press another *)
      (* valid key for that entry, keep it highlighted *)
      match oldHighlightNewIndex with
      | Some newIndex ->
          Some newIndex
      | None ->
          if newCount = 0
          then (* if nothing matches, highlight nothing *)
            None
          else if oldQueryString = queryString
          then (* If we didn't change anything, don't change anything *)
            None
          else (* If an entry vanishes, highlight 0 *)
            Some 0
  in
  { old with
    index
  ; query = Some (TL.id tl, ti)
  ; completions = newCompletions
  ; invalidCompletions }


let regenerate (m : model) (a : autocomplete) ((tlid, ti) : query) :
    autocomplete =
  let tl = TL.getExn m tlid in
  let queryString = toQueryString ti in
  let dval = dvalForToken m tl ti in
  let query = (tl, ti, dval, queryString) in
  generate m a query |> refilter m query


(* ---------------------------- *)
(* Autocomplete state *)
(* ---------------------------- *)
let reset (m : model) : autocomplete =
  let userFunctionMetadata =
    m.userFunctions
    |> TLIDDict.mapValues ~f:(fun x -> x.ufMetadata)
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
  | FACKeyword KThread ->
      Some "Pipe into another expression"
  | FACPattern p ->
    ( match p with
    | FPAConstructor (_, _, name, args) ->
        documentationForItem (FACConstructorName (name, List.length args))
    | FPAVariable (_, _, name) ->
        documentationForItem (FACVariable name)
    | FPABool (_, _, var) ->
        documentationForItem (FACLiteral (string_of_bool var))
    | FPANull _ ->
        Some "A 'null' literal" )


let isOpened (ac : fluidAutocompleteState) : bool = Option.isSome ac.index
