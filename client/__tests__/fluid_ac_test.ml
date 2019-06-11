open Tc
open Types
open Prelude
open Jest
open Expect
open Fluid
module AC = FluidAutocomplete
module B = Blank
module K = FluidKeyboard

let sampleFunctions : function_ list =
  [ ("Twit::somefunc", TObj)
  ; ("Twit::someOtherFunc", TObj)
  ; ("Twit::yetAnother", TObj)
  ; ("+", TInt)
  ; ("Int::add", TInt)
  ; ("Dict::keys", TObj)
  ; ("List::head", TList)
  ; ("withlower", TObj)
  ; ("withLower", TObj)
  ; ("SomeModule::withLower", TObj)
  ; ("SomeOtherModule::withlower", TObj)
  ; ("HTTP::post", TAny)
  ; ("HTTP::head", TAny)
  ; ("HTTP::get", TAny)
  ; ("HTTP::options", TAny)
  ; ("Some::deprecated", TAny)
  ; ("DB::deleteAll", TDB)
  ; ("Option::withDefault", TOption)
  ; ("Result::catchError", TResult) ]
  |> List.map ~f:(fun (fnName, paramTipe) ->
         { fnName
         ; fnParameters =
             [ { paramName = "x"
               ; paramTipe
               ; paramBlock_args = []
               ; paramOptional = false
               ; paramDescription = "" } ]
         ; fnReturnTipe = TBool
         ; fnPreviewExecutionSafe = false
         ; fnDescription = ""
         ; fnInfix = true
         ; fnDeprecated = fnName = "Some::deprecated" } )


let defaultTLID = gtlid ()

let defaultID = gid ()

let defaultID2 = gid ()

let defaultExpr = EBlank defaultID

let aMatchExpr ?(mID = defaultID) ?(patID = gid ()) () =
  EMatch
    ( mID
    , EVariable (gid (), "request")
    , [(FPBlank (mID, patID), EBlank (gid ()))] )


let defaultToplevel =
  { id = defaultTLID
  ; pos = Defaults.origin
  ; data =
      TLHandler
        { ast = toExpr defaultExpr
        ; spec =
            { module_ = Blank (gid ())
            ; name = Blank (gid ())
            ; modifier = Blank (gid ()) }
        ; tlid = defaultTLID } }


let defaultDval = DNull

let defaultTokenInfo =
  { startRow = 0
  ; startCol = 0
  ; startPos = 0
  ; endPos = 0
  ; length = 0
  ; token = TBlank defaultID }


let defaultFullQuery ?(tl = defaultToplevel) (m : model) (query : string) :
    AC.fullQuery =
  let ti =
    match tl.data with
    | TLHandler {ast; _} | TLFunc {ufAST = ast; _} ->
        ast
        |> fromExpr m.fluidState
        |> toTokens m.fluidState
        |> List.head
        |> Option.withDefault ~default:defaultTokenInfo
    | _ ->
        defaultTokenInfo
  in
  let _, ti =
    m.fluidState.ac.query |> Option.withDefault ~default:(tl.id, ti)
  in
  (tl, ti, None, query)


let fillingCS ?(tlid = defaultTLID) ?(_id = defaultID) () : cursorState =
  FluidEntering tlid


let creatingCS : cursorState = FluidEntering defaultTLID

(* Sets the model with the appropriate toplevels *)
let defaultModel
    ?(dbs = [])
    ?(handlers = [])
    ?(userFunctions = [])
    ?(userTipes = [])
    ~cursorState
    () : model =
  let default = Defaults.defaultModel in
  { default with
    toplevels = dbs @ handlers
  ; userFunctions
  ; userTipes
  ; cursorState
  ; builtInFunctions = sampleFunctions }


let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultExpr)
    ?(module_ : string option = None)
    () : toplevel =
  let module_ =
    match module_ with None -> B.new_ () | Some name -> B.newF name
  in
  let spec = {module_; name = B.new_ (); modifier = B.new_ ()} in
  { id = tlid
  ; pos = {x = 0; y = 0}
  ; data = TLHandler {ast = toExpr expr; spec; tlid} }


let aFunction ?(tlid = defaultTLID) ?(expr = defaultExpr) () : userFunction =
  { ufTLID = tlid
  ; ufMetadata =
      { ufmName = B.newF "myFunc"
      ; ufmParameters = []
      ; ufmDescription = ""
      ; ufmReturnTipe = B.newF TStr
      ; ufmInfix = false }
  ; ufAST = toExpr expr }


let aDB ?(tlid = defaultTLID) ?(fieldid = defaultID) ?(typeid = defaultID2) ()
    : toplevel =
  { id = tlid
  ; pos = {x = 0; y = 0}
  ; data =
      TLDB
        { dbTLID = tlid
        ; dbName = B.newF "MyDB"
        ; cols = [(Blank fieldid, Blank typeid)]
        ; version = 0
        ; oldMigrations = []
        ; activeMigration = None } }


let enteringFunction
    ?(dbs = []) ?(handlers = []) ?(userFunctions = []) ?(userTipes = []) () :
    model =
  defaultModel
    ~cursorState:(fillingCS ())
    ~dbs
    ~handlers
    ~userTipes
    ~userFunctions:(aFunction () :: userFunctions)
    ()


let enteringDBField
    ?(dbs = []) ?(handlers = []) ?(userFunctions = []) ?(userTipes = []) () :
    model =
  defaultModel
    ~cursorState:(fillingCS ())
    ~dbs:([aDB ()] @ dbs)
    ~handlers
    ~userTipes
    ~userFunctions
    ()


let enteringDBType
    ?(dbs = []) ?(handlers = []) ?(userFunctions = []) ?(userTipes = []) () :
    model =
  defaultModel
    ~cursorState:(fillingCS ())
    ~dbs:([aDB ~fieldid:defaultID2 ~typeid:defaultID ()] @ dbs)
    ~handlers
    ~userTipes
    ~userFunctions
    ()


let enteringHandler ?(module_ : string option = None) ?(expr = defaultExpr) ()
    : model =
  defaultModel
    ~cursorState:(fillingCS ())
    ~handlers:[aHandler ~module_ ~expr ()]
    ()


(* AC targeting a tlid and pointer *)
let acFor
    ?(target = Some (defaultTLID, PExpr (toExpr defaultExpr))) (m : model) :
    AC.autocomplete =
  let tlid, ti =
    match target with
    | Some (tlid, PExpr expr) ->
        let ti =
          fromExpr Defaults.defaultModel.fluidState expr
          |> toTokens Defaults.defaultModel.fluidState
          |> List.head
          |> Option.withDefault ~default:defaultTokenInfo
        in
        (tlid, ti)
    | _ ->
        (defaultTLID, defaultTokenInfo)
  in
  match m.cursorState with
  | Entering (Creating _) ->
      AC.regenerate m (AC.init m) (tlid, ti)
  | Entering (Filling _) ->
      AC.regenerate m (AC.init m) (tlid, ti)
  | _ ->
      AC.regenerate m (AC.init m) (tlid, ti)


let setQuery (m : model) (q : string) (a : AC.autocomplete) : AC.autocomplete =
  let fullQ = defaultFullQuery m q in
  AC.refilter m fullQ a


let itemPresent (aci : AC.autocompleteItem) (ac : AC.autocomplete) : bool =
  List.member ~value:aci ac.completions


let fromFluidACI (aci : fluidAutocompleteItem) : Types.autocompleteItem option
    =
  match aci with
  | FACFunction f ->
      Some (ACFunction f)
  | FACConstructorName (name, _) ->
      Some (ACConstructorName name)
  | FACField name ->
      Some (ACField name)
  | FACVariable name ->
      Some (ACVariable name)
  | FACLiteral lit ->
      Some (ACLiteral lit)
  | FACKeyword kw ->
      Some (ACKeyword kw)
  | FACPattern _ ->
      None


let fromFluidAC (ac : fluidAutocompleteState) : Types.autocomplete =
  let value =
    match ac.query with
    | Some (_, ti) ->
        FluidToken.toText ti.token
    | None ->
        ""
  in
  { functions = ac.functions
  ; admin = false
  ; completions = ac.completions |> List.map ~f:fromFluidACI |> Option.values
  ; invalidCompletions =
      ac.invalidCompletions |> List.map ~f:fromFluidACI |> Option.values
  ; allCompletions =
      ac.allCompletions |> List.map ~f:fromFluidACI |> Option.values
  ; index = (match ac.index with Some x -> x | None -> -1)
  ; value
  ; prevValue = ""
  ; target = None
  ; targetDval = None
  ; isCommandMode = false
  ; visible = ac.index = None }


let () =
  describe "autocomplete" (fun () ->
      (* TODO: not yet working in fluid
      describe "generation" (fun () ->
          test
            "invalidated cursor state/acFor still produces a valid autocomplete"
            (fun () ->
              expect (fun () ->
                  defaultModel ~cursorState:(fillingCS ()) ()
                  |> fun x -> acFor x )
              |> not_
              |> toThrow ) ;
          () ) ;
          *)
      describe "validate httpName varnames" (fun () ->
          let module_ = Some "HTTP" in
          let tl = aHandler ~module_ () in
          let pd = PEventName (Types.F (ID "0", "foo")) in
          test "/foo/bar is valid, no variables" (fun () ->
              let value = "/foo/bar" in
              expect (Entry.validate tl pd value) |> toEqual None ) ;
          test "/:some/:variableNames/:here_1 is valid" (fun () ->
              let value = "/:some/:variableNames/:here_1" in
              expect (Entry.validate tl pd value) |> toEqual None ) ;
          test
            "/:here-1 is not valid, no hyphens allowed in varnames"
            (fun () ->
              let value = "/:here-1" in
              expect (Entry.validate tl pd value)
              |> toEqual
                   (Some "route variables must match /[a-z_][a-zA-Z0-9_]*/") )
      ) ;
      describe "queryWhenEntering" (fun () ->
          let m = enteringHandler () in
          test "empty autocomplete doesn't highlight" (fun () ->
              expect (acFor m |> fun x -> x.index) |> toEqual None ) ;
          test
            "pressing a letter from the AC.selected entry keeps the entry AC.selected"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::someOtherFunc"
                |> setQuery m "T"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          (* TODO: not yet working in fluid
           test "Returning to empty unselects" (fun () ->
              expect
                (acFor m |> setQuery m "lis" |> setQuery m "" |> AC.highlighted)
              |> toEqual None ) ; *)
          test "resetting the query refilters" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::somefunc"
                |> setQuery m "Twit::some"
                |> AC.selectDown
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "deprecated functions are removed" (fun () ->
              expect (acFor m |> setQuery m "deprecated" |> AC.highlighted)
              |> toEqual None ) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect
                ( acFor m
                |> setQuery m "lis"
                |> (fun x -> x.completions)
                |> List.map ~f:AC.asName )
              |> toEqual ["List::head"] ) ;
          test "search finds multiple results for prefix" (fun () ->
              expect
                ( acFor m
                |> setQuery m "twit::"
                |> (fun x -> x.completions)
                (* |> List.filter ~f:isStaticItem *)
                |> List.map ~f:AC.asName )
              |> toEqual
                   ["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"]
          ) ;
          test "search finds only prefixed" (fun () ->
              expect
                ( acFor m
                |> setQuery m "twit::y"
                |> (fun x -> x.completions)
                (* |> List.filter ~f:isStaticItem *)
                |> List.map ~f:AC.asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "show results when the only option is the setQuery m" (fun () ->
              expect
                ( acFor m
                |> setQuery m "List::head"
                |> (fun x -> x.completions)
                (* |> List.filter ~f:isStaticItem *)
                |> List.map ~f:AC.asName
                |> List.length )
              |> toEqual 1 ) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 2) ) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectDown
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 0) ) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectDown
                |> AC.selectUp
                |> AC.selectUp
                |> fun x -> x.index )
              |> toEqual (Some 2) ) ;
          test
            "scrolling loops the other way without going forward first"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectUp
                |> AC.selectUp
                |> fun x -> x.index )
              |> toEqual (Some 1) ) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> setQuery m "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual None ) ;
          (* test "Filter by method signature for typed values" ( fun () ->
              expect
                ( acFor m
                |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.map ~f:AC.asName
                |> Set.fromList
                |> (==) (Set.fromList ["List::head"]) )
              |> toEqual true ) ;

          test "Show allowed fields for objects" ( fun () ->
              expect
                ( acFor m
                |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing}
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.map ~f:AC.asName
                |> Set.fromList
                |> (==) (Set.fromList ["Int::add", "+"]))
              |> toEqual true ) ;
           *)
          test
            "ordering = startsWith then case match then case insensitive match"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "withLo"
                |> (fun x -> x.completions)
                (* |> List.filter ~f:isStaticItem *)
                |> List.map ~f:AC.asName )
              |> toEqual
                   [ "withLower"
                   ; "withlower"
                   ; "SomeModule::withLower"
                   ; "SomeOtherModule::withlower" ] ) ;
          test
            "a specific bug where `+` is interpreted as an FACLiteral"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "+"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "+") ) ;
          test "null works" (fun () ->
              expect (acFor m |> setQuery m "nu" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "null")) ) ;
          test "Ok works" (fun () ->
              expect (acFor m |> setQuery m "Ok" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Ok", 1))) ) ;
          test "Error works" (fun () ->
              expect (acFor m |> setQuery m "Error" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Error", 1))) ) ;
          test "true works" (fun () ->
              expect (acFor m |> setQuery m "tr" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true")) ) ;
          test "case insensitive true works" (fun () ->
              expect (acFor m |> setQuery m "tR" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true")) ) ;
          test "false works" (fun () ->
              expect (acFor m |> setQuery m "fa" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "false")) ) ;
          test "if works" (fun () ->
              expect (acFor m |> setQuery m "if" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KIf)) ) ;
          test "let works" (fun () ->
              expect (acFor m |> setQuery m "let" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLet)) ) ;
          test "Lambda works" (fun () ->
              expect (acFor m |> setQuery m "lambda" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLambda)) ) ;
          test "http handlers have request" (fun () ->
              let module_ = Some "HTTP" in
              let m = enteringHandler ~module_ () in
              expect
                ( acFor m
                |> setQuery m "request"
                |> itemPresent (FACVariable "request") )
              |> toEqual true ) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let ac = acFor m in
                 [ ac
                   |> setQuery m "request"
                   |> itemPresent (FACVariable "request")
                 ; ac
                   |> setQuery m "event"
                   |> itemPresent (FACVariable "event") ])
              |> toEqual [true; true] ) ;
          (* TODO: not yet working in fluid
           * test "functions have DB names in the autocomplete" (fun () ->
              let blankid = ID "123" in
              let dbNameBlank = EBlank blankid in
              let fntlid = TLID "fn123" in
              let fn =
                aFunction
                  ~tlid:fntlid
                  ~expr:
                    (EFnCall (gid (), "DB::deleteAll", [dbNameBlank], NoRail))
                  ()
              in
              let m =
                defaultModel
                  ~cursorState:(fillingCS ~tlid:fntlid ())
                  ~dbs:[aDB ~tlid:(TLID "db123") ()]
                  ~userFunctions:[fn]
                  ()
              in
              let target = Some (fntlid, PExpr (toExpr dbNameBlank)) in
              let ac = acFor ~target m in
              Js.log2
                "DB name completions blankOr: "
                ( List.map ~f:AC.show_autocompleteItem ac.completions
                |> String.join ~sep:"; " ) ;
              expect (ac |> itemPresent (FACVariable "MyDB")) |> toEqual true
          ) ;*)
          () ) ;
      describe "filter" (fun () ->
          test "Cannot use DB variable when type of blank isn't TDB" (fun () ->
              let m =
                defaultModel ~cursorState:(fillingCS ()) ~dbs:[aDB ()] ()
              in
              let ac = acFor m in
              let _valid, invalid =
                AC.filter m ac [FACVariable "MyDB"] (defaultFullQuery m "")
              in
              expect (List.member ~value:(FACVariable "MyDB") invalid)
              |> toEqual true ) ;
          let consFAC =
            [ FACConstructorName ("Just", 1)
            ; FACConstructorName ("Nothing", 0)
            ; FACConstructorName ("Ok", 1)
            ; FACConstructorName ("Error", 1) ]
          in
          let patternFAC mID firstPatID queryString =
            let patID = firstPatID in
            let incr (ID id) count =
              ID (count + int_of_string id |> string_of_int)
            in
            [ FPAVariable (mID, patID, queryString)
            ; FPABool (mID, incr patID 1, true)
            ; FPABool (mID, incr patID 2, false)
            ; FPAConstructor
                (mID, incr patID 3, "Just", [FPBlank (mID, incr patID 8)])
            ; FPAConstructor (mID, incr patID 4, "Nothing", [])
            ; FPAConstructor
                (mID, incr patID 5, "Ok", [FPBlank (mID, incr patID 9)])
            ; FPAConstructor
                (mID, incr patID 6, "Error", [FPBlank (mID, incr patID 10)])
            ; FPANull (mID, incr patID 7) ]
            |> List.map ~f:(fun x -> FACPattern x)
          in
          (* TODO: not yet working in fluid
          test "Only Just and Nothing are allowed in Option-blank" (fun () ->
              let param1id = ID "123" in
              let expr =
                EFnCall
                  (gid (), "Option::withDefault", [EBlank param1id], NoRail)
              in
              let handler = aHandler ~expr () in
              let m =
                defaultModel ~handlers:[handler] ~cursorState:(fillingCS ()) ()
              in
              let target = Some (defaultTLID, PExpr (Blank param1id)) in
              let ac = acFor ~target m in
              let newM = {m with complete = fromFluidAC ac} in
              let ti =
                match toTokens newM.fluidState expr |> List.head with
                | Some ti ->
                    ti
                | _ ->
                    defaultTokenInfo
              in
              let dv =
                Analysis.getCurrentLiveValue
                  newM
                  handler.id
                  (ti.token |> FluidToken.tid)
              in
              let fullQ = (handler, ti, dv, "") in
              let valid, _invalid = AC.filter newM ac consFAC fullQ in
              expect
                ( List.length valid = 2
                && List.member ~value:(FACConstructorName ("Just", 1)) valid
                && List.member ~value:(FACConstructorName ("Nothing", 0)) valid
                )
              |> toEqual true ) ; *)
          (* TODO: not yet working in fluid
           * test "Only Ok and Error are allowed in Result-blank" (fun () ->
              let param1id = ID "123" in
              let expr =
                EFnCall
                  (gid (), "Result::catchError", [EBlank param1id], NoRail)
              in
              let handler = aHandler ~expr () in
              let m =
                defaultModel ~handlers:[handler] ~cursorState:(fillingCS ()) ()
              in
              let target = Some (defaultTLID, PExpr (Blank param1id)) in
              let ac = acFor ~target m in
              let newM = {m with complete = fromFluidAC ac} in
              let ti =
                match toTokens newM.fluidState expr |> List.head with
                | Some ti ->
                    ti
                | _ ->
                    defaultTokenInfo
              in
              let dv =
                Analysis.getCurrentLiveValue
                  newM
                  handler.id
                  (ti.token |> FluidToken.tid)
              in
              let fullQ = (handler, ti, dv, "") in
              let valid, _invalid = AC.filter newM ac consFAC fullQ in
              expect
                ( List.length valid = 2
                && List.member ~value:(FACConstructorName ("Ok", 1)) valid
                && List.member ~value:(FACConstructorName ("Error", 1)) valid
                )
              |> toEqual true ) ;*)
          test "Constructors are also available in Any blankOr" (fun () ->
              let m = enteringHandler () in
              let ac = acFor m in
              let valid, _invalid =
                AC.filter m ac consFAC (defaultFullQuery m "")
              in
              expect
                ( List.member ~value:(FACConstructorName ("Ok", 1)) valid
                && List.member ~value:(FACConstructorName ("Error", 1)) valid
                && List.member ~value:(FACConstructorName ("Just", 1)) valid
                && List.member ~value:(FACConstructorName ("Nothing", 0)) valid
                )
              |> toEqual true ) ;
          test "Pattern expressions are available in pattern blank" (fun () ->
              let tlid = TLID "789" in
              let mID = ID "1234" in
              let patID = ID "456" in
              let expr = aMatchExpr ~mID ~patID () in
              let m =
                defaultModel
                  ~cursorState:(fillingCS ~tlid ~_id:patID ())
                  ~handlers:[aHandler ~tlid ~expr ()]
                  ()
              in
              let query = "o" in
              let patternFAC = patternFAC mID patID query in
              let expected =
                [ FPAVariable (mID, ID "456", query)
                ; FPAConstructor (mID, ID "460", "Nothing", [])
                ; FPAConstructor
                    (mID, ID "461", "Ok", [FPBlank (mID, ID "465")])
                ; FPAConstructor
                    (mID, ID "462", "Error", [FPBlank (mID, ID "466")]) ]
                |> List.map ~f:(fun x -> FACPattern x)
              in
              let ac = acFor ~target:(Some (tlid, PExpr (toExpr expr))) m in
              let valid, _invalid =
                AC.filter m ac patternFAC (defaultFullQuery m query)
              in
              expect
                ( List.length valid = List.length expected
                && List.all ~f:(fun v -> List.member ~value:v valid) expected
                )
              |> toEqual true ) ;
          () ) ;
      () ) ;
  ()
