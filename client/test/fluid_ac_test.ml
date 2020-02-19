open Tester
open Prelude
open Fluid
module AC = FluidAutocomplete
module B = BlankOr
module K = FluidKeyboard
open FluidExpression
open Fluid_test_data
open FluidShortcuts

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
  ; ("DB::generateKey", TStr)
  ; ("DB::getAll_v2", TList)
  ; ("DB::getAll_v1", TList)
    (* ordering is deliberate - we want the query to order s.t. get is before getAll *)
  ; ("DB::get_v1", TList)
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
         ; fnDeprecated = fnName = "Some::deprecated" })


let defaultTLID = TLID "7"

let defaultID = gid ()

let defaultID2 = gid ()

let defaultExpr = E.EBlank defaultID

let defaultToplevel =
  TLHandler
    { ast = defaultExpr
    ; spec =
        { space = Blank (gid ())
        ; name = Blank (gid ())
        ; modifier = Blank (gid ()) }
    ; hTLID = defaultTLID
    ; pos = Defaults.origin }


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
    match tl with
    | TLHandler {ast; _} | TLFunc {ufAST = ast; _} ->
        ast
        |> Printer.tokensForSplit ~index:0
        |> List.head
        |> Option.withDefault ~default:defaultTokenInfo
    | _ ->
        defaultTokenInfo
  in
  let _, ti =
    m.fluidState.ac.query |> Option.withDefault ~default:(TL.id tl, ti)
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
  let default = Fluid_test_data.defaultTestModel in
  { default with
    handlers = Handlers.fromList handlers
  ; dbs = DB.fromList dbs
  ; userFunctions = UserFunctions.fromList userFunctions
  ; userTipes = UserTypes.fromList userTipes
  ; cursorState
  ; builtInFunctions = sampleFunctions
  ; analyses =
      StrDict.singleton (* The default traceID for TLID 7 *)
        ~key:"94167980-f909-527e-a4af-bc3155f586d3"
        ~value:
          (LoadableSuccess
             (StrDict.singleton
                ~key:"12"
                ~value:
                  (ExecutedResult
                     (DObj
                        (StrDict.fromList [("title", DNull); ("author", DNull)])))))
  }


let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultExpr)
    ?(space : string option = None)
    () : handler =
  let space = match space with None -> B.new_ () | Some name -> B.newF name in
  let spec = {space; name = B.new_ (); modifier = B.new_ ()} in
  {ast = expr; spec; hTLID = tlid; pos = {x = 0; y = 0}}


let aFunction ?(tlid = defaultTLID) ?(expr = defaultExpr) () : userFunction =
  { ufTLID = tlid
  ; ufMetadata =
      { ufmName = B.newF "myFunc"
      ; ufmParameters = []
      ; ufmDescription = ""
      ; ufmReturnTipe = B.newF TStr
      ; ufmInfix = false }
  ; ufAST = expr }


let aDB ?(tlid = defaultTLID) ?(fieldid = defaultID) ?(typeid = defaultID2) () :
    db =
  { dbTLID = tlid
  ; dbName = B.newF "MyDB"
  ; cols = [(Blank fieldid, Blank typeid)]
  ; version = 0
  ; oldMigrations = []
  ; activeMigration = None
  ; pos = {x = 0; y = 0} }


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


let enteringHandler ?(space : string option = None) ?(expr = defaultExpr) () :
    model =
  defaultModel
    ~cursorState:(fillingCS ())
    ~handlers:[aHandler ~space ~expr ()]
    ()


(* AC targeting a tlid and pointer *)
let acFor ?(tlid = defaultTLID) ?(pos = 0) (m : model) : AC.autocomplete =
  let ti =
    match TL.get m tlid with
    | Some (TLHandler {ast; _}) | Some (TLFunc {ufAST = ast; _}) ->
        ast
        |> Fluid.getToken {m.fluidState with newPos = pos}
        |> Option.withDefault ~default:defaultTokenInfo
    | _ ->
        defaultTokenInfo
  in
  AC.regenerate m (AC.init m) (tlid, ti)


let setQuery (m : model) (q : string) (a : AC.autocomplete) : AC.autocomplete =
  let fullQ = defaultFullQuery m q in
  AC.refilter m fullQ a


let itemPresent (aci : AC.autocompleteItem) (ac : AC.autocomplete) : bool =
  List.member ~value:aci ac.completions


let run () =
  describe "autocomplete" (fun () ->
      describe "queryWhenEntering" (fun () ->
          let m = enteringHandler () in
          let acForQueries (qs : string list) =
            List.foldl qs ~init:(acFor m) ~f:(setQuery m)
            |> (fun x -> x.completions)
            |> List.map ~f:AC.asName
          in
          let acForQuery (q : string) = acForQueries [q] in
          test "empty autocomplete doesn't highlight" (fun () ->
              expect (acFor m |> fun x -> x.index) |> toEqual None) ;
          test
            "pressing a letter from the AC.selected entry does not keep the entry AC.selected"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::somef"
                |> setQuery m "Twit::someO"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc")) ;
          test "Returning to empty unselects" (fun () ->
              expect
                (acFor m |> setQuery m "lis" |> setQuery m "" |> AC.highlighted)
              |> toEqual None) ;
          test "resetting the query refilters" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::somefunc"
                |> setQuery m "Twit::some"
                |> AC.selectDown
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc")) ;
          test "deprecated functions are removed" (fun () ->
              expect (acFor m |> setQuery m "deprecated" |> AC.highlighted)
              |> toEqual None) ;
          test "sorts correctly without typing ::" (fun () ->
              expect (acForQuery "dbget" |> List.head)
              |> toEqual (Some "DB::get_v1")) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect (acForQuery "listh") |> toEqual ["List::head"]) ;
          test "DB::get_v1 occurs before DB::getAll_v1" (fun () ->
              expect (acForQuery "DB::get" |> List.head)
              |> toEqual (Some "DB::get_v1")) ;
          test "DB::getAll_v1 occurs before DB::getAll_v2" (fun () ->
              expect (acForQuery "DB::getA" |> List.head)
              |> toEqual (Some "DB::getAll_v1")) ;
          test "DB::getAll_v2 is reachable" (fun () ->
              expect (acForQuery "DB::getA")
              |> toEqual ["DB::getAll_v1"; "DB::getAll_v2"]) ;
          test "search finds only prefixed" (fun () ->
              expect (acForQuery "twit::y") |> toEqual ["Twit::yetAnother"]) ;
          test "show results when the only option is the setQuery m" (fun () ->
              expect (acForQuery "List::head" |> List.length) |> toEqual 1) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 2)) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectDown
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 0)) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectDown
                |> AC.selectUp
                |> AC.selectUp
                |> fun x -> x.index )
              |> toEqual (Some 2)) ;
          test
            "scrolling loops the other way without going forward first"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> AC.selectUp
                |> AC.selectUp
                |> fun x -> x.index )
              |> toEqual (Some 1)) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> setQuery m "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual None) ;
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
                   ; "SomeOtherModule::withlower" ]) ;
          test
            "a specific bug where `+` is interpreted as an FACLiteral"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "+"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "+")) ;
          test "null works" (fun () ->
              expect (acFor m |> setQuery m "nu" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "null"))) ;
          test "Ok works" (fun () ->
              expect (acFor m |> setQuery m "Ok" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Ok", 1)))) ;
          test "Error works" (fun () ->
              expect (acFor m |> setQuery m "Error" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Error", 1)))) ;
          test "true works" (fun () ->
              expect (acFor m |> setQuery m "tr" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true"))) ;
          test "case insensitive true works" (fun () ->
              expect (acFor m |> setQuery m "tR" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true"))) ;
          test "false works" (fun () ->
              expect (acFor m |> setQuery m "fa" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "false"))) ;
          test "if works" (fun () ->
              expect (acFor m |> setQuery m "if" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KIf))) ;
          test "let works" (fun () ->
              expect (acFor m |> setQuery m "let" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLet))) ;
          test "Lambda works" (fun () ->
              expect (acFor m |> setQuery m "lambda" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLambda))) ;
          test "http handlers have request" (fun () ->
              let space = Some "HTTP" in
              let m = enteringHandler ~space () in
              expect
                ( acFor m
                |> setQuery m "request"
                |> itemPresent (FACVariable ("request", None)) )
              |> toEqual true) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let ac = acFor m in
                 [ ac
                   |> setQuery m "request"
                   |> itemPresent (FACVariable ("request", None))
                 ; ac
                   |> setQuery m "event"
                   |> itemPresent (FACVariable ("event", None)) ])
              |> toEqual [true; true]) ;
          test "functions have DB names in the autocomplete" (fun () ->
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
              let ac = acFor ~tlid:fntlid ~pos:14 m in
              expect
                (ac |> itemPresent (FACVariable ("MyDB", Some (DDB "MyDB"))))
              |> toEqual true) ;
          ()) ;
      describe "filter" (fun () ->
          test "Cannot use DB variable when type of blank isn't TDB" (fun () ->
              let m =
                defaultModel ~cursorState:(fillingCS ()) ~dbs:[aDB ()] ()
              in
              let ac = acFor m in
              let _valid, invalid =
                AC.filter
                  m
                  ac
                  [FACVariable ("MyDB", None)]
                  (defaultFullQuery m "")
              in
              expect (List.member ~value:(FACVariable ("MyDB", None)) invalid)
              |> toEqual true) ;
          let consFAC =
            [ FACConstructorName ("Just", 1)
            ; FACConstructorName ("Nothing", 0)
            ; FACConstructorName ("Ok", 1)
            ; FACConstructorName ("Error", 1) ]
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
          test "Constructors are also available in Any expression" (fun () ->
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
              |> toEqual true) ;
          test "Pattern expressions are available in pattern blank" (fun () ->
              let tlid = TLID "789" in
              let mID = ID "1234" in
              let patID = ID "456" in
              let pattern = P.FPVariable (mID, patID, "o") in
              let expr = match' b [(pattern, b)] in
              let m =
                defaultModel
                  ~cursorState:(fillingCS ~tlid ~_id:patID ())
                  ~handlers:[aHandler ~tlid ~expr ()]
                  ()
                |> fun m -> {m with builtInFunctions = []}
              in
              expect
                ( acFor ~tlid ~pos:13 m
                |> (fun x -> x.completions)
                |> List.map ~f:(fun x -> AC.asName x) )
              |> toEqual ["o"; "Ok"; "Nothing"; "Error"]) ;
          ()) ;
      ()) ;
  ()
