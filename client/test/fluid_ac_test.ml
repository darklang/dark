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

let defaultTraceID = "94167980-f909-527e-a4af-bc3155f586d3"

let defaultID = gid ()

let defaultID2 = gid ()

let defaultExpr = E.EBlank defaultID

let defaultToplevel =
  TLHandler
    { ast = FluidAST.ofExpr defaultExpr
    ; spec =
        { space = Blank (gid ())
        ; name = Blank (gid ())
        ; modifier = Blank (gid ()) }
    ; hTLID = defaultTLID
    ; pos = Defaults.origin }


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
        |> FluidAST.toExpr
        |> Printer.tokenize
        |> List.head
        |> Option.withDefault ~default:defaultTokenInfo
    | _ ->
        defaultTokenInfo
  in
  let _, ti =
    m.fluidState.ac.query |> Option.withDefault ~default:(TL.id tl, ti)
  in
  (tl, ti, None, query)


let aHandler
    ?(tlid = defaultTLID)
    ?(expr = defaultExpr)
    ?(space : string option = None)
    () : handler =
  let space = match space with None -> B.new_ () | Some name -> B.newF name in
  let spec = {space; name = B.new_ (); modifier = B.new_ ()} in
  {ast = FluidAST.ofExpr expr; spec; hTLID = tlid; pos = {x = 0; y = 0}}


let aFunction ?(tlid = defaultTLID) ?(expr = defaultExpr) () : userFunction =
  { ufTLID = tlid
  ; ufMetadata =
      { ufmName = B.newF "myFunc"
      ; ufmParameters = []
      ; ufmDescription = ""
      ; ufmReturnTipe = B.newF TStr
      ; ufmInfix = false }
  ; ufAST = FluidAST.ofExpr expr }


let aDB ?(tlid = defaultTLID) ?(fieldid = defaultID) ?(typeid = defaultID2) () :
    db =
  { dbTLID = tlid
  ; dbName = B.newF "MyDB"
  ; cols = [(Blank fieldid, Blank typeid)]
  ; version = 0
  ; oldMigrations = []
  ; activeMigration = None
  ; pos = {x = 0; y = 0} }


(* Sets the model with the appropriate toplevels *)
let defaultModel
    ?(tlid = defaultTLID)
    ?(analyses = [])
    ?(dbs = [])
    ?(expr = defaultExpr)
    ?(handlers = [aHandler ~expr ()])
    ?(userFunctions = [])
    ?(userTipes = [])
    () : model =
  let analyses =
    analyses
    |> List.map ~f:(fun (ID id, value) -> (id, ExecutedResult value))
    |> StrDict.fromList
  in
  let default = Fluid_test_data.defaultTestModel in
  { default with
    handlers = Handlers.fromList handlers
  ; dbs = DB.fromList dbs
  ; userFunctions = UserFunctions.fromList userFunctions
  ; userTipes = UserTypes.fromList userTipes
  ; cursorState = FluidEntering tlid
  ; builtInFunctions = sampleFunctions
  ; fluidState =
      { default.fluidState with
        ac = {default.fluidState.ac with functions = sampleFunctions} }
  ; analyses =
      StrDict.singleton ~key:defaultTraceID ~value:(LoadableSuccess analyses) }


(* AC targeting a tlid and pointer *)
let acFor ?(tlid = defaultTLID) ?(pos = 0) (m : model) : AC.autocomplete =
  let ti =
    match TL.get m tlid with
    | Some (TLHandler {ast; _}) | Some (TLFunc {ufAST = ast; _}) ->
        Fluid.getToken ast {m.fluidState with newPos = pos}
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
          let m = defaultModel () in
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
              let m = defaultModel ~handlers:[aHandler ~space ()] () in
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
                  ~tlid:fntlid
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
          let isConstructor = function
            | FACConstructorName _ ->
                true
            | _ ->
                false
          in
          let isVariable = function FACVariable _ -> true | _ -> false in
          let filterFor m ~pos =
            let ac = acFor ~pos m in
            let m = {m with fluidState = {m.fluidState with ac}} in
            let newAC = setQuery m "" ac in
            (newAC.completions, newAC.invalidCompletions)
          in
          test "Cannot use DB variable when type of blank isn't TDB" (fun () ->
              let targetID = gid () in
              let expr = fn "Int::add" [EBlank targetID; b] in
              let m =
                defaultModel
                  ~analyses:[(targetID, DDB "MyDB")]
                  ~dbs:[aDB ~tlid:(TLID "23") ()]
                  ~handlers:[aHandler ~expr ()]
                  ()
              in
              let _valid, invalid = filterFor m ~pos:0 in
              expect (List.filter invalid ~f:isVariable)
              |> toEqual [FACVariable ("MyDB", Some (DDB "MyDB"))]) ;
          (* test "Only Just and Nothing are allowed in Option-blank" (fun () -> *)
          test "Constructors are available in Any expression" (fun () ->
              let m = defaultModel () in
              let valid, _invalid = filterFor m ~pos:0 in
              expect (List.filter valid ~f:isConstructor)
              |> toEqual
                   [ FACConstructorName ("Just", 1)
                   ; FACConstructorName ("Nothing", 0)
                   ; FACConstructorName ("Ok", 1)
                   ; FACConstructorName ("Error", 1) ]) ;
          (*     let expr = fn ~ster:NoRail "Option::withDefault" [b] in *)
          (*     let handler = aHandler ~expr () in *)
          (*     let m = defaultModel ~handlers:[handler] () in *)
          (*     let valid, _invalid = filterFor m consFAC ~pos:20 in *)
          (*     expect valid *)
          (*     |> toEqual *)
          (*          [ FACConstructorName ("Just", 1) *)
          (*          ; FACConstructorName ("Nothing", 0) ]) ; *)
          (* test "Only Ok and Error are allowed in Result blank" (fun () -> *)
          (*     let expr = fn ~ster:NoRail "Result::withDefault" [b] in *)
          (*     let handler = aHandler ~expr () in *)
          (*     let m = defaultModel ~handlers:[handler] () in *)
          (*     let valid, _invalid = filterFor m consFAC ~pos:20 in *)
          (*     expect valid *)
          (*     |> toEqual *)
          (*          [ FACConstructorName ("Ok", 1) *)
          (*          ; FACConstructorName ("Error", 1) ]) ; *)
          test "Pattern expressions are available in pattern blank" (fun () ->
              let tlid = TLID "789" in
              let mID = ID "1234" in
              let patID = ID "456" in
              let pattern = P.FPVariable (mID, patID, "o") in
              let expr = match' b [(pattern, b)] in
              let m =
                defaultModel ~handlers:[aHandler ~tlid ~expr ()] ()
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
