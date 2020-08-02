open Tester
open Prelude
module E = FluidExpression
module AC = FluidAutocomplete
module B = BlankOr
module K = FluidKeyboard
module P = FluidPattern
module Printer = FluidTokenizer
module TL = Toplevel
open FluidExpression
open Fluid_test_data
open FluidShortcuts

let sampleFunctions : function_ list =
  [ ("Twit::somefunc", [TObj], TAny)
  ; ("Twit::someOtherFunc", [TObj], TAny)
  ; ("Twit::yetAnother", [TObj], TAny)
  ; ("+", [TInt; TInt], TInt)
  ; ("Int::add", [TInt; TInt], TInt)
  ; ("Dict::keys", [TObj], TList)
  ; ("List::head", [TList], TAny)
  ; ("withlower", [TObj], TObj)
  ; ("withLower", [TObj], TObj)
  ; ("SomeModule::withLower", [TObj], TObj)
  ; ("SomeOtherModule::withlower", [TObj], TObj)
  ; ("HTTP::post", [TAny], TAny)
  ; ("HTTP::head", [TAny], TAny)
  ; ("HTTP::get", [TAny], TAny)
  ; ("HTTP::options", [TAny], TAny)
  ; ("Some::deprecated", [TAny], TAny)
  ; ("DB::deleteAll", [TDB], TNull)
  ; ("DB::generateKey", [], TStr)
  ; ("DB::getAll_v2", [TDB], TList)
  ; ("DB::getAll_v1", [TDB], TList)
    (* ordering is deliberate - we want the query to order s.t. get is before getAll *)
  ; ("DB::get_v1", [TDB], TList)
  ; ("String::append", [TStr; TStr], TStr)
  ; ("List::append", [TList; TList], TList)
  ; ("String::newline", [], TStr)
  ; ("Option::withDefault", [TOption], TAny)
  ; ("Result::withDefault", [TResult], TAny) ]
  |> List.map ~f:(fun (fnName, paramTipes, fnReturnTipe) ->
         { fnName
         ; fnParameters =
             List.map paramTipes ~f:(fun paramTipe ->
                 { paramName = "x"
                 ; paramTipe
                 ; paramBlock_args = []
                 ; paramOptional = false
                 ; paramDescription = "" })
         ; fnReturnTipe
         ; fnPreviewSafety = Unsafe
         ; fnDescription = ""
         ; fnInfix = true
         ; fnDeprecated = fnName = "Some::deprecated"
         ; fnIsSupportedInQuery = false
         ; fnOrigin = Builtin })


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
  ; token = TBlank (defaultID, None) }


let defaultFullQuery ?(tl = defaultToplevel) (ac : AC.t) (queryString : string)
    : AC.fullQuery =
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
  let _, ti = ac.query |> Option.withDefault ~default:(TL.id tl, ti) in
  {tl; ti; fieldList = []; pipedDval = None; queryString}


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
    ?(handlers = [aHandler ()])
    ?(userFunctions = [])
    ?(userTipes = [])
    () : model =
  let analyses =
    analyses
    |> List.map ~f:(fun (id, value) -> (ID.toString id, ExecutedResult value))
    |> StrDict.fromList
  in
  let default = Fluid_test_data.defaultTestModel in
  { default with
    handlers = Handlers.fromList handlers
  ; dbs = DB.fromList dbs
  ; userFunctions = UserFunctions.fromList userFunctions
  ; userTipes = UserTypes.fromList userTipes
  ; cursorState = FluidEntering tlid
  ; functions =
      {Functions.empty with builtinFunctions = sampleFunctions}
      |> Functions.update defaultFunctionsProps
  ; analyses =
      StrDict.singleton ~key:defaultTraceID ~value:(LoadableSuccess analyses) }


(* AC targeting a tlid and pointer *)
let acFor ?(tlid = defaultTLID) ?(pos = 0) (m : model) : AC.t =
  let ti =
    TL.get m tlid
    |> Option.andThen ~f:TL.getAST
    |> Option.andThen ~f:(fun ast ->
           Fluid.ASTInfo.make
             defaultTestProps
             ast
             {m.fluidState with newPos = pos}
           |> Fluid.ASTInfo.getToken)
    |> Option.withDefault ~default:defaultTokenInfo
  in
  AC.regenerate m AC.init (tlid, ti)


let setQuery (q : string) (a : AC.t) : AC.t =
  let fullQ = defaultFullQuery a q in
  let props = defaultTestProps in
  AC.refilter
    {functions = props.functions}
    fullQ
    a
    (List.map ~f:(fun {item; _} -> item) a.completions)


let filterValid (a : AC.t) : AC.item list =
  List.filterMap a.completions ~f:(function
      | {item; validity = FACItemValid} ->
          Some item
      | _ ->
          None)


let filterInvalid (a : AC.t) : AC.item list =
  List.filterMap a.completions ~f:(function
      | {validity = FACItemValid; _} ->
          None
      | {item; _} ->
          Some item)


let run () =
  describe "autocomplete" (fun () ->
      describe "queryWhenEntering" (fun () ->
          let m = defaultModel () in
          let acForQueries (qs : string list) =
            List.foldl qs ~init:(acFor m) ~f:setQuery
            |> filterValid
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
                |> setQuery "Twit::somef"
                |> setQuery "Twit::someO"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc")) ;
          test "Returning to empty unselects" (fun () ->
              expect (acFor m |> setQuery "lis" |> setQuery "" |> AC.highlighted)
              |> toEqual None) ;
          test "resetting the query refilters" (fun () ->
              expect
                ( acFor m
                |> setQuery "Twit::somefunc"
                |> setQuery "Twit::some"
                |> AC.selectDown
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "Twit::someOtherFunc")) ;
          test "deprecated functions are removed" (fun () ->
              expect (acFor m |> setQuery "deprecated" |> AC.highlighted)
              |> toEqual None) ;
          test "sorts correctly without typing ::" (fun () ->
              expect (acForQuery "dbget" |> List.head)
              |> toEqual (Some "DB::get_v1")) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect (acForQuery "listh") |> toEqual ["List::head"]) ;
          test "DB::get_v1 occurs before DB::getAll_v1" (fun () ->
              expect (acForQuery "DB::get" |> List.head)
              |> toEqual (Some "DB::get_v1")) ;
          test "DB::getAll_v2 occurs before DB::getAll_v1" (fun () ->
              expect (acForQuery "DB::getA" |> List.head)
              |> toEqual (Some "DB::getAll_v2")) ;
          test "DB::getAll_v1 is reachable" (fun () ->
              expect (acForQuery "DB::getA")
              |> toEqual ["DB::getAll_v2"; "DB::getAll_v1"]) ;
          test "search finds only prefixed" (fun () ->
              expect (acForQuery "twit::y") |> toEqual ["Twit::yetAnother"]) ;
          test "show results when the only option is the setQuery" (fun () ->
              expect (acForQuery "List::head" |> List.length) |> toEqual 1) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( acFor m
                |> setQuery "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 2)) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( acFor m
                |> setQuery "Twit:"
                |> AC.selectDown
                |> AC.selectDown
                |> AC.selectDown
                |> fun x -> x.index )
              |> toEqual (Some 0)) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( acFor m
                |> setQuery "Twit:"
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
                |> setQuery "Twit:"
                |> AC.selectUp
                |> AC.selectUp
                |> fun x -> x.index )
              |> toEqual (Some 1)) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( acFor m
                |> setQuery "Twit"
                |> AC.selectDown
                |> AC.selectDown
                |> setQuery "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual None) ;
          test
            "ordering = startsWith then case match then case insensitive match"
            (fun () ->
              expect
                ( acFor m
                |> setQuery "withLo"
                |> filterValid
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
                |> setQuery "+"
                |> AC.highlighted
                |> Option.map ~f:AC.asName )
              |> toEqual (Some "+")) ;
          test "null works" (fun () ->
              expect (acFor m |> setQuery "nu" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "null"))) ;
          test "Ok works" (fun () ->
              expect (acFor m |> setQuery "Ok" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Ok", 1)))) ;
          test "Error works" (fun () ->
              expect (acFor m |> setQuery "Error" |> AC.highlighted)
              |> toEqual (Some (FACConstructorName ("Error", 1)))) ;
          test "true works" (fun () ->
              expect (acFor m |> setQuery "tr" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true"))) ;
          test "case insensitive true works" (fun () ->
              expect (acFor m |> setQuery "tR" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "true"))) ;
          test "false works" (fun () ->
              expect (acFor m |> setQuery "fa" |> AC.highlighted)
              |> toEqual (Some (FACLiteral "false"))) ;
          test "if works" (fun () ->
              expect (acFor m |> setQuery "if" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KIf))) ;
          test "let works" (fun () ->
              expect (acFor m |> setQuery "let" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLet))) ;
          test "Lambda works" (fun () ->
              expect (acFor m |> setQuery "lambda" |> AC.highlighted)
              |> toEqual (Some (FACKeyword KLambda))) ;
          test "http handlers have request" (fun () ->
              let space = Some "HTTP" in
              let m = defaultModel ~handlers:[aHandler ~space ()] () in
              expect (acFor m |> setQuery "request" |> filterValid)
              |> toEqual [FACVariable ("request", None)]) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let ac = acFor m in
                 ( ac |> setQuery "request" |> filterValid
                 , ac |> setQuery "event" |> filterValid ))
              |> toEqual
                   ( [FACVariable ("request", None)]
                   , [FACVariable ("event", None)] )) ;
          test "functions have DB names in the autocomplete" (fun () ->
              let blankid = ID.fromString "123" in
              let dbNameBlank = EBlank blankid in
              let fntlid = TLID.fromString "fn123" in
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
                  ~dbs:[aDB ~tlid:(TLID.fromString "db123") ()]
                  ~userFunctions:[fn]
                  ()
              in
              let ac = acFor ~tlid:fntlid ~pos:14 m in
              expect (ac |> setQuery "MyDB" |> filterValid)
              |> toEqual [FACVariable ("MyDB", Some (DDB "MyDB"))]) ;
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
            (filterValid ac, filterInvalid ac)
          in
          test "Cannot use DB variable when type of blank isn't TDB" (fun () ->
              let id = gid () in
              let expr = fn "Int::add" [EBlank id; b] in
              let m =
                defaultModel
                  ~analyses:[(id, DDB "MyDB")]
                  ~dbs:[aDB ~tlid:(TLID.fromString "23") ()]
                  ~handlers:[aHandler ~expr ()]
                  ()
              in
              let _valid, invalid = filterFor m ~pos:9 in
              expect (List.filter invalid ~f:isVariable)
              |> toEqual [FACVariable ("MyDB", Some (DDB "MyDB"))]) ;
          test "Constructors are available in Any expression" (fun () ->
              let m = defaultModel () in
              let valid, _invalid = filterFor m ~pos:0 in
              expect (List.filter valid ~f:isConstructor)
              |> toEqual
                   [ FACConstructorName ("Just", 1)
                   ; FACConstructorName ("Nothing", 0)
                   ; FACConstructorName ("Ok", 1)
                   ; FACConstructorName ("Error", 1) ]) ;
          test "Method argument filters by variable type" (fun () ->
              let id = gid () in
              let id2 = gid () in
              let expr =
                let'
                  "mystr"
                  (str ~id "asd")
                  (let' "myint" (int ~id:id2 5) (fn "String::append" [b; b]))
              in
              let m =
                defaultModel
                  ~analyses:[(id, DStr "asd"); (id2, DInt 5)]
                  ~handlers:
                    [ aHandler
                        ~space:
                          (Some "REPL" (* remove `request` var from valid *))
                        ~expr
                        () ]
                  ()
              in
              let valid, invalid = filterFor m ~pos:47 in
              expect
                ( List.filter valid ~f:isVariable
                , List.filter invalid ~f:isVariable )
              |> toEqual
                   ( [FACVariable ("mystr", Some (DStr "asd"))]
                   , [FACVariable ("myint", Some (DInt 5))] )) ;
          test "Method argument filters by fn return type " (fun () ->
              let expr = fn "String::append" [b; b] in
              let m = defaultModel ~handlers:[aHandler ~expr ()] () in
              let valid, invalid = filterFor m ~pos:15 in
              expect
                ( valid
                  |> List.map ~f:AC.asName
                  |> List.member ~value:"String::append"
                , invalid
                  |> List.map ~f:AC.asName
                  |> List.member ~value:"Int::add" )
              |> toEqual (true, true)) ;
          test "Only Just and Nothing are allowed in Option-blank" (fun () ->
              let expr = fn "Option::withDefault" [b] in
              let m = defaultModel ~handlers:[aHandler ~expr ()] () in
              let valid, _invalid = filterFor m ~pos:20 in
              expect (valid |> List.filter ~f:isConstructor)
              |> toEqual
                   [ FACConstructorName ("Just", 1)
                   ; FACConstructorName ("Nothing", 0) ]) ;
          test "Only Ok and Error are allowed in Result blank" (fun () ->
              let expr = fn "Result::withDefault" [b] in
              let m = defaultModel ~handlers:[aHandler ~expr ()] () in
              let valid, _invalid = filterFor m ~pos:20 in
              expect (valid |> List.filter ~f:isConstructor)
              |> toEqual
                   [ FACConstructorName ("Ok", 1)
                   ; FACConstructorName ("Error", 1) ]) ;
          test "Use piped types" (fun () ->
              let id = gid () in
              let expr = pipe (str ~id "asd") [partial "append" b] in
              let m =
                defaultModel
                  ~analyses:[(id, DStr "asd")]
                  ~handlers:[aHandler ~expr ()]
                  ()
              in
              let valid, _invalid = filterFor m ~pos:14 in
              expect
                (valid |> List.filter ~f:AC.isFnCall |> List.map ~f:AC.asName)
              |> toEqual ["String::append"]) ;
          test "functions with no arguments are invalid when piping" (fun () ->
              let id = gid () in
              let expr = pipe (int ~id 5) [partial "string" b] in
              let m =
                defaultModel
                  ~analyses:[(id, DInt 5)]
                  ~handlers:[aHandler ~expr ()]
                  ()
              in
              let _valid, invalid = filterFor m ~pos:10 in
              expect
                ( invalid
                |> List.map ~f:AC.asName
                |> List.filter ~f:(( = ) "String::newline") )
              |> toEqual ["String::newline"]) ;
          test "Pattern expressions are available in pattern blank" (fun () ->
              let tlid = TLID.fromString "789" in
              let mID = ID.fromString "1234" in
              let patID = ID.fromString "456" in
              let pattern = P.FPVariable (mID, patID, "o") in
              let expr = match' b [(pattern, b)] in
              let m =
                defaultModel ~handlers:[aHandler ~tlid ~expr ()] ()
                |> fun m -> {m with functions = Functions.empty}
              in
              expect
                (acFor ~tlid ~pos:13 m |> filterValid |> List.map ~f:AC.asName)
              |> toEqual ["o"; "Ok"; "Nothing"; "Error"]) ;
          ()) ;
      ()) ;
  ()
