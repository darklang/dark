open Tc
open Types
open Autocomplete
open Prelude
open Jest
open Expect
module B = Blank

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
  ; ("DB::deleteAll", TDB) ]
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

let defaultExpr = Blank defaultID

let fillingCS ?(tlid = defaultTLID) ?(id = defaultID) () : cursorState =
  Entering (Filling (tlid, id))


let creatingCS : cursorState = Entering (Creating {x = 0; y = 0})

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
  {id = tlid; pos = {x = 0; y = 0}; data = TLHandler {ast = expr; spec; tlid}}


let aFunction ?(tlid = defaultTLID) ?(expr = defaultExpr) () : userFunction =
  { ufTLID = tlid
  ; ufMetadata =
      { ufmName = B.newF "myFunc"
      ; ufmParameters = []
      ; ufmDescription = ""
      ; ufmReturnTipe = B.newF TStr
      ; ufmInfix = false }
  ; ufAST = expr }


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


let enteringHandler ?(module_ : string option = None) () : model =
  defaultModel ~cursorState:(fillingCS ()) ~handlers:[aHandler ~module_ ()] ()


let enteringEventNameHandler ?(module_ : string option = None) () : model =
  let handler = aHandler ~module_ () in
  let id =
    handler
    |> TL.asHandler
    |> deOption "should have created a handler"
    |> fun h -> B.toID h.spec.name
  in
  defaultModel ~cursorState:(fillingCS ~id ()) ~handlers:[handler] ()


let creatingOmni : model =
  { Defaults.defaultModel with
    cursorState = Entering (Creating {x = 0; y = 0})
  ; builtInFunctions = sampleFunctions }


(* AC targeting a tlid and pointer *)
let acFor ?(target = Some (defaultTLID, PExpr defaultExpr)) (m : model) :
    autocomplete =
  match m.cursorState with
  | Entering (Creating _) ->
      init m |> setTarget m None
  | Entering (Filling _) ->
      init m |> setTarget m target
  | _ ->
      init m |> setTarget m target


let itemPresent (aci : autocompleteItem) (ac : autocomplete) : bool =
  List.member ~value:aci ac.completions


let () =
  describe "autocomplete" (fun () ->
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
              expect (acFor m |> fun x -> x.index) |> toEqual (-1) ) ;
          test
            "pressing a letter from the selected entry keeps the entry selected"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::someOtherFunc"
                |> setQuery m "T"
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "Returning to empty unselects" (fun () ->
              expect
                (acFor m |> setQuery m "lis" |> setQuery m "" |> highlighted)
              |> toEqual None ) ;
          test "resetting the query refilters" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit::somefunc"
                |> setQuery m "Twit::some"
                |> selectDown
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "deprecated functions are removed" (fun () ->
              expect (acFor m |> setQuery m "deprecated" |> highlighted)
              |> toEqual None ) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect
                ( acFor m
                |> setQuery m "lis"
                |> (fun x -> x.completions)
                |> List.map ~f:asName )
              |> toEqual ["List::head"] ) ;
          test "search finds multiple results for prefix" (fun () ->
              expect
                ( acFor m
                |> setQuery m "twit::"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual
                   ["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"]
          ) ;
          test "search finds only prefixed" (fun () ->
              expect
                ( acFor m
                |> setQuery m "twit::y"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "search works anywhere in term" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Another"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "show results when the only option is the setQuery m" (fun () ->
              expect
                ( acFor m
                |> setQuery m "List::head"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName
                |> List.length )
              |> toEqual 1 ) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 2 ) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> selectDown
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 0 ) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> selectDown
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toEqual 2 ) ;
          test
            "scrolling loops the other way without going forward first"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit:"
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toEqual 1 ) ;
          test "scrolling backward works if we haven't searched yet" (fun () ->
              expect (acFor m |> selectUp |> selectUp |> fun x -> x.index)
              |> toBeGreaterThan 15 ) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( acFor m
                |> setQuery m "Twit"
                |> selectDown
                |> selectDown
                |> setQuery m "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual (-1) ) ;
          (* test "Filter by method signature for typed values" ( fun () ->
              expect
                ( acFor m
                |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.map ~f:asName
                |> Set.fromList
                |> (==) (Set.fromList ["List::head"]) )
              |> toEqual true ) ;

          test "Show allowed fields for objects" ( fun () ->
              expect
                ( acFor m
                |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing}
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.map ~f:asName
                |> Set.fromList
                |> (==) (Set.fromList ["Int::add", "+"]))
              |> toEqual true ) ;
           *)
          test "By default the list shows results" (fun () ->
              expect
                ( acFor m
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.length )
              |> not_
              |> toEqual 0 ) ;
          test
            "ordering = startsWith then case match then case insensitive match"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "withL"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual
                   [ "withLower"
                   ; "withlower"
                   ; "SomeModule::withLower"
                   ; "SomeOtherModule::withlower" ] ) ;
          test "typing literals works" (fun () ->
              expect
                ( acFor m
                |> setQuery m "21434234"
                |> selectDown
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "21434234") ) ;
          test
            "a specific bug where `+` is interpreted as an ACLiteral"
            (fun () ->
              expect
                ( acFor m
                |> setQuery m "+"
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "+") ) ;
          test "null works" (fun () ->
              expect (acFor m |> setQuery m "nu" |> highlighted)
              |> toEqual (Some (ACLiteral "null")) ) ;
          test "Ok works" (fun () ->
              expect (acFor m |> setQuery m "Ok" |> highlighted)
              |> toEqual (Some (ACConstructorName "Ok")) ) ;
          test "Error works" (fun () ->
              expect (acFor m |> setQuery m "Error" |> highlighted)
              |> toEqual (Some (ACConstructorName "Error")) ) ;
          test "true works" (fun () ->
              expect (acFor m |> setQuery m "tr" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "case insensitive true works" (fun () ->
              expect (acFor m |> setQuery m "tR" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "false works" (fun () ->
              expect (acFor m |> setQuery m "fa" |> highlighted)
              |> toEqual (Some (ACLiteral "false")) ) ;
          test "float literal works" (fun () ->
              expect (acFor m |> setQuery m "3.452" |> highlighted)
              |> toEqual (Some (ACLiteral "3.452")) ) ;
          test "if works" (fun () ->
              expect (acFor m |> setQuery m "if" |> highlighted)
              |> toEqual (Some (ACKeyword KIf)) ) ;
          test "let works" (fun () ->
              expect (acFor m |> setQuery m "let" |> highlighted)
              |> toEqual (Some (ACKeyword KLet)) ) ;
          test "Lambda works" (fun () ->
              expect (acFor m |> setQuery m "lambda" |> highlighted)
              |> toEqual (Some (ACKeyword KLambda)) ) ;
          test "http handlers have request" (fun () ->
              let module_ = Some "HTTP" in
              let m = enteringHandler ~module_ () in
              expect
                ( acFor m
                |> setQuery m "request"
                |> itemPresent (ACVariable "request") )
              |> toEqual true ) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let ac = acFor m in
                 [ ac
                   |> setQuery m "request"
                   |> itemPresent (ACVariable "request")
                 ; ac |> setQuery m "event" |> itemPresent (ACVariable "event")
                 ])
              |> toEqual [true; true] ) ;
          test "functions have DB names in the autocomplete" (fun () ->
              let blankid = ID "123" in
              let dbNameBlank = Blank blankid in
              let fntlid = TLID "fn123" in
              let fn =
                aFunction
                  ~tlid:fntlid
                  ~expr:
                    (B.newF
                       (FnCall (B.newF "DB::deleteAll", [dbNameBlank], NoRail)))
                  ()
              in
              let m =
                defaultModel
                  ~cursorState:(fillingCS ~tlid:fntlid ~id:blankid ())
                  ~dbs:[aDB ~tlid:(TLID "db123") ()]
                  ~userFunctions:[fn]
                  ()
              in
              let target = Some (fntlid, PExpr dbNameBlank) in
              let ac = acFor ~target m in
              let newM = {m with complete = ac} in
              expect (setQuery newM "" ac |> itemPresent (ACVariable "MyDB"))
              |> toEqual true ) ;
          test
            "autocomplete does not have slash when handler is not HTTP"
            (fun () ->
              let m = enteringEventNameHandler ~module_:(Some "HANDLER") () in
              expect
                ( acFor m
                |> setQuery m ""
                |> itemPresent (ACEventName "/")
                |> not )
              |> toEqual true ) ;
          test "autocomplete supports password type" (fun () ->
              let m = enteringDBType () in
              expect
                ( acFor m
                |> setQuery m "Pass"
                |> itemPresent (ACDBColType "Password")
                |> not )
              |> toEqual true ) ;
          () ) ;
      describe "filter" (fun () ->
          test "Cannot use DB variable when type of blank isn't TDB" (fun () ->
              let m =
                defaultModel
                  ~cursorState:(fillingCS ())
                  ~dbs:[aDB ~tlid:(TLID "db123") ()]
                  ()
              in
              let ac = acFor m in
              let _valid, invalid = filter m ac [ACVariable "MyDB"] "" in
              expect (List.member ~value:(ACVariable "MyDB") invalid)
              |> toEqual true ) ;
          () ) ;
      describe "omnibox completion" (fun () ->
          let m = creatingOmni in
          test "entering a DB name that used to be invalid works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "HTTP"
                |> itemPresent (ACOmniAction (NewDB (Some "HTTP"))) )
              |> toEqual true ) ;
          test "entering an invalid DB name works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m ":[]'/31234myDB[]"
                |> itemPresent (ACOmniAction (NewDB (Some "MyDB"))) )
              |> toEqual true ) ;
          test "entering a DB name works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "Mydbname"
                |> itemPresent (ACOmniAction (NewDB (Some "Mydbname"))) )
              |> toEqual true ) ;
          test "entering a short DB name works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "me"
                |> itemPresent (ACOmniAction (NewDB (Some "Me"))) )
              |> toEqual true ) ;
          test "db names can be multicase" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "MyDBnaMe"
                |> itemPresent (ACOmniAction (NewDB (Some "MyDBnaMe"))) )
              |> toEqual true ) ;
          test "alphabetical only DB names #1" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "dbname1234::"
                |> itemPresent (ACOmniAction (NewDB (Some "Dbname1234"))) )
              |> toEqual true ) ;
          test "alphabetical only DB names #2" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "db_name::"
                |> itemPresent (ACOmniAction (NewDB (Some "Db_name"))) )
              |> toEqual true ) ;
          test "add capital for DB names" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "mydbname"
                |> itemPresent (ACOmniAction (NewDB (Some "Mydbname"))) )
              |> toEqual true ) ;
          test "General HTTP handler" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "asdkkasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asdkkasd"))) )
              |> toEqual true ) ;
          test
            "can create handlers for spaces that are substrings of HTTP"
            (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "HTT"
                |> itemPresent (ACOmniAction (NewEventSpace "HTT")) )
              |> toEqual true ) ;
          test "can create routes #1 (base case)" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "/"
                |> itemPresent (ACOmniAction (NewHTTPHandler (Some "/"))) )
              |> toEqual true ) ;
          test "can create routes #2 (normal)" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "/asasdasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asasdasd"))) )
              |> toEqual true ) ;
          test "can create routes #3 (parameterized)" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "/user/:userid/card/:cardid"
                |> itemPresent
                     (ACOmniAction
                        (NewHTTPHandler (Some "/user/:userid/card/:cardid")))
                )
              |> toEqual true ) ;
          test "entering an invalid route name works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "[]/31234myDB[]"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/31234myDB"))) )
              |> toEqual true ) ;
          test "fix names for routes" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "asasdasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asasdasd"))) )
              |> toEqual true ) ;
          test "fix slashes for routes" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "//12//////345/6789//12/"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/12/345/6789/12")))
                )
              |> toEqual true ) ;
          test "create DB from route name" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "/route"
                |> itemPresent (ACOmniAction (NewDB (Some "Route"))) )
              |> toEqual true ) ;
          test "entering an invalid function name works" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m ":[]'/31234MyFn[]"
                |> itemPresent (ACOmniAction (NewFunction (Some "myFn"))) )
              |> toEqual true ) ;
          test "new handler option available by default" (fun () ->
              expect
                ( acFor ~target:None m
                |> itemPresent (ACOmniAction (NewHandler None)) )
              |> toEqual true ) ;
          test "new function option available by default" (fun () ->
              expect
                ( acFor ~target:None m
                |> itemPresent (ACOmniAction (NewFunction None)) )
              |> toEqual true ) ;
          test "new HTTP option available by default" (fun () ->
              expect
                ( acFor ~target:None m
                |> itemPresent (ACOmniAction (NewHTTPHandler None)) )
              |> toEqual true ) ;
          test "can create function with name from query" (fun () ->
              expect
                ( acFor ~target:None m
                |> setQuery m "myfunction"
                |> itemPresent (ACOmniAction (NewFunction (Some "myfunction")))
                )
              |> toEqual true ) ;
          () ) ;
      () ) ;
  ()
