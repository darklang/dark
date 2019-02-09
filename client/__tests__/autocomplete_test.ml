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
  ; ("Some::deprecated", TAny) ]
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


let tlid = gtlid ()

let blankID = gid ()

let enteringModel ?(module_ : string option = None) () =
  let module_ =
    match module_ with None -> B.new_ () | Some name -> B.newF name
  in
  let spec = {module_; name = B.new_ (); modifier = B.new_ ()} in
  let toplevel =
    { id = tlid
    ; pos = {x = 0; y = 0}
    ; data = TLHandler {ast = Blank blankID; spec; tlid} }
  in
  let cursor = Entering (Filling (tlid, blankID)) in
  let default = Defaults.defaultModel in
  { default with
    toplevels = [toplevel]
  ; cursorState = cursor
  ; builtInFunctions = sampleFunctions }


let createEntering ?(module_ : string option = None) () : autocomplete =
  let m = enteringModel ~module_ () in
  init m |> setTarget m (Some (tlid, PExpr (Blank blankID)))


let creatingModel =
  let cursor = Entering (Creating {x = 0; y = 0}) in
  let default = Defaults.defaultModel in
  {default with cursorState = cursor; builtInFunctions = sampleFunctions}


let createCreating () : autocomplete =
  let m = creatingModel in
  init m |> setTarget m None


let itemPresent (aci : autocompleteItem) (ac : autocomplete) : bool =
  List.member ~value:aci ac.completions


let () =
  describe "autocomplete" (fun () ->
      describe "queryWhenEntering" (fun () ->
          let m = enteringModel () in
          test "empty autocomplete doesn't highlight" (fun () ->
              expect (createEntering () |> fun x -> x.index) |> toEqual (-1) ) ;
          test
            "pressing a letter from the selected entry keeps the entry selected"
            (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Twit::someOtherFunc"
                |> setQuery m "T"
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "Returning to empty unselects" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "lis"
                |> setQuery m ""
                |> highlighted )
              |> toEqual None ) ;
          test "resetting the query refilters" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Twit::somefunc"
                |> setQuery m "Twit::some"
                |> selectDown
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "deprecated functions are removed" (fun () ->
              expect
                (createEntering () |> setQuery m "deprecated" |> highlighted)
              |> toEqual None ) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "lis"
                |> (fun x -> x.completions)
                |> List.map ~f:asName )
              |> toEqual ["List::head"] ) ;
          test "search finds multiple results for prefix" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "twit::"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual
                   ["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"]
          ) ;
          test "search finds only prefixed" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "twit::y"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "search works anywhere in term" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Another"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "show results when the only option is the setQuery m" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "List::head"
                |> (fun x -> x.completions)
                |> List.filter ~f:isStaticItem
                |> List.map ~f:asName
                |> List.length )
              |> toEqual 1 ) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Twit"
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 2 ) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Twit:"
                |> selectDown
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 0 ) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( createEntering ()
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
                ( createEntering ()
                |> setQuery m "Twit:"
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toEqual 1 ) ;
          test "scrolling backward works if we haven't searched yet" (fun () ->
              expect
                (createEntering () |> selectUp |> selectUp |> fun x -> x.index)
              |> toBeGreaterThan 15 ) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "Twit"
                |> selectDown
                |> selectDown
                |> setQuery m "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual (-1) ) ;
          (* test "Filter by method signature for typed values" ( fun () ->
              expect
                ( createEntering ()
                |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing}
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.map ~f:asName
                |> Set.fromList
                |> (==) (Set.fromList ["List::head"]) )
              |> toEqual true ) ;

          test "Show allowed fields for objects" ( fun () ->
              expect
                ( createEntering ()
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
                ( createEntering ()
                |> setQuery m ""
                |> (fun x -> x.completions)
                |> List.length )
              |> not_
              |> toEqual 0 ) ;
          test
            "ordering = startsWith then case match then case insensitive match"
            (fun () ->
              expect
                ( createEntering ()
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
                ( createEntering ()
                |> setQuery m "21434234"
                |> selectDown
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "21434234") ) ;
          test
            "a specific bug where `+` is interpreted as an ACLiteral"
            (fun () ->
              expect
                ( createEntering ()
                |> setQuery m "+"
                |> highlighted
                |> Option.map ~f:asName )
              |> toEqual (Some "+") ) ;
          test "null works" (fun () ->
              expect (createEntering () |> setQuery m "nu" |> highlighted)
              |> toEqual (Some (ACLiteral "null")) ) ;
          test "Ok works" (fun () ->
              expect (createEntering () |> setQuery m "Ok" |> highlighted)
              |> toEqual (Some (ACConstructorName "Ok")) ) ;
          test "Error works" (fun () ->
              expect (createEntering () |> setQuery m "Err" |> highlighted)
              |> toEqual (Some (ACConstructorName "Error")) ) ;
          test "true works" (fun () ->
              expect (createEntering () |> setQuery m "tr" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "case insensitive true works" (fun () ->
              expect (createEntering () |> setQuery m "tR" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "false works" (fun () ->
              expect (createEntering () |> setQuery m "fa" |> highlighted)
              |> toEqual (Some (ACLiteral "false")) ) ;
          test "float literal works" (fun () ->
              expect (createEntering () |> setQuery m "3.452" |> highlighted)
              |> toEqual (Some (ACLiteral "3.452")) ) ;
          test "if works" (fun () ->
              expect (createEntering () |> setQuery m "if" |> highlighted)
              |> toEqual (Some (ACKeyword KIf)) ) ;
          test "let works" (fun () ->
              expect (createEntering () |> setQuery m "let" |> highlighted)
              |> toEqual (Some (ACKeyword KLet)) ) ;
          test "Lambda works" (fun () ->
              expect (createEntering () |> setQuery m "lambda" |> highlighted)
              |> toEqual (Some (ACKeyword KLambda)) ) ;
          test "http handlers have request" (fun () ->
              let module_ = Some "HTTP" in
              let m = enteringModel ~module_ () in
              expect
                ( createEntering ~module_ ()
                |> setQuery m "request"
                |> itemPresent (ACVariable "request") )
              |> toEqual true ) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let handler = createEntering () in
                 [ handler
                   |> setQuery m "request"
                   |> itemPresent (ACVariable "request")
                 ; handler
                   |> setQuery m "event"
                   |> itemPresent (ACVariable "event") ])
              |> toEqual [true; true] ) ) ;
      describe "omnibox completion" (fun () ->
          let m = creatingModel in
          test "entering a DB name that used to be invalid works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "HTTP"
                |> itemPresent (ACOmniAction (NewDB (Some "HTTP"))) )
              |> toEqual true ) ;
          test "entering an invalid DB name works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m ":[]'/31234myDB[]"
                |> itemPresent (ACOmniAction (NewDB (Some "MyDB"))) )
              |> toEqual true ) ;
          test "entering a DB name works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "Mydbname"
                |> itemPresent (ACOmniAction (NewDB (Some "Mydbname"))) )
              |> toEqual true ) ;
          test "entering a short DB name works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "me"
                |> itemPresent (ACOmniAction (NewDB (Some "Me"))) )
              |> toEqual true ) ;
          test "db names can be multicase" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "MyDBnaMe"
                |> itemPresent (ACOmniAction (NewDB (Some "MyDBnaMe"))) )
              |> toEqual true ) ;
          test "alphabetical only DB names #1" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "dbname1234::"
                |> itemPresent (ACOmniAction (NewDB (Some "Dbname1234"))) )
              |> toEqual true ) ;
          test "alphabetical only DB names #2" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "db_name::"
                |> itemPresent (ACOmniAction (NewDB (Some "Db_name"))) )
              |> toEqual true ) ;
          test "add capital for DB names" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "mydbname"
                |> itemPresent (ACOmniAction (NewDB (Some "Mydbname"))) )
              |> toEqual true ) ;
          test "General HTTP handler" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "asdkkasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asdkkasd"))) )
              |> toEqual true ) ;
          test
            "can create handlers for spaces that are substrings of HTTP"
            (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "HTT"
                |> itemPresent (ACOmniAction (NewEventSpace "HTT")) )
              |> toEqual true ) ;
          test "can create routes #1 (base case)" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "/"
                |> itemPresent (ACOmniAction (NewHTTPHandler (Some "/"))) )
              |> toEqual true ) ;
          test "can create routes #2 (normal)" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "/asasdasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asasdasd"))) )
              |> toEqual true ) ;
          test "can create routes #3 (parameterized)" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "/user/:userid/card/:cardid"
                |> itemPresent
                     (ACOmniAction
                        (NewHTTPHandler (Some "/user/:userid/card/:cardid")))
                )
              |> toEqual true ) ;
          test "entering an invalid route name works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "[]/31234myDB[]"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/31234myDB"))) )
              |> toEqual true ) ;
          test "fix names for routes" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "asasdasd"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/asasdasd"))) )
              |> toEqual true ) ;
          test "fix slashes for routes" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "//12//////345/6789//12/"
                |> itemPresent
                     (ACOmniAction (NewHTTPHandler (Some "/12/345/6789/12")))
                )
              |> toEqual true ) ;
          test "create DB from route name" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "/route"
                |> itemPresent (ACOmniAction (NewDB (Some "Route"))) )
              |> toEqual true ) ;
          test "entering an invalid function name works" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m ":[]'/31234MyFn[]"
                |> itemPresent (ACOmniAction (NewFunction (Some "myFn"))) )
              |> toEqual true ) ;
          test "new handler option available by default" (fun () ->
              expect
                ( createCreating ()
                |> itemPresent (ACOmniAction (NewHandler None)) )
              |> toEqual true ) ;
          test "new function option available by default" (fun () ->
              expect
                ( createCreating ()
                |> itemPresent (ACOmniAction (NewFunction None)) )
              |> toEqual true ) ;
          test "new HTTP option available by default" (fun () ->
              expect
                ( createCreating ()
                |> itemPresent (ACOmniAction (NewHTTPHandler None)) )
              |> toEqual true ) ;
          test "can create function with name from query" (fun () ->
              expect
                ( createCreating ()
                |> setQuery m "myfunction"
                |> itemPresent (ACOmniAction (NewFunction (Some "myfunction")))
                )
              |> toEqual true ) ) ) ;
  ()
