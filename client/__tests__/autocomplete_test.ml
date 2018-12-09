open! Porting
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
  ; ("HTTP::options", TAny) ]
  |> List.map (fun (fnName, paramTipe) ->
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
         ; fnDeprecated = false } )


type role =
  | Admin
  | User

let isAdmin (r : role) : bool = match r with Admin -> true | _ -> false

let createEntering ?(module_ : string option = None) (role : role) :
    autocomplete =
  let module_ =
    match module_ with None -> B.new_ () | Some name -> B.newF name
  in
  let targetBlankID = gid () in
  let tlid = gtlid () in
  let spec = {module_; name = B.new_ (); modifier = B.new_ ()} in
  let toplevel =
    { id = tlid
    ; pos = {x = 0; y = 0}
    ; data = TLHandler {ast = Blank targetBlankID; spec; tlid} }
  in
  let cursor = Entering (Filling (tlid, targetBlankID)) in
  let default = Defaults.defaultModel in
  let m = {default with toplevels = [toplevel]; cursorState = cursor} in
  init sampleFunctions (isAdmin role)
  |> setTarget m (Some (tlid, PExpr (Blank targetBlankID)))


let createCreating (role : role) : autocomplete =
  let cursor = Entering (Creating {x = 0; y = 0}) in
  let default = Defaults.defaultModel in
  let m = {default with cursorState = cursor} in
  init sampleFunctions (isAdmin role) |> setTarget m None


let () =
  describe "autocomplete" (fun () ->
      describe "sharedPrefix" (fun () ->
          test "same character prefix" (fun () ->
              expect (sharedPrefixList ["aaaab"; "aab"; "aaxb"])
              |> toEqual "aa" ) ;
          test "different character prefix" (fun () ->
              expect (sharedPrefixList ["abcdd"; "abcdde"]) |> toEqual "abcdd"
          ) ;
          test "no common prefix" (fun () ->
              expect (sharedPrefixList ["abcdd"; "bdcdee"]) |> toEqual "" ) ) ;
      describe "queryWhenEntering" (fun () ->
          test "empty autocomplete doesn't highlight" (fun () ->
              expect (createEntering User |> fun x -> x.index) |> toEqual (-1)
          ) ;
          test
            "pressing a letter from the selected entry keeps the entry selected"
            (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit::someOtherFunc"
                |> setQuery "T"
                |> highlighted
                |> Option.map asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "Returning to empty unselects" (fun () ->
              expect
                ( createEntering User
                |> setQuery "lis"
                |> setQuery ""
                |> highlighted )
              |> toEqual None ) ;
          test "resetting the query refilters" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit::somefunc"
                |> setQuery "Twit::some"
                |> selectDown
                |> highlighted
                |> Option.map asName )
              |> toEqual (Some "Twit::someOtherFunc") ) ;
          test "lowercase search still finds uppercase results" (fun () ->
              expect
                ( createEntering User
                |> setQuery "lis"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.map asName )
              |> toEqual ["List::head"] ) ;
          test "search finds multiple results for prefix" (fun () ->
              expect
                ( createEntering User
                |> setQuery "twit::"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.filter isStaticItem
                |> List.map asName )
              |> toEqual
                   ["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"]
          ) ;
          test "search finds only prefixed" (fun () ->
              expect
                ( createEntering User
                |> setQuery "twit::y"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.filter isStaticItem
                |> List.map asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "search works anywhere in term" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Another"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.filter isStaticItem
                |> List.map asName )
              |> toEqual ["Twit::yetAnother"] ) ;
          test "show results when the only option is the setQuery" (fun () ->
              expect
                ( createEntering User
                |> setQuery "List::head"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.filter isStaticItem
                |> List.map asName
                |> List.length )
              |> toEqual 1 ) ;
          test "scrolling down a bit works" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit"
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 2 ) ;
          test "scrolling loops one way" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit:"
                |> selectDown
                |> selectDown
                |> selectDown
                |> fun x -> x.index )
              |> toEqual 0 ) ;
          test "scrolling loops the other way" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit:"
                |> selectDown
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toEqual 2 ) ;
          test
            "scrolling loops the other way without going forward first"
            (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit:"
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toEqual 1 ) ;
          test "scrolling backward works if we haven't searched yet" (fun () ->
              expect
                ( createEntering User
                |> selectUp
                |> selectUp
                |> fun x -> x.index )
              |> toBeGreaterThan 15 ) ;
          test "Don't highlight when the list is empty" (fun () ->
              expect
                ( createEntering User
                |> setQuery "Twit"
                |> selectDown
                |> selectDown
                |> setQuery "Twit::1334xxx"
                |> fun x -> x.index )
              |> toEqual (-1) ) ;
          (* -- Filter by method signature for typed values *)
          (* -- , \_ -> createEntering User *)
          (* -- |> forLiveValue {value="[]", tipe=TList,json="[]", exc=Nothing} *)
          (* -- |> setQuery "" *)
          (* -- |> .completions *)
          (* -- |> List.map asName *)
          (* -- |> Set.fromList *)
          (* -- |> (==) (Set.fromList ["List::head"]) *)
          (*  *)
          (* -- Show allowed fields for objects *)
          (* -- , \_ -> createEntering User *)
          (* -- |> forLiveValue {value="5", tipe=TInt, json="5", exc=Nothing} *)
          (* -- |> setQuery "" *)
          (* -- |> .completions *)
          (* -- |> List.map asName *)
          (* -- |> Set.fromList *)
          (* -- |> (==) (Set.fromList ["Int::add", "+"]) *)
          (*  *)
          test "By default the list shows results" (fun () ->
              expect
                ( createEntering User
                |> setQuery ""
                |> (fun x -> x.completions)
                |> List.concat
                |> List.length )
              |> not_
              |> toEqual 0 ) ;
          test
            "ordering = startsWith then case match then case insensitive match"
            (fun () ->
              expect
                ( createEntering User
                |> setQuery "withL"
                |> (fun x -> x.completions)
                |> List.concat
                |> List.filter isStaticItem
                |> List.map asName )
              |> toEqual
                   [ "withLower"
                   ; "withlower"
                   ; "SomeModule::withLower"
                   ; "SomeOtherModule::withlower" ] ) ;
          test "typing literals works" (fun () ->
              expect
                ( createEntering User
                |> setQuery "21434234"
                |> selectDown
                |> highlighted
                |> Option.map asName )
              |> toEqual (Some "21434234") ) ;
          test
            "a specific bug where `+` is interpreted as an ACLiteral"
            (fun () ->
              expect
                ( createEntering User
                |> setQuery "+"
                |> highlighted
                |> Option.map asName )
              |> toEqual (Some "+") ) ;
          test "null works" (fun () ->
              expect (createEntering User |> setQuery "nu" |> highlighted)
              |> toEqual (Some (ACLiteral "null")) ) ;
          test "true works" (fun () ->
              expect (createEntering User |> setQuery "tr" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "case insensitive true works" (fun () ->
              expect (createEntering User |> setQuery "tR" |> highlighted)
              |> toEqual (Some (ACLiteral "true")) ) ;
          test "false works" (fun () ->
              expect (createEntering User |> setQuery "fa" |> highlighted)
              |> toEqual (Some (ACLiteral "false")) ) ;
          test "float literal works" (fun () ->
              expect (createEntering User |> setQuery "3.452" |> highlighted)
              |> toEqual (Some (ACLiteral "3.452")) ) ;
          test "if works" (fun () ->
              expect (createEntering User |> setQuery "if" |> highlighted)
              |> toEqual (Some (ACKeyword KIf)) ) ;
          test "let works" (fun () ->
              expect (createEntering User |> setQuery "let" |> highlighted)
              |> toEqual (Some (ACKeyword KLet)) ) ;
          test "Lambda works" (fun () ->
              expect (createEntering User |> setQuery "lambda" |> highlighted)
              |> toEqual (Some (ACKeyword KLambda)) ) ) ;
      describe "queryWhenCreating" (fun () ->
          let itemPresent (aci : autocompleteItem) (ac : autocomplete) : bool =
            List.member aci (List.concat ac.completions)
          in
          let itemMissing (aci : autocompleteItem) (ac : autocomplete) : bool =
            not (itemPresent aci ac)
          in
          test "entering a DB name works" (fun () ->
              expect
                ( createCreating User
                |> setQuery "Mydbname"
                |> itemPresent (ACOmniAction (NewDB "Mydbname")) )
              |> toEqual true ) ;
          test "entering a DB name works" (fun () ->
              expect
                ( createCreating User
                |> setQuery "Mydbname"
                |> itemPresent (ACOmniAction (NewDB "Mydbname")) )
              |> toEqual true ) ;
          test "db names can be multicase" (fun () ->
              expect
                ( createCreating User
                |> setQuery "MyDBnaMe"
                |> itemPresent (ACOmniAction (NewDB "MyDBnaMe")) )
              |> toEqual true ) ;
          test "alphabetical only DB names #1" (fun () ->
              expect
                ( createCreating User
                |> setQuery "dbname1234::"
                |> itemMissing (ACOmniAction (NewDB "dbname1234::")) )
              |> toEqual true ) ;
          test "alphabetical only DB names #2" (fun () ->
              expect
                ( createCreating User
                |> setQuery "db_name::"
                |> itemMissing (ACOmniAction (NewDB "db_name::")) )
              |> toEqual true ) ;
          test "require capital for DB names" (fun () ->
              expect
                ( createCreating User
                |> setQuery "mydbname"
                |> itemMissing (ACOmniAction (NewDB "mydbname")) )
              |> toEqual true ) ;
          test "No HTTP handler in general" (fun () ->
              expect
                ( createCreating User
                |> setQuery "asdkkasd"
                |> itemMissing (ACOmniAction NewHTTPHandler) )
              |> toEqual true ) ;
          test
            "can create handlers for spaces that are substrings of HTTP"
            (fun () ->
              expect
                ( createCreating User
                |> setQuery "HTT"
                |> itemPresent (ACOmniAction (NewEventSpace "HTT")) )
              |> toEqual true ) ;
          test "can create routes #1" (fun () ->
              expect
                ( createCreating User
                |> setQuery "/"
                |> itemPresent (ACOmniAction (NewHTTPRoute "/")) )
              |> toEqual true ) ;
          test "can create routes #2" (fun () ->
              expect
                ( createCreating User
                |> setQuery "/asasdasd"
                |> itemPresent (ACOmniAction (NewHTTPRoute "/asasdasd")) )
              |> toEqual true ) ;
          test "new handler option available by default" (fun () ->
              expect
                (createCreating User |> itemPresent (ACOmniAction NewHandler))
              |> toEqual true ) ;
          test "new function option available by default" (fun () ->
              expect
                ( createCreating User
                |> itemPresent (ACOmniAction (NewFunction None)) )
              |> toEqual true ) ;
          test "can create function with name from query" (fun () ->
              expect
                ( createCreating User
                |> setQuery "myfunction"
                |> itemPresent (ACOmniAction (NewFunction (Some "myfunction")))
                )
              |> toEqual true ) ;
          test "http handlers have request" (fun () ->
              expect
                ( createEntering ~module_:(Some "HTTP") User
                |> setQuery "request"
                |> itemPresent (ACVariable "request") )
              |> toEqual true ) ;
          test "handlers with no route have request and event" (fun () ->
              expect
                (let handler = createEntering User in
                 [ handler
                   |> setQuery "request"
                   |> itemPresent (ACVariable "request")
                 ; handler
                   |> setQuery "event"
                   |> itemPresent (ACVariable "event") ])
              |> toEqual [true; true] ) ) ) ;
  ()
