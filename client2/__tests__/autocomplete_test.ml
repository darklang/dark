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
  ; ("HTTP::options", TAny)
  ]
  |> List.map
    (fun (fnName, paramTipe) ->
      { fnName
      ; fnParameters=
        [ { paramName= "x"
          ; paramTipe
          ; paramBlock_args= []
          ; paramOptional= false
          ; paramDescription= ""
          }
        ]
      ; fnReturnTipe= TBool
      ; fnPreviewExecutionSafe= false
      ; fnDescription= ""
      ; fnInfix= true
      ; fnDeprecated= false }
    )

type role = Admin | User

let isAdmin (r : role) : bool =
  match r with
    Admin -> true
  | _ -> false

let createEntering (role : role) : autocomplete =
  let targetBlankID = gid () in
  let tlid = gtlid () in
  let spec = {module_= B.new_ (); name= B.new_ (); modifier= B.new_ ()} in
  let toplevel =
    { id= tlid
    ; pos= {x= 0; y= 0}
    ; data= TLHandler {ast= Blank targetBlankID; spec; tlid} }
  in
  let cursor = Entering (Filling (tlid, targetBlankID)) in
  let default = Defaults.defaultModel in
  let m = {default with toplevels= [toplevel]; cursorState= cursor} in
  init sampleFunctions (isAdmin role)
  |> setTarget m (Some (tlid, PExpr (Blank targetBlankID)))

let createCreating (role : role) : autocomplete =
  let cursor = Entering (Creating {x= 0; y= 0}) in
  let default = Defaults.defaultModel in
  let m = {default with cursorState= cursor} in
  init sampleFunctions (isAdmin role) |> setTarget m None

let () =
  describe "autocomplete" (fun () ->
    describe "sharedPrefix" (fun () ->
      test "same character prefix" (fun () ->
        expect
          (sharedPrefixList ["aaaab"; "aab"; "aaxb"])
        |> toEqual "aa"
      );
      test "different character prefix" (fun () ->
        expect
          (sharedPrefixList ["abcdd"; "abcdde"])
        |> toEqual "abcdd"
      );
      test "no common prefix" (fun () ->
        expect
          (sharedPrefixList ["abcdd"; "bdcdee"])
        |> toEqual ""
      );
    );

    describe "queryWhenEntering" (fun () ->
      test "empty autocomplete doesn't highlight" (fun () ->
        expect
          (createEntering User |> (fun x -> x.index))
        |> toEqual (-1)
      );
      test "pressing a letter from the selected entry keeps the entry selected" (fun () ->
        expect
          begin
            createEntering User
            |> selectDown
            |> selectDown
            |> selectDown
            |> selectDown
            |> selectDown
            |> selectDown
            |> selectDown
            |> selectDown
            |> setQuery "T"
            |> highlighted
            |> Option.map asName
          end
        |> toEqual (Some "Twit::someOtherFunc")
      );
      test "Returning to empty unselects" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "lis"
            |> setQuery ""
            |> highlighted
          end
        |> toEqual (None)
      );
      test "resetting the query refilters" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit::somefunc"
            |> setQuery "Twit::some"
            |> selectDown
            |> highlighted
            |> Option.map asName
          end
        |> toEqual (Some "Twit::someOtherFunc")
      );
      test "lowercase search still finds uppercase results" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "lis"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.map asName
          end
        |> toEqual (["List::head"])
      );
      test "search finds multiple results for prefix" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "twit::"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.filter isStaticItem
            |> List.map asName
          end
        |> toEqual (["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"])
      );
      test "search finds only prefixed" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "twit::y"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.filter isStaticItem
            |> List.map asName
          end
        |> toEqual ["Twit::yetAnother"]
      );
      test "search works anywhere in term" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Another"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.filter isStaticItem
            |> List.map asName
          end
        |> toEqual ["Twit::yetAnother"]
      );
      test "show results when the only option is the setQuery" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "List::head"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.filter isStaticItem
            |> List.map asName
            |> List.length
          end
        |> toEqual 1
      );
      test "scrolling down a bit works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit"
            |> selectDown
            |> selectDown
            |> (fun x -> x.index)
          end
        |> toEqual 2
      );
      test "scrolling loops one way" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit:"
            |> selectDown
            |> selectDown
            |> selectDown
            |> (fun x -> x.index)
          end
        |> toEqual 0
      );
      test "scrolling loops the other way" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit:"
            |> selectDown
            |> selectUp
            |> selectUp
            |> (fun x -> x.index)
          end
        |> toEqual 2
      );
      test "scrolling loops the other way without going forward first" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit:"
            |> selectUp
            |> selectUp
            |> (fun x -> x.index)
          end
        |> toEqual 1
      );
      test "scrolling backward works if we haven't searched yet" (fun () ->
        expect
          begin
            createEntering User
            |> selectUp
            |> selectUp
            |> selectUp
            |> selectUp
            |> selectUp
            |> selectUp
            |> (fun x -> x.index)
          end
        |> toEqual 15
      );
      test "Don't highlight when the list is empty" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "Twit"
            |> selectDown
            |> selectDown
            |> setQuery "Twit::1334xxx"
            |> (fun x -> x.index)
          end
        |> toEqual (-1)
      );

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
          begin
            createEntering User
            |> setQuery ""
            |> (fun x -> x.completions)
            |> List.concat
            |> List.length
          end
        |> not_
        |> toEqual (0)
      );
      test "ordering = startsWith then case match then case insensitive match" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "withL"
            |> (fun x -> x.completions)
            |> List.concat
            |> List.filter isStaticItem
            |> List.map asName
          end
        |> toEqual [ "withLower"
                   ; "withlower"
                   ; "SomeModule::withLower"
                   ; "SomeOtherModule::withlower"
                   ]
      );
      test "typing literals works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "21434234"
            |> selectDown
            |> highlighted
            |> Option.map asName
          end
        |> toEqual (Some "21434234")
      );
      test "a specific bug where `+` is interpreted as an ACLiteral" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "+"
            |> highlighted
            |> Option.map asName
          end
        |> toEqual (Some "+")
      );
      test "null works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "nu"
            |> highlighted
          end
        |> toEqual (Some (ACLiteral "null"))
      );
      test "true works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "tr"
            |> highlighted
          end
        |> toEqual (Some (ACLiteral "true"))
      );
      test "case insensitive true works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "tR"
            |> highlighted
          end
        |> toEqual (Some (ACLiteral "true"))
      );
      test "false works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "fa"
            |> highlighted
          end
        |> toEqual (Some (ACLiteral "false"))
      );
      test "float literal works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "3.452"
            |> highlighted
          end
        |> toEqual (Some (ACLiteral "3.452"))
      );
      test "if works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "if"
            |> highlighted
          end
        |> toEqual (Some (ACKeyword KIf))
      );
      test "let works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "let"
            |> highlighted
          end
        |> toEqual (Some (ACKeyword KLet))
      );
      test "Lambda works" (fun () ->
        expect
          begin
            createEntering User
            |> setQuery "lambda"
            |> highlighted
          end
        |> toEqual (Some (ACKeyword KLambda))
      );
    );

    describe "queryWhenCreating" (fun () ->
      let itemPresent (aci : autocompleteItem) (ac : autocomplete) : bool =
        List.member aci (List.concat ac.completions)
      in

      let itemMissing (aci : autocompleteItem) (ac : autocomplete) : bool =
        not (itemPresent aci ac)
      in

      test "entering a DB name works" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "Mydbname"
            |> itemPresent (ACOmniAction (NewDB "Mydbname"))
          end
        |> toEqual true
      );
      test "entering a DB name works" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "Mydbname"
            |> itemPresent (ACOmniAction (NewDB "Mydbname"))
          end
        |> toEqual true
      );
      test "db names can be multicase" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "MyDBnaMe"
            |> itemPresent (ACOmniAction (NewDB "MyDBnaMe"))
          end
        |> toEqual true
      );
      test "alphabetical only DB names #1" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "dbname1234::"
            |> itemMissing (ACOmniAction (NewDB "dbname1234::"))
          end
        |> toEqual true
      );
      test "alphabetical only DB names #2" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "db_name::"
            |> itemMissing (ACOmniAction (NewDB "db_name::"))
          end
        |> toEqual true
      );
      test "require capital for DB names" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "mydbname"
            |> itemMissing (ACOmniAction (NewDB "mydbname"))
          end
        |> toEqual true
      );
      test "No HTTP handler in general" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "asdkkasd"
            |> itemMissing (ACOmniAction NewHTTPHandler)
          end
        |> toEqual true
      );
      test "can create handlers for spaces that are substrings of HTTP" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "HTT"
            |> itemPresent (ACOmniAction (NewEventSpace "HTT"))
          end
        |> toEqual true
      );
      test "can create routes #1" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "/"
            |> itemPresent (ACOmniAction (NewHTTPRoute "/"))
          end
        |> toEqual true
      );
      test "can create routes #2" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "/asasdasd"
            |> itemPresent (ACOmniAction (NewHTTPRoute "/asasdasd"))
          end
        |> toEqual true
      );
      test "new handler option available by default" (fun () ->
        expect
          begin
            createCreating User
            |> itemPresent (ACOmniAction NewHandler)
          end
        |> toEqual true
      );
      test "new function option available by default" (fun () ->
        expect
          begin
            createCreating User
            |> itemPresent (ACOmniAction (NewFunction None))
          end
        |> toEqual true
      );
      test "can create function with name from query" (fun () ->
        expect
          begin
            createCreating User
            |> setQuery "myfunction"
            |> itemPresent (ACOmniAction (NewFunction (Some "myfunction")))
          end
        |> toEqual true
      );

    );
  );
  ()
