open Tea
open! Porting
open Autocomplete
module B = Blank
open Prelude
open Test
open Types

let d s fs =
  describe s
    (List.indexedMap
       (fun i f ->
         test ("test " ^ string_of_int i) (fun _ -> Expect.true_ "" (f ())) )
       fs)

let sampleFunctions =
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
  |> List.map (fun (name, tipe) ->
         { name
         ; parameters=
             [ { name= "x"
               ; tipe
               ; block_args= []
               ; optional= false
               ; description= "" } ]
         ; returnTipe= TBool
         ; previewExecutionSafe= false
         ; description= ""
         ; infix= true
         ; deprecated= false } )

let debug msg ac =
  let _ = Debug.log msg (highlighted ac) in
  ac

type role = Admin | User

let isAdmin r = match r with Admin -> true | _ -> false

let createEntering role =
  let targetBlankID = gid () in
  let tlid = gtlid () in
  let spec =
    { module_= B.new_ ()
    ; name= B.new_ ()
    ; modifier= B.new_ ()
    ; types= {input= B.new_ (); output= B.new_ ()} }
  in
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

let createCreating role =
  let cursor = Entering (Creating {x= 0; y= 0}) in
  let default = Defaults.defaultModel in
  let m = {default with cursorState= cursor} in
  init sampleFunctions (isAdmin role) |> setTarget m None

let itemPresent aci ac = List.member aci (List.concat ac.completions)

let itemMissing aci ac = not (itemPresent aci ac)

let all =
  describe "autocomplete"
    [ d "sharedPrefix"
        [ (fun _ -> sharedPrefixList ["aaaab"; "aab"; "aaxb"] = "aa")
        ; (fun _ -> sharedPrefixList ["abcdd"; "abcdde"] = "abcdd")
        ; (fun _ -> sharedPrefixList ["abcdd"; "bcddee"] = "") ]
    ; d "queryWhenEntering"
        [ (fun _ -> createEntering User |> (fun x -> x.index) |> ( = ) (-1))
        ; (fun _ ->
            createEntering User |> selectDown |> selectDown |> selectDown
            |> selectDown |> selectDown |> setQuery "T" |> highlighted
            |> Option.map asName
            |> ( = ) (Some "Twit::someOtherFunc") )
        ; (fun _ ->
            createEntering User |> setQuery "lis" |> setQuery "" |> highlighted
            |> ( = ) None )
        ; (fun _ ->
            createEntering User |> setQuery "Twit::somefunc"
            |> setQuery "Twit::some" |> selectDown |> highlighted
            |> Option.map asName
            |> ( = ) (Some "Twit::someOtherFunc") )
        ; (fun _ ->
            createEntering User |> setQuery "lis"
            |> (fun x -> x.completions)
            |> List.concat |> List.map asName
            |> ( = ) ["List::head"] )
        ; (fun _ ->
            createEntering User |> setQuery "twit::"
            |> (fun x -> x.completions)
            |> List.concat |> List.filter isStaticItem |> List.map asName
            |> ( = )
                 ["Twit::somefunc"; "Twit::someOtherFunc"; "Twit::yetAnother"]
            )
        ; (fun _ ->
            createEntering User |> setQuery "twit::y"
            |> (fun x -> x.completions)
            |> List.concat |> List.filter isStaticItem |> List.map asName
            |> ( = ) ["Twit::yetAnother"] )
        ; (fun _ ->
            createEntering User |> setQuery "Another"
            |> (fun x -> x.completions)
            |> List.concat |> List.filter isStaticItem |> List.map asName
            |> ( = ) ["Twit::yetAnother"] )
        ; (fun _ ->
            createEntering User |> setQuery "List::head"
            |> (fun x -> x.completions)
            |> List.concat |> List.filter isStaticItem |> List.map asName
            |> List.length |> ( = ) 1 )
        ; (fun _ ->
            createEntering User |> setQuery "Twit" |> selectDown |> selectDown
            |> (fun x -> x.index)
            |> ( = ) 2 )
        ; (fun _ ->
            createEntering User |> setQuery "Twit:" |> selectDown |> selectDown
            |> selectDown
            |> (fun x -> x.index)
            |> ( = ) 0 )
        ; (fun _ ->
            createEntering User |> setQuery "Twit:" |> selectDown |> selectUp
            |> selectUp
            |> (fun x -> x.index)
            |> ( = ) 2 )
        ; (fun _ ->
            createEntering User |> setQuery "Twit:" |> selectUp |> selectUp
            |> (fun x -> x.index)
            |> ( = ) 1 )
        ; (fun _ ->
            createEntering User |> selectUp |> selectUp |> selectUp |> selectUp
            |> selectUp
            |> (fun x -> x.index)
            |> ( = ) 13 )
        ; (fun _ ->
            createEntering User |> setQuery "Twit" |> selectDown |> selectDown
            |> setQuery "Twit::1334xxx"
            |> (fun x -> x.index)
            |> ( = ) (-1) )
        ; (fun _ ->
            createEntering User |> setQuery ""
            |> (fun x -> x.completions)
            |> List.concat |> List.length |> ( <> ) 0 )
        ; (fun _ ->
            createEntering User |> setQuery "withL"
            |> (fun x -> x.completions)
            |> List.concat |> List.filter isStaticItem |> List.map asName
            |> ( = )
                 [ "withLower"
                 ; "withlower"
                 ; "SomeModule::withLower"
                 ; "SomeOtherModule::withlower" ] )
        ; (fun _ ->
            createEntering User |> setQuery "21434234" |> selectDown
            |> highlighted |> Option.map asName |> ( = ) (Some "21434234") )
        ; (fun _ ->
            createEntering User |> setQuery "+" |> highlighted
            |> Option.map asName |> ( = ) (Some "+") )
        ; (fun _ ->
            createEntering User |> setQuery "nu" |> highlighted
            |> ( = ) (Some (ACLiteral "null")) )
        ; (fun _ ->
            createEntering User |> setQuery "tr" |> highlighted
            |> ( = ) (Some (ACLiteral "true")) )
        ; (fun _ ->
            createEntering User |> setQuery "tR" |> highlighted
            |> ( = ) (Some (ACLiteral "true")) )
        ; (fun _ ->
            createEntering User |> setQuery "false" |> highlighted
            |> ( = ) (Some (ACLiteral "false")) )
        ; (fun _ ->
            createEntering User |> setQuery "3.452" |> highlighted
            |> ( = ) (Some (ACLiteral "3.452")) )
        ; (fun _ ->
            createEntering User |> setQuery "if" |> highlighted
            |> ( = ) (Some (ACKeyword KIf)) )
        ; (fun _ ->
            createEntering User |> setQuery "let" |> highlighted
            |> ( = ) (Some (ACKeyword KLet)) )
        ; (fun _ ->
            createEntering User |> setQuery "lambda" |> highlighted
            |> ( = ) (Some (ACKeyword KLambda)) ) ]
    ; d "queryWhenCreating"
        [ (fun _ ->
            createCreating User |> setQuery "Mydbname"
            |> itemPresent (ACOmniAction (NewDB "Mydbname")) )
        ; (fun _ ->
            createCreating User |> setQuery "MyDBnaMe"
            |> itemPresent (ACOmniAction (NewDB "MyDBnaMe")) )
        ; (fun _ ->
            createCreating User |> setQuery "dbname1234::"
            |> itemMissing (ACOmniAction (NewDB "dbname1234::")) )
        ; (fun _ ->
            createCreating User |> setQuery "db_name::"
            |> itemMissing (ACOmniAction (NewDB "db_name::")) )
        ; (fun _ ->
            createCreating User |> setQuery "mydbname"
            |> itemMissing (ACOmniAction (NewDB "mydbname")) )
        ; (fun _ ->
            createCreating User |> setQuery "asdkkasd"
            |> itemMissing (ACOmniAction NewHTTPHandler) )
        ; (fun _ ->
            createCreating User |> setQuery "HTT"
            |> itemPresent (ACOmniAction (NewEventSpace "HTT")) )
        ; (fun _ ->
            createCreating User |> setQuery "/"
            |> itemPresent (ACOmniAction (NewHTTPRoute "/")) )
        ; (fun _ ->
            createCreating User |> setQuery "/asasdasd"
            |> itemPresent (ACOmniAction (NewHTTPRoute "/asasdasd")) )
        ; (fun _ ->
            createCreating User |> itemPresent (ACOmniAction NewHandler) )
        ; (fun _ ->
            createCreating User
            |> itemPresent (ACOmniAction (NewFunction None)) )
        ; (fun _ ->
            createCreating User |> setQuery "myFunction"
            |> itemPresent (ACOmniAction (NewFunction (Some "myFunction"))) )
        ] ]
