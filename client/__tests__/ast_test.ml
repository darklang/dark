open Tester
open! Tc
open Types
open Prelude
open AST
open Fluid_test_data
module B = Blank

type ('a, 'b) transformation_test_result =
  | Pass
  | Fail of 'a * 'b

let completion =
  let fnParam (name : string) (t : tipe) ?(blockArgs = []) (opt : bool) :
      Types.parameter =
    { paramName = name
    ; paramTipe = t
    ; paramBlock_args = blockArgs
    ; paramOptional = opt
    ; paramDescription = "" }
  in
  { Defaults.defaultModel.complete with
    functions =
      [ { fnName = "Dict::map"
        ; fnParameters =
            [ fnParam "dict" TObj false
            ; fnParam "f" TBlock false ~blockArgs:["key"; "value"] ]
        ; fnReturnTipe = TObj
        ; fnDescription =
            "Iterates each `key` and `value` in Dictionary `dict` and mutates it according to the provided lambda"
        ; fnPreviewExecutionSafe = true
        ; fnDeprecated = false
        ; fnInfix = false } ] }


let run () =
  describe "ast" (fun () ->
      let id1 = ID "5" in
      let id2 = ID "10" in
      let id3 = ID "11" in
      let id4 = ID "12" in
      let id5 = ID "13" in
      let id6 = ID "14" in
      let id7 = ID "15" in
      test "lambda var is not free" (fun () ->
          expect
            (freeVariables
               (ELambda (id1, [(id2, "var")], EVariable (id3, "var"))))
          |> toEqual []) ;
      test "match pattern is not free" (fun () ->
          let e =
            EConstructor (id2, id3, "Just", [EVariable (id4, "request")])
          in
          let pats =
            [ ( FPConstructor
                  (id5, id1, "Just", [FPVariable (id6, id1, "anything")])
              , EVariable (id7, "anything") ) ]
          in
          expect (freeVariables (EMatch (id1, e, pats)))
          |> toEqual [(id4, "request")]) ;
      test "parent of a field is the expr" (fun () ->
          expect
            (let obj = var "obj" in
             let fieldID = gid () in
             let expr = EFieldAccess (gid (), obj, fieldID, "field") in
             let parent = findParentOfWithin fieldID expr in
             if parent = expr then Pass else Fail (parent, expr))
          |> toEqual Pass) ;
      test
        "variablesIn correctly identifies available vars in let RHS with incomplete LHS"
        (fun () ->
          let testId = ID "testme" in
          let inner = ELet (gid (), gid (), "", EBlank testId, E.newB ()) in
          let outer = ELet (gid (), gid (), "variable", int "4", inner) in
          let vars = variablesIn outer |> StrDict.get ~key:"testme" in
          let varsFor = vars |> Option.map ~f:(fun d -> StrDict.keys d) in
          expect varsFor |> toEqual (Some ["variable"])) ;
      test "variablesIn correctly gets id of latest let definition" (fun () ->
          let a0id = ID "a0id" in
          let a1id = ID "a1id" in
          let lastBlank = EBlank (ID "lastBlankid") in
          let ast =
            ELet
              ( gid ()
              , a0id
              , "a"
              , int "4"
              , ELet (gid (), a1id, "a", int "9", lastBlank) )
          in
          expect
            ( variablesIn ast
            |> StrDict.get ~key:"lastBlankid"
            |> Option.andThen ~f:(fun d -> StrDict.get ~key:"a" d) )
          |> toEqual (Some a1id)) ;
      ()) ;
  ()
