open Tester
open! Tc
open Types
open AST
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
               (F (id1, Lambda ([F (id2, "var")], F (id3, Variable "var")))))
          |> toEqual []) ;
      test "match pattern is not free" (fun () ->
          let e =
            F (id2, Constructor (F (id3, "Just"), [F (id4, Variable "request")]))
          in
          let pats =
            [ ( F (id5, PConstructor ("Just", [F (id6, PVariable "anything")]))
              , F (id7, Variable "anything") ) ]
          in
          expect (freeVariables (F (id1, Match (e, pats))))
          |> toEqual [(id4, "request")]) ;
      test "replacing a function in a thread works" (fun () ->
          expect
            (let replacement =
               B.newF (FnCall (B.newF "+", [B.new_ (); B.new_ ()], NoRail))
             in
             let orig = B.new_ () in
             let result =
               replace
                 (PExpr orig)
                 (PExpr replacement)
                 (B.newF (Thread [orig; B.new_ ()]))
             in
             match result with
             | F (_, Thread [r; _]) ->
                 if r = replacement then Pass else Fail (orig, result)
             | _ ->
                 Fail (orig, result))
          |> toEqual Pass) ;
      test "parent of a field is the expr" (fun () ->
          expect
            (let obj = B.newF (Variable "obj") in
             let fieldname = B.newF "field" in
             let expr = B.newF (FieldAccess (obj, fieldname)) in
             let parent = findParentOfWithin (B.toID fieldname) expr in
             if parent = expr then Pass else Fail (parent, expr))
          |> toEqual Pass) ;
      test "usesRail returns true when at top" (fun () ->
          expect
            (let expr = B.newF (FnCall (B.newF "test", [], Rail)) in
             usesRail expr)
          |> toEqual true) ;
      test "usesRail returns true when deep" (fun () ->
          expect
            (let withRail = B.newF (FnCall (B.newF "test2", [], Rail)) in
             let l = B.newF (Let (B.newF "v", withRail, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual true) ;
      test "usesRail returns false when norail" (fun () ->
          expect
            (let deep = B.newF (FnCall (B.newF "test2", [], NoRail)) in
             let l = B.newF (Let (B.newF "v", deep, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual false) ;
      test
        "variablesIn correctly identifies available vars in let RHS with incomplete LHS"
        (fun () ->
          let testId = ID "testme" in
          let inner = B.newF (Let (B.new_ (), Blank testId, B.new_ ())) in
          let outer =
            B.newF (Let (B.newF "variable", B.newF (Value "4"), inner))
          in
          let vars = variablesIn outer |> StrDict.get ~key:"testme" in
          let varsFor = vars |> Option.map ~f:(fun d -> StrDict.keys d) in
          expect varsFor |> toEqual (Some ["variable"])) ;
      test "variablesIn correctly gets id of latest let definition" (fun () ->
          let a0id = ID "a0id" in
          let a0def, a0assign = (F (a0id, "a"), B.newF (Value "4")) in
          let a1id = ID "a1id" in
          let a1def, a1assign = (F (a1id, "a"), B.newF (Value "9")) in
          let lastBlank = Blank (ID "lastBlankid") in
          let ast =
            B.newF
              (Let (a0def, a0assign, B.newF (Let (a1def, a1assign, lastBlank))))
          in
          expect
            ( variablesIn ast
            |> StrDict.get ~key:"lastBlankid"
            |> Option.andThen ~f:(fun d -> StrDict.get ~key:"a" d) )
          |> toEqual (Some a1id)) ;
      ()) ;
  ()
