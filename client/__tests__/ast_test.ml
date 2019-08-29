open! Tc
open Types
open Jest
open Expect
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


let () =
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
          |> toEqual [] ) ;
      test "match pattern is not free" (fun () ->
          let e =
            F
              ( id2
              , Constructor (F (id3, "Just"), [F (id4, Variable "request")]) )
          in
          let pats =
            [ ( F (id5, PConstructor ("Just", [F (id6, PVariable "anything")]))
              , F (id7, Variable "anything") ) ]
          in
          expect (freeVariables (F (id1, Match (e, pats))))
          |> toEqual [(id4, "request")] ) ;
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
          |> toEqual Pass ) ;
      test
        "promoting a threaded FnCall by removing the Thread, re-adds the missing argument"
        (fun () ->
          expect
            (let threaded =
               B.newF
                 (Thread
                    [ B.new_ ()
                    ; B.new_ ()
                    ; F
                        ( ID "6"
                        , FnCall
                            (F (ID "6_name", "+"), [Blank (ID "5")], NoRail) )
                    ; B.new_ () ])
             in
             match closeThreads threaded with
             | F
                 ( ID "6"
                 , FnCall (F (_, "+"), [Blank _; Blank (ID "5")], NoRail) ) ->
                 Pass
             | r ->
                 Fail (threaded, r))
          |> toEqual Pass ) ;
      test
        "don't re-add the argument if it was already in the right place"
        (fun () ->
          expect
            (let fn =
               B.newF
                 (FnCall
                    ( B.newF "+"
                    , [B.newF (Value "3"); B.newF (Value "5")]
                    , NoRail ))
             in
             let open_ = B.newF (Thread [fn; B.new_ ()]) in
             let closed = closeThreads open_ in
             if closed = fn then Pass else Fail (fn, closed))
          |> toEqual Pass ) ;
      test "simple thread is closed properly" (fun () ->
          expect
            (let open_ = B.newF (Thread [B.newF (Value "3"); B.new_ ()]) in
             let closed = closeThreads open_ in
             match closed with
             | F (_, Value "3") ->
                 Pass
             | _ ->
                 Fail (open_, closed))
          |> toEqual Pass ) ;
      test "parent of a field is the expr" (fun () ->
          expect
            (let obj = B.newF (Variable "obj") in
             let fieldname = B.newF "field" in
             let expr = B.newF (FieldAccess (obj, fieldname)) in
             let parent = findParentOfWithin (B.toID fieldname) expr in
             if parent = expr then Pass else Fail (parent, expr))
          |> toEqual Pass ) ;
      test "usesRail returns true when at top" (fun () ->
          expect
            (let expr = B.newF (FnCall (B.newF "test", [], Rail)) in
             usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns true when deep" (fun () ->
          expect
            (let withRail = B.newF (FnCall (B.newF "test2", [], Rail)) in
             let l = B.newF (Let (B.newF "v", withRail, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual true ) ;
      test "usesRail returns false when norail" (fun () ->
          expect
            (let deep = B.newF (FnCall (B.newF "test2", [], NoRail)) in
             let l = B.newF (Let (B.newF "v", deep, B.new_ ())) in
             let expr = B.newF (FnCall (B.newF "test", [l], NoRail)) in
             usesRail expr)
          |> toEqual false ) ;
      test "parseAst completes lambda in blank Ast" (fun () ->
          expect
            (let initAst = B.new_ () in
             let finalAst =
               Entry.parseAst initAst completion (ACKeyword KLambda) "lambda"
             in
             match finalAst with
             | Some (F (_, Lambda ([F (_, "var")], Blank _))) ->
                 Pass
             | _ ->
                 Fail (initAst, finalAst))
          |> toEqual Pass ) ;
      test
        "parseAst completes lambda in function argument inside thread"
        (fun () ->
          (* This doesn't really work in non-fluid because so, instead the operation just returns a lambda with a single default "var" binding*)
          expect Pass |> toEqual Pass ) ;
      (* test
        "variablesIn correctly identifies available vars in let RHS with incomplete LHS"
        (fun () ->
          let testId = ID "testme" in
          let inner = B.newF (Let (B.new_ (), Blank testId, B.new_ ())) in
          let outer =
            B.newF (Let (B.newF "variable", B.newF (Value "4"), inner))
          in
          (* let variable = 4
           * let _ = _ (id: "testme") -- "variable" should be available
           * _
           *)
          let vars = variablesIn outer in
          expect (StrDict.get ~key:"testme" vars)
          |> toEqual (Some [("variable", Some (DInt 4))]) ) ; *)
      () ) ;
  ()
