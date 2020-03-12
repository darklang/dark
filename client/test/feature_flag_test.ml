open Tester
module F = FluidShortcuts

(** [testWrap name fn expected] tests FeatureFlag.wrap
 *
 * Recal that FeatureFlag.wrap is called with [wrap ast id]. The [fn] expected
 * here is given the [id] and expected to return the [ast] (as a
 * FluidExpression to make life easier).
 *
 * That means that in order to target a specific expression to be wrapped, use
 * the passed [id] as the ID of that expression when constructing it. For
 * example, to wrap a simple string:
 *   (fun id -> FluidShortcuts.str ~id "a")
 *
 * The [expected] string is in eToTestcase format. (That is, using an
 * S-expression-like syntax with the FluidShortcuts helpers.)
 *)
let testWrap
    (name : string) (fn : ID.t -> FluidExpression.t) (expected : string) =
  test name (fun () ->
      let id = Shared.gid () in
      let ast = FluidAST.ofExpr (fn id) in
      let newAST = FeatureFlags.wrap ast id in
      expect (FluidAST.toExpr newAST |> FluidPrinter.eToTestcase)
      |> toEqual expected)


let run () =
  describe "FeatureFlag.wrap" (fun () ->
      testWrap
        "wrapping a simple expression puts expression in the old code"
        (fun id -> F.str ~id "a")
        {|(ff (b) (str "a") (b))|} ;
      testWrap
        "wrapping a let puts the RHS in the old code"
        (fun id -> F.let' ~id "a" (F.int 1) (F.var "a"))
        {|(let' "a" (ff (b) (int 1) (b)) (var "a"))|} ;
      testWrap
        "does not wrap an expr inside FF condition"
        (fun id -> F.flag (F.bool ~id true) (F.str "old") (F.str "new"))
        {|(ff (bool true) (str "old") (str "new"))|} ;
      testWrap
        "does not wrap an expr inside FF oldCode"
        (fun id -> F.flag (F.bool true) (F.str ~id "old") (F.str "new"))
        {|(ff (bool true) (str "old") (str "new"))|} ;
      testWrap
        "does not wrap an expr inside FF oldCode"
        (fun id -> F.flag (F.bool true) (F.str "old") (F.str ~id "new"))
        {|(ff (bool true) (str "old") (str "new"))|})
