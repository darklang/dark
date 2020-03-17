open Tc
open Tester
open FluidShortcuts
module FF = FeatureFlags

(** [testWrap name fn expected] tests FeatureFlag.wrap.
 *
 * Recall that FeatureFlag.wrap is called with [wrap ast id].
 *
 * The [fn] here is given the [id] and expected to return the [ast]
 * (as a FluidExpression to make life easier).
 *
 * That means that in order to target a specific expression to be wrapped, use
 * the passed [id] as the ID of that expression when constructing it. For
 * example, to wrap a simple string:
 *   (fun id -> FluidShortcuts.str ~id "a")
 *
 * The [expected] string is in eToTestcase format. (That is, using an
 * S-expression-like syntax with the FluidShortcuts helpers.)
 *
 * [testUnwrap] * works the same way, but has two expectations: the result when
 * keeping the old code and the result when keeping the new code. *)
let testWrap
    (name : string) (exprFn : ID.t -> FluidExpression.t) (expected : string) =
  test name (fun () ->
      let id = Shared.gid () in
      let ast = FluidAST.ofExpr (exprFn id) in
      let newAST = FF.wrap ast id in
      expect (FluidAST.toExpr newAST |> FluidPrinter.eToTestcase)
      |> toEqual expected)


let testUnwrap
    (name : string)
    (exprFn : ID.t -> FluidExpression.t)
    ~(keepOld : string)
    ~(keepNew : string) =
  test name (fun () ->
      let id = Shared.gid () in
      let ast = FluidAST.ofExpr (exprFn id) in
      let keepOldAST = FF.unwrap FF.KeepOld ast id |> FluidAST.toExpr in
      let keepNewAST = FF.unwrap FF.KeepNew ast id |> FluidAST.toExpr in
      expect (List.map [keepOldAST; keepNewAST] ~f:FluidPrinter.eToTestcase)
      |> toEqual [keepOld; keepNew])


let run () =
  describe "FeatureFlag.wrap" (fun () ->
      testWrap
        "wrapping a simple expression puts expression in the old code"
        (fun id -> str ~id "a")
        {|(ff (b) (str "a") (b))|} ;
      testWrap
        "wrapping a let puts the RHS in the old code"
        (fun id -> let' ~id "a" (int 1) (var "a"))
        {|(let' "a" (ff (b) (int 1) (b)) (var "a"))|} ;
      testWrap
        "does not wrap an expr inside FF condition"
        (fun id -> flag (bool ~id true) (str "old") (str "new"))
        {|(ff (bool true) (str "old") (str "new"))|} ;
      testWrap
        "does not wrap an expr inside FF oldCode"
        (fun id -> flag (bool true) (str ~id "old") (str "new"))
        {|(ff (bool true) (str "old") (str "new"))|} ;
      testWrap
        "does not wrap an expr inside FF oldCode"
        (fun id -> flag (bool true) (str "old") (str ~id "new"))
        {|(ff (bool true) (str "old") (str "new"))|}) ;
  describe "FeatureFlag.unwrap" (fun () ->
      testUnwrap
        "unwrapping a simple expression leaves old code"
        (fun id -> flag ~id (blank ()) (str "old") (str "new"))
        ~keepOld:{|(str "old")|}
        ~keepNew:{|(str "new")|} ;
      testUnwrap
        "unwrapping from within the old leaves the old code"
        (fun id -> flag (blank ()) (str ~id "old") (str "new"))
        ~keepOld:{|(str "old")|}
        ~keepNew:{|(str "new")|} ;
      testUnwrap
        "unwrapping from within the new code leaves the old code"
        (fun id -> flag (blank ()) (str "old") (str ~id "new"))
        ~keepOld:{|(str "old")|}
        ~keepNew:{|(str "new")|})
