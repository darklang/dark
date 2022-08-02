open Prelude
open Tester
open ViewBlankOr

let run = () => {
  describe("placeholdersDisplayed", () => {
    test("shows placeholders in user functions", () => {
      let ast = ProgramTypes.Expr.EBlank(gid())
      let tlFunc = TLFunc({
        body: FluidAST.ofExpr(ast),
        tlid: gtlid(),
        name: "",
        nameID: gid(),
        parameters: list{},
        description: "",
        returnType: DType.any,
        returnTypeID: gid(),
        infix: false,
      })

      let tl = tlFunc
      let vp: ViewUtils.viewProps = {
        tl: tl,
        astInfo: FluidTokenizer.ASTInfo.make(
          FluidTestData.defaultTestProps,
          FluidAST.ofExpr(ast),
          FluidTypes.State.default,
        ),
        cursorState: Deselected,
        fluidState: FluidTypes.State.default,
        tlid: gtlid(),
        isAdmin: false,
        hovering: None,
        ac: {
          completions: list{},
          allCompletions: list{},
          index: -1,
          value: "",
          prevValue: "",
          target: None,
          visible: true,
        },
        showEntry: false,
        showLivevalue: false,
        dbLocked: false,
        analysisStore: LoadableSuccess(ID.Map.empty),
        traces: list{},
        dbStats: Map.String.empty,
        functions: Functions.empty,
        executingFunctions: list{},
        tlTraceIDs: TLID.Dict.empty,
        testVariants: list{},
        handlerProp: None,
        canvasName: "",
        userContentHost: "",
        usedInRefs: list{},
        refersToRefs: list{},
        hoveringRefs: list{},
        avatarsList: list{},
        permission: Some(ReadWrite),
        workerStats: None,
        menuState: {isOpen: false},
        isExecuting: false,
        secretValues: list{},
        fnProps: {
          draggingParamIndex: None,
          dragOverSpaceIndex: None,
          justMovedParam: None,
        },
        showHandlerASTs: false,
      }

      expect(placeHolderFor(vp, ParamName)) |> toEqual("param name")
    })
    ()
  })
  ()
}
