open Prelude
open Tester
open ViewBlankOr

let run () =
  describe "placeholdersDisplayed" (fun () ->
      test "shows placeholders in user functions" (fun () ->
          let ast = FluidExpression.EBlank (gid ()) in
          let tlFunc =
            TLFunc
              { ufAST = FluidAST.ofExpr ast
              ; ufTLID = gtlid ()
              ; ufMetadata =
                  { ufmName = Blank (gid ())
                  ; ufmParameters = []
                  ; ufmDescription = ""
                  ; ufmReturnTipe = Blank (gid ())
                  ; ufmInfix = false } }
          in
          let tl = tlFunc in
          let vp : ViewUtils.viewProps =
            { tl
            ; astInfo =
                FluidTokenizer.ASTInfo.make
                  Fluid_test_data.defaultTestProps
                  (FluidAST.ofExpr ast)
                  Defaults.defaultFluidState
            ; cursorState = Deselected
            ; fluidState = Defaults.defaultFluidState
            ; tlid = gtlid ()
            ; isAdmin = false
            ; hovering = None
            ; ac =
                { admin = false
                ; completions = []
                ; allCompletions = []
                ; index = -1
                ; value = ""
                ; prevValue = ""
                ; target = None
                ; visible = true }
            ; showEntry = false
            ; showLivevalue = false
            ; dbLocked = false
            ; analysisStore = LoadableSuccess Belt.Map.String.empty
            ; traces = []
            ; dbStats = Map.String.empty
            ; functions = Functions.empty
            ; executingFunctions = []
            ; tlTraceIDs = TLIDDict.empty
            ; testVariants = []
            ; featureFlags = Map.String.empty
            ; handlerProp = None
            ; canvasName = ""
            ; userContentHost = ""
            ; usedInRefs = []
            ; refersToRefs = []
            ; hoveringRefs = []
            ; avatarsList = []
            ; permission = Some ReadWrite
            ; workerStats = None
            ; menuState = {isOpen = false}
            ; isExecuting = false
            ; secretValues = []
            ; fnProps =
                { draggingParamIndex = None
                ; dragOverSpaceIndex = None
                ; justMovedParam = None }
            ; showHandlerASTs = false }
          in
          expect (placeHolderFor vp ParamName) |> toEqual "param name") ;
      ()) ;
  ()
