open Prelude
open Tester
open ViewBlankOr

let run () =
  describe "placeholdersDisplayed" (fun () ->
      test "shows placeholders in user functions" (fun () ->
          let ast = FluidExpression.EBlank (gid ()) in
          let tlFunc =
            TLFunc
              { ufAST = ast
              ; ufTLID = gtlid ()
              ; ufMetadata =
                  { ufmName = Blank (gid ())
                  ; ufmParameters = []
                  ; ufmDescription = ""
                  ; ufmReturnTipe = Blank (gid ())
                  ; ufmInfix = false } }
          in
          let tl = tlFunc in
          let vs : ViewUtils.viewState =
            { tl
            ; cursorState = Deselected
            ; fluidState = Defaults.defaultFluidState
            ; tlid = gtlid ()
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
            ; analysisStore = LoadableSuccess StrDict.empty
            ; traces = []
            ; fns = []
            ; dbStats = StrDict.empty
            ; ufns = []
            ; executingFunctions = []
            ; tlTraceIDs = TLIDDict.empty
            ; testVariants = []
            ; featureFlags = StrDict.empty
            ; handlerProp = None
            ; canvasName = ""
            ; userContentHost = ""
            ; usedInRefs = []
            ; refersToRefs = []
            ; hoveringRefs = []
            ; avatarsList = []
            ; permission = Some ReadWrite
            ; workerStats = None
            ; tokens = []
            ; menuState = {isOpen = false}
            ; isExecuting = false
            ; fnSpace =
                { draggingParamIndex = None
                ; dragOverSpaceIndex = None
                ; justMovedParam = None } }
          in
          expect (placeHolderFor vs ParamName) |> toBe "param name") ;
      ()) ;
  ()
