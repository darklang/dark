open! Tc
open Prelude
open Types
open Jest
open Expect
open ViewBlankOr

let () =
  describe "placeholdersDisplayed" (fun () ->
      test "shows placeholders in user functions" (fun () ->
          let id = gid () in
          let ast =
            Blank.newF
              (FnCall
                 (F (gid (), "Int::add_v0"), [Blank id; Blank.new_ ()], NoRail))
          in
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
                { functions = []
                ; admin = false
                ; completions = []
                ; invalidCompletions = []
                ; allCompletions = []
                ; index = -1
                ; value = ""
                ; prevValue = ""
                ; target = None
                ; targetDval = None
                ; isCommandMode = false
                ; visible = true }
            ; showEntry = false
            ; showLivevalue = false
            ; dbLocked = false
            ; currentResults = {liveValues = StrDict.empty}
            ; traces = []
            ; analyses = StrDict.empty
            ; fns = []
            ; dbStats = StrDict.empty
            ; ufns = []
            ; relatedBlankOrs = []
            ; tooWide = false
            ; executingFunctions = []
            ; tlCursors = StrDict.empty
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
            ; liveValues = StrDict.empty
            ; pointers = [] }
          in
          expect (placeHolderFor vs id ParamName) |> toBe "param name" ) ;
      () ) ;
  ()
