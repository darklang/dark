open Tc
open Types

let entryID : string = "entry-box"

let leftButton : int = 0

let initialVPos : vPos = {vx = 475; vy = 200}

let centerPos : pos = {x = 475; y = 200}

let origin : pos = {x = 0; y = 0}

let focusCodePos : pos = {x = 475; y = 100}

let moveSize : int = 50

let pageHeight : int = 400

let pageWidth : int = 500

let defaultEditor : serializableEditor =
  { timersEnabled = true
  ; cursorState = Deselected
  ; lockedHandlers = []
  ; routingTableOpenDetails = StrSet.empty
  ; tlCursors = StrDict.empty
  ; featureFlags = StrDict.empty }


let defaultUrlState : urlState = {lastPos = Some {x = 0; y = 0}}

let defaultCanvasProps : canvasProps = {offset = origin; enablePan = true}

let defaultModel : model =
  { error = {message = None; showDetails = false}
  ; lastMsg = IgnoreMsg
  ; lastMod = NoChange (* this is awkward, but avoids circular deps *)
  ; complete =
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
  ; userFunctions = []
  ; deletedUserFunctions = []
  ; builtInFunctions = []
  ; currentPage = Architecture {x = 0; y = 0}
  ; hovering = []
  ; tests = []
  ; toplevels = []
  ; deletedToplevels = []
  ; analyses = StrDict.empty
  ; traces = StrDict.empty
  ; f404s = []
  ; unlockedDBs = []
  ; integrationTestState = NoIntegrationTest
  ; visibility = Native.PageVisibility.Visible (* partially saved in editor *)
  ; syncState = StrSet.empty
  ; urlState = defaultUrlState
  ; timersEnabled = true (* saved in editor *)
  ; cursorState = Deselected
  ; executingFunctions = []
  ; tlCursors = StrDict.empty
  ; featureFlags = StrDict.empty
  ; lockedHandlers = []
  ; canvasProps = defaultCanvasProps
  ; canvasName = "builtwithdark"
  ; userContentHost = "builtwithdark.com"
  ; environment = "none"
  ; csrfToken = "UNSET_CSRF"
  ; routingTableOpenDetails = StrSet.empty
  ; usedDBs = StrDict.empty
  ; usedFns = StrDict.empty
  ; staticAssets = []
  }
