open Prelude

// Dark
module AC = Autocomplete
module B = BlankOr
module P = Pointer
module RT = Runtime
module TL = Toplevel
module Dom = Webapi.Dom

type modification = AppTypes.modification
type model = AppTypes.model
module Mod = AppTypes.Modification
module Msg = AppTypes.Msg

let openOmnibox = (~openAt: option<Pos.t>=None, ()): modification => OpenOmnibox(openAt)

// ---------------------
// Focus
// ---------------------

// selection
type range = {
  @meth
  "setStart": (Web_node.t, int) => unit,
  @meth
  "setEnd": (Web_node.t, int) => unit,
  @get
  "startContainer": Web_node.t,
}

@val @scope("document") external createRange: unit => range = "createRange"

type selection = {
  @meth
  "toString": unit => string,
  @meth
  "removeAllRanges": unit => unit,
  @meth
  "addRange": range => unit,
  @get
  "anchorOffset": int,
  @get
  "focusOffset": int,
  @get
  "anchorNode": Web_node.t,
  @meth
  "getRangeAt": int => range,
}

@ocaml.doc(" [findFirstAncestorWithClass className node] returns the first ancestor of
  * [node] (including self) that has a class of [className] ")
let rec findFirstAncestorWithClass = (className: string, node: Dom.Node.t): option<Dom.Node.t> =>
  Dom.Element.ofNode(node)
  |> Option.andThen(~f=el =>
    if el->Dom.Element.classList->Dom.DomTokenList.contains(className) {
      Some(node)
    } else {
      None
    }
  )
  |> Option.orElseLazy(_ =>
    node |> Dom.Node.parentNode |> Option.andThen(~f=findFirstAncestorWithClass(className))
  )

@ocaml.doc(" [preorderWalkUntil f node] recurses through all children of [node], calling
  * [f] with each Node before recursing. If [f] returns false, the walk stops. ")
let preorderWalkUntil = (~f: Dom.Node.t => bool, node: Dom.Node.t): unit => {
  module Node = Dom.Node
  let rec walk = (~f: Node.t => bool, n: Node.t): bool =>
    if f(n) {
      let continue = Node.firstChild(n) |> Option.map(~f=walk(~f)) |> Option.unwrap(~default=true)

      if continue {
        Node.nextSibling(n) |> Option.map(~f=walk(~f)) |> Option.unwrap(~default=true)
      } else {
        false
      }
    } else {
      false
    }

  Node.firstChild(node)
  |> Option.map(~f=walk(~f))
  |> recoverOption("could not find child element")
  |> ignore
}

@ocaml.doc(" getFluidSelectionRange returns the [begin, end] indices of the last
  * selection/caret placement within a fluid editor.
  *
  * If there has been no selection/caret placement or if the selected nodes are
  * not part of a fluid editor, returns None.
  *
  * [begin] may be greater than, less than, or equal to [end], depending on the
  * selection direction. If there is no selection but rather a caret positior,
  * then [begin] == [end].
  *
  * This function works by first finding the fluid-editor div that the
  * selection starts in, then iterating through all its children nodes until
  * it finds both the selection start and finish nodes (anchor and focus,
  * respectively). Each non-matching node that is passed by increments a
  * cursor, which is used to calculate the absolute 0-based index from the
  * beginning of the editor. ")
let getFluidSelectionRange = (): option<(int, int)> => {
  module Node = Dom.Node
  module Window = Dom.Window
  module Selection = Dom.Selection
  Dom.window
  ->Window.getSelection
  ->Option.andThen(~f=sel =>
    Option.andThen2(Selection.anchorNode(sel), Selection.focusNode(sel), ~f=(
      anchorNode,
      focusNode,
    ) =>
      findFirstAncestorWithClass("fluid-editor", anchorNode)
      |> recoverOption("could not find fluid-editor")
      |> Option.andThen(~f=editor => {
        let cursor = ref(0)
        let (anchorIdx, focusIdx) = (ref(None), ref(None))
        preorderWalkUntil(editor, ~f=node => {
          if anchorNode === node {
            anchorIdx := Some(cursor.contents)
          }
          if focusNode === node {
            focusIdx := Some(cursor.contents)
          }
          /* If node is not a leaf, then advance cursor. This is
           * probably a span or other container element. We'll see the
           * actual text node later, and we don't want to double-count
           * the textContent. */
          if !(Node.firstChild(node) |> Option.is_some) {
            cursor := cursor.contents + (node |> Node.textContent |> String.length)
          }
          let have_both = Option.pair(anchorIdx.contents, focusIdx.contents) |> Option.is_some

          !have_both
        })
        let anchorOffset = sel |> Selection.anchorOffset
        let focusOffset = sel |> Selection.focusOffset
        Option.map2(anchorIdx.contents, focusIdx.contents, ~f=(anchor, focus) => (
          anchor + anchorOffset,
          focus + focusOffset,
        ))
      })
    )
  )
}

let getFluidCaretPos = (): option<int> =>
  switch getFluidSelectionRange() {
  | Some(selStart, selEnd) =>
    if selStart == selEnd {
      Some(selEnd)
    } else {
      // Should we change this to return None in selection cases?
      Some(selEnd)
    }
  | None => None
  }

@ocaml.doc(" setFluidSelectionRange([beginIdx, endIdx]) attempts to select the passed
  * region in the currently selected fluid editor, if there is one.
  * If beginIdx == endIdx, it sets the caret position (0-width selection).
  *
  * This function assumes we never want to place the selection within a
  * nested DOM node (it crawls siblings).
  *
  * See getFluidSelectionRange for the counterpart. Note that it is not
  * strictly symmetrical with it, so there might be future edge-cases. ")
let setFluidSelectionRange = (beginIdx: int, endIdx: int): unit => {
  module Node = Dom.Node
  module Element = Dom.Element
  module Window = Dom.Window
  module Document = Dom.Document
  module Selection = Dom.Selection
  module NodeList = Dom.NodeList
  let clamp = (min: int, max: int, n: int) =>
    if n < min {
      min
    } else if n > max {
      max
    } else {
      n
    }

  Dom.document->Document.querySelector(".selected #active-editor")
  |> recoverOption(
    ~sendToRollbar=false,
    "setFluidSelectionRange querySelector failed to find #active-editor",
  )
  |> Option.andThen(~f=editor => {
    let maxChars = editor |> Element.textContent |> String.length
    let anchorBound = beginIdx |> clamp(0, maxChars)
    let focusBound = endIdx |> clamp(0, maxChars)
    let childNodes = Element.childNodes(editor) |> NodeList.toArray
    let findNodeAndOffset = (bound: int): (option<Node.t>, int) => {
      let offset = ref(bound)
      childNodes
      |> Array.find(~f=child => {
        let nodeLen =
          child
          |> /* First child is the text node of the span we use in the
           * editor. Nodes can also have a dropdown which we want to
           * avoid in our calculations. */
          Node.firstChild
          |> Option.map(~f=Node.textContent)
          |> Option.map(~f=String.length)
          |> Option.unwrap(~default=0)

        if offset.contents <= nodeLen {
          true
        } else {
          offset := offset.contents - nodeLen
          false
        }
      })
      |> Option.andThen(~f=Node.firstChild)
      |> (n => (n, offset.contents))
    }

    let (maybeAnchor, anchorOffset) = findNodeAndOffset(anchorBound)
    let (maybeFocus, focusOffset) = findNodeAndOffset(focusBound)
    Option.map2(maybeAnchor, maybeFocus, ~f=(anchorNode, focusNode) =>
      Dom.window
      ->Window.getSelection
      ->Option.map(~f=sel =>
        Selection.setBaseAndExtent(sel, anchorNode, anchorOffset, focusNode, focusOffset)
      )
    ) |> recoverOption(
      ~debug=(maybeAnchor, maybeFocus),
      "setFluidSelectionRange failed to find selection nodes",
    )
  })
  |> ignore
}

let setFluidCaret = (idx: int): unit => setFluidSelectionRange(idx, idx)

type browserPlatform =
  | Mac
  | Linux
  | Windows
  | UnknownPlatform

@val @scope("window")
external jsGetBrowserPlatform: unit => Js.Nullable.t<browserPlatform> = "getBrowserPlatform"

let getBrowserPlatform = (): browserPlatform =>
  jsGetBrowserPlatform() |> Js.Nullable.toOption |> Option.unwrap(~default=UnknownPlatform)

@val @scope("window") external jsSendHeapioMessage: string => unit = "sendHeapioMessage"

let string_of_heapio_track = (e: heapioTrack): string =>
  switch e {
  | WelcomeModal => "Welcome Modal"
  | OpenDocs => "Open docs"
  | InviteUser => "User shared dark"
  | OpenFnRef => "Open function refs"
  | OpenKeyboardRef => "Open keyboard refs"
  }

let sendHeapioMessage = (event: heapioTrack): unit =>
  string_of_heapio_track(event) |> jsSendHeapioMessage |> ignore

@val @scope("window")
external jsUnsupportedBrowser: unit => Js.Nullable.t<bool> = "unsupportedBrowser"

let unsupportedBrowser = (): bool =>
  jsUnsupportedBrowser() |> Js.Nullable.toOption |> Option.unwrap(~default=false)

let newHandler = (m: model, spec, pos) => {
  let tlid = gtlid()
  let spaceid = gid()
  let handler: PT.Handler.t = {
    ast: FluidAST.ofExpr(EBlank(gid())),
    spec: spec,
    tlid: tlid,
    pos: pos,
  }

  let astID = handler.ast |> FluidAST.toID
  let idToEnter =
    /* TL.getNextBlank requires that there be a tl in the model to operate on;
     * here, we're setting an ID to focus before the model is updated, so we
     * generate our list of blankOrDatas here */
    // Fallback to ast if spec has no blanks
    handler.spec |> SpecHeaders.firstBlank |> Option.unwrap(~default=astID)

  let tooltipState = Tooltips.assignTutorialToHTTPHandler(m.tooltipState, TLHandler(handler), tlid)

  let fluidMods = {
    let s = m.fluidState
    let newS = {...s, newPos: 0}
    let cursorState = if idToEnter == astID {
      AppTypes.CursorState.FluidEntering(tlid)
    } else {
      Entering(tlid, idToEnter)
    }

    list{
      Mod.ReplaceAllModificationsWithThisOne(
        (m: model) =>
          {...m, fluidState: newS, tooltipState: tooltipState} |> CursorState.setCursorState(
            cursorState,
          ),
      ),
    }
  }

  let pageChanges = list{Mod.SetPage(FocusedHandler(tlid, None, true))}
  let rpc = Mod.AddOps(list{SetHandler(tlid, pos, handler)}, FocusNext(tlid, Some(spaceid)))

  Mod.Many(list{rpc, ...Belt.List.concat(pageChanges, fluidMods)})
}

let submitOmniAction = (
  m: model,
  pos: Pos.t,
  action: AppTypes.AutoComplete.omniAction,
): modification => {
  let pos: Pos.t = {x: pos.x - 17, y: pos.y - 70}
  module S = PT.Handler.Spec
  let ids: S.IDs.t = {moduleID: gid(), nameID: gid(), modifierID: gid()}
  switch action {
  | NewDB(maybeName) => Refactor.createNewDB(m, maybeName, pos)
  | NewFunction(name) => Refactor.createNewFunction(m, name)
  | NewHTTPHandler(route) =>
    newHandler(m, S.HTTP(Belt.Option.getWithDefault(route, ""), "", ids), pos)
  | NewWorkerHandler(name) =>
    newHandler(m, S.Worker(Belt.Option.getWithDefault(name, ""), ids), pos)
  | NewCronHandler(name) =>
    newHandler(m, S.Cron(Belt.Option.getWithDefault(name, ""), None, ids), pos)
  | NewReplHandler(name) =>
    // When creating a repl, dont ask the user for a name
    let name = Option.unwrap(
      name,
      ~default=Util.Namer.generateAnimalWithPersonality(~space="REPL", ()),
    )
    newHandler(m, S.REPL(name, ids), pos)
  | Goto(page, tlid, _, _) => Many(list{SetPage(page), Select(tlid, STTopLevelRoot)})
  }
}

type nextMove =
  | StartThread
  | StayHere
  | GotoNext

let validate = (tl: toplevel, pd: blankOrData, value: string): option<string> => {
  let v = (pattern, name) =>
    if Regex.exactly(~re=pattern, value) {
      None
    } else {
      Some(name ++ (" must match /" ++ (pattern ++ "/")))
    }

  switch pd {
  | PDBName(_) => v(AC.dbNameValidator, "DB name")
  | PDBColType(_) => v(AC.dbColTypeValidator, "DB type")
  | PDBColName(_) => v(AC.dbColNameValidator, "DB column name")
  | PEventName(_) =>
    if TL.isHTTPHandler(tl) {
      // Must satisfy both, else error
      v(AC.httpNameValidator, "route name") |> Option.or_else(
        AC.validateHttpNameValidVarnames(value),
      )
    } else {
      v(AC.eventNameValidator, "event name")
    }
  | PEventModifier(_) =>
    if TL.isHTTPHandler(tl) {
      v(AC.httpVerbValidator, "verb")
    } else if TL.isCronHandler(tl) {
      AC.cronIntervalValidator(value)
    } else {
      v(AC.eventModifierValidator, "event modifier")
    }
  | PEventSpace(_) => v(AC.eventSpaceValidator, "event space")
  | PFnName(_) =>
    if String.startsWith(~prefix="dark/", value) {
      v(AC.packageFnNameValidator, "function name")
    } else {
      v(AC.fnNameValidator, "function name")
    }
  | PFnReturnType(_) => v(AC.paramTypeValidator, "return type")
  | PParamName(oldParam) =>
    v(AC.paramNameValidator, "param name") |> Option.orElse(
      AC.validateFnParamNameFree(tl, oldParam, value),
    )
  | PParamType(_) => v(AC.paramTypeValidator, "param type")
  | PTypeName(_) => v(AC.typeNameValidator, "type name")
  | PTypeFieldName(_) => v(AC.fieldNameValidator, "type field name")
  | PTypeFieldType(_) => v(AC.paramTypeValidator, "type field type")
  }
}

let submitACItem = (
  m: model,
  tlid: TLID.t,
  id: id,
  item: AppTypes.AutoComplete.item,
  move: nextMove,
): modification => {
  let stringValue = AC.getValue(m.complete)
  switch TL.getTLAndPD(m, tlid, id) {
  | Some(tl, Some(pd)) =>
    switch validate(tl, pd, stringValue) {
    | Some(error) =>
      /* We submit when users click away from an input, but they might not have typed anything! We
       * don't want to adjust the validators to allow empty strings where they are not allowed, but we
       * also don't want to display an error when they were not responsible for it! */
      if stringValue == "" {
        NoChange
      } else {
        Model.updateErrorMod(Error.set(error))
      }
    | None =>
      let wrap = (ops, next) => {
        let wasEditing = P.isBlank(pd) |> not
        let focus = if wasEditing && move == StayHere {
          switch next {
          | None => AppTypes.Focus.FocusSame
          | Some(nextID) => FocusExact(tlid, nextID)
          }
        } else {
          FocusNext(tlid, next)
        }

        Mod.AddOps(ops, focus)
      }

      let wrapID = ops => wrap(ops, Some(id))
      let wrapNew = (ops, new_) => wrap(ops, Some(P.toID(new_)))
      let save = (newtl, next) =>
        if newtl == tl {
          Mod.NoChange
        } else {
          switch newtl {
          | TLHandler(h) => wrapNew(list{SetHandler(tlid, h.pos, h)}, next)
          | TLFunc(f) => wrapNew(list{SetFunction(f)}, next)
          | TLType(t) => wrapNew(list{SetType(t)}, next)
          | TLPmFunc(_) => recover("no vars in pmfn", ~debug=tl, Mod.NoChange)
          | TLDB(_) => recover("no vars in DBs", ~debug=tl, Mod.NoChange)
          }
        }

      let saveH = (h, next) => save(TLHandler(h), next)
      let replace = new_ => tl |> TL.replace(pd, new_) |> (tl_ => save(tl_, new_))

      switch (pd, item, tl) {
      | (PDBName(F(id, oldName)), ACDBName(value), TLDB(_)) =>
        if AC.assertValid(AC.dbNameValidator, value) != value {
          Model.updateErrorMod(
            Error.set("DB name must match " ++ (AC.dbNameValidator ++ " pattern")),
          )
        } else if oldName == value /* leave as is */ {
          // TODO: I think this should actually be STCaret with a target indicating the end of the ac item?
          Select(tlid, STID(id))
        } else if List.member(~value, TL.allDBNames(m.dbs)) {
          Model.updateErrorMod(Error.set("There is already a DB named " ++ value))
        } else {
          let varrefs = Refactor.renameDBReferences(m, oldName, value)
          AddOps(list{RenameDBname(tlid, value), ...varrefs}, FocusNothing)
        }
      | (PDBColType(ct), ACDBColType(value), TLDB(_)) =>
        if B.toOption(ct) |> Option.map(~f=DType.type2str) == Some(value) {
          // TODO: I think this should actually be STCaret with a target indicating the end of the ac item?
          Select(tlid, STID(id))
        } else if B.isBlank(ct) {
          wrapID(list{SetDBColType(tlid, id, value), AddDBCol(tlid, gid(), gid())})
        } else {
          wrapID(list{ChangeDBColType(tlid, id, value)})
        }

      | (PDBColName(cn), ACDBColName(value), TLDB(db)) =>
        if B.toOption(cn) == Some(value) {
          // TODO: I think this should actually be STCaret with a target indicating the end of the ac item?
          Select(tlid, STID(id))
        } else if DB.hasCol(db, value) {
          Model.updateErrorMod(Error.set("Can't have two DB fields with the same name: " ++ value))
        } else if B.isBlank(cn) {
          wrapID(list{SetDBColName(tlid, id, value)})
        } else {
          wrapID(list{ChangeDBColName(tlid, id, value)})
        }
      | (PEventName(_), ACCronName(value), _)
      | (PEventName(_), ACReplName(value), _)
      | (PEventName(_), ACWorkerName(value), _) =>
        replace(PEventName(F(id, value)))

      | (PEventName(_), ACHTTPRoute(value), TLHandler(h)) =>
        // Check if the ACHTTPRoute value is a 404 path
        let f404s =
          m.f404s |> List.find(~f=(f404: AnalysisTypes.FourOhFour.t) => f404.path == value)

        switch f404s {
        | Some(f404) =>
          let new_ = B.newF(value)
          let modifier = switch PT.Handler.Spec.modifier(h.spec) {
          | None => f404.modifier
          | Some(mod) => B.toString(mod)
          }
          let specInfo: PT.Handler.Spec.t = PT.Handler.Spec.newHTTP(f404.path, modifier)

          // We do not delete the 404 on the server because the list of 404s is
          // generated by filtering through the unused HTTP handlers
          Many(list{saveH({...h, spec: specInfo}, PEventName(new_)), Delete404(f404)})
        | None => replace(PEventName(F(id, value)))
        }
      // allow arbitrary HTTP modifiers
      | (PEventModifier(_), ACHTTPModifier(value), _)
      | (PEventModifier(_), ACCronTiming(value), _)
      | (PEventModifier(_), ACEventModifier(value), _) =>
        replace(PEventModifier(F(id, value)))

      | (PEventSpace(_), ACEventSpace(newSpace), TLHandler(h)) =>
        module Spec = PT.Handler.Spec
        let newSpace = String.toUppercase(newSpace)
        // HttpBasicHandlerTODO: We'll likely need to adjust here; not sure how
        let newSpec = if newSpace == "HTTP" {
          switch h.spec {
          | HTTP(_) | HTTPBasic(_) => h.spec
          | Worker(_)
          | OldWorker(_)
          | Cron(_)
          | REPL(_)
          | UnknownHandler(_, _, _) => {
              let name = Spec.name(h.spec)->B.toString
              let newPath = if !String.startsWith(~prefix="/", name) {
                "/" ++ name
              } else {
                name
              }
              let newModifier = Spec.modifier(h.spec)->B.optionToString
              HTTP(newPath, newModifier, Spec.ids(h.spec))
            }
          }
        } else {
          let name = switch h.spec {
          | HTTP(_)
          | HTTPBasic(_) => {
              let name = Spec.name(h.spec)->B.toString
              // convert /projects => projects
              let name = if String.startsWith(~prefix="/", name) {
                String.dropLeft(~count=1, name)
              } else {
                name
              }
              // convert  /projects/:user/:id => /projects/user/id
              name |> String.split(~on=":") |> String.join(~sep="")
            }
          | Worker(_)
          | OldWorker(_)
          | Cron(_)
          | REPL(_)
          | UnknownHandler(_) =>
            Spec.name(h.spec)->B.toString
          }
          switch newSpace {
          | "WORKER" => Worker(name, Spec.ids(h.spec))
          | "CRON" => Cron(name, None, Spec.ids(h.spec))
          | "REPL" => REPL(name, Spec.ids(h.spec))
          | _ => h.spec
          }
        }

        let new = B.F(id, newSpace)
        saveH({...h, spec: newSpec}, PEventSpace(new))

      | (PFnName(_), ACFnName(value), TLFunc(old)) =>
        if old.name == value {
          NoChange
        } else if List.member(~value, UserFunctions.allNames(m.userFunctions)) {
          Model.updateErrorMod(Error.set("There is already a Function named " ++ value))
        } else {
          let newPD = PFnName(F(id, value))
          let new_ = {
            ...old,
            name: value,
            nameID: id,
          }

          let changedNames = Refactor.renameFunction(m, old, User(value))
          wrapNew(list{SetFunction(new_), ...changedNames}, newPD)
        }

      | (PFnReturnType(_), ACReturnType(typ), _) => replace(PFnReturnType(F(id, typ)))
      | (PParamName(_), ACParamName(value), _) => replace(PParamName(F(id, value)))
      | (PParamType(_), ACParamType(typ), _) => replace(PParamType(F(id, typ)))
      | (PTypeName(_), ACTypeName(value), TLType(old)) =>
        if List.member(~value, UserTypes.allNames(m.userTypes)) {
          Model.updateErrorMod(Error.set("There is already a Type named " ++ value))
        } else {
          let newPD = PTypeName(F(id, value))
          let new_ = UserTypes.replace(pd, newPD, old)
          let changedNames = Refactor.renameUserType(m, old, new_)
          wrapNew(list{SetType(new_), ...changedNames}, newPD)
        }
      | (PTypeFieldName(_), ACTypeFieldName(value), _) => replace(PTypeFieldName(F(id, value)))
      | (PTypeFieldType(_), ACTypeFieldType(typ), _) => replace(PTypeFieldType(F(id, typ)))
      | (pd, item, _) =>
        ReplaceAllModificationsWithThisOne(
          m => {
            let custom =
              Types.show_blankOrData(pd) ++ (", " ++ AppTypes.AutoComplete.show_item(item))

            Rollbar.displayAndReportError(m, "Invalid autocomplete option", None, Some(custom))
          },
        )
      }
    }
  | _ => recover("Missing tl/pd", ~debug=(tlid, id), Mod.NoChange)
  }
}

let submit = (m: model, tlid: TLID.t, id: id, move: nextMove): modification =>
  switch AC.highlighted(m.complete) {
  | Some(ACOmniAction(_)) =>
    recover("Shouldnt allow omniactions here", ~debug=(tlid, id), Mod.NoChange)
  | Some(item) => submitACItem(m, tlid, id, item, move)
  | None =>
    // We removed ACExtra to define more specific autocomplete items.
    // These are all autocomplete items who's target accepts and handles a free form value
    let item = {
      let value = m.complete.value
      switch m.complete.target {
      | Some(_, p) =>
        switch P.typeOf(p) {
        | DBColName => Some(AppTypes.AutoComplete.ACDBColName(value))
        | FnName => Some(ACFnName(value))
        | ParamName => Some(ACParamName(value))
        | TypeName => Some(ACTypeName(value))
        | TypeFieldName => Some(ACTypeFieldName(value))
        | EventModifier =>
          // Does not accept freeform inputs, but goes to validation call for more specific error message displayed to user
          Some(ACEventModifier(value))
        | _ => None
        }
      | None => None
      }
    }

    switch item {
    | Some(acItem) => submitACItem(m, tlid, id, acItem, move)
    | None =>
      /* There's no good error message when the user submits an empty string, but just not doing anything
       * shows that it's not a valid input */
      if m.complete.value == "" {
        NoChange
      } else {
        Model.updateErrorMod(Error.set("Invalid input"))
      }
    }
  }

/* Submit, but don't move the cursor
 *
 * This was added to to cleanly express "commit the state of an input box when I click away",
 * but is more generally intended to express "commit the state and I'll handle the cursor"
 * */
let commit = (m: model, tlid: TLID.t, id: id) => submit(m, tlid, id, StayHere)
