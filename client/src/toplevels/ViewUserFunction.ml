open Prelude

(* Dark *)
module B = BlankOr

type viewProps = ViewUtils.viewProps

let fontAwesome = ViewUtils.fontAwesome

let onEvent = ViewUtils.onEvent

type exeFunction =
  | CanExecute of traceID * dval list
  | CannotExecute of string
  | IsExecuting

let viewUserFnName
    ~(classes : string list) (vp : viewProps) (v : string blankOr) :
    msg Html.html =
  ViewBlankOr.viewText ~classes ~enterable:true FnName vp v


let viewExecuteBtn (vp : viewProps) (fn : userFunction) : msg Html.html =
  let exeStatus =
    if vp.isExecuting
    then IsExecuting
    else
      (* Attempts to get trace inputValues for this function *)
      match Analysis.selectedTrace vp.tlTraceIDs vp.traces vp.tlid with
      | Some (traceID, Ok td) ->
          let args = UserFunctions.inputToArgs fn td.input in
          (* If any of the args is Incomplete/Error then we don't want to bother allowing this function to be executed *)
          if List.any
               ~f:(fun dv -> match dv with DIncomplete _ -> true | _ -> false)
               args
          then CannotExecute "Cannot run function with incomplete arguments"
          else if List.any
                    ~f:(fun dv -> match dv with DError _ -> true | _ -> false)
                    args
          then
            CannotExecute "Cannot run function with arguments that has an error"
          else CanExecute (traceID, args)
      | _ ->
          CannotExecute "Cannot run function with no trace data"
  in
  let events =
    (* If function is ready for re-execution, attach onClick listener *)
    match (fn.ufMetadata.ufmName, exeStatus) with
    | F (_, fnName), CanExecute (traceID, args) ->
        ViewUtils.eventNoPropagation
          ~key:("run-fun" ^ "-" ^ TLID.toString fn.ufTLID ^ "-" ^ traceID)
          "click"
          (fun _ ->
            ExecuteFunctionFromWithin
              { efpTLID = fn.ufTLID
              ; efpCallerID = FluidAST.toID fn.ufAST
              ; efpTraceID = traceID
              ; efpFnName = fnName
              ; efpArgs = args })
    | _ ->
        Vdom.noProp
  in
  let title =
    match exeStatus with
    | CannotExecute msg ->
        msg
    | CanExecute _ ->
        "Click to execute function"
    | IsExecuting ->
        "Function is executing"
  in
  Html.div
    [ Html.classList
        [ ("execution-button", true)
        ; ( "is-ready"
          , vp.permission = Some ReadWrite
            && match exeStatus with CanExecute _ -> true | _ -> false )
        ; ("is-executing", exeStatus = IsExecuting) ]
    ; events
    ; Html.title title ]
    [fontAwesome "redo"]


let viewMetadata (vp : viewProps) (fn : functionTypes) : msg Html.html =
  let addParamBtn =
    match fn with
    | UserFunction fn ->
      ( match vp.permission with
      | Some ReadWrite ->
          let strTLID = TLID.toString fn.ufTLID in
          Html.div
            ~unique:("add-param-col-" ^ strTLID)
            [ Html.class' "col new-parameter"
            ; ViewUtils.eventNoPropagation
                ~key:("aufp-" ^ strTLID)
                "click"
                (fun _ -> AddUserFunctionParameter fn.ufTLID) ]
            [ Html.div
                [Html.class' "parameter-btn allowed add"]
                [fontAwesome "plus-circle"]
            ; Html.span [Html.class' "btn-label"] [Html.text "add new parameter"]
            ]
      | Some Read | None ->
          Vdom.noNode )
    | PackageFn _ ->
        Vdom.noNode
  in
  let titleRow =
    let titleText =
      match fn with
      | UserFunction fn ->
          fn.ufMetadata.ufmName
      | PackageFn fn ->
          BlankOr.newF (fn.fnname ^ "_v" ^ string_of_int fn.version)
    in
    let executeBtn =
      match fn with
      | UserFunction fn ->
          let menuView =
            let uploadPackageFnAction : TLMenu.menuItem =
              { title = "Upload Function"
              ; key = "upload-ufn-"
              ; icon = Some "upload"
              ; action = (fun _ -> UploadFn fn.ufTLID)
              ; disableMsg = None }
            in
            let delAct : TLMenu.menuItem =
              let disableMsg =
                if not (UserFunctions.canDelete vp.usedInRefs fn.ufTLID)
                then
                  Some
                    "Cannot delete this function as it is used in your code base. Use the references on the right to find and change this function's callers, after which you'll be able to delete it."
                else None
              in
              { title = "Delete"
              ; key = "del-ufn-"
              ; icon = Some "times"
              ; action = (fun _ -> DeleteUserFunction fn.ufTLID)
              ; disableMsg }
            in
            let menuItems =
              if vp.isAdmin then [delAct; uploadPackageFnAction] else [delAct]
            in
            Html.div
              [Html.class' "menu"]
              [TLMenu.viewMenu vp.menuState vp.tlid menuItems]
          in
          Html.div [Html.class' "fn-actions"] [viewExecuteBtn vp fn; menuView]
      | PackageFn _ ->
          Html.span [Html.class' "fn-readonly"] [Html.text "Read Only"]
    in
    Html.div
      [Html.class' "spec-header"]
      [ ViewUtils.darkIcon "fn"
      ; viewUserFnName vp ~classes:["fn-name-content"] titleText
      ; executeBtn ]
  in
  let paramRows =
    Html.div
      [Html.id "fnparams"; Html.class' "params"]
      (FnParams.view fn vp @ [addParamBtn])
  in
  let returnRow =
    let returnType =
      match fn with
      | UserFunction fn ->
          fn.ufMetadata.ufmReturnTipe
      | PackageFn fn ->
          BlankOr.newF fn.return_type
    in
    Html.div
      [Html.id "fnreturn"; Html.class' "col param"]
      [ fontAwesome "level-down-alt"
      ; ViewBlankOr.viewTipe
          ~classes:["type"]
          ~enterable:true
          FnReturnTipe
          vp
          returnType ]
  in
  Html.div [Html.class' "fn-header"] [titleRow; paramRows; returnRow]


let view (vp : viewProps) (fn : functionTypes) : msg Html.html =
  Html.div
    [Html.class' "user-fn-toplevel"]
    [ Html.div [Html.class' "metadata"] [viewMetadata vp fn]
    ; Html.div [Html.class' "function-body expand"] (FluidView.view vp []) ]
