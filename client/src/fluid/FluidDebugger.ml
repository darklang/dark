open Prelude
module Attrs = Tea.Html2.Attributes
module Printer = FluidPrinter
module Expression = FluidExpression
module Token = FluidToken

let view (m : model) (ast : Expression.t) : Types.msg Html.html =
  let s = m.fluidState in
  (* this is a bit of a hack to find the correct sub-expression based on the
   * active editor panel within the TL. It would be better if we somehow had
   * access to the editor's viewState, but we don't. *)
  let tokens =
    Expression.find m.fluidState.activeEditorId ast
    |> Option.map ~f:Printer.tokenize
    |> Option.withDefault ~default:[]
  in
  let ddText txt = Html.dd [] [Html.text txt] in
  let dtText txt = Html.dt [] [Html.text txt] in
  let posData =
    let oldGrid = Fluid.gridFor ~pos:s.oldPos tokens in
    let newGrid = Fluid.gridFor ~pos:s.newPos tokens in
    [ dtText "pos"
    ; Html.dd
        []
        [ Html.text (string_of_int s.oldPos)
        ; Html.text " -> "
        ; Html.text (string_of_int s.newPos) ]
    ; dtText "grid"
    ; Html.dd
        []
        [ Html.text (oldGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (oldGrid.row |> string_of_int)
        ; Html.text " -> "
        ; Html.text (newGrid.col |> string_of_int)
        ; Html.text ","
        ; Html.text (newGrid.row |> string_of_int) ]
    ; dtText "TLID"
    ; Html.dd
        []
        [ Html.text
            ( tlidOf m.cursorState
            |> Option.map ~f:deTLID
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "ast root"
    ; Html.dd [] [Html.text (Expression.toID ast |> deID)]
    ; dtText "act edit"
    ; Html.dd [] [Html.text (s.activeEditorId |> deID)]
    ; dtText "flags"
    ; Html.dd
        []
        [ Html.text
            ( Expression.filter ast ~f:(function
                  | EFeatureFlag _ ->
                      true
                  | _ ->
                      false)
            |> List.map ~f:(Expression.ofSubtree >> Expression.toID >> deID)
            |> String.join ~sep:"," ) ]
    ; dtText "acIndex"
    ; Html.dd
        []
        [ Html.text
            ( s.ac.index
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "acEntryCount"
    ; Html.dd [] [Html.text (s.ac.completions |> List.length |> string_of_int)]
    ; dtText "upDownCol"
    ; Html.dd
        []
        [ Html.text
            ( s.upDownCol
            |> Option.map ~f:string_of_int
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "lastInput"
    ; Html.dd [] [Html.text (show_fluidInputEvent s.lastInput)]
    ; dtText "selection"
    ; Html.dd
        []
        [ Html.text
            ( s.selectionStart
            |> Option.map ~f:(fun selStart ->
                   string_of_int selStart ^ "->" ^ string_of_int s.newPos)
            |> Option.withDefault ~default:"None" ) ]
    ; dtText "midClick"
    ; Html.dd [] [Html.text (string_of_bool s.midClick)] ]
  in
  let error =
    [dtText "error"; ddText (Option.withDefault s.error ~default:"None")]
  in
  let tokenData =
    let left, right, next = Fluid.getNeighbours tokens ~pos:s.newPos in
    let ddNoProp1 txt = Html.dd [Html.noProp] [Html.text txt] in
    let tokenInfo tkn =
      Html.dd [Attrs.class' "tokenInfo"] [Token.show_tokenInfo tkn]
    in
    let ddLeft =
      match left with
      | L (_, left) ->
          tokenInfo left
      | R (_, _) ->
          ddNoProp1 "Right"
      | No ->
          ddNoProp1 "None"
    in
    let ddRight =
      match right with
      | L (_, _) ->
          ddNoProp1 "Left"
      | R (_, right) ->
          tokenInfo right
      | No ->
          ddNoProp1 "None"
    in
    let ddNext =
      match next with Some next -> tokenInfo next | None -> ddNoProp1 "None"
    in
    [dtText "left"; ddLeft; dtText "right"; ddRight; dtText "next"; ddNext]
  in
  let actions =
    [ dtText "actions"
    ; Html.dd
        [Attrs.class' "actions"]
        [ Html.ul
            []
            (List.map s.actions ~f:(fun txt -> Html.li [] [Html.text txt])) ] ]
  in
  let cursorState =
    [dtText "cursorState"; ddText (show_cursorState m.cursorState)]
  in
  let lastMod = [dtText "lastMod"; ddText (show_modification m.lastMod)] in
  let status =
    List.concat [posData; error; tokenData; actions; cursorState; lastMod]
  in
  Html.div [Attrs.id "fluid-status"] [Html.dl [] status]
