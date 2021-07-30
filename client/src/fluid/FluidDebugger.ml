open Prelude
module Attrs = Tea.Html2.Attributes
module Printer = FluidTokenizer
module Expression = FluidExpression
module Token = FluidToken

let view (m : model) (ast : FluidAST.t) : Types.msg Html.html =
  let s = m.fluidState in
  let tokens = FluidTokenizer.tokensForEditor m.fluidState.activeEditor ast in
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
            ( CursorState.tlidOf m.cursorState
            |> Option.map ~f:TLID.toString
            |> Option.unwrap ~default:"None" ) ]
    ; dtText "ast root"
    ; Html.dd [] [Html.text (FluidAST.toID ast |> ID.toString)]
    ; dtText "active editor"
    ; Html.dd [] [Html.text (show_fluidEditor s.activeEditor)]
    ; dtText "acIndex"
    ; Html.dd
        []
        [ Html.text
            ( s.ac.index
            |> Option.map ~f:string_of_int
            |> Option.unwrap ~default:"None" ) ]
    ; dtText "acEntryCount"
    ; Html.dd
        []
        [Html.text (FluidAutocomplete.numCompletions s.ac |> string_of_int)]
    ; dtText "upDownCol"
    ; Html.dd
        []
        [ Html.text
            ( s.upDownCol
            |> Option.map ~f:string_of_int
            |> Option.unwrap ~default:"None" ) ]
    ; dtText "lastInput"
    ; Html.dd [] [Html.text (show_fluidInputEvent s.lastInput)]
    ; dtText "selection"
    ; Html.dd
        []
        [ Html.text
            ( s.selectionStart
            |> Option.map ~f:(fun selStart ->
                   string_of_int selStart ^ "->" ^ string_of_int s.newPos)
            |> Option.unwrap ~default:"None" ) ]
    ; dtText "midClick"
    ; Html.dd [] [Html.text (string_of_bool s.midClick)] ]
  in
  let error =
    [dtText "error"; ddText (Option.unwrap s.error ~default:"None")]
  in
  let tokenData =
    let left, right, next = FluidTokenizer.getNeighbours tokens ~pos:s.newPos in
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
  let status = List.flatten [posData; error; tokenData; actions; cursorState] in
  Html.div [Attrs.id "fluid-status"] [Html.dl [] status]
