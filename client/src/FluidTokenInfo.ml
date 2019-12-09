open Prelude
module Token = FluidToken

type t = Types.fluidTokenInfo

let debugInfo (t : t) =
  Html.dl
    []
    [ Html.dt [] [Html.text "pos"]
    ; Html.dd [] [Html.text (Printf.sprintf "(%d, %d)" t.startPos t.endPos)]
    ; Html.dt [] [Html.text "tok"]
    ; Html.dd [] [Html.text (Token.toText t.token)]
    ; Html.dt [] [Html.text "id"]
    ; Html.dd [] [Html.text (Token.id t.token |> deID)]
    ; Html.dt [] [Html.text "type"]
    ; Html.dd [] [Html.text (Token.toTypeName t.token)]
    ; Html.dt [] [Html.text "debug"]
    ; Html.dd [] [Html.text (Token.toDebugInfo t.token)] ]
