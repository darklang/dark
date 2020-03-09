open Prelude

(* Dark *)
module E = FluidExpression

let viewFunction (fn : Prelude.function_) (sendToRail : E.sendToRail option) :
    'a Vdom.t =
  let handleRailMsg =
    match sendToRail with
    | Some Rail ->
        Html.text "Use `take-function-off-rail` to handle the `Nothing` case."
    | Some NoRail ->
        Html.text "Use `put-function-on-rail` to restore this behavior."
    | None ->
        Html.noNode
  in
  let errorRail =
    Html.a
      [ Html.class' "link"
      ; Html.href
          "https://ops-documentation.builtwithdark.com/user-manual/error-handling#error-rail"
      ; Html.target "_blank" ]
      [Html.text "error rail"]
  in
  match fn.fnReturnTipe with
  | TOption ->
      Html.p
        []
        [ Html.text "By default, this function goes to the "
        ; errorRail
        ; Html.text
            " on `Nothing` and returns the unwrapped value in `Just value` otherwise. "
        ; handleRailMsg ]
  | TResult ->
      Html.p
        []
        [ Html.text "By default, this function goes to the "
        ; errorRail
        ; Html.text
            " on `Error` and returns the unwrapped value in `Ok value` otherwise. "
        ; handleRailMsg ]
  | _ ->
      Html.noNode
