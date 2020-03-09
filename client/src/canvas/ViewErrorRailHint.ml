open Prelude

(* Dark *)
module E = FluidExpression

let viewFunction (fn : Prelude.function_) (sendToRail : E.sendToRail option) :
    'a Vdom.t =
  let errorRail =
    Html.a
      [ Html.class' "link"
      ; Html.href
          "https://ops-documentation.builtwithdark.com/user-manual/error-handling#error-rail"
      ; Html.target "_blank" ]
      [Html.text "error rail"]
  in
  Option.withDefault
    ~default:Html.noNode
    ( match sendToRail with
    | None ->
      (* If we don't know if the function is on the rail, return a generic message: *)
      ( match fn.fnReturnTipe with
      | TOption ->
          Some
            (Html.p
               []
               [ Html.text "By default, this function goes to the "
               ; errorRail
               ; Html.text
                   " on `Nothing` and returns the unwrapped value in `Just value` otherwise. "
               ])
      | TResult ->
          Some
            (Html.p
               []
               [ Html.text "By default, this function goes to the "
               ; errorRail
               ; Html.text
                   " on `Error` and returns the unwrapped value in `Ok value` otherwise. "
               ])
      | _ ->
          None )
    | Some sendToRail ->
      (* If we know if the function is on the rail, return a specific message: *)
      ( match (fn.fnReturnTipe, sendToRail) with
      | TOption, Rail ->
          Some
            (Html.p
               []
               [ Html.text "This function goes to the "
               ; errorRail
               ; Html.text
                   " on `Nothing` and returns the unwrapped value in `Just value` otherwise. "
               ; Html.text
                   "Use `take-function-off-rail` to handle the `Nothing` case."
               ])
      | TOption, NoRail ->
          Some
            (Html.p
               []
               [ Html.text "This function is not on the "
               ; errorRail
               ; Html.text
                   " so you need to handle `Just value` and `Nothing` manually. "
               ; Html.text
                   "Alternatively, use `put-function-on-rail`."
               ])
      | TResult, Rail ->
          Some
            (Html.p
               []
               [ Html.text "This function goes to the "
               ; errorRail
               ; Html.text
                   " on `Error` and returns the unwrapped value in `Ok value` otherwise. "
               ; Html.text
                   "Use `take-function-off-rail` to handle the `Error` case." ])
      | TResult, NoRail ->
          Some
            (Html.p
               []
               [ Html.text "This function is not on the "
               ; errorRail
               ; Html.text
                   " so you need to handle `Error error` and `Ok value` manually. "
               ; Html.text
                   "Alternatively, use `put-function-on-rail`."
               ])
      | _, (Rail | NoRail) ->
          None ) )
