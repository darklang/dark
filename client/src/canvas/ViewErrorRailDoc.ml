open Prelude

(* Dark *)
module E = FluidExpression
open PrettyDocs

(** [hintForFunction fn sendToRail] returns a (possibly noNode) DOM node that
 * provides a contextual hint about error-rail usage for the function [fn].
 *
 * The message in the node is customized based on the function return value
 * and the error rail status in [sendToRail].
 * The error rail status is an option because there are cases where the error rail
 * status has yet to be determined (eg autocomplete). Pass `None` for [sendToRail]
 * in such cases.
 *)
let hintForFunction (fn : Prelude.function_) (sendToRail : E.sendToRail option)
    : 'a Vdom.t =
  let errorRail =
    Html.a
      [ Html.class' "link"
      ; Html.href
          "https://ops-documentation.builtwithdark.com/user-manual/error-handling#error-rail"
      ; Html.target "_blank" ]
      [Html.text "error rail"]
  in
  Option.unwrap
    ~default:Html.noNode
    ( match sendToRail with
    | None ->
      (* If we don't know if the function is on the rail, return a generic message: *)
      ( match fn.fnReturnTipe with
      | TOption ->
          Some
            (Html.p
               []
               [ txt "By default, this function goes to the "
               ; errorRail
               ; txt " on "
               ; tag "code" [txt "Nothing"]
               ; txt " and returns the unwrapped "
               ; tag "var" [txt "value"]
               ; txt " in "
               ; tag "code" [txt "Just "; tag "var" [txt "value"]]
               ; txt " otherwise." ])
      | TResult ->
          Some
            (Html.p
               []
               [ txt "By default, this function goes to the "
               ; errorRail
               ; txt " on "
               ; tag "code" [txt "Error"]
               ; txt " and returns the unwrapped "
               ; tag "var" [txt "value"]
               ; txt " in "
               ; tag "code" [txt "Ok "; tag "var" [txt "value"]]
               ; txt " otherwise." ])
      | _ ->
          None )
    | Some sendToRail ->
      (* If we know if the function is on the rail, return a specific message: *)
      ( match (fn.fnReturnTipe, sendToRail) with
      | TOption, Rail ->
          Some
            (Html.p
               []
               [ txt "This function goes to the "
               ; errorRail
               ; txt " on "
               ; tag "code" [txt "Nothing"]
               ; txt " and returns the unwrapped "
               ; tag "var" [txt "value"]
               ; txt " in "
               ; tag "code" [txt "Just "; tag "var" [txt "value"]]
               ; txt " otherwise. Use the command "
               ; tag "cmd" [txt "take-function-off-rail"]
               ; txt " to handle the "
               ; tag "code" [txt "Nothing"]
               ; txt " case manually." ])
      | TOption, NoRail ->
          Some
            (Html.p
               []
               [ txt "This function is not on the "
               ; errorRail
               ; txt ", so you need to handle "
               ; tag "code" [txt "Just "; tag "var" [txt "value"]]
               ; txt " and "
               ; tag "code" [txt "Nothing"]
               ; txt " manually. Alternatively, use the command "
               ; tag "cmd" [txt "put-function-on-rail"]
               ; txt
                   " to let the error rail handle the result of this function."
               ])
      | TResult, Rail ->
          Some
            (Html.p
               []
               [ txt "This function goes to the "
               ; errorRail
               ; txt " on "
               ; tag "code" [txt "Error _"]
               ; txt " and returns the unwrapped "
               ; tag "var" [txt "value"]
               ; txt " in "
               ; tag "code" [txt "Ok "; tag "var" [txt "value"]]
               ; txt " otherwise. Use the command "
               ; tag "cmd" [txt "take-function-off-rail"]
               ; txt " to handle the "
               ; tag "code" [txt "Error "; tag "var" [txt "errorMessage"]]
               ; txt " case." ])
      | TResult, NoRail ->
          Some
            (Html.p
               []
               [ txt "This function is not on the "
               ; errorRail
               ; txt ", so you need to handle "
               ; tag "code" [txt "Error "; tag "var" [txt "errorMessage"]]
               ; txt " and "
               ; tag "code" [txt "Ok "; tag "var" [txt "value"]]
               ; txt " manually. Alternatively, use "
               ; tag "cmd" [txt "put-function-on-rail"]
               ; txt " to let the ErrorRail handle the result of this function."
               ])
      | _, (Rail | NoRail) ->
          None ) )
