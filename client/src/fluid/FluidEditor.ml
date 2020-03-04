open Tc

(** editorViewKind represents the type of editorView. This impacts, for
  * example, how expressions are tokenized within the view. *)
type viewKind =
  | MainView
  | FeatureFlagView
[@@deriving show {with_path = false}]

type t =
  { id : string
        (** the unique id of this editor panel, used to identify it, eg, when
          * it is clicked and needs focus *)
  ; expressionId : ID.t  (** the id of the top-most expression in this panel *)
  ; kind : viewKind }
[@@deriving show {with_path = false}]

let build (ast : FluidAST.t) : t StrDict.t =
  FluidAST.filter ast ~f:(function EFeatureFlag _ -> true | _ -> false)
  |> List.map ~f:(fun e ->
         let expressionId = FluidExpression.toID e in
         let id = "flag-" ^ ID.toString expressionId in
         (id, {id; expressionId; kind = FeatureFlagView}))
  |> StrDict.fromList
