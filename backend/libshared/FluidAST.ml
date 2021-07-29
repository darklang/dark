module E = FluidExpression

type t = Root of E.t

let show (Root e) = E.show e

let pp f (Root e) = E.pp f e

let toExpr (Root e) = e

let ofExpr e = Root e

let toID (Root e) = E.toID e

let map ~(f : E.t -> E.t) (ast : t) : t = toExpr ast |> f |> ofExpr

let replace ~(replacement : E.t) (target : Shared.id) (ast : t) : t =
  map ast ~f:(E.replace ~replacement target)


let update
    ?(failIfMissing = true) ~(f : E.t -> E.t) (target : Shared.id) (ast : t) : t
    =
  map ast ~f:(E.update ~failIfMissing ~f target)


let filter (ast : t) ~(f : E.t -> bool) : E.t list = toExpr ast |> E.filter ~f

let blanks (ast : t) : E.t list = toExpr ast |> E.blanks

let ids (ast : t) : Shared.id list = toExpr ast |> E.ids

let find (target : Shared.id) (ast : t) : E.t option =
  toExpr ast |> E.find target


let findParent (target : Shared.id) (ast : t) : E.t option =
  toExpr ast |> E.findParent target


let ancestors (target : Shared.id) (ast : t) : E.t list =
  toExpr ast |> E.ancestors target


let getFeatureFlags (ast : t) : E.t list =
  filter ast ~f:(function EFeatureFlag _ -> true | _ -> false)


let clone = map ~f:E.clone

let testEqualIgnoringIds (a : t) (b : t) : bool =
  E.testEqualIgnoringIds (toExpr a) (toExpr b)
