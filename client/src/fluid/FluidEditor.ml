open Tc

type viewKind =
  | MainView
  | FeatureFlagView
[@@deriving show {with_path = false}]

type t =
  { id : string
  ; tlid : TLID.t
  ; expressionId : ID.t
  ; kind : viewKind
  ; isOpen : bool }
[@@deriving show {with_path = false}]

type t' = t (* just so we can use it inside State *)
[@@deriving show {with_path = false}]

module State = struct
  type t = t' StrDict.t [@@deriving show {with_path = false}]

  let empty : t = StrDict.empty

  let init (tlid : TLID.t) (ast : FluidAST.t) : t =
    FluidAST.filter ast ~f:(function EFeatureFlag _ -> true | _ -> false)
    |> List.map ~f:(fun e ->
           let expressionId = FluidExpression.toID e in
           let id = "flag-" ^ ID.toString expressionId in
           (id, {id; tlid; expressionId; isOpen = false; kind = FeatureFlagView}))
    |> StrDict.fromList


  let setOpen (isOpen : bool) (id : string) (d : t) : t =
    StrDict.updateIfPresent d ~key:id ~f:(fun v -> {v with isOpen})


  let hideAll (d : t) : t = StrDict.map d ~f:(fun v -> {v with isOpen = false})

  let get ~(id : string) (d : t) : t' option = StrDict.get ~key:id d

  (* don't omit params or type-checker complains *)
  let map ~f d = StrDict.mapValues ~f d
end
