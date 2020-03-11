open Tc

type kind = FeatureFlag [@@deriving show {with_path = false}]

module State = struct
  type t =
    { expressionId : ID.t
    ; tlid : TLID.t
    ; kind : kind
    ; isOpen : bool }
  [@@deriving show {with_path = false}]
end

module Group = struct
  type t = State.t ID.Dict.t [@@deriving show {with_path = false}]

  let empty : t = ID.Dict.empty

  let init (tlid : TLID.t) (ast : FluidAST.t) : t =
    FluidAST.filter ast ~f:(function EFeatureFlag _ -> true | _ -> false)
    |> List.map ~f:(fun e ->
           let expressionId = FluidExpression.toID e in
           ( expressionId
           , {State.tlid; expressionId; isOpen = false; kind = FeatureFlag} ))
    |> ID.Dict.fromList


  let setOpen (isOpen : bool) (id : ID.t) (group : t) : t =
    ID.Dict.updateIfPresent group ~key:id ~f:(fun v -> {v with State.isOpen})


  let hideAll (group : t) : t =
    ID.Dict.map group ~f:(fun v -> {v with isOpen = false})


  let get ~(id : ID.t) (group : t) : State.t option = ID.Dict.get ~key:id group

  let map ~f group = ID.Dict.mapValues ~f group

  let filter ~(f : State.t -> bool) (group : t) : t =
    ID.Dict.toList group
    |> List.filter ~f:(fun (_, v) -> f v)
    |> ID.Dict.fromList
end
