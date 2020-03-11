(** kind tells which type of FluidPanel this is, which can affect things like
  * how it's expression is tokenized *)
type kind = FeatureFlag [@@deriving show {with_path = false}]

module State : sig
  (** a State.t holds the state of a single Panel. This is mostly which
    * expression it refers to and if it's open or closed.
    *
    * See docs/fluid-views.md for more. *)
  type t =
    { expressionId : ID.t
          (** the id of the top-most expression in this panel.
            * Used to identify the panel in a Group. *)
    ; tlid : TLID.t
    ; kind : kind
    ; isOpen : bool }
  [@@deriving show {with_path = false}]
end

module Group : sig
  (** a Group.t is a collection of panel states, stored as a dict of
    * {expressionId => State.t}. This is embedded within fluidState, to hold the
    * collective state of all the panels of a FluidView. *)
  type t [@@deriving show {with_path = false}]

  (** [init tlid ast] returns a Group.t for the given [ast] *)
  val init : TLID.t -> FluidAST.t -> t

  (** [empty] returns an empty Group.t *)
  val empty : t

  (** [setOpen b id g] sets the isOpen field of the panel in [g] with id =
    * [id] to [b]. Does nothing if no panel with [id] exists. *)
  val setOpen : bool -> ID.t -> t -> t

  (** [hideAll g] sets isOpen = false for all panels in [g]. *)
  val hideAll : t -> t

  (** [get ~id g] returns the panel in [g] with id = [id] or None. *)
  val get : id:ID.t -> t -> State.t option

  (** [map ~f group] returns a list of the result of [f s] for each State.t [s] in [group] *)
  val map : f:(State.t -> 'a) -> t -> 'a list

  (** [filter ~f group] returns a new Group.t containing every State.t [s] where [f s] is true. *)
  val filter : f:(State.t -> bool) -> t -> t
end
