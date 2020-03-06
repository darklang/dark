type viewKind =
  | MainView
  | FeatureFlagView
[@@deriving show {with_path = false}]

type t =
  { id : string
        (** the unique id of this editor panel, used to identify it, eg, when
          * it is clicked and needs focus *)
  ; tlid : TLID.t
  ; expressionId : ID.t  (** the id of the top-most expression in this panel *)
  ; kind : viewKind
  ; isOpen : bool }
[@@deriving show {with_path = false}]

type t' = t

module State : sig
  (** [init ast] returns a State.t for the given ast *)
  type t [@@deriving show {with_path = false}]

  val init : TLID.t -> FluidAST.t -> t

  (** [empty] returns an empty State.t *)
  val empty : t

  (** [setOpen b id s] sets the isOpen field of the editor in [s] with id =
    * [id] to [b]. Does nothing if no editor with [id] exists. *)
  val setOpen : bool -> string -> t -> t

  (** [hideAll s] sets isOpen = false for all editors in [s]. *)
  val hideAll : t -> t

  (** [get ~id s] returns the editor in [s] with id = [id] or None. *)
  val get : id:string -> t -> t' option

  (** [map ~f s] returns a list of the result of [f e] for each editor [e] in [s] *)
  val map : f:(t' -> 'a) -> t -> 'a list
end
