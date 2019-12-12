open Core_kernel
open Types
open Types.RuntimeT
open Types.RuntimeT.HandlerT
module RT = Runtime

let is_complete (h : handler) : bool =
  match (h.spec.module_, h.spec.name, h.spec.modifier) with
  | Filled (_, _), Filled (_, _), Filled (_, _) ->
      true
  | _ ->
      false


let module_for (h : handler) : string option =
  match h.spec.module_ with Filled (_, m) -> Some m | _ -> None


let module_type (h : handler) =
  match module_for h with
  | Some m when String.Caseless.equal "http" m ->
      `Http
  | Some m when String.Caseless.equal "cron" m ->
      `Cron
  | Some m when String.Caseless.equal "worker" m ->
      `Worker
  | Some m when String.Caseless.equal "repl" m ->
      `Repl
  | Some m ->
      `Worker
  | None ->
      `Unknown


let is_http (h : handler) : bool =
  (* match `Unknown because http is generally what we want *)
  match module_type h with `Http | `Unknown -> true | _ -> false


let is_worker (h : handler) : bool =
  match module_type h with `Worker -> true | _ -> false


let is_cron (h : handler) : bool =
  match module_type h with `Cron -> true | _ -> false


let module_for_exn (h : handler) : string =
  match module_for h with
  | Some s ->
      s
  | None ->
      Exception.internal
        "Called module_for_exn on a toplevel without a `module` param"


let event_name_for (h : handler) : string option =
  match h.spec.name with Filled (_, name) -> Some name | _ -> None


let event_name_for_exn (h : handler) : string =
  match event_name_for h with
  | Some s ->
      s
  | None ->
      Exception.internal
        "Called event_name_for_exn on a toplevel without a `event_name` param"


let modifier_for (h : handler) : string option =
  match h.spec.modifier with
  | Filled (_, modifier) ->
      Some modifier
  | _ ->
      None


let modifier_for_exn (h : handler) : string =
  match modifier_for h with
  | Some s ->
      s
  | None ->
      Exception.internal
        "Called modifier_for_exn on a toplevel without a `modifier` param"


let event_desc_for (h : handler) : (string * string * string) option =
  (* Stored_event.event_desc  *)
  match (module_for h, event_name_for h, modifier_for h) with
  | Some m, Some en, Some mo ->
      Some (m, en, mo)
  | _ ->
      None


let matches_event_desc (d : string * string * string) (h : handler) : bool =
  let space, name, modifier = d in
  module_for h = Some space
  && event_name_for h = Some name
  && modifier_for h = Some modifier


(** has_valid_spec returns true if the spec is complete and valid.
    Specifically, this is called from Canvas.verify, meaning every save and
    load validates the canvas's handlers through this function. *)
let has_valid_spec (h : handler) : bool =
  (* Legacy workers with arbitrary module may have any modifier, but new
   * workers must have "_" *)
  match module_for h with
  (* Cannot use module_type here, as we only want explicitly module = "WORKER",
   * not any `Worker. *)
  | Some m when String.Caseless.equal "worker" m ->
      modifier_for h = Some "_"
  | Some m when String.Caseless.equal "repl" m ->
      modifier_for h = Some "_"
  | Some _ | None ->
      true
