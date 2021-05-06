open Core_kernel
open Libexecution
open Util
open Canvas
open Tc
module Op = Libserialize.Op

let is_op_that_creates_toplevel (op : Types.op) : bool =
  match op with
  | SetHandler _
  | CreateDB _
  | SetFunction _
  | CreateDBWithBlankOr _
  | SetType _ ->
      true
  | DeleteFunction _
  | DeleteTLForever _
  | DeleteFunctionForever _
  | DeleteType _
  | DeleteTypeForever _
  | DeleteTL _
  | DeleteDBCol _
  | RenameDBname _
  | ChangeDBColName _
  | ChangeDBColType _
  | SetExpr _
  | AddDBCol _
  | SetDBColName _
  | SetDBColType _
  | CreateDBMigration _
  | AddDBColToDBMigration _
  | SetDBColNameInDBMigration _
  | SetDBColTypeInDBMigration _
  | AbandonDBMigration _
  | DeleteColInDBMigration _
  | TLSavepoint _
  | DeprecatedInitDbm _
  | UndoTL _
  | RedoTL _
  | MoveTL _ ->
      false


(** [only_ops_since_last_savepoint ops] When we clone a canvas, we sometimes
   * want to only copy over ops since the last toplevel definition (create) op - this erases
   * history, which is invisible and we don't really know at clone-time what's
   * in there, because we don't have any UI for inspecting history, nor do we
   * store timestamps or edited-by-user for ops
   * ("git blame"). *)
let only_ops_since_last_savepoint (ops : Types.oplist) : Types.oplist =
  let encountered_create_op = ref false in
  List.reverse ops
  |> List.take_while ~f:(fun op ->
         if !encountered_create_op
         then false
         else (
           encountered_create_op := is_op_that_creates_toplevel op ;
           true ))
  |> List.reverse


(** [update_hosts_in_op op ~old_host ~new_host] Given an [op], and an
   * [old_host] and a [new_host], update string literals from
   * the old to the new host. Say your canvas contains a string literal that is
   * (or contains) a url pointing to the [old_host]
   * ("://oldhost.builtwithdark.com/stuff", or its localhost equivalent), the
   * [op] will be transformed to refer to the [new_host] *)
let update_hosts_in_op (op : Types.op) ~(old_host : string) ~(new_host : string)
    : Types.op =
  let replace_host (str : string) : string =
    (* This match covers string literals containing
     * ://old_host.builtwithdark.com/stuff (or the localhost
     * equivalent), and replaces them with
     * ://new_host.builtwithdark.com. *)
    let host canvas : string =
      Printf.sprintf "://%s.%s" canvas Config.user_content_host
    in
    Util.string_replace (host old_host) (host new_host) str
  in
  let rec update_hosts_in_pattern (pattern : Libshared.FluidPattern.t) :
      Libshared.FluidPattern.t =
    Libshared.FluidPattern.postTraversal pattern ~f:(function
        | FPString {matchID; patternID; str} ->
            FPString {matchID; patternID; str = replace_host str}
        | pat ->
            pat)
  in
  Op.ast_of op
  |> Option.map
       ~f:
         (Libshared.FluidExpression.postTraversal ~f:(function
             | EString (id, str) ->
                 EString (id, replace_host str)
             | EMatch (id, cond, branches) ->
                 let newBranches =
                   branches
                   |> List.map ~f:(fun (pattern, expr) ->
                          (update_hosts_in_pattern pattern, expr))
                 in
                 EMatch (id, cond, newBranches)
             | expr ->
                 expr))
  |> Option.map ~f:(fun new_ast -> Op.with_ast new_ast op)
  |> Option.with_default ~default:op


(** Given two canvas names, clone TLs from one to the other.
   * - returns an error if from_canvas doesn't exist, or if to_canvas does
   *   ("don't clobber an existing canvas")
   * - optionally removes history - only copies ops since the last TLSavepoint (per TL)
   * - if there are string literals referring to the old canvas' url, rewrite them to
   *   refer to the new one (see update_hosts_in_op)
   * - runs in a DB transaction, so this should be all-or-nothing
   * *)
let clone_canvas ~from_canvas_name ~to_canvas_name ~(preserve_history : bool) :
    (unit, string) result =
  (* Ensure we can copy from and to - from_canvas must exist, to_canvas must
   * not. Yes, this is potentially racy, if to_canvas gets created by user
   * before we finish this function. I think this is unlikely enough as to be
   * an acceptable risk - users would have to get their welcome to dark email,
   * reset their password, and log in, before we finish running clone_canvas.
   * *)
  ( match
      (id_for_name_option from_canvas_name, id_for_name_option to_canvas_name)
    with
  | None, _ ->
      Error
        (Printf.sprintf
           "Can't clone from %s, no  such canvas exists"
           from_canvas_name)
  | _, Some _ ->
      Error
        (Printf.sprintf
           "Can't clone onto %s, that canvas already exists"
           to_canvas_name)
  | Some _, None ->
      Ok (from_canvas_name, to_canvas_name) )
  |> Result.and_then ~f:(fun (from_canvas_name, _) ->
         (* Load from_canvas *)
         let from_canvas = load_all from_canvas_name [] in
         from_canvas |> Result.map_error (String.join ~sep:", "))
  |> Result.map (fun (from_canvas : canvas ref) ->
         (* Transform the ops - remove pre-savepoint ops and update hosts
          * (canvas names) in string literals *)
         let to_ops =
           !from_canvas.ops
           |> List.map ~f:(fun ((tlid, ops) : Types.tlid * Types.oplist) ->
                  (* We always "preserve history" for DBs because their ops are
                   * cumulative *)
                  if preserve_history
                     || Map.mem !from_canvas.dbs tlid
                     || Map.mem !from_canvas.deleted_dbs tlid
                  then (tlid, ops)
                  else (tlid, ops |> only_ops_since_last_savepoint))
           |> List.map ~f:(fun (tlid, ops) ->
                  let new_ops =
                    List.map
                      ops
                      ~f:
                        (update_hosts_in_op
                           ~old_host:from_canvas_name
                           ~new_host:to_canvas_name)
                  in
                  (tlid, new_ops))
         in
         let owner : Uuidm.t =
           Account.auth_domain_for to_canvas_name
           |> Account.id_of_username
           |> Option.value_exn
         in
         (* In a transaction, save the new ops into to_canvas *)
         Db.transaction ~name:"clone_canvas" (fun () ->
             (* fetch_canvas_id is what actually creates the canvas record,
              * which must preceed save_all *)
             Serialize.fetch_canvas_id owner to_canvas_name |> ignore ;
             let to_canvas : canvas ref =
               Canvas.init to_canvas_name (to_ops |> Op.tlid_oplists2oplist)
               |> Core_kernel__Result.map_error
                    ~f:(Core_kernel__.String.concat ~sep:", ")
               |> Core_kernel__Result.ok_or_failwith
             in
             save_all !to_canvas))
