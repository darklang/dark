open Core_kernel
open Libexecution
open Libcommon
open Util
open Types

(* Permission levels, scoped to a single auth_domain.
   These are stored in the database for granted access,
   or returned by the functions in this namespace to indicate a
   particular user's permissions for a particular auth_domain.

   We often use `permission option' to include the case where
   a user has no special access to a particular auth_domain.
*)

type permission =
  | Read
  | ReadWrite
[@@deriving eq, show, ord]

(* Order matters here -- the derived ord will
   go in order, and None < Some anything, so
   None < Some Read < Some ReadWrite. *)

let permission_to_db p =
  match p with Read -> Db.String "r" | ReadWrite -> Db.String "rw"


let permission_of_db p =
  match p with
  | "r" ->
      Read
  | "rw" ->
      ReadWrite
  | _ ->
      Exception.internal "couldn't decode permission"


let set_user_access (user : Uuidm.t) (org : Uuidm.t) (p : permission option) :
    unit =
  match p with
  | None ->
      Db.run
        ~name:"set_user_access"
        "DELETE from access
        WHERE access.access_account = $1 AND access.organization_account = $2"
        ~params:[Uuid user; Uuid org] ;
      ()
  | Some p ->
      Db.run
        ~name:"set_user_access"
        "INSERT into access
        (access_account, organization_account, permission)
        VALUES
        ($1, $2, $3)
        ON CONFLICT (access_account, organization_account) DO UPDATE SET permission = EXCLUDED.permission"
        ~params:[Uuid user; Uuid org; permission_to_db p] ;
      ()


(* If a user has a DB row indicating granted access to this auth_domain,
   find it. *)
let granted_permission ~(username : Account.username) ~(auth_domain : string) :
    permission option =
  Db.fetch
    ~name:"check_access"
    "SELECT permission FROM access
       INNER JOIN accounts user_ ON access.access_account = user_.id
       INNER JOIN accounts org ON access.organization_account = org.id
       WHERE org.username = $1 AND user_.username = $2"
    ~params:[String auth_domain; String username]
  |> List.hd
  |> Option.bind ~f:List.hd
  |> Option.map ~f:permission_of_db


(* If a user is an admin they get write on everything. *)
let admin_permission ~(username : Account.username) =
  if Account.is_admin ~username then Some ReadWrite else None


(* We special-case some users, so they have access to particular shared auth_domains *)
let special_cases =
  [ ("pixelkeet", "laxels")
  ; ("rootvc", "adam")
  ; ("rootvc", "lee")
  ; ("talkhiring", "harris")
  ; ("talkhiring", "anson") ]


let special_case_permission
    ~(username : Account.username) ~(auth_domain : string) =
  if Tablecloth.List.any special_cases ~f:(fun (dom, user) ->
         String.Caseless.equal dom auth_domain
         && String.Caseless.equal user username )
  then Some ReadWrite
  else None


(* People should have access to the auth_domains under their name *)
let match_permission ~(username : Account.username) ~(auth_domain : string) =
  if String.Caseless.equal username auth_domain then Some ReadWrite else None


(* Everyone should have access to 'sample'. Soon (when we have UI support for read-only canvases)
   this will be read-only access. *)
let sample_permission ~(auth_domain : string) =
  if String.Caseless.equal "sample" auth_domain then Some Read else None


(* What's the highest level of access a particular user has to a
   particular auth_domain? *)
let permission ~(auth_domain : string) ~(username : Account.username) =
  (* Return the greatest `permission option` of a set of functions producing
   `permission option`, lazily, so we don't hit the db unnecessarily *)
  let max_permission_f fs =
    List.fold_left fs ~init:None ~f:(fun p f ->
        match p with Some ReadWrite -> p | _ -> max p (f ()) )
  in
  max_permission_f
    (* These first three don't hit the db, so do them first. *)
    [ (fun _ -> match_permission ~username ~auth_domain)
    ; (fun _ -> special_case_permission ~username ~auth_domain)
    ; (fun _ -> sample_permission ~auth_domain)
    ; (fun _ -> granted_permission ~username ~auth_domain)
    ; (fun _ -> admin_permission ~username) ]


let can_edit_canvas ~(canvas : string) ~(username : Account.username) : bool =
  let auth_domain = Account.auth_domain_for canvas in
  permission ~auth_domain ~username = Some ReadWrite


let can_view_canvas ~(canvas : string) ~(username : Account.username) : bool =
  let auth_domain = Account.auth_domain_for canvas in
  permission ~auth_domain ~username >= Some Read
